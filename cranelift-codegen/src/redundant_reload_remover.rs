//! A late-stage redundant-reload remover, that runs after registers have been
//! allocated and stack slots have been given specific offsets.

use crate::cursor::{Cursor, EncCursor};
use crate::dominator_tree::DominatorTree;
use crate::ir::{
    Ebb, Function, Inst, InstBuilder, InstructionData, Opcode, StackSlotData, Value, ValueLoc,
};
use crate::isa::{RegInfo, RegUnit, TargetIsa};

use std::vec::Vec;

/// The remover's state.
pub struct RedundantReloadRemover {
    // The running environment used to detect redundancies.  This is reinitialised at the start of
    // each EBB.
    env: AvailEnv,
}

impl RedundantReloadRemover {
    /// Create a new remover state.
    pub fn new() -> Self {
        Self {
            env: AvailEnv::new(0),
        }
    }

    /// Clear the state of the remover.
    pub fn clear(&mut self) {
        self.env.map.clear();
        self.env.size = 0;
    }

    // Initialise the state of the remover.
    pub fn init(&mut self, size: usize) {
        self.clear();
        for _ in 0..size {
            self.env.map.push(None);
        }
        self.env.size = size;
    }

    /// Run the remover.
    pub fn run(&mut self, isa: &dyn TargetIsa, func: &mut Function, domtree: &DominatorTree) {
        let mut ctx = Context {
            cur: EncCursor::new(func, isa),
            reginfo: isa.register_info(),
            domtree: domtree,
            state: &mut RedundantReloadRemover::new(),
        };
        ctx.run()
    }
}

struct Context<'a> {
    // Current instruction as well as reference to function and ISA.
    cur: EncCursor<'a>,

    // Cached ISA information.  We save it here to avoid frequent virtual function calls on the
    // `TargetIsa` trait object.
    reginfo: RegInfo,

    // References to contextual data structures we need.
    domtree: &'a DominatorTree,

    // The running state
    state: &'a mut RedundantReloadRemover,
}

impl<'a> Context<'a> {
    fn run(&mut self) {
        self.cleanup_func();
    }
}

// ======== A description of the redundant-fill-removal algorithm ========
//
// The simplest incarnation simply detects fills from the same stack slot into the same register,
// and removes them:
//
//  v1 = fill v0
//  ...
//  v2 = fill v0
//
// The second fill is redundant provided that
//   (1) v1 and v2 have the same register, and
//   (2) "..." overwrites neither that register nor v0's stack slot, and
//   (3) the second fill is dominated by the first.
// Condition (3) always holds since we're doing this only inside EBBs, but we have to check (1)
// and (2).
//
// In this example, the instruction processing loop below creates an environment entry on
// encountering "v1 = fill v0", and uses that entry to determine that "v2 = fill v0" is redundant.
//
// The loop must also, even in the simplest version, check all instructions for writes to
// registers and stack slots, and invalidate environment entries accordingly.
//
// === First refinement -- implemented
//
// The simplest version only observes that a register and stack slot have the same value when it
// finds a fill.  However, a spill also causes a register and stack slot to have the same value.
// Hence this first refinement is to also record such spill-created equivalences in the
// environment, in the hope of increasing our chances of proving later fills redundant.
//
// === Second refinement -- implemented
//
// For a fill, even if we can't find an exact match in the environment, finding a load of the same
// spill slot into a different register is still a win, since we can convert the fill into a
// reg-reg copy.
//
// === Other tricks that don't seem to help
//
// Tracking the effects of InstructionData::Copy in the environment for the case when both source
// and destination are registers.  But that didn't cause any more fills to be removed or converted
// to copies.  It's not clear why.
//
// It's also possible for a spill to be detected as redundant, if such a binding already appears
// in the environment.  This was also implemented, but it was almost entirely ineffective, so was
// removed again.

// This describes a spill slot in the obvious way.  It also tracks the SSA value name associated
// with the slot.
#[derive(Clone, Copy)]
struct SlotInfo {
    value: Value,
    offset: i32,
    size: u32,
}

// Find out if the range `[offset, +size)` overlaps with the range in `si`.
fn any_overlap(si: &SlotInfo, offset: i32, size: u32) -> bool {
    let a_offset = si.offset as i64;
    let a_size = si.size as i64;
    let b_offset = offset as i64;
    let b_size = size as i64;
    let no_overlap = a_offset + a_size <= b_offset || b_offset + b_size <= a_offset;
    !no_overlap
}

// Find, in `reginfo`, the register bank that `reg` lives in, and return the lower limit and size
// of the bank.  This is so the caller can conveniently iterate over all `RegUnits` in the bank
// that `reg` lives in.
fn find_bank_limits(reginfo: &RegInfo, reg: RegUnit) -> (RegUnit, u16) {
    if let Some(bank) = reginfo.bank_containing_regunit(reg) {
        return (bank.first_unit, bank.units);
    }
    // We should never get here, since `reg` must come from *some* RegBank.
    panic!("find_regclass_limits: reg not found");
}

// The redundant-fill remover processes each EBB independently.  It makes a single forward pass
// over each EBB, carrying along a running environment, `AvailEnv`, that maps `RegUnits` to an
// `Option<SlotInfo>`.
//
// If the triplet `(regunit, offset, size)` appears in the environment, this indicates that, at
// this point, `regunit` and the stack slot denoted by `(offset, size)` have the same value.  We
// also keep track of the SSA `Value` associated with the `reg`, because that is necessary to
// create `copy` instructions that read `reg`.
//
// Invariants for the environment:
//
// The environment may have multiple different registers bound to the same stack slot -- that is,
// `(offset, size)` pair.  That's OK, and reflects the reality that those two registers contain
// the same value.
//
// The `value` fields must however be unique: it isn't allowable to have two entries in the
// environment that have the same `value` field.  To do so would be to imply that that SSA value
// has two different locations, since each environment entry is associated with a different
// register, and so this would imply that the Value resided in both of those registers.  Function
// `invariants_ok` checks this.
//
// It is also the case that the set of `(offset, size)` segments in the environment must be
// non-overlapping.  `invariants_ok` doesn't check that, though.

struct AvailEnv {
    map: Vec<Option<SlotInfo>>,
    size: usize,
}

impl AvailEnv {
    // Create a new one.
    fn new(size: usize) -> Self {
        let mut env = AvailEnv {
            map: Vec::<Option<SlotInfo>>::new(),
            size: size,
        };
        env.map.resize(size, None);
        debug_assert!(env.map.len() == size);
        env
    }

    // Debug only: checks (some of) the required AvailEnv invariants.
    fn invariants_ok(&self) -> bool {
        if self.map.len() != self.size {
            return false;
        }
        // Check that the SSA names are unique.  This is super lame (quadratic), but it's only
        // used in debug builds.
        for i in 0..self.size {
            if let Some(si) = self.map[i] {
                for j in i + 1..self.size {
                    if let Some(sj) = self.map[j] {
                        if si.value == sj.value {
                            return false;
                        }
                    }
                }
            }
        }
        true
    }

    // Invalidates the binding associated with `reg`.  Note that by construction of `AvailEnv`,
    // `reg` can only be associated with one binding at once.
    fn invalidate_by_reg(&mut self, reg: RegUnit) {
        self.map[reg as usize] = None;
    }

    // Invalidates any binding characterised by `(offset, size)`.  Per discussion of invariants
    // above, `(offs, size)` can only be associated with one binding at once.
    fn invalidate_by_offset(&mut self, offset: i32, size: u32) {
        for i in 0..self.size {
            if let Some(si) = &self.map[i] {
                if any_overlap(&si, offset, size) {
                    self.map[i] = None;
                }
            }
        }
    }

    // Invalidates all bindings in the enviroment.
    fn invalidate_all(&mut self) {
        for i in 0..self.size {
            self.map[i] = None;
        }
    }

    // Updates the environment to track the effect of an `InstructionData::RegMove` instruction.
    // This moves a value from one register to another, but doesn't create a new SSA name.  Hence
    // we copy `src`'s entry to `dst` verbatim, without installing a new value in `dst`'s `Value`
    // field.
    fn move_reg(&mut self, src: RegUnit, dst: RegUnit) {
        self.map[dst as usize] = self.map[src as usize];
        self.map[src as usize] = None;
    }

    // Does `env` have the exact binding characterised by `(ref, offset, size)` ?
    fn has_exact_binding(&self, reg: RegUnit, offset: i32, size: u32) -> bool {
        if let Some(si) = &self.map[reg as usize] {
            return si.offset == offset && si.size == size;
        }
        // No such binding.
        false
    }

    // Does `env` have a binding characterised by `(offset, size)` but to a register, let's call
    // it `reg2`, that isn't `reg`?  If so, return `reg2` and its associated SSA name.  Note that
    // `reg2` will have the same bank as `reg`.  It is a checked error to call this function with
    // a binding matching all three of `(reg, offset, size)`.
    fn has_inexact_binding(
        &self,
        reginfo: &RegInfo,
        reg: RegUnit,
        offset: i32,
        size: u32,
    ) -> Option<(RegUnit, Value)> {
        // Find the range of RegUnit numbers for the bank that contains `reg`, and use that as our
        // search space.  This is so as to guarantee that any match is restricted to the same bank
        // as `reg`.
        let (first_unit, num_units) = find_bank_limits(reginfo, reg);
        for reg2 in first_unit..first_unit + num_units {
            if let Some(si) = &self.map[reg2 as usize] {
                if si.offset == offset && si.size == size {
                    if reg2 == reg {
                        panic!("has_inexact_binding: binding *is* exact!");
                    }
                    return Some((reg2, si.value));
                }
            }
        }
        // No such binding.
        None
    }

    // Create the binding `(reg, value, offset, size)` in `env`, and throw away any previous
    // binding associated with `reg`.
    fn bind(&mut self, reg: RegUnit, value: Value, offset: i32, size: u32) {
        debug_assert!(self.map.len() == self.size);
        self.map[reg as usize] = Some(SlotInfo {
            value,
            offset,
            size,
        });
    }
}

impl<'a> Context<'a> {
    /// Run the remover.
    fn cleanup_func(&mut self) {
        let mut total_regunits = 0;
        for ru in self.reginfo.banks {
            total_regunits += ru.units;
        }
        // Perform redundant-fill removal.  We can visit the EBBs in any order, since there's no
        // context carried across EBB boundaries at present.
        for &ebb in self.domtree.cfg_postorder().iter().rev() {
            self.cleanup_ebb(ebb, total_regunits);
        }
    }

    // Returns the regunit that `v` is allocated to.  Assumes that `v` actually resides in a
    // register.
    fn reg_of_value(&self, v: Value) -> RegUnit {
        match self.cur.func.locations[v] {
            ValueLoc::Reg(ru) => ru,
            _ => panic!("reg_of_value: value isn't in a reg"),
        }
    }

    // Returns the stack slot that `v` is allocated to.  Assumes that `v` actually resides in a
    // stack slot.
    fn slot_of_value(&self, v: Value) -> &StackSlotData {
        match self.cur.func.locations[v] {
            ValueLoc::Stack(slot) => &self.cur.func.stack_slots[slot],
            _ => panic!("slot_of_value: value isn't in a stack slot"),
        }
    }

    // Invalidates any binding associated with a regunit that is written by `inst`.
    fn invalidate_regs_written_by_inst(&mut self, inst: Inst) {
        let dst_vals: &[Value] = self.cur.func.dfg.inst_results(inst);
        for v in dst_vals.iter() {
            if let ValueLoc::Reg(ru) = self.cur.func.locations[*v] {
                self.state.env.invalidate_by_reg(ru);
            }
        }
    }

    // This is the main EBB processing function.  It carries an `AvailEnv` forwards along the
    // block, using it to detect redundant fills.
    fn cleanup_ebb(&mut self, ebb: Ebb, total_regunits: u16) {
        self.state.init(total_regunits as usize);
        self.cur.goto_top(ebb);

        while let Some(inst) = self.cur.next_inst() {
            debug_assert!(
                self.state.env.invariants_ok(),
                "cleanup_ebb: env invariants not ok"
            );

            // To avoid difficulties with the borrow checker, do this in two stages.  First,
            // examine the instruction to see if it can deleted or modified, and park the relevant
            // information in `replacement`.  Update `env` too.  Later, use `replacement` to
            // actually do the transformation if necessary.
            let mut replacement: Option<(bool, Value)> = None;

            match &self.cur.func.dfg[inst] {
                InstructionData::Unary {
                    opcode: Opcode::Spill,
                    arg: src_value,
                } => {
                    // Extract: (src_reg, src_value, offset, size)
                    // Invalidate: (offset, size)
                    // Add new: {src_reg -> (src_value, offset, size)}
                    let slot = self.slot_of_value(self.cur.func.dfg.inst_results(inst)[0]);
                    let src_reg = self.reg_of_value(*src_value);
                    if let Some(offset) = slot.offset {
                        let size = slot.size;
                        self.state.env.invalidate_by_offset(offset, size);
                        self.state.env.bind(src_reg, *src_value, offset, size);
                    } else {
                        panic!("cleanup_ebb: spill with no offset");
                    }
                }
                InstructionData::Unary {
                    opcode: Opcode::Fill,
                    arg: src_value,
                } => {
                    // Extract: (dst_value, dst_regunit, offset, size)
                    // Invalidate: (offset, size)
                    // Add new: {dst_regunit -> (dst_value, offset, size)}
                    let slot = self.slot_of_value(*src_value);
                    let dst_value = self.cur.func.dfg.inst_results(inst)[0];
                    let dst_reg = self.reg_of_value(dst_value);
                    if let Some(offset) = slot.offset {
                        let size = slot.size;
                        if self.state.env.has_exact_binding(dst_reg, offset, size) {
                            // This instruction is an exact copy of a fill we saw earlier, and the
                            // loaded value is still valid.  So we'll schedule this instruction
                            // for deletion (below).  We also update the binding so as to
                            // associate this instruction's result SSA name with it.
                            replacement = Some((true, *src_value));
                            self.state.env.bind(dst_reg, dst_value, offset, size);
                        } else if let Some((reg2, val2)) =
                            self.state
                                .env
                                .has_inexact_binding(&self.reginfo, dst_reg, offset, size)
                        {
                            // This fill is from the required slot, but into a different register
                            // `reg2`.  So replace it with a copy from `reg2` to `dst_reg` and
                            // update `dst_reg`s binding to make it the same as `reg2`'s, so as to
                            // maximise the chances of future matches after this instruction.
                            debug_assert!(reg2 != dst_reg);
                            replacement = Some((false, val2));
                            self.state.env.bind(dst_reg, dst_value, offset, size);
                        } else {
                            // This fill creates some new binding we don't know about.  Update
                            // `env` to track it.
                            self.state.env.invalidate_by_offset(offset, size);
                            self.state.env.bind(dst_reg, dst_value, offset, size);
                        }
                    } else {
                        panic!("cleanup_ebb: fill with no offset");
                    }
                }
                InstructionData::RegSpill { .. }
                | InstructionData::RegFill { .. }
                | InstructionData::Call { .. }
                | InstructionData::CallIndirect { .. } => {
                    // Play safe: forget everything we know.
                    self.state.env.invalidate_all();
                }
                InstructionData::RegMove {
                    opcode: _,
                    arg: _,
                    src,
                    dst,
                } => {
                    // Although this happens rarely, we must update the environment accordingly,
                    // because it changes the register associated with an SSA name, and subsequent
                    // fill-to-copy conversions will be incorrect if we don't do that.  It happens
                    // rarely enough that we could get away with merely invalidating the bindings
                    // for `src` and `dst`.  But it's easy enough to track the move, and for
                    // `wasm_zlib` it does in fact help to do so, so let's do this properly.
                    self.state.env.move_reg(*src, *dst);
                }
                _ => {
                    // Invalidate: any env entry associated with a reg written by `inst`.
                    self.invalidate_regs_written_by_inst(inst);
                }
            }

            // Actually do the transformation.
            match replacement {
                Some((true, arg)) => {
                    // Load is completely redundant.  Convert it to a no-op.
                    self.cur.func.dfg.replace(inst).fill_nop(arg);
                    let ok = self.cur.func.update_encoding(inst, self.cur.isa).is_ok();
                    debug_assert!(ok, "fill_nop encoding missing for this type");
                }
                Some((false, arg)) => {
                    // We already have the relevant value in some other register.  Convert the
                    // load into a reg-reg copy.
                    self.cur.func.dfg.replace(inst).copy(arg);
                    let ok = self.cur.func.update_encoding(inst, self.cur.isa).is_ok();
                    debug_assert!(ok, "copy encoding missing for this type");
                }
                _ => (),
            }
        }
    }
}
