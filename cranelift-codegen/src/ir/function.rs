//! Intermediate representation of a function.
//!
//! The `Function` struct defined in this module owns all of its extended basic blocks and
//! instructions.

use crate::binemit::CodeOffset;
use crate::entity::{PrimaryMap, SecondaryMap};
use crate::ir;
use crate::ir::builder::ReplaceBuilder;
use crate::ir::{DataFlowGraph, ExternalName, Layout, Signature};
use crate::ir::{
    Ebb, ExtFuncData, FuncRef, GlobalValue, GlobalValueData, Heap, HeapData, Inst, JumpTable,
    JumpTableData, SigRef, StackSlot, StackSlotData, Table, TableData,
};
use crate::ir::{EbbOffsets, InstEncodings, SourceLocs, StackSlots, ValueLocations};
use crate::ir::{JumpTableOffsets, JumpTables};
use crate::isa::{CallConv, EncInfo, Encoding, Legalize, TargetIsa};
use crate::regalloc::RegDiversions;
use crate::write::{write_function, write_operands};
use core::fmt;

/// A function.
///
/// Functions can be cloned, but it is not a very fast operation.
/// The clone will have all the same entity numbers as the original.
#[derive(Clone)]
pub struct Function {
    /// Name of this function. Mostly used by `.clif` files.
    pub name: ExternalName,

    /// Signature of this function.
    pub signature: Signature,

    /// Stack slots allocated in this function.
    pub stack_slots: StackSlots,

    /// Global values referenced.
    pub global_values: PrimaryMap<ir::GlobalValue, ir::GlobalValueData>,

    /// Heaps referenced.
    pub heaps: PrimaryMap<ir::Heap, ir::HeapData>,

    /// Tables referenced.
    pub tables: PrimaryMap<ir::Table, ir::TableData>,

    /// Jump tables used in this function.
    pub jump_tables: JumpTables,

    /// Data flow graph containing the primary definition of all instructions, EBBs and values.
    pub dfg: DataFlowGraph,

    /// Layout of EBBs and instructions in the function body.
    pub layout: Layout,

    /// Encoding recipe and bits for the legal instructions.
    /// Illegal instructions have the `Encoding::default()` value.
    pub encodings: InstEncodings,

    /// Location assigned to every value.
    pub locations: ValueLocations,

    /// Code offsets of the EBB headers.
    ///
    /// This information is only transiently available after the `binemit::relax_branches` function
    /// computes it, and it can easily be recomputed by calling that function. It is not included
    /// in the textual IR format.
    pub offsets: EbbOffsets,

    /// Code offsets of Jump Table headers.
    pub jt_offsets: JumpTableOffsets,

    /// Source locations.
    ///
    /// Track the original source location for each instruction. The source locations are not
    /// interpreted by Cranelift, only preserved.
    pub srclocs: SourceLocs,
}

impl Function {
    /// Create a function with the given name and signature.
    pub fn with_name_signature(name: ExternalName, sig: Signature) -> Self {
        Self {
            name,
            signature: sig,
            stack_slots: StackSlots::new(),
            global_values: PrimaryMap::new(),
            heaps: PrimaryMap::new(),
            tables: PrimaryMap::new(),
            jump_tables: PrimaryMap::new(),
            dfg: DataFlowGraph::new(),
            layout: Layout::new(),
            encodings: SecondaryMap::new(),
            locations: SecondaryMap::new(),
            offsets: SecondaryMap::new(),
            jt_offsets: SecondaryMap::new(),
            srclocs: SecondaryMap::new(),
        }
    }

    /// Clear all data structures in this function.
    pub fn clear(&mut self) {
        self.signature.clear(CallConv::Fast);
        self.stack_slots.clear();
        self.global_values.clear();
        self.heaps.clear();
        self.tables.clear();
        self.jump_tables.clear();
        self.dfg.clear();
        self.layout.clear();
        self.encodings.clear();
        self.locations.clear();
        self.offsets.clear();
        self.srclocs.clear();
    }

    /// Create a new empty, anonymous function with a Fast calling convention.
    pub fn new() -> Self {
        Self::with_name_signature(ExternalName::default(), Signature::new(CallConv::Fast))
    }

    /// Creates a jump table in the function, to be used by `br_table` instructions.
    pub fn create_jump_table(&mut self, data: JumpTableData) -> JumpTable {
        self.jump_tables.push(data)
    }

    /// Creates a stack slot in the function, to be used by `stack_load`, `stack_store` and
    /// `stack_addr` instructions.
    pub fn create_stack_slot(&mut self, data: StackSlotData) -> StackSlot {
        self.stack_slots.push(data)
    }

    /// Adds a signature which can later be used to declare an external function import.
    pub fn import_signature(&mut self, signature: Signature) -> SigRef {
        self.dfg.signatures.push(signature)
    }

    /// Declare an external function import.
    pub fn import_function(&mut self, data: ExtFuncData) -> FuncRef {
        self.dfg.ext_funcs.push(data)
    }

    /// Declares a global value accessible to the function.
    pub fn create_global_value(&mut self, data: GlobalValueData) -> GlobalValue {
        self.global_values.push(data)
    }

    /// Declares a heap accessible to the function.
    pub fn create_heap(&mut self, data: HeapData) -> Heap {
        self.heaps.push(data)
    }

    /// Declares a table accessible to the function.
    pub fn create_table(&mut self, data: TableData) -> Table {
        self.tables.push(data)
    }

    /// Return an object that can display this function with correct ISA-specific annotations.
    pub fn display<'a, I: Into<Option<&'a TargetIsa>>>(&'a self, isa: I) -> DisplayFunction<'a> {
        DisplayFunction(self, isa.into())
    }

    /// Find a presumed unique special-purpose function parameter value.
    ///
    /// Returns the value of the last `purpose` parameter, or `None` if no such parameter exists.
    pub fn special_param(&self, purpose: ir::ArgumentPurpose) -> Option<ir::Value> {
        let entry = self.layout.entry_block().expect("Function is empty");
        self.signature
            .special_param_index(purpose)
            .map(|i| self.dfg.ebb_params(entry)[i])
    }

    /// Get an iterator over the instructions in `ebb`, including offsets and encoded instruction
    /// sizes.
    ///
    /// The iterator returns `(offset, inst, size)` tuples, where `offset` if the offset in bytes
    /// from the beginning of the function to the instruction, and `size` is the size of the
    /// instruction in bytes, or 0 for unencoded instructions.
    ///
    /// This function can only be used after the code layout has been computed by the
    /// `binemit::relax_branches()` function.
    pub fn inst_offsets<'a>(&'a self, ebb: Ebb, encinfo: &EncInfo) -> InstOffsetIter<'a> {
        assert!(
            !self.offsets.is_empty(),
            "Code layout must be computed first"
        );
        InstOffsetIter {
            encinfo: encinfo.clone(),
            func: self,
            divert: RegDiversions::new(),
            encodings: &self.encodings,
            offset: self.offsets[ebb],
            iter: self.layout.ebb_insts(ebb),
        }
    }

    /// Wrapper around `encode` which assigns `inst` the resulting encoding.
    pub fn update_encoding(&mut self, inst: ir::Inst, isa: &TargetIsa) -> Result<(), Legalize> {
        self.encode(inst, isa).map(|e| self.encodings[inst] = e)
    }

    /// Wrapper around `TargetIsa::encode` for encoding an existing instruction
    /// in the `Function`.
    pub fn encode(&self, inst: ir::Inst, isa: &TargetIsa) -> Result<Encoding, Legalize> {
        isa.encode(&self, &self.dfg[inst], self.dfg.ctrl_typevar(inst))
    }

    /// Returns an object that displays `inst`.
    pub fn display_inst<'a, I: Into<Option<&'a TargetIsa>>>(
        &'a self,
        inst: Inst,
        isa: I,
    ) -> DisplayInst<'a> {
        DisplayInst(self, isa.into(), inst)
    }

    /// Create a `ReplaceBuilder` that will replace `inst` with a new instruction in place.
    pub fn replace(&mut self, inst: Inst) -> ReplaceBuilder {
        ReplaceBuilder::new(self, inst)
    }
}

/// Wrapper type capable of displaying a `Function` with correct ISA annotations.
pub struct DisplayFunction<'a>(&'a Function, Option<&'a TargetIsa>);

impl<'a> fmt::Display for DisplayFunction<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write_function(fmt, self.0, self.1)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write_function(fmt, self, None)
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write_function(fmt, self, None)
    }
}

/// Object that can display an instruction.
pub struct DisplayInst<'a>(&'a Function, Option<&'a TargetIsa>, Inst);

impl<'a> fmt::Display for DisplayInst<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let func = self.0;
        let isa = self.1;
        let inst = self.2;

        if let Some((first, rest)) = func.dfg.inst_results(inst).split_first() {
            write!(f, "{}", first)?;
            for v in rest {
                write!(f, ", {}", v)?;
            }
            write!(f, " = ")?;
        }

        let typevar = func.dfg.ctrl_typevar(inst);
        if typevar.is_invalid() {
            write!(f, "{}", func.dfg[inst].opcode())?;
        } else {
            write!(f, "{}.{}", func.dfg[inst].opcode(), typevar)?;
        }
        write_operands(f, func, isa, inst)
    }
}

/// Iterator returning instruction offsets and sizes: `(offset, inst, size)`.
pub struct InstOffsetIter<'a> {
    encinfo: EncInfo,
    divert: RegDiversions,
    func: &'a Function,
    encodings: &'a InstEncodings,
    offset: CodeOffset,
    iter: ir::layout::Insts<'a>,
}

impl<'a> Iterator for InstOffsetIter<'a> {
    type Item = (CodeOffset, ir::Inst, CodeOffset);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|inst| {
            self.divert.apply(&self.func.dfg[inst]);
            let byte_size =
                self.encinfo
                    .byte_size(self.encodings[inst], inst, &self.divert, self.func);
            let offset = self.offset;
            self.offset += byte_size;
            (offset, inst, byte_size)
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::ir::types;
    use crate::ir::{Function, InstructionData, Opcode, TrapCode, ValueDef};
    use std::string::ToString;

    #[test]
    fn make_inst() {
        let mut func = Function::new();

        let idata = InstructionData::UnaryImm {
            opcode: Opcode::Iconst,
            imm: 0.into(),
        };
        let inst = func.dfg.make_inst(idata);

        func.dfg.make_inst_results(inst, types::I32);
        assert_eq!(inst.to_string(), "inst0");
        assert_eq!(
            func.display_inst(inst, None).to_string(),
            "v0 = iconst.i32 0"
        );

        // Immutable reference resolution.
        {
            let immfunc = &func;
            let ins = &immfunc.dfg[inst];
            assert_eq!(ins.opcode(), Opcode::Iconst);
        }

        // Results.
        let val = func.dfg.first_result(inst);
        assert_eq!(func.dfg.inst_results(inst), &[val]);

        assert_eq!(func.dfg.value_def(val), ValueDef::Result(inst, 0));
        assert_eq!(func.dfg.value_type(val), types::I32);

        // Replacing results.
        assert!(func.dfg.value_is_attached(val));
        let v2 = func.dfg.replace_result(val, types::F64);
        assert!(!func.dfg.value_is_attached(val));
        assert!(func.dfg.value_is_attached(v2));
        assert_eq!(func.dfg.inst_results(inst), &[v2]);
        assert_eq!(func.dfg.value_def(v2), ValueDef::Result(inst, 0));
        assert_eq!(func.dfg.value_type(v2), types::F64);
    }

    #[test]
    fn no_results() {
        let mut func = Function::new();

        let idata = InstructionData::Trap {
            opcode: Opcode::Trap,
            code: TrapCode::User(0),
        };
        let inst = func.dfg.make_inst(idata);
        assert_eq!(func.display_inst(inst, None).to_string(), "trap user0");

        // Result slice should be empty.
        assert_eq!(func.dfg.inst_results(inst), &[]);
    }
}
