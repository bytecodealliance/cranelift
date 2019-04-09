//! Delay slot filling.

use crate::binemit::CodeOffset;
use crate::cursor::{Cursor, FuncCursor};
use crate::ir::{Function, InstructionData, Opcode};
use crate::isa::{EncInfo, TargetIsa};
use crate::iterators::IteratorExtras;
use crate::regalloc::RegDiversions;
use crate::timing;
use crate::CodegenResult;
use log::debug;

/// Fill delay slots and calculate code size.
pub fn fill_delay_slots(func: &mut Function, isa: &TargetIsa) -> CodegenResult<CodeOffset> {
    let _tt = timing::fill_delay_slots();

    let encinfo = isa.encoding_info();

    // Clear all offsets so we can recognize EBBs that haven't been visited yet.
    func.offsets.clear();
    func.offsets.resize(func.dfg.num_ebbs());

    let mut offset = 0;
    let mut divert = RegDiversions::new();

    // Fill the delay slots.
    {
        // Visit all instructions in layout order.
        let mut cur = FuncCursor::new(func);
        while let Some(ebb) = cur.next_ebb() {
            divert.clear();

            while let Some(inst) = cur.next_inst() {
                divert.apply(&cur.func.dfg[inst]);

                let enc = cur.func.encodings[inst];

                if cur.func.dfg[inst].opcode().has_delay_slot() {
                    isa.fill_delay_slot_for_inst(&mut cur, &divert, &encinfo);
                    continue;
                }

                offset += encinfo.byte_size(enc, inst, &divert, &cur.func);
            }
        }
    }

    // Fix up the offsets.
    {
        let mut cur = FuncCursor::new(func);
        offset = 0;
        while let Some(ebb) = cur.next_ebb() {
            divert.clear();
            cur.func.offsets[ebb] = offset;
            while let Some(inst) = cur.next_inst() {
                divert.apply(&cur.func.dfg[inst]);
                let enc = cur.func.encodings[inst];
                offset += encinfo.byte_size(enc, inst, &divert, &cur.func);
            }
        }
    }

    for (jt, jt_data) in func.jump_tables.iter() {
        func.jt_offsets[jt] = offset;
        // TODO: this should be computed based on the min size needed to hold
        //        the furthest branch.
        offset += jt_data.len() as u32 * 4;
    }

    Ok(offset)
}
