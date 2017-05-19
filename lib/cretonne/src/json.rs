//! Converting Cretonne IL to text.
//!
//! The `json` module provides the `json_function` function which converts an IL
//! `Function` to a JSON representation.

use ir::{Function, DataFlowGraph, Ebb, Inst, Value, ValueDef, Type};
use isa::{TargetIsa, RegInfo};
use std::fmt::{self, Result, Write};

fn comma(w: &mut Write, first: &mut bool) -> Result {
    if *first {
        *first = false;
        Ok(())
    } else {
        write!(w, ",")
    }
}

/// Write `func` to `w` as JSON text.
/// Use `isa` to emit ISA-dependent annotations.
pub fn json_function(w: &mut Write, func: &Function, isa: Option<&TargetIsa>) -> Result {
    let regs = isa.map(TargetIsa::register_info);
    let regs = regs.as_ref();

    write!(w, "{{")?;

    json_spec(w, func, regs)?;
    write!(w, "\"body\":[")?;
    let mut first = true;
    json_preamble(w, func, regs, &mut first)?;
    for ebb in &func.layout {
        comma(w, &mut first)?;
        json_ebb(w, func, isa, ebb)?;
    }
    write!(w, "]")?;
    write!(w, "}}")
}

// ====--------------------------------------------------------------------------------------====//
//
// Function spec.
//
// ====--------------------------------------------------------------------------------------====//

fn json_spec(w: &mut Write, func: &Function, regs: Option<&RegInfo>) -> Result {
    write!(w, "\"name\":\"{}\",", func.name)?;
    write!(w, "\"signature\":\"{}\",", func.signature.display(regs))
}

fn json_preamble(w: &mut Write,
                 func: &Function,
                 regs: Option<&RegInfo>,
                 first: &mut bool)
                 -> Result {
    for ss in func.stack_slots.keys() {
        comma(w, first)?;
        write!(w, "\"{} = {}\"", ss, func.stack_slots[ss])?;
    }

    // Write out all signatures before functions since function declarations can refer to
    // signatures.
    for sig in func.dfg.signatures.keys() {
        comma(w, first)?;
        write!(w,
               "\"{} = signature{}\"",
               sig,
               func.dfg.signatures[sig].display(regs))?;
    }

    for fnref in func.dfg.ext_funcs.keys() {
        comma(w, first)?;
        write!(w, "\"{} = {}\"", fnref, func.dfg.ext_funcs[fnref])?;
    }

    for jt in func.jump_tables.keys() {
        comma(w, first)?;
        write!(w, "\"{} = {}\"", jt, func.jump_tables[jt])?;
    }

    Ok(())
}

// ====--------------------------------------------------------------------------------------====//
//
// Basic blocks
//
// ====--------------------------------------------------------------------------------------====//

fn json_arg(w: &mut Write, func: &Function, arg: Value, first: &mut bool) -> Result {
    comma(w, first)?;
    write!(w, "\"{}: {}\"", arg, func.dfg.value_type(arg))
}

fn json_ebb_header(w: &mut Write, func: &Function, ebb: Ebb) -> Result {
    write!(w, "\"name\":\"{}\",", ebb)?;
    write!(w, "\"arguments\":[")?;

    let mut first = true;
    for arg in func.dfg.ebb_args(ebb) {
        json_arg(w, func, *arg, &mut first)?;
    }

    write!(w, "],")
}

fn json_ebb(w: &mut Write, func: &Function, isa: Option<&TargetIsa>, ebb: Ebb) -> Result {
    write!(w, "{{")?;
    json_ebb_header(w, func, ebb)?;
    write!(w, "\"instructions\":[")?;
    let mut first = true;
    for inst in func.layout.ebb_insts(ebb) {
        json_instruction(w, func, isa, inst, &mut first)?;
    }
    write!(w, "]")?;
    write!(w, "}}")
}


// ====--------------------------------------------------------------------------------------====//
//
// Instructions
//
// ====--------------------------------------------------------------------------------------====//

// Should `inst` be printed with a type suffix?
//
// Polymorphic instructions may need a suffix indicating the value of the controlling type variable
// if it can't be trivially inferred.
//
// TODO: Factor this out with write.rs' version.
//
fn type_suffix(func: &Function, inst: Inst) -> Option<Type> {
    let inst_data = &func.dfg[inst];
    let constraints = inst_data.opcode().constraints();

    if !constraints.is_polymorphic() {
        return None;
    }

    // If the controlling type variable can be inferred from the type of the designated value input
    // operand, we don't need the type suffix.
    if constraints.use_typevar_operand() {
        let ctrl_var = inst_data.typevar_operand(&func.dfg.value_lists).unwrap();
        let def_ebb = match func.dfg.value_def(ctrl_var) {
            ValueDef::Res(instr, _) => func.layout.inst_ebb(instr),
            ValueDef::Arg(ebb, _) => Some(ebb),
        };
        if def_ebb.is_some() && def_ebb == func.layout.inst_ebb(inst) {
            return None;
        }
    }

    let rtype = func.dfg.ctrl_typevar(inst);
    assert!(!rtype.is_void(),
            "Polymorphic instruction must produce a result");
    Some(rtype)
}

// Write out any value aliases appearing in `inst`.
fn json_value_aliases(w: &mut Write, func: &Function, inst: Inst, first: &mut bool) -> Result {
    for &arg in func.dfg.inst_args(inst) {
        let resolved = func.dfg.resolve_aliases(arg);
        if resolved != arg {
            comma(w, first)?;
            write!(w, "\"{} -> {}\"", arg, resolved)?;
        }
    }
    Ok(())
}

fn json_instruction(w: &mut Write,
                    func: &Function,
                    isa: Option<&TargetIsa>,
                    inst: Inst,
                    first: &mut bool)
                    -> Result {
    // Value aliases come out on lines before the instruction using them.
    json_value_aliases(w, func, inst, first)?;

    comma(w, first)?;
    write!(w, "{{")?;

    // Write out encoding info.
    if let Some(enc) = func.encodings.get(inst).cloned() {
        let mut s = String::with_capacity(16);
        write!(w, "\"encoding\":\"")?;
        if let Some(isa) = isa {
            write!(s, "[{}", isa.encoding_info().display(enc))?;
            // Write value locations, if we have them.
            if !func.locations.is_empty() {
                let regs = isa.register_info();
                for &r in func.dfg.inst_results(inst) {
                    write!(s, ",{}", func.locations.get_or_default(r).display(&regs))?
                }
            }
            write!(s, "]")?;
        } else {
            write!(s, "[{}]", enc)?;
        }
        write!(w, "\",")?;
    }

    write!(w, "\"instruction\":\"")?;

    // Write out the result values, if any.
    let mut has_results = false;
    for r in func.dfg.inst_results(inst) {
        if !has_results {
            has_results = true;
            write!(w, "{}", r)?;
        } else {
            write!(w, ", {}", r)?;
        }
    }
    if has_results {
        write!(w, " = ")?;
    }

    // Then the opcode, possibly with a '.type' suffix.
    let opcode = func.dfg[inst].opcode();

    match type_suffix(func, inst) {
        Some(suf) => write!(w, "{}.{}", opcode, suf)?,
        None => write!(w, "{}", opcode)?,
    }

    json_operands(w, &func.dfg, isa, inst)?;

    write!(w, "\"")?;

    json_successors(w, func, inst)?;

    write!(w, "}}")
}

/// Write the operands of `inst` to `w`.
fn json_operands(w: &mut Write,
                 dfg: &DataFlowGraph,
                 isa: Option<&TargetIsa>,
                 inst: Inst)
                 -> Result {
    let pool = &dfg.value_lists;
    use ir::instructions::InstructionData::*;
    match dfg[inst] {
        Nullary { .. } => Ok(()),
        Unary { arg, .. } => write!(w, " {}", arg),
        UnaryImm { imm, .. } => write!(w, " {}", imm),
        UnaryIeee32 { imm, .. } => write!(w, " {}", imm),
        UnaryIeee64 { imm, .. } => write!(w, " {}", imm),
        Binary { args, .. } => write!(w, " {}, {}", args[0], args[1]),
        BinaryImm { arg, imm, .. } => write!(w, " {}, {}", arg, imm),
        Ternary { args, .. } => write!(w, " {}, {}, {}", args[0], args[1], args[2]),
        MultiAry { ref args, .. } => {
            if args.is_empty() {
                Ok(())
            } else {
                write!(w, " {}", DisplayValues(args.as_slice(pool)))
            }
        }
        InsertLane { lane, args, .. } => write!(w, " {}, {}, {}", args[0], lane, args[1]),
        ExtractLane { lane, arg, .. } => write!(w, " {}, {}", arg, lane),
        IntCompare { cond, args, .. } => write!(w, " {} {}, {}", cond, args[0], args[1]),
        IntCompareImm { cond, arg, imm, .. } => write!(w, " {} {}, {}", cond, arg, imm),
        FloatCompare { cond, args, .. } => write!(w, " {} {}, {}", cond, args[0], args[1]),
        Jump {
            destination,
            ref args,
            ..
        } => {
            if args.is_empty() {
                write!(w, " {}", destination)
            } else {
                write!(w,
                       " {}({})",
                       destination,
                       DisplayValues(args.as_slice(pool)))
            }
        }
        Branch {
            destination,
            ref args,
            ..
        } => {
            let args = args.as_slice(pool);
            write!(w, " {}, {}", args[0], destination)?;
            if args.len() > 1 {
                write!(w, "({})", DisplayValues(&args[1..]))?;
            }
            Ok(())
        }
        BranchIcmp {
            cond,
            destination,
            ref args,
            ..
        } => {
            let args = args.as_slice(pool);
            write!(w, " {} {}, {}, {}", cond, args[0], args[1], destination)?;
            if args.len() > 2 {
                write!(w, "({})", DisplayValues(&args[2..]))?;
            }
            Ok(())
        }
        BranchTable { arg, table, .. } => write!(w, " {}, {}", arg, table),
        Call { func_ref, ref args, .. } => {
            write!(w, " {}({})", func_ref, DisplayValues(args.as_slice(pool)))
        }
        IndirectCall { sig_ref, ref args, .. } => {
            let args = args.as_slice(pool);
            write!(w,
                   " {}, {}({})",
                   sig_ref,
                   args[0],
                   DisplayValues(&args[1..]))
        }
        StackLoad { stack_slot, offset, .. } => write!(w, " {}{}", stack_slot, offset),
        StackStore {
            arg,
            stack_slot,
            offset,
            ..
        } => write!(w, " {}, {}{}", arg, stack_slot, offset),
        HeapLoad { arg, offset, .. } => write!(w, " {}{}", arg, offset),
        HeapStore { args, offset, .. } => write!(w, " {}, {}{}", args[0], args[1], offset),
        Load { flags, arg, offset, .. } => write!(w, "{} {}{}", flags, arg, offset),
        Store {
            flags,
            args,
            offset,
            ..
        } => write!(w, "{} {}, {}{}", flags, args[0], args[1], offset),
        RegMove { arg, src, dst, .. } => {
            if let Some(isa) = isa {
                let regs = isa.register_info();
                write!(w,
                       " {}, {} -> {}",
                       arg,
                       regs.display_regunit(src),
                       regs.display_regunit(dst))
            } else {
                write!(w, " {}, %{} -> %{}", arg, src, dst)
            }
        }

    }
}

/// Write the successors of `inst` to `w`.
fn json_successors(w: &mut Write, func: &Function, inst: Inst) -> Result {
    use ir::instructions::InstructionData::*;
    match func.dfg[inst] {
        Nullary { .. } => Ok(()),
        Unary { .. } => Ok(()),
        UnaryImm { .. } => Ok(()),
        UnaryIeee32 { .. } => Ok(()),
        UnaryIeee64 { .. } => Ok(()),
        Binary { .. } => Ok(()),
        BinaryImm { .. } => Ok(()),
        Ternary { .. } => Ok(()),
        MultiAry { .. } => Ok(()),
        InsertLane { .. } => Ok(()),
        ExtractLane { .. } => Ok(()),
        IntCompare { .. } => Ok(()),
        IntCompareImm { .. } => Ok(()),
        FloatCompare { .. } => Ok(()),
        Jump { destination, .. } => write!(w, ",\"successors\":[\"{}\"]", destination),
        Branch { destination, .. } => write!(w, ",\"successors\":[\"{}\"]", destination),
        BranchIcmp {
            cond: _,
            destination,
            ..
        } => write!(w, ",\"successors\":[\"{}\"]", destination),
        BranchTable { arg: _, table, .. } => {
            write!(w, ",\"successors\":[")?;
            let mut first = true;
            for (_, ebb) in func.jump_tables[table].entries() {
                comma(w, &mut first)?;
                write!(w, "\"{}\"", ebb)?;
            }
            write!(w, "]")
        }
        Call { .. } => Ok(()),
        IndirectCall { .. } => Ok(()),
        StackLoad { .. } => Ok(()),
        StackStore { .. } => Ok(()),
        HeapLoad { .. } => Ok(()),
        HeapStore { .. } => Ok(()),
        Load { .. } => Ok(()),
        Store { .. } => Ok(()),
        RegMove { .. } => Ok(()),
    }
}

/// Displayable slice of values.
struct DisplayValues<'a>(&'a [Value]);

impl<'a> fmt::Display for DisplayValues<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result {
        for (i, val) in self.0.iter().enumerate() {
            if i == 0 {
                write!(f, "{}", val)?;
            } else {
                write!(f, ", {}", val)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use ir::{Function, FunctionName, StackSlotData};
    use ir::types;

    #[test]
    fn basic() {
        let mut f = Function::new();
        assert_eq!(f.json_display(None).to_string(),
                   "{\"name\":\"\"\"\",\"signature\":\"()\",\"body\":[]}");

        f.name = FunctionName::new("foo".to_string());
        assert_eq!(f.json_display(None).to_string(),
                   "{\"name\":\"foo\",\"signature\":\"()\",\"body\":[]}");

        f.stack_slots.push(StackSlotData::new(4));
        assert_eq!(f.json_display(None).to_string(),
                   "{\"name\":\"foo\",\"signature\":\"()\",\"body\":[\"ss0 = stack_slot 4\"]}");

        let ebb = f.dfg.make_ebb();
        f.layout.append_ebb(ebb);
        assert_eq!(f.json_display(None).to_string(),
                   "{\"name\":\"foo\",\"signature\":\"()\",\"body\":".to_string() +
                   "[\"ss0 = stack_slot 4\",{\"name\":\"ebb0\"," +
                   "\"arguments\":[],\"instructions\":[]}]}");

        f.dfg.append_ebb_arg(ebb, types::I8);
        assert_eq!(f.json_display(None).to_string(),
                   "{\"name\":\"foo\",\"signature\":\"()\",\"body\":".to_string() +
                   "[\"ss0 = stack_slot 4\",{\"name\":\"ebb0\"," +
                   "\"arguments\":[\"v0: i8\"],\"instructions\":[]}]}");

        f.dfg.append_ebb_arg(ebb, types::F32.by(4).unwrap());
        assert_eq!(f.json_display(None).to_string(),
                   "{\"name\":\"foo\",\"signature\":\"()\",\"body\":".to_string() +
                   "[\"ss0 = stack_slot 4\",{\"name\":\"ebb0\"," +
                   "\"arguments\":[\"v0: i8\",\"v1: f32x4\"],\"instructions\":[]}]}");
    }
}
