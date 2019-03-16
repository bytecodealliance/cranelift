//! Generate binary emission code for each ISA.

use crate::cdsl::isa::{EncRecipe, OperandConstraint};
use crate::cdsl::formats::FormatRegistry;
use crate::error;
use crate::srcgen::Formatter;

/// Generate code to handle a single recipe.
///
/// - Unpack the instruction data, knowing the format.
/// - Determine register locations for operands with register constraints.
/// - Determine stack slot locations for operands with stack constraints.
/// - Call hand-written code for the actual emission.
fn gen_recipe(formats: &FormatRegistry, recipe: &EncRecipe, fmt: &mut Formatter) {
    let iform = formats.get(recipe.format);
    let nvops = iform.num_value_operands;
    let want_args = recipe.ins.iter().any(|c| match c {
        OperandConstraint::RegClass(_) | OperandConstraint::Stack(_) => true,
        OperandConstraint::Register(_) | OperandConstraint::InputReference(_) => false,
    });
    let want_outs = recipe.outs.iter().any(|c| match c {
        OperandConstraint::RegClass(_) | OperandConstraint::Stack(_) => true,
        OperandConstraint::Register(_) | OperandConstraint::InputReference(_) => false,
    });

    let is_regmove = ["RegMove", "RegSpill", "RegFill"].contains(&iform.name);

    fmtln!(fmt, "if let InstructionData::{} {{", iform.name);
    fmt.indent(|fmt| {
        fmt.line("opcode,");
        for f in &iform.imm_fields {
            fmtln!(fmt, "{},", f.member);
        }
        if want_args {
            if iform.has_value_list || nvops > 1 {
                fmt.line("ref args,");
            } else {
                fmt.line("arg,");
            }
            fmt.line("..");
            fmt.indent_pop();
            fmt.line("} = func.dfg[inst] {");
            fmt.indent_push();

            // Pass recipe arguments in this order: inputs, imm_fields, outputs.
            let mut args = String::new();

            // Pass recipe arguments in this order: inputs, imm_fields, outputs.
            if want_args && !is_regmove {
                if iform.has_value_list {
                    fmt.line("let args = args.as_slice(&func.dfg.value_lists);");
                } else if nvops == 1 {
                    fmt.line("let args = [arg];");
                }
                args += &unwrap_values(&recipe.ins, "in", "args", fmt);
            }

            for f in &iform.imm_fields {
                args += ", ";
                args += f.member;
            }

            // Unwrap interesting output arguments.
            if want_outs {
                if recipe.outs.len() == 1 {
                    fmt.line("let results = [func.dfg.first_result(inst)];")
                } else {
                    fmt.line("let results = func.dfg.inst_results(inst);");
                }
                args += &unwrap_values(&recipe.outs, "out", "results", fmt);
            }

            // Special handling for regmove instructions. Update the register
            // diversion tracker
            match &*iform.name {
                "RegMove" => fmt.line("divert.regmove(arg, src, dst);"),
                "RegSpill" => fmt.line("divert.regspill(arg, src, dst);"),
                "RegFill" => fmt.line("divert.regfill(arg, src, dst);"),
                _ => {}
            }

            if let Some(emit) = &recipe.emit {
                fmt.multi_line(emit);
                fmt.line("return;");
            } else {
                fmtln!(fmt, "return recipe_{}(func, inst, sink, bits{});", recipe.name.to_lowercase(), args)
            }
        }
    });
    fmt.line("}");
}

/// Emit code that unwraps values living in registers or stack slots.
///
/// :param args: Input or output constraints.
/// :param prefix: Prefix to be used for the generated local variables.
/// :param values: Name of slice containing the values to be unwrapped.
/// :returns: Comma separated list of the generated variables
fn unwrap_values(args: &[OperandConstraint], prefix: &str, values: &str, fmt: &mut Formatter) -> String {
    let mut varlist = String::new();
    for (i, cst) in args.iter().enumerate() {
        match cst {
            OperandConstraint::RegClass(_reg_class) => {
                let v = format!("{}_reg{}", prefix, i);
                varlist += ", ";
                varlist += &v;
                fmtln!(fmt, "let {} = divert.reg({}[{}], &func.locations);", v, values, i);

            }
            OperandConstraint::Stack(stack) => {
                let v = format!("{}_stk{}", prefix, i);
                varlist += ", ";
                varlist += &v;
                fmtln!(fmt, "let {} = StackRef::masked(", v);
                fmt.indent(|fmt| {
                    fmtln!(fmt, "divert.stack({}[{}], &func.locations),", values, i);
                    fmt.line(stack.stack_base_mask());
                    fmt.line("&func.stack_slots,");
                });
                fmt.line(").unwrap();");
            }
            _ => {}
        }
    }

    varlist
}

fn gen_isa(formats: &FormatRegistry, isa_name: &str, all_recipes: &[EncRecipe], fmt: &mut Formatter) {
    fmt.doc_comment(format!("Emit binary machine code for `inst` for the {} ISA", isa_name));
    fmt.line("#[allow(unused_variables, unreachable_code)]");
    fmt.line("pub fn emit_inst<CS: CodeSink + ?Sized>(");
    fmt.indent(|fmt| {
        fmt.line("func: &Function,");
        fmt.line("inst: Inst,");
        fmt.line("divert: &mut RegDiversions,");
        fmt.line("sink: &mut CS,");
    });
    fmt.line(") {");
    fmt.indent(|fmt| {
        if all_recipes.is_empty() {
            // No encoding recipes: Emit a stub.
            fmt.line("bad_encoding(func, inst);");
        } else {
            fmt.line("let encoding = func.encodings[inst]");
            fmt.line("let bits = encoding.bits();");
            fmt.line("match func.encodings[inst].recipe() {");
            fmt.indent(|fmt| {
                for (i, recipe) in all_recipes.iter().enumerate() {
                    fmt.comment(format!("Recipe {}", recipe.name));
                    fmtln!(fmt, "{} => {{", i);
                    fmt.indent(|fmt| {
                        gen_recipe(formats, recipe, fmt);
                    });
                    fmt.line("}");
                }
                fmt.line("_ => {},");
            });
            fmt.line("}");

            // Allow for un-encoded ghost instructions.
            // Verifier checks the details.
            fmt.line("if encoding.is_legal() {");
            fmt.indent(|fmt| {
                fmt.line("bad_encoding(func, inst);");
            });
            fmt.line("}");
        }
    });
    fmt.line("}");
}

// FIXME use it
#[allow(dead_code)]
pub fn generate(
    formats: &FormatRegistry,
    isa_name: &str,
    all_recipes: &[EncRecipe],
    binemit_filename: &str,
    out_dir: &str,
) -> Result<(), error::Error> {
    let mut fmt = Formatter::new();
    gen_isa(formats, isa_name, all_recipes, &mut fmt);
    fmt.update_file(binemit_filename, out_dir)?;

    Ok(())
}
