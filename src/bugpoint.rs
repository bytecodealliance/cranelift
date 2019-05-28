//! CLI tool to read Cranelift IR files and compile them into native code.

use crate::disasm::{PrintRelocs, PrintTraps};
use crate::utils::{parse_sets_and_triple, read_to_string};
use cranelift_codegen::print_errors::pretty_error;
use cranelift_codegen::settings::FlagsOrIsa;
use cranelift_codegen::timing;
use cranelift_codegen::Context;
use cranelift_codegen::ir::{Ebb, Function, Inst, InstBuilder};
use cranelift_codegen::isa::TargetIsa;
use cranelift_reader::parse_test;
use std::path::Path;
use std::path::PathBuf;

pub fn run(
    filename: &str,
    flag_set: &[String],
    flag_isa: &str,
) -> Result<(), String> {
    let parsed = parse_sets_and_triple(flag_set, flag_isa)?;

    let path = Path::new(&filename);
    let name = String::from(path.as_os_str().to_string_lossy());
    handle_module(
        &path.to_path_buf(),
        &name,
        parsed.as_fisa(),
    )?;
    Ok(())
}

fn handle_module(
    path: &PathBuf,
    name: &str,
    fisa: FlagsOrIsa,
) -> Result<(), String> {
    let buffer = read_to_string(&path).map_err(|e| format!("{}: {}", name, e))?;
    let test_file = parse_test(&buffer, None, None).map_err(|e| format!("{}: {}", name, e))?;

    // If we have an isa from the command-line, use that. Otherwise if the
    // file contains a unique isa, use that.
    let isa = if let Some(isa) = fisa.isa {
        isa
    } else if let Some(isa) = test_file.isa_spec.unique_isa() {
        isa
    } else {
        return Err(String::from("compilation requires a target isa"));
    };

    std::env::set_var("RUST_BACKTRACE", "0"); // Disable backtraces to reduce verbosity

    for (func, _) in test_file.functions {
        reduce(isa, func);
    }

    //print!("{}", timing::take_current());

    Ok(())
}

/// This stores the current thing to reduce
enum Phase {
    /// Try to remove this instruction
    RemoveInst(Ebb, Inst),
}

fn reduce(isa: &TargetIsa, mut func: Function) {
    'outer_loop: for _ in 0..10 {
        let mut was_reduced = false;
        let first_ebb = func.layout.entry_block().unwrap();
        let mut phase = Phase::RemoveInst(first_ebb, func.layout.first_inst(first_ebb).unwrap());

        'inner_loop: for _ in 0..1000 {
            let mut func2 = func.clone();

            match phase {
                Phase::RemoveInst(ref mut ebb, ref mut inst) => {
                    if let Some(next_inst) = func2.layout.next_inst(*inst) {
                        print!("remove inst {}: ", inst);
                        //func2.dfg.replace(*inst).nop();
                        func2.layout.remove_inst(*inst);
                        *inst = next_inst;
                    } else if let Some(next_ebb) = func2.layout.next_ebb(*ebb) {
                        print!("next ebb {}: ", next_ebb);
                        *ebb = next_ebb;
                        *inst = func2.layout.first_inst(*ebb).unwrap();
                    } else {
                        break 'inner_loop;
                    }
                }
            }

            use std::io::Write;
            std::io::stdout().flush().unwrap(); // Flush stdout to sync with panic messages on stderr

            match check_for_crash(isa, &func2) {
                Res::Succeed => {
                    println!("succeeded");
                    continue;
                }
                Res::Verifier(err) => {
                    println!("verifier error {:?}", err);
                    continue;
                }
                Res::Panic => {
                    was_reduced = true;
                    func = func2;
                }
            }
        }

        if !was_reduced {
            break 'outer_loop;
        }
    }

    println!("{}", func);
}

enum Res {
    Succeed,
    Verifier(cranelift_codegen::CodegenError),
    Panic,
}

fn check_for_crash(isa: &TargetIsa, func: &Function) -> Res {
    let mut context = Context::new();
    context.func = func.clone();

    let mut relocs = PrintRelocs::new(false);
    let mut traps = PrintTraps::new(false);
    let mut mem = vec![];

    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        if let Err(verifier_err) = context.compile_and_emit(isa, &mut mem, &mut relocs, &mut traps) {
            Res::Verifier(verifier_err)
        } else {
            Res::Succeed
        }
    })) {
        Ok(res) => res,
        Err(_panic) => Res::Panic,
    }
}
