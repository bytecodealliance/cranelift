//! CLI tool to read Cranelift IR files and compile them into native code.

use crate::disasm::{PrintRelocs, PrintTraps};
use crate::utils::{parse_sets_and_triple, read_to_string};
use cranelift_codegen::ir::{Ebb, Function, Inst, InstBuilder, TrapCode};
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::settings::FlagsOrIsa;
use cranelift_codegen::timing;
use cranelift_codegen::Context;
use cranelift_reader::parse_test;
use std::path::Path;
use std::path::PathBuf;

pub fn run(filename: &str, flag_set: &[String], flag_isa: &str) -> Result<(), String> {
    let parsed = parse_sets_and_triple(flag_set, flag_isa)?;

    let path = Path::new(&filename);
    let name = String::from(path.as_os_str().to_string_lossy());
    handle_module(&path.to_path_buf(), &name, parsed.as_fisa())
}

fn handle_module(path: &PathBuf, name: &str, fisa: FlagsOrIsa) -> Result<(), String> {
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
    /// Try to replace inst with iconst
    ReplaceInstWithIconst(Ebb, Inst),
    /// Try to replace inst with trap
    ReplaceInstWithTrap(Ebb, Inst),
    /// Try to remove an ebb
    RemoveEbb(Ebb),
}

fn next_inst_ret_prev(func: &Function, ebb: &mut Ebb, inst: &mut Inst) -> Option<Inst> {
    loop {
        if let Some(next_inst) = func.layout.next_inst(*inst) {
            let prev_inst = *inst;
            *inst = next_inst;
            return Some(prev_inst);
        } else if let Some(next_ebb) = func.layout.next_ebb(*ebb) {
            *ebb = next_ebb;
            *inst = func.layout.first_inst(*ebb).unwrap();
            continue;
        } else {
            return None;
        }
    }
}

fn reduce(isa: &TargetIsa, mut func: Function) {
    'outer_loop: for pass_idx in 0..100 {
        let mut was_reduced = false;
        let first_ebb = func.layout.entry_block().unwrap();
        let mut phase = Phase::RemoveInst(first_ebb, func.layout.first_inst(first_ebb).unwrap());

        'inner_loop: for _ in 0..1000 {
            let mut func2 = func.clone();

            let msg = match phase {
                Phase::RemoveInst(ref mut ebb, ref mut inst) => {
                    if let Some(prev_inst) = next_inst_ret_prev(&func2, ebb, inst) {
                        func2.layout.remove_inst(prev_inst);
                        format!("Remove inst {}", prev_inst)
                    } else {
                        phase = Phase::ReplaceInstWithIconst(
                            first_ebb,
                            func2.layout.first_inst(first_ebb).unwrap(),
                        );
                        continue 'inner_loop;
                    }
                }
                Phase::ReplaceInstWithIconst(ref mut ebb, ref mut inst) => {
                    if let Some(prev_inst) = next_inst_ret_prev(&func2, ebb, inst) {
                        let results = func2.dfg.inst_results(prev_inst);
                        if results.len() == 1 {
                            let ty = func2.dfg.value_type(results[0]);
                            func2.dfg.replace(prev_inst).iconst(ty, 0);
                            format!("Replace inst {} with iconst.{}", prev_inst, ty)
                        } else {
                            continue 'inner_loop; // No change, continue with next instruction
                        }
                    } else {
                        phase = Phase::ReplaceInstWithTrap(
                            first_ebb,
                            func2.layout.first_inst(first_ebb).unwrap(),
                        );
                        continue 'inner_loop;
                    }
                }
                Phase::ReplaceInstWithTrap(ref mut ebb, ref mut inst) => {
                    if let Some(prev_inst) = next_inst_ret_prev(&func2, ebb, inst) {
                        func2.dfg.replace(prev_inst).trap(TrapCode::User(0));
                        format!("Replace inst {} with trap", prev_inst)
                    } else {
                        phase = Phase::RemoveEbb(first_ebb);
                        continue 'inner_loop;
                    }
                }
                Phase::RemoveEbb(ref mut ebb) => {
                    let prev_ebb = *ebb;
                    if let Some(next_ebb) = func2.layout.next_ebb(*ebb) {
                        *ebb = next_ebb;
                        func2.layout.remove_ebb(*ebb);
                        format!("Remove ebb {}", prev_ebb)
                    } else {
                        break 'inner_loop;
                    }
                }
            };

            print!("{}: ", msg);

            match check_for_crash(isa, &func2) {
                Res::Succeed => {
                    // Shrinking didn't hit the problem anymore, discard changes.
                    println!("succeeded");
                    continue;
                }
                Res::Verifier(err) => {
                    // Shrinking produced invalid clif, discard changes.
                    println!("verifier error {:?}", err);
                    continue;
                }
                Res::Panic => {
                    // Panic remained while shrinking, make changes definitive.
                    was_reduced = true;
                    func = func2;
                    println!("{}: shrink", msg);
                }
            }
        }

        println!("Pass {} finished", pass_idx);

        if !was_reduced {
            // No new shrinking opportunities have been found this pass. This means none will ever
            // be found. Skip the rest of the passes over the function.
            break 'outer_loop;
        } else {
            println!("Next pass");
        }
    }

    println!("{}", func);
}

enum Res {
    Succeed,
    Verifier(String),
    Panic,
}

fn check_for_crash(isa: &TargetIsa, func: &Function) -> Res {
    let mut context = Context::new();
    context.func = func.clone();

    let mut relocs = PrintRelocs::new(false);
    let mut traps = PrintTraps::new(false);
    let mut mem = vec![];

    use std::io::Write;
    std::io::stdout().flush().unwrap(); // Flush stdout to sync with panic messages on stderr

    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        if let Err(err) = cranelift_codegen::verifier::verify_function(&func, isa) {
            Some(err)
        } else {
            None
        }
    })) {
        Ok(Some(err)) => return Res::Verifier(err.to_string()),
        Ok(None) => {}
        Err(err) => {
            // FIXME prevent verifier panic on removing ebb1
            return Res::Verifier(format!("verifier panicked: {:?}", err.downcast::<&'static str>()));
        }
    }

    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        if let Err(verifier_err) = context.compile_and_emit(isa, &mut mem, &mut relocs, &mut traps)
        {
            Res::Verifier(verifier_err.to_string())
        } else {
            Res::Succeed
        }
    })) {
        Ok(res) => res,
        Err(_panic) => Res::Panic,
    }
}
