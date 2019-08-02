//! CLI tool to reduce Cranelift IR files crashing during compilation.

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

use indicatif::{ProgressBar, ProgressDrawTarget, ProgressStyle};

pub fn run(
    filename: &str,
    flag_set: &[String],
    flag_isa: &str,
    verbose: bool,
) -> Result<(), String> {
    let parsed = parse_sets_and_triple(flag_set, flag_isa)?;
    let fisa = parsed.as_fisa();

    let path = Path::new(&filename).to_path_buf();

    let buffer = read_to_string(&path).map_err(|e| format!("{}: {}", filename, e))?;
    let test_file = parse_test(&buffer, None, None).map_err(|e| format!("{}: {}", filename, e))?;

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
        reduce(isa, func, verbose);
    }

    Ok(())
}

enum MutationKind {
    /// The mutation reduced the amount of instructions or ebbs.
    Shrinked,
    /// The mutation only changed an instruction. Performing another round of mutations may only
    /// reduce the test case if another mutation shrank the test case.
    Changed,
}

trait Mutator {
    fn name(&self) -> &'static str;

    fn mutation_count(&self, func: &Function) -> Option<usize>;

    fn mutate(&mut self, func: Function) -> Option<(Function, String, MutationKind)>;

    fn reduce(
        &mut self,
        ccc: &mut CrashCheckContext,
        mut func: Function,
        progress_bar_prefix: String,
        verbose: bool,
        should_keep_reducing: &mut bool,
    ) -> Function {
        let progress = ProgressBar::with_draw_target(
            self.mutation_count(&func).unwrap_or(0) as u64,
            ProgressDrawTarget::stdout(),
        );
        progress.set_style(
            ProgressStyle::default_bar().template("{bar:60} {prefix:40} {pos:>4}/{len:>4} {msg}"),
        );

        progress.set_prefix(&(progress_bar_prefix + &format!(" phase {}", self.name())));

        for _ in 0..10000 {
            progress.inc(1);

            let (mutated_func, msg, mutation_kind) = match self.mutate(func.clone()) {
                Some(res) => res,
                None => {
                    break;
                }
            };

            progress.set_message(&msg);

            match ccc.check_for_crash(&mutated_func) {
                CheckResult::Succeed => {
                    // Shrinking didn't hit the problem anymore, discard changes.
                    continue;
                }
                CheckResult::Crash(_) => {
                    // Panic remained while shrinking, make changes definitive.
                    func = mutated_func;
                    match mutation_kind {
                        MutationKind::Shrinked => {
                            *should_keep_reducing = true;
                            if verbose {
                                progress.println(format!("{}: shrink", msg));
                            }
                        }
                        MutationKind::Changed => {
                            if verbose {
                                progress.println(format!("{}: changed", msg));
                            }
                        }
                    }
                }
            }
        }

        progress.set_message("done");
        progress.finish();

        func
    }
}

/// Try to remove instructions.
struct RemoveInst {
    ebb: Ebb,
    inst: Inst,
}

impl RemoveInst {
    fn new(func: &Function) -> Self {
        let first_ebb = func.layout.entry_block().unwrap();
        let first_inst = func.layout.first_inst(first_ebb).unwrap();
        Self {
            ebb: first_ebb,
            inst: first_inst,
        }
    }
}

impl Mutator for RemoveInst {
    fn name(&self) -> &'static str {
        "remove inst"
    }

    fn mutation_count(&self, func: &Function) -> Option<usize> {
        Some(inst_count(func))
    }

    fn mutate(&mut self, mut func: Function) -> Option<(Function, String, MutationKind)> {
        if let Some((prev_ebb, prev_inst)) =
            next_inst_ret_prev(&func, &mut self.ebb, &mut self.inst)
        {
            func.layout.remove_inst(prev_inst);
            if func.layout.ebb_insts(prev_ebb).next().is_none() {
                // Make sure empty ebbs are removed, as `next_inst_ret_prev` depends on non empty ebbs
                func.layout.remove_ebb(prev_ebb);
                Some((
                    func,
                    format!("Remove inst {} and empty ebb {}", prev_inst, prev_ebb),
                    MutationKind::Shrinked,
                ))
            } else {
                Some((
                    func,
                    format!("Remove inst {}", prev_inst),
                    MutationKind::Shrinked,
                ))
            }
        } else {
            None
        }
    }
}

/// Try to replace instructions with `iconst`.
struct ReplaceInstWithIconst {
    ebb: Ebb,
    inst: Inst,
}

impl ReplaceInstWithIconst {
    fn new(func: &Function) -> Self {
        let first_ebb = func.layout.entry_block().unwrap();
        let first_inst = func.layout.first_inst(first_ebb).unwrap();
        Self {
            ebb: first_ebb,
            inst: first_inst,
        }
    }
}

impl Mutator for ReplaceInstWithIconst {
    fn name(&self) -> &'static str {
        "replace inst with iconst"
    }

    fn mutation_count(&self, func: &Function) -> Option<usize> {
        Some(inst_count(func))
    }

    fn mutate(&mut self, mut func: Function) -> Option<(Function, String, MutationKind)> {
        if let Some((_prev_ebb, prev_inst)) =
            next_inst_ret_prev(&func, &mut self.ebb, &mut self.inst)
        {
            let results = func.dfg.inst_results(prev_inst);
            if results.len() == 1 {
                let ty = func.dfg.value_type(results[0]);
                func.dfg.replace(prev_inst).iconst(ty, 0);
                Some((
                    func,
                    format!("Replace inst {} with iconst.{}", prev_inst, ty),
                    MutationKind::Changed,
                ))
            } else {
                Some((func, format!(""), MutationKind::Changed))
            }
        } else {
            None
        }
    }
}

/// Try to replace instructions with `trap`.
struct ReplaceInstWithTrap {
    ebb: Ebb,
    inst: Inst,
}

impl ReplaceInstWithTrap {
    fn new(func: &Function) -> Self {
        let first_ebb = func.layout.entry_block().unwrap();
        let first_inst = func.layout.first_inst(first_ebb).unwrap();
        Self {
            ebb: first_ebb,
            inst: first_inst,
        }
    }
}

impl Mutator for ReplaceInstWithTrap {
    fn name(&self) -> &'static str {
        "replace inst with trap"
    }

    fn mutation_count(&self, func: &Function) -> Option<usize> {
        Some(inst_count(func))
    }

    fn mutate(&mut self, mut func: Function) -> Option<(Function, String, MutationKind)> {
        if let Some((_prev_ebb, prev_inst)) =
            next_inst_ret_prev(&func, &mut self.ebb, &mut self.inst)
        {
            func.dfg.replace(prev_inst).trap(TrapCode::User(0));
            Some((
                func,
                format!("Replace inst {} with trap", prev_inst),
                MutationKind::Changed,
            ))
        } else {
            None
        }
    }
}

/// Try to remove an ebb.
struct RemoveEbb {
    ebb: Ebb,
}

impl RemoveEbb {
    fn new(func: &Function) -> Self {
        Self {
            ebb: func.layout.entry_block().unwrap(),
        }
    }
}

impl Mutator for RemoveEbb {
    fn name(&self) -> &'static str {
        "remove ebb"
    }

    fn mutation_count(&self, func: &Function) -> Option<usize> {
        Some(ebb_count(func))
    }

    fn mutate(&mut self, mut func: Function) -> Option<(Function, String, MutationKind)> {
        if let Some(next_ebb) = func.layout.next_ebb(self.ebb) {
            self.ebb = next_ebb;
            while let Some(inst) = func.layout.last_inst(self.ebb) {
                func.layout.remove_inst(inst);
            }
            func.layout.remove_ebb(self.ebb);
            Some((
                func,
                format!("Remove ebb {}", next_ebb),
                MutationKind::Shrinked,
            ))
        } else {
            None
        }
    }
}

fn next_inst_ret_prev(func: &Function, ebb: &mut Ebb, inst: &mut Inst) -> Option<(Ebb, Inst)> {
    let prev = (*ebb, *inst);
    if let Some(next_inst) = func.layout.next_inst(*inst) {
        *inst = next_inst;
        return Some(prev);
    } else if let Some(next_ebb) = func.layout.next_ebb(*ebb) {
        *ebb = next_ebb;
        *inst = func.layout.first_inst(*ebb).expect("no inst");
        return Some(prev);
    } else {
        return None;
    }
}

fn ebb_count(func: &Function) -> usize {
    func.layout.ebbs().count()
}

fn inst_count(func: &Function) -> usize {
    func.layout
        .ebbs()
        .map(|ebb| func.layout.ebb_insts(ebb).count())
        .sum()
}

fn resolve_aliases(func: &mut Function) {
    for ebb in func.layout.ebbs() {
        for inst in func.layout.ebb_insts(ebb) {
            func.dfg.resolve_aliases_in_arguments(inst);
        }
    }
}

fn reduce(isa: &TargetIsa, mut func: Function, verbose: bool) {
    let mut ccc = CrashCheckContext::new(isa);

    match ccc.check_for_crash(&func) {
        CheckResult::Succeed => {
            println!("Given function compiled successfully or gave an verifier error.");
            return;
        }
        CheckResult::Crash(_) => {}
    }

    let (orig_ebb_count, orig_inst_count) = (ebb_count(&func), inst_count(&func));

    resolve_aliases(&mut func);

    for pass_idx in 0..100 {
        let mut should_keep_reducing = false;
        let mut phase = 0;

        loop {
            let mut mutator = match phase {
                0 => Box::new(RemoveInst::new(&func)) as Box<dyn Mutator>,
                1 => Box::new(ReplaceInstWithIconst::new(&func)) as Box<dyn Mutator>,
                2 => Box::new(ReplaceInstWithTrap::new(&func)) as Box<dyn Mutator>,
                3 => Box::new(RemoveEbb::new(&func)) as Box<dyn Mutator>,
                _ => break,
            };

            func = mutator.reduce(
                &mut ccc,
                func,
                format!("pass {}", pass_idx),
                verbose,
                &mut should_keep_reducing,
            );

            phase += 1;
        }

        if !should_keep_reducing {
            // No new shrinking opportunities have been found this pass. This means none will ever
            // be found. Skip the rest of the passes over the function.
            break;
        }
    }

    match ccc.check_for_crash(&func) {
        CheckResult::Succeed => unreachable!("Used to crash, but doesn't anymore???"),
        CheckResult::Crash(crash_msg) => {
            println!("Crash message: {}", crash_msg);
        }
    }

    println!("\n{}", func);

    println!(
        "{} ebbs {} insts -> {} ebbs {} insts",
        orig_ebb_count,
        orig_inst_count,
        ebb_count(&func),
        inst_count(&func)
    );
}

struct CrashCheckContext<'a> {
    /// Cached `Context`, to prevent repeated allocation.
    context: Context,

    /// The target isa to compile for.
    isa: &'a TargetIsa,
}

fn get_panic_string(panic: Box<std::any::Any>) -> String {
    let panic = match panic.downcast::<&'static str>() {
        Ok(panic_msg) => panic_msg.to_owned(),
        Err(panic) => panic,
    };
    match panic.downcast::<String>() {
        Ok(panic_msg) => *panic_msg,
        Err(_) => "Box<Any>".to_owned(),
    }
}

enum CheckResult {
    /// The function compiled fine, or the verifier noticed an error.
    Succeed,

    /// The compilation of the function panicked.
    Crash(String)
}

impl<'a> CrashCheckContext<'a> {
    fn new(isa: &'a TargetIsa) -> Self {
        CrashCheckContext {
            context: Context::new(),
            isa,
        }
    }

    fn check_for_crash(&mut self, func: &Function) -> CheckResult {
        self.context.clear();
        self.context.func = func.clone();

        let mut relocs = PrintRelocs::new(false);
        let mut traps = PrintTraps::new(false);
        let mut mem = vec![];

        use std::io::Write;
        std::io::stdout().flush().unwrap(); // Flush stdout to sync with panic messages on stderr

        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        cranelift_codegen::verifier::verify_function(&func, self.isa).err()
    })) {
            Ok(Some(_)) => return CheckResult::Succeed,
            Ok(None) => {}
            // The verifier panicked. compiling it will probably give the same panic.
            // We treat it as succeeding to make it possible to reduce for the actual error.
            // FIXME prevent verifier panic on removing ebb1
            Err(_) => return CheckResult::Succeed,
        }

        let old_panic_hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {})); // silence panics

        let res = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _ = self.context.compile_and_emit(self.isa, &mut mem, &mut relocs, &mut traps);
        })) {
            Ok(()) => CheckResult::Succeed,
            Err(err) => CheckResult::Crash(get_panic_string(err)),
        };

        std::panic::set_hook(old_panic_hook);

        res
    }
}
