//! CLI tool to compile Cranelift IR files to native code in memory and execute them.

use crate::utils::read_to_string;
use core::mem;
use cranelift_codegen::binemit::{Reloc, Stackmap};
use cranelift_codegen::ir::{ExternalName, Function, JumpTable};
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::Context;
use cranelift_codegen::{binemit, ir};
use cranelift_native::builder as host_isa_builder;
use cranelift_reader::{parse_test, Details, IsaSpec};
use mmap::{MapOption, MemoryMap};
use region;
use region::Protection;
use std::path::PathBuf;
use walkdir::{DirEntry, WalkDir};

pub fn run(files: Vec<String>, flag_print: bool) -> Result<(), String> {
    let mut total = 0;
    let mut errors = 0;
    for file in iterate_files(files) {
        total += 1;
        match run_single_file(&file) {
            Ok(_) => {
                if flag_print {
                    println!("{}", file.to_string_lossy());
                }
            }
            Err(e) => {
                if flag_print {
                    println!("{}: {}", file.to_string_lossy(), e);
                }
                errors += 1;
            }
        }
    }

    if flag_print {
        match total {
            0 => println!("0 files"),
            1 => println!("1 file"),
            n => println!("{} files", n),
        }
    }

    match errors {
        0 => Ok(()),
        1 => Err(String::from("1 failure")),
        n => Err(format!("{} failures", n)),
    }
}

/// Iterate over all of the files passed as arguments, recursively iterating through directories
fn iterate_files(files: Vec<String>) -> impl Iterator<Item = PathBuf> {
    files
        .into_iter()
        .flat_map(WalkDir::new)
        .filter(|f| match f {
            Ok(d) => !is_hidden(d) && !is_directory(d),
            Err(e) => {
                println!("Unable to read file: {}", e);
                false
            }
        })
        .map(|f| {
            f.expect("This should not happen: we have already filtered out the errors")
                .into_path()
        })
}
fn is_hidden(d: &DirEntry) -> bool {
    d.file_name().to_str().map_or(false, |s| s.starts_with("."))
}
fn is_directory(d: &DirEntry) -> bool {
    d.file_type().is_dir()
}

/// Run all functions in a file that are succeeded by "run:" comments
fn run_single_file(path: &PathBuf) -> Result<(), String> {
    let file_contents = read_to_string(&path).map_err(|e| e.to_string())?;
    run_file_contents(file_contents)
}

/// Main body of `run_single_file` separated for testing
fn run_file_contents(file_contents: String) -> Result<(), String> {
    let test_file = parse_test(&file_contents, None, None).map_err(|e| e.to_string())?;
    let isa = create_target_isa(test_file.isa_spec)?;
    for (func, Details { comments, .. }) in test_file.functions {
        if comments.iter().filter(|c| c.text.contains("run")).count() > 0 {
            run_single_function(func, isa.as_ref())?
        }
    }
    Ok(())
}

/// Build an ISA based on the current machine running this code (the host)
fn create_target_isa(isa_spec: IsaSpec) -> Result<Box<dyn TargetIsa>, String> {
    if let IsaSpec::None(flags) = isa_spec {
        // build an ISA for the current machine
        let builder = host_isa_builder()?;
        Ok(builder.finish(flags))
    } else {
        Err(String::from("A target ISA was specified in the file but should not have been--only the host ISA can be used for running CLIF files"))?
    }
}

/// Compile and execute a single function, expecting a boolean to be returned; a 'true' value is
/// interpreted as a successful test execution and mapped to Ok whereas a 'false' value is
/// interpreted as a failed test and mapped to Err.
fn run_single_function(func: Function, isa: &dyn TargetIsa) -> Result<(), String> {
    if !(func.signature.params.is_empty()
        && func.signature.returns.len() == 1
        && func.signature.returns.first().unwrap().value_type.is_bool())
    {
        return Err(String::from(
            "Functions must have a signature like: () -> boolean",
        ));
    }
    // TODO observed segfaults with return types of b64

    // set up the context
    let mut context = Context::new();
    context.func = func;

    // compile and encode the result to machine code
    let relocs = &mut NullRelocSink {};
    let traps = &mut NullTrapSink {};
    let stackmaps = &mut NullStackmapSink {};
    let code_info = context.compile(isa).map_err(|e| e.to_string())?;
    let code_page = MemoryMap::new(code_info.total_size as usize, &[MapOption::MapWritable])
        .map_err(|e| e.to_string())?;
    let callable_fn: fn() -> bool = unsafe {
        context.emit_to_memory(isa, code_page.data(), relocs, traps, stackmaps);
        region::protect(code_page.data(), code_page.len(), Protection::ReadExecute)
            .map_err(|e| e.to_string())?;
        mem::transmute(code_page.data())
    };

    // execute
    match callable_fn() {
        true => Ok(()),
        false => Err(format!("Failed: {}", context.func.name.to_string())),
    }
}

/// Relocation helper
struct NullRelocSink {}

impl binemit::RelocSink for NullRelocSink {
    fn reloc_ebb(&mut self, _: u32, _: Reloc, _: u32) {}
    fn reloc_external(&mut self, _: u32, _: Reloc, _: &ExternalName, _: i64) {}
    fn reloc_jt(&mut self, _: u32, _: Reloc, _: JumpTable) {}
}

/// Trap helper
struct NullTrapSink {}

impl binemit::TrapSink for NullTrapSink {
    fn trap(&mut self, _: binemit::CodeOffset, _: ir::SourceLoc, _: ir::TrapCode) {}
}

/// Stack helper
struct NullStackmapSink {}

impl binemit::StackmapSink for NullStackmapSink {
    fn add_stackmap(&mut self, _: u32, _: Stackmap) {}
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn nop() {
        let code = String::from(
            "
            function %test() -> b8 system_v {
            ebb0:
                nop
                v1 = bconst.b8 true
                return v1
            }
            
            ; run
            ",
        );
        run_file_contents(code).unwrap()
    }
}
