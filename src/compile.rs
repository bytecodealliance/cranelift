//! CLI tool to read Cranelift IR files and compile them into native code.

use crate::disasm::{print_bytes, print_disassembly, print_readonly_data, PrintRelocs, PrintTraps};
use crate::utils::{parse_sets_and_triple, read_to_string};
use cranelift_codegen::binemit;
use cranelift_codegen::print_errors::pretty_error;
use cranelift_codegen::settings::FlagsOrIsa;
use cranelift_codegen::timing;
use cranelift_codegen::Context;
use cranelift_reader::parse_test;
use std::path::Path;
use std::path::PathBuf;

pub fn run(
    files: Vec<String>,
    flag_print: bool,
    flag_disasm: bool,
    flag_report_times: bool,
    flag_set: &[String],
    flag_isa: &str,
) -> Result<(), String> {
    let parsed = parse_sets_and_triple(flag_set, flag_isa)?;

    for filename in files {
        let path = Path::new(&filename);
        let name = String::from(path.as_os_str().to_string_lossy());
        handle_module(
            flag_print,
            flag_disasm,
            flag_report_times,
            &path.to_path_buf(),
            &name,
            parsed.as_fisa(),
        )?;
    }
    Ok(())
}

fn handle_module(
    flag_print: bool,
    flag_disasm: bool,
    flag_report_times: bool,
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

    for (func, _) in test_file.functions {
        let mut context = Context::new();
        context.func = func;

        // Compile and encode the result to machine code.
        let total_size = context
            .compile(isa)
            .map_err(|err| pretty_error(&context.func, Some(isa), err))?;

        let mut mem = vec![0; total_size as usize];
        let mut relocs = PrintRelocs { flag_print };
        let mut traps = PrintTraps { flag_print };
        let mut code_sink: binemit::MemoryCodeSink;
        unsafe {
            code_sink = binemit::MemoryCodeSink::new(mem.as_mut_ptr(), &mut relocs, &mut traps);
        }
        isa.emit_function_to_memory(&context.func, &mut code_sink);

        if flag_print {
            println!("{}", context.func.display(isa));
        }

        if flag_disasm {
            print_bytes(&mem);
            print_disassembly(isa, &mem[0..code_sink.code_size as usize])?;
            print_readonly_data(&mem[code_sink.code_size as usize..total_size as usize]);
        }
    }

    if flag_report_times {
        print!("{}", timing::take_current());
    }

    Ok(())
}
