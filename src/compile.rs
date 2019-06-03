//! CLI tool to read Cranelift IR files and compile them into native code.

use crate::clif_shared::{check_translation, check_translation_preamble,
print_size, report_times, module_code_size};
use crate::disasm::{print_all, PrintRelocs, PrintStackmaps, PrintTraps};
use crate::utils::{parse_sets_and_triple, read_to_string};
use cranelift_codegen::print_errors::pretty_error;
use cranelift_codegen::settings::FlagsOrIsa;
use cranelift_codegen::Context;
use cranelift_reader::{parse_test, ParseOptions};
use std::path::Path;
use std::path::PathBuf;

pub fn run(
    files: Vec<String>,
    flag_print: bool,
    flag_disasm: bool,
    flag_report_times: bool,
    flag_set: &[String],
    flag_isa: &str,
    flag_print_size: bool,
    flag_check_translation: bool,
    flag_verbose: bool,
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
            flag_print_size,
            flag_check_translation,
            flag_verbose,
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
    flag_print_size: bool,
    flag_check_translation: bool,
    flag_verbose: bool,
) -> Result<(), String> {

    let buffer = read_to_string(&path).map_err(|e| format!("{}: {}", name, e))?;
    let test_file =
        parse_test(&buffer, ParseOptions::default()).map_err(|e| format!("{}: {}", name, e))?;

    // If we have an isa from the command-line, use that. Otherwise if the
    // file contains a unique isa, use that.
    let isa = if let Some(isa) = fisa.isa {
        isa
    } else if let Some(isa) = test_file.isa_spec.unique_isa() {
        isa
    } else {
        return Err(String::from("compilation requires a target isa"));
    };

    check_translation_preamble(flag_check_translation, flag_verbose);

    let mut total_module_code_size = 0;
    for (func, _) in test_file.functions {
        let mut context = Context::new();
        context.func = func;

        let mut relocs = PrintRelocs::new(flag_print);
        let mut traps = PrintTraps::new(flag_print);
        let mut stackmaps = PrintStackmaps::new(flag_print);
        let mut mem = vec![];

        if flag_check_translation {
            return check_translation(fisa, context);
        } else {

        // Compile and encode the result to machine code.
        let code_info = context
            .compile_and_emit(isa, &mut mem, &mut relocs, &mut traps, &mut stackmaps)
            .map_err(|err| pretty_error(&context.func, Some(isa), err))?;
		if flag_print_size {
            print_size(&code_info);

		    total_module_code_size += code_info.total_size;
		}
        if flag_print {
            println!("{}", context.func.display(isa));
        }

        if flag_disasm {
            print_all(
                isa,
                &mem,
                code_info.code_size,
                code_info.jumptables_size + code_info.rodata_size,
                &relocs,
                &traps,
                &stackmaps,
            )?;
        }
        }
    }

    module_code_size(flag_check_translation, flag_print_size, total_module_code_size);

    report_times(flag_report_times);

    Ok(())
}
