//! Code shared by CLI tools `src/compile.rs` and `src/wasm.rs`

use cranelift_codegen::print_errors::pretty_verifier_error;
use cranelift_codegen::binemit::CodeInfo;
use cranelift_codegen::settings::FlagsOrIsa;
use cranelift_codegen::Context;
use cranelift_codegen::timing;
use term;

macro_rules! vprint {
    ($x: expr, $($tts:tt)*) => {
        if $x {
            print!($($tts)*);
        }
    }
}

pub fn print_size(
    code: &CodeInfo
) {
    println!(
	    "Code total_size: {} bytes",
	    code.code_size,
    );
}

pub fn check_translation(
    fisa: FlagsOrIsa,
    context: Context
) -> Result<(), String> {
    if let Err(errors) = context.verify(fisa) {
        return Err(pretty_verifier_error(&context.func, fisa.isa, None, errors));
    }
    Ok(())
}

pub fn check_translation_preamble(
    flag_check_translation: bool,
    flag_verbose: bool
) {
    let mut term = term::stdout().unwrap();
    let _ = term.fg(term::color::MAGENTA);
    if flag_check_translation {
        vprint!(flag_verbose, "Checking... ");
    } else {
        vprint!(flag_verbose, "Compiling... ");
    }
    let _ = term.reset();
}

pub fn report_times(
    flag_report_times: bool
) {
    if flag_report_times {
        println!("{}", timing::take_current());
    }
}

pub fn module_code_size(
    flag_check_translation: bool,
    flag_print_size: bool,
    total_module_code_size: u32
) {
    if !flag_check_translation && flag_print_size {
        println!("Total module code size: {} bytes", total_module_code_size);
    }
}
