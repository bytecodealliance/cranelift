//! Generate sources with instruction info.
//! 
//! This generates the `opcodes.rs` file which is included in
//! `lib/codegen/ir/instructions.rs`. The file provides definitions of 
//! `InstructionFormat`, `InstructionData`, `Opcode`, `OPCODE_FORMAT`,
//! `opcode_name`, `OPCODE_HASH_TABLE`, `OPCODE_CONSTRAINTS`, `TYPE_SETS`,
//! and `OPERAND_CONSTRAINTS`.

use error;
use srcgen::Formatter;

/// Generate an instruction format enumeration.
fn gen_formats(fmt: &mut Formatter) -> Result<(), error::Error> {
    fmt.doc_comment("
        An instruction format

        Every opcode has a corresponding instruction format
        which is represented by both the `InstructionFormat`
        and the `InstructionData` enums.
    ");
    fmt.line("#[derive(Copy, Clone, PartialEq, Eq, Debug)]");
    fmt.indent_with("pub enum InstructionFormat {", "}", |fmt| {

    });

    Ok(())
}

pub fn generate(filename: &str, out_dir: &str) -> Result<(), error::Error> {
    let mut fmt = Formatter::new();

    gen_formats(&mut fmt)?;

    fmt.update_file(filename, out_dir)?;

    Ok(())
}