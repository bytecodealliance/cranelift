//! The `json` sub-command.
//!
//! Read a sequence of Cretonne IL files and print them to stdout in JSON format.

use cton_reader::parse_functions;
use CommandResult;
use utils::read_to_string;

pub fn run(files: Vec<String>) -> CommandResult {
    let mut first = true;
    print!("[");
    for f in files {
        let buffer = read_to_string(&f).map_err(|e| format!("{}: {}", f, e))?;
        let items = parse_functions(&buffer)
            .map_err(|e| format!("{}: {}", f, e))?;

        for func in items {
            if first {
                first = false;
            } else {
                print!(",");
            }
            print!("{}", func.json_display(None));
        }
    }
    println!("]");
    Ok(())
}
