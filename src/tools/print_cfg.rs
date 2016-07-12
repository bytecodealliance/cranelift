//! The `print-cfg` sub-command.
//!
//! Read a series of Cretonne IL files and print their control flow graphs
//! in graphviz format.
use std::fs::File;
use std::io::Read;

use CommandResult;
use cretonne::cfg::ControlFlowGraph;
use cton_reader::parser::Parser;

pub fn run(files: Vec<String>) -> CommandResult {
    for (i, f) in files.into_iter().enumerate() {
        if i != 0 {
            println!("");
        }
        try!(print_cfg(f))
    }
    Ok(())
}

/// TODO: Write this in a sane useful way.
fn print_cfg(filename: String) -> CommandResult {
    let mut file = try!(File::open(&filename).map_err(|e| format!("{}: {}", filename, e)));
    let mut buffer = String::new();
    try!(file.read_to_string(&mut buffer)
        .map_err(|e| format!("Couldn't read {}: {}", filename, e)));
    let items = try!(Parser::parse(&buffer).map_err(|e| format!("{}: {}", filename, e)));

    for (idx, func) in items.into_iter().enumerate() {
        let cfg = try!(ControlFlowGraph::new(&func));

        if idx != 0 {
            println!("");
        }

        println!("digraph {}", '{');

        for (ebb, basic_blocks) in cfg.iter() {
            for &(pred_ebb, _) in basic_blocks {
                println!("\"{}\" -> \"{}\";",
                         format!("{:?}", pred_ebb),
                         format!("{:?}", ebb));
            }
        }

        println!("{}", '}');
    }

    Ok(())
}
