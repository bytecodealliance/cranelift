//! CLI tool to interpret Cranelift IR files.

use cranelift_interpreter::runner::FileRunner;
use std::path::PathBuf;
use walkdir::WalkDir;

pub fn run(files: Vec<String>, flag_print: bool) -> Result<(), String> {
    let mut total = 0;
    let mut errors = 0;
    for file in iterate_files(files) {
        total += 1;
        let runner = FileRunner::from_path(file).map_err(|e| e.to_string())?;
        match runner.run() {
            Ok(_) => {
                if flag_print {
                    println!("{}", runner.path());
                }
            }
            Err(e) => {
                if flag_print {
                    println!("{}: {}", runner.path(), e.to_string());
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
            Ok(d) => {
                // filter out hidden files (starting with .)
                !d.file_name().to_str().map_or(false, |s| s.starts_with('.'))
                    // filter out directories
                    && !d.file_type().is_dir()
            }
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn nop() {
        let code = String::from(
            "
            function %test() -> b8 {
            ebb0:
                nop
                v1 = bconst.b8 true
                v2 = iconst.i8 42
                return v1
            }
            ; run
            ",
        );
        FileRunner::from_inline_code(code).run().unwrap()
    }
}
