use cranelift_interpreter::runner::FileRunner;
use std::path::PathBuf;
use walkdir::WalkDir;

#[test]
fn filetests() {
    let _ = pretty_env_logger::try_init();
    for path in iterate_files(vec!["tests".to_string()]) {
        println!("{}:", path.to_string_lossy());
        FileRunner::from_path(path).unwrap().run().unwrap();
    }
}

/// Iterate over all of the files passed as arguments, recursively iterating through directories.
fn iterate_files(files: Vec<String>) -> impl Iterator<Item = PathBuf> {
    files
        .into_iter()
        .flat_map(WalkDir::new)
        .filter(|f| match f {
            Ok(d) => d.path().extension().filter(|&e| e.eq("clif")).is_some(),
            _ => false,
        })
        .map(|f| {
            f.expect("This should not happen: we have already filtered out the errors")
                .into_path()
        })
}
