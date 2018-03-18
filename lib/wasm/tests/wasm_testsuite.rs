extern crate cton_wasm;
extern crate cretonne;
extern crate tempdir;
extern crate wabt;

use cton_wasm::{translate_module, DummyEnvironment};
use std::path::PathBuf;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::fs;
use cretonne::settings::{self, Configurable, Flags};
use cretonne::verifier;
use cretonne::print_errors::pretty_verifier_error;

#[test]
fn testsuite() {
    let mut paths: Vec<_> = fs::read_dir("../../wasmtests")
        .unwrap()
        .map(|r| r.unwrap())
        .filter(|p| {
            // Ignore files starting with `.`, which could be editor temporary files
            if let Some(stem) = p.path().file_stem() {
                if let Some(stemstr) = stem.to_str() {
                    return !stemstr.starts_with(".");
                }
            }
            false
        })
        .collect();
    paths.sort_by_key(|dir| dir.path());
    let flags = Flags::new(&settings::builder());
    for path in paths {
        let path = path.path();
        handle_module(path, &flags);
    }
}

#[test]
fn return_at_end() {
    let mut flag_builder = settings::builder();
    flag_builder.enable("return_at_end").unwrap();
    let flags = Flags::new(&flag_builder);
    handle_module(PathBuf::from("../../wasmtests/return_at_end.wat"), &flags);
}

fn read_file(path: PathBuf) -> Result<Vec<u8>, io::Error> {
    let mut buf: Vec<u8> = Vec::new();
    let mut file = File::open(path)?;
    file.read_to_end(&mut buf)?;
    Ok(buf)
}

fn handle_module(path: PathBuf, flags: &Flags) {
    let data = match path.extension() {
        None => {
            panic!("the file extension is not wasm or wat");
        }
        Some(ext) => {
            match ext.to_str() {
                Some("wasm") => read_file(path.clone()).expect("error reading wasm file"),
                Some("wat") => {
                    let wat = read_file(path.clone()).expect("error reading wat file");
                    match wabt::wat2wasm(&wat) {
                        Ok(wasm) => wasm,
                        Err(err) => {
                            panic!("error running wat2wasm: {:?}", err);
                        }
                    }
                }
                None | Some(&_) => panic!("the file extension for {:?} is not wasm or wat", path),
            }
        }
    };
    let mut dummy_environ = DummyEnvironment::with_flags(flags.clone());
    translate_module(&data, &mut dummy_environ).unwrap();
    for func in &dummy_environ.info.function_bodies {
        verifier::verify_function(func, flags)
            .map_err(|err| panic!(pretty_verifier_error(func, None, err)))
            .unwrap();
    }
}
