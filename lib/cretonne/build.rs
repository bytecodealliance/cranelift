// Build script.
//
// This program is run by Cargo when building lib/cretonne. It is used to generate Rust code from
// the language definitions in the lib/cretonne/meta directory.
//
// Environment:
//
// OUT_DIR
//     Directory where generated files should be placed.
//
// The build script expects to be run from the directory where this build.rs file lives. The
// current directory is used to find the sources.


use std::env;
use std::process;

fn main() {
    let out_dir = env::var("OUT_DIR").expect("The OUT_DIR environment variable must be set");

    output_target_cfgs();

    println!("Build script generating files in {}", out_dir);

    let cur_dir = env::current_dir().expect("Can't access current working directory");
    let crate_dir = cur_dir.as_path();

    // Make sure we rebuild is this build script changes.
    // I guess that won't happen if you have non-UTF8 bytes in your path names.
    // The `build.py` script prints out its own dependencies.
    println!("cargo:rerun-if-changed={}", crate_dir.join("build.rs").to_string_lossy());

    // Scripts are in `$crate_dir/meta`.
    let meta_dir = crate_dir.join("meta");
    let build_script = meta_dir.join("build.py");

    // Launch build script with Python. We'll just find python in the path.
    let status = process::Command::new("python")
        .current_dir(crate_dir)
        .arg(build_script)
        .arg("--out-dir")
        .arg(out_dir)
        .status()
        .expect("Failed to launch second-level build script");
    if !status.success() {
        process::exit(status.code().unwrap());
    }
}

fn output_target_cfgs() {
    let config = env::var("CRETONNE_TARGETS").unwrap_or("all".into());
    let possible_isas = ["riscv", "intel", "arm32", "arm64"];

    match config.as_ref() {
        "all" => {
            for isa in &possible_isas {
                println!("cargo:rustc-cfg=build_{}", isa);
            }
        }

        "native" => {
            let target = env::var("TARGET").expect("The TARGET environment variable must be set");
            let arch = target.split("-").next().unwrap();

            let isa = match arch {
                "aarch64" => "arm64",
                "arm" | "armv7" | "armv7s" => "arm32",
                "x86_64" | "i386" | "i586" | "i686" => "intel",
                "riscv" => "riscv",
                _ => {
                    println!("no supported isa found for target triple `{}`", target);
                    process::exit(1);
                }
            };

            println!("cargo:rustc-cfg=build_{}", isa);
        }

        _ => {
            let mut found = false;

            for isa in config.split(',') {
                if possible_isas.contains(&isa) {
                    println!("cargo:rustc-cfg=build_{}", isa);
                    found = true;

                } else {
                    println!("unknown isa `{}`", isa);
                    process::exit(1);
                }
            }

            if !found {
                println!("no supported isa found in `{}`", config);
                process::exit(1);
            }
        }
    };
}

