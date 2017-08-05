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
// TARGET
//     Target triple provided by Cargo.
//
// The build script expects to be run from the directory where this build.rs file lives. The
// current directory is used to find the sources.


use std::env;
use std::process;

fn main() {
    let out_dir = env::var("OUT_DIR").expect("The OUT_DIR environment variable must be set");
    let target_triple = env::var("TARGET").expect("The TARGET environment variable must be set");

    // Get isa targets enabled by cargo features.
    let supported_isa_targets = Isa::all();
    let mut isa_targets = supported_isa_targets.iter().filter(|isa| {
        let key = format!("CARGO_FEATURE_{}", isa.name().to_uppercase());
        env::var(key).is_ok()
    });
    if isa_targets.next().is_none() {
        // If none of isa targets enabled try to use native.
        match Isa::from_arch(target_triple.split('-').next().unwrap()) {
            Some(isa) => println!("cargo:rustc-cfg=feature=\"{}\"", isa.name()),
            None => {
                eprintln!(
                    "Error: no supported isa found for target triple `{}`",
                    target_triple
                );
                process::exit(1);
            }
        }
    }

    println!("Build script generating files in {}", out_dir);

    let cur_dir = env::current_dir().expect("Can't access current working directory");
    let crate_dir = cur_dir.as_path();

    // Make sure we rebuild if this build script changes.
    // I guess that won't happen if you have non-UTF8 bytes in your path names.
    // The `build.py` script prints out its own dependencies.
    println!(
        "cargo:rerun-if-changed={}",
        crate_dir.join("build.rs").to_string_lossy()
    );

    // Scripts are in `$crate_dir/meta`.
    let meta_dir = crate_dir.join("meta");
    let build_script = meta_dir.join("build.py");

    // Launch build script with Python. We'll just find python in the path.
    // Use -B to disable .pyc files, because they cause trouble for vendoring
    // scripts, and this is a build step that isn't run very often anyway.
    let status = process::Command::new("python")
        .current_dir(crate_dir)
        .arg("-B")
        .arg(build_script)
        .arg("--out-dir")
        .arg(out_dir)
        .status()
        .expect("Failed to launch second-level build script");
    if !status.success() {
        process::exit(status.code().unwrap());
    }
}

/// Represents known ISA target.
#[derive(Copy, Clone)]
enum Isa {
    Riscv,
    Intel,
    Arm32,
    Arm64,
}

impl Isa {
    /// Creates isa target from arch.
    fn from_arch(arch: &str) -> Option<Self> {
        Isa::all().iter().cloned().find(
            |isa| isa.is_arch_applicable(arch),
        )
    }

    /// Returns all supported isa targets.
    fn all() -> [Isa; 4] {
        [Isa::Riscv, Isa::Intel, Isa::Arm32, Isa::Arm64]
    }

    /// Returns name of the isa target.
    ///
    /// Names of the isa targets should be in sync with features defined in Cargo.toml.
    fn name(&self) -> &'static str {
        match *self {
            Isa::Riscv => "riscv",
            Isa::Intel => "intel",
            Isa::Arm32 => "arm32",
            Isa::Arm64 => "arm64",
        }
    }

    /// Checks if arch is applicable for the isa target.
    fn is_arch_applicable(&self, arch: &str) -> bool {
        match *self {
            Isa::Riscv => arch == "riscv",
            Isa::Intel => ["x86_64", "i386", "i586", "i686"].contains(&arch),
            Isa::Arm32 => arch.starts_with("arm") || arch.starts_with("thumb"),
            Isa::Arm64 => arch == "aarch64",
        }
    }
}
