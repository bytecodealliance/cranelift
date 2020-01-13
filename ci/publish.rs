//! Helper script to manage cranelift crate versions as well as publish them.
//!
//! Usage:
//!
//! ```text
//! $ cd path/to/checkout/of/cranelift
//! $ rustc ci/publish.rs
//! $ ./publish bump
//! ```
//!
//! Then send a PR with all the bumped versions. On merge make a tag, then push
//! the tag and everything will get published from CI!

use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

// note that this list must be topologically sorted by dependencies
const CRATES_TO_PUBLISH: &[&str] = &[
    "cranelift-entity",
    "cranelift-bforest",
    "cranelift-codegen-shared",
    "cranelift-codegen-meta",
    "cranelift-codegen",
    "cranelift-frontend",
    "cranelift-native",
    "cranelift-preopt",
    "cranelift-reader",
    "cranelift-wasm",
    "cranelift-module",
    "cranelift-faerie",
    "cranelift",
    "cranelift-simplejit",
    "cranelift-object",
];

const CRATES_TO_AVOID_PUBLISH: &[&str] = &[
    "cranelift-tools",
    "cranelift-filetests",
    "cranelift-serde",
    "clif-wasm-fuzz",
];

struct Crate {
    manifest: PathBuf,
    name: String,
    version: String,
    next_version: String,
}

fn main() {
    let mut crates = Vec::new();
    find_crates(".".as_ref(), &mut crates);

    let pos = CRATES_TO_PUBLISH
        .iter()
        .chain(CRATES_TO_AVOID_PUBLISH)
        .enumerate()
        .map(|(i, c)| (*c, i))
        .collect::<HashMap<_, _>>();
    crates.sort_by_key(|krate| pos.get(&krate.name[..]));

    match &env::args().nth(1).expect("must have one argument")[..] {
        "bump" => {
            for krate in crates.iter() {
                bump_version(&krate, &crates);
            }
        }

        "publish" => {
            let dry_run = env::var("NO_DRY_RUN").is_err();
            let mut any_failed = false;
            for krate in crates.iter() {
                publish(&krate, dry_run, &mut any_failed);

                // Sleep for a few seconds to allow the server to update the
                // index, and for more info on this see
                // https://internals.rust-lang.org/t/changes-to-how-crates-io-handles-index-updates/9608
                if !dry_run {
                    std::thread::sleep(std::time::Duration::from_secs(15));
                }
            }
            if any_failed {
                panic!("failed to publish some crates");
            }
        }

        "create-tag" => {
            let cranelift = crates.iter().find(|c| c.name == "cranelift").unwrap();
            println!("Creating git tag `v{}`...", cranelift.version);
            let status = Command::new("git")
                .arg("tag")
                .arg(format!("v{}", cranelift.version))
                .status()
                .expect("failed to spawn git");
            assert!(status.success());
        }

        s => panic!("unknown command: {}", s),
    }
}

/// Finds all crates in `dir` and pushes them onto `dst`.
///
/// Skips any crates in `CRATES_TO_AVOID_PUBLISH`, and otherwise requires crates
/// to be listed in `CRATES_TO_PUBLISH`
fn find_crates(dir: &Path, dst: &mut Vec<Crate>) {
    if dir.join("Cargo.toml").exists() {
        let krate = read_crate(&dir.join("Cargo.toml"));
        if CRATES_TO_PUBLISH
            .iter()
            .chain(CRATES_TO_AVOID_PUBLISH)
            .any(|c| krate.name == *c)
        {
            dst.push(krate);
        } else {
            panic!("failed to find {:?} in whitelist or blacklist", krate.name);
        }
    }

    for entry in dir.read_dir().unwrap() {
        let entry = entry.unwrap();
        if entry.file_type().unwrap().is_dir() {
            find_crates(&entry.path(), dst);
        }
    }

    fn read_crate(manifest: &Path) -> Crate {
        let mut name = None;
        let mut version = None;

        // This is a "poor man's toml parser", but works well enough for our
        // manifests.
        for line in fs::read_to_string(manifest).unwrap().lines() {
            if name.is_none() && line.starts_with("name = \"") {
                name = Some(
                    line.replace("name = \"", "")
                        .replace("\"", "")
                        .trim()
                        .to_string(),
                );
            }
            if version.is_none() && line.starts_with("version = \"") {
                version = Some(
                    line.replace("version = \"", "")
                        .replace("\"", "")
                        .trim()
                        .to_string(),
                );
            }
        }
        let name = name.unwrap();
        let version = version.unwrap();
        let next_version = if CRATES_TO_PUBLISH.contains(&&name[..]) {
            bump(&version)
        } else {
            version.clone()
        };
        Crate {
            manifest: manifest.to_path_buf(),
            name,
            version,
            next_version,
        }
    }
}

fn bump_version(krate: &Crate, crates: &[Crate]) {
    let contents = fs::read_to_string(&krate.manifest).unwrap();

    let mut new_manifest = String::new();
    let mut is_deps = false;
    for line in contents.lines() {
        let mut rewritten = false;
        if line.starts_with("version =") {
            if CRATES_TO_PUBLISH.contains(&&krate.name[..]) {
                println!(
                    "bump `{}` {} => {}",
                    krate.name, krate.version, krate.next_version
                );
                new_manifest.push_str(&line.replace(&krate.version, &krate.next_version));
                rewritten = true;
            }
        }

        is_deps = if line.starts_with("[") {
            line.contains("dependencies")
        } else {
            is_deps
        };

        for other in crates {
            if !is_deps || !line.starts_with(&format!("{} ", other.name)) {
                continue;
            }
            let to_replace = format!("\"{}\"", other.version);
            let replace_with = format!("\"{}\"", other.next_version);
            if !line.contains(&to_replace) {
                if !line.contains("version =") {
                    continue;
                }
                panic!(
                    "{:?} has a dep on {} but doesn't list version {}",
                    krate.manifest, other.name, other.version
                );
            }
            rewritten = true;
            new_manifest.push_str(&line.replace(&to_replace, &replace_with));
            break;
        }
        if !rewritten {
            new_manifest.push_str(line);
        }
        new_manifest.push_str("\n");
    }
    fs::write(&krate.manifest, new_manifest).unwrap();
}

/// Bumps a semver version to the next semver version that we should publish.
///
/// Currently this bumps the minor number, which for cranelift changes X.Y.Z to
/// X.Y+1.Z, e.g. 0.49.0 to 0.50.0
fn bump(version: &str) -> String {
    let mut iter = version.split('.').map(|s| s.parse::<u32>().unwrap());
    let major = iter.next().expect("major version");
    let minor = iter.next().expect("minor version");
    format!("{}.{}.0", major, minor + 1)
}

fn publish(krate: &Crate, dry_run: bool, any_failed: &mut bool) {
    if !CRATES_TO_PUBLISH.iter().any(|s| *s == krate.name) {
        return;
    }

    // Skip the simplejit crate since it tries to generate a lock file and may
    // point to unpublished dependencies. Only skip it in dry run mode though.
    if dry_run && krate.name.contains("cranelift-simplejit") {
        return;
    }

    let mut cmd = Command::new("cargo");
    cmd.arg("publish")
        .current_dir(krate.manifest.parent().unwrap());
    if dry_run {
        cmd.arg("--dry-run");
        cmd.arg("--no-verify");
    }
    let status = cmd.status().expect("failed to run cargo");
    if !status.success() {
        eprintln!("FAIL: failed to publish `{}`: {}", krate.name, status);
        *any_failed = true;
    }
}
