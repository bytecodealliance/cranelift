//! Performs autodetection of the host for the purposes of running
//! Cretonne to generate code to run on the same machine.

#![deny(missing_docs)]

extern crate cretonne;

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
extern crate raw_cpuid;

use cretonne::isa;
use cretonne::settings::{self, Configurable};

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
use raw_cpuid::CpuId;

/// Return `settings` and `isa` builders configured for the current host
/// machine, or `Err(())` if the host machine is not supported
/// in the current configuration.
pub fn builders() -> Result<(settings::Builder, isa::Builder), ()> {
    let mut flag_builder = settings::builder();

    // TODO: Add RISC-V support once Rust supports it.

    let name = if cfg!(target_arch = "x86") {
        parse_x86_cpuid(&mut flag_builder);
        "intel"
    } else if cfg!(target_arch = "x86_64") {
        flag_builder.enable("is_64bit").unwrap();
        parse_x86_cpuid(&mut flag_builder);
        "intel"
    } else if cfg!(target_arch = "arm") {
        "arm32"
    } else if cfg!(target_arch = "aarch64") {
        flag_builder.enable("is_64bit").unwrap();
        "arm64"
    } else {
        return Err(());
    };

    isa::lookup(name).map(|x| (flag_builder, x)).map_err(
        |err| {
            match err {
                isa::LookupError::Unknown => panic!(),
                isa::LookupError::Unsupported => (()),
            }
        },
    )
}

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
fn parse_x86_cpuid(flag_builder: &mut settings::Builder) {
    let cpuid = CpuId::new();

    if let Some(info) = cpuid.get_feature_info() {
        if info.has_sse2() {
            flag_builder.enable("has_sse2").unwrap();
        }
        if info.has_sse3() {
            flag_builder.enable("has_sse3").unwrap();
        }
        if info.has_sse41() {
            flag_builder.enable("has_sse41").unwrap();
        }
        if info.has_sse42() {
            flag_builder.enable("has_sse42").unwrap();
        }
        if info.has_popcnt() {
            flag_builder.enable("has_popcnt").unwrap();
        }
        if info.has_avx() {
            flag_builder.enable("has_avx").unwrap();
        }
    }
    if let Some(info) = cpuid.get_extended_feature_info() {
        if info.has_bmi1() {
            flag_builder.enable("has_bmi1").unwrap();
        }
        if info.has_bmi2() {
            flag_builder.enable("has_bmi2").unwrap();
        }
    }
    if let Some(info) = cpuid.get_extended_function_info() {
        if info.has_lzcnt() {
            flag_builder.enable("has_lzcnt").unwrap();
        }
    }
}
