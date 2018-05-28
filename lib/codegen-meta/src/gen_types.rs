//! Generate sources with type info.
//!
//! This generates a `types.rs` file which is included in
//! `lib/codegen/ir/types.rs`. The file provides constant definitions for the
//! most commonly used types, including all of the scalar types.
//!
//! This ensures that Python and Rust use the same type numbering.

use cdsl::types as cdsl_types;
use error;
use srcgen;

/// Emit a constant definition of a single value type.
fn emit_type(ty: &cdsl_types::ValueType, fmt: &mut srcgen::Formatter) -> Result<(), error::Error> {
    let name = ty.name().to_uppercase();
    let number = ty.number().ok_or_else(|| {
        error::Error::with_msg(format!(
            "Could not emit type `{}` which has no number.",
            name
        ))
    })?;

    let definition = format!("pub const {}: Type = Type({:#x});\n", name, number);

    fmt.doc_comment(&ty.doc());
    fmt.line(&definition);

    Ok(())
}

/// Emit definition for all vector types with `bits` total size.
fn emit_vectors(bits: u64, fmt: &mut srcgen::Formatter) -> Result<(), error::Error> {
    let vec_size: u64 = bits / 8;
    cdsl_types::ValueType::all_lane_types()
        .map(|ty| (ty, cdsl_types::ValueType::from(ty).membytes()))
        .filter(|(_, lane_size)| *lane_size != 0 && *lane_size < vec_size)
        .map(|(ty, lane_size)| (ty, vec_size / lane_size))
        .map(|(ty, lanes)| cdsl_types::VectorType::new(ty, lanes))
        .try_for_each(|vec| emit_type(&cdsl_types::ValueType::from(vec), fmt))?;

    Ok(())
}

/// Emit types using the given formatter object.
fn emit_types(fmt: &mut srcgen::Formatter) -> Result<(), error::Error> {
    // Emit all of the special types, such as types for CPU flags.
    cdsl_types::ValueType::all_special_types().try_for_each(|spec| emit_type(&spec, fmt))?;

    // Emit all of the lane types, such integers, floats, and booleans.
    cdsl_types::ValueType::all_lane_types()
        .map(cdsl_types::ValueType::from)
        .try_for_each(|ty| emit_type(&ty, fmt))?;

    // Emit vector definitions for common SIMD sizes.
    [64_u64, 128, 256, 512]
        .into_iter()
        .try_for_each(|&b| emit_vectors(b, fmt))?;

    Ok(())
}

/// Generate the types file.
pub fn generate(filename: &str, out_dir: &str) -> Result<(), error::Error> {
    let mut fmt = srcgen::Formatter::new();
    emit_types(&mut fmt)?;
    fmt.update_file(filename, out_dir)?;
    Ok(())
}
