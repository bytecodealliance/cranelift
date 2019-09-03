//! Shared definitions for the Cranelift intermediate language.

pub mod entities;
pub mod formats;
pub mod immediates;
pub mod instructions;
pub mod legalize;
pub mod settings;
pub mod types;

use crate::cdsl::formats::FormatRegistry;
use crate::cdsl::instructions::{AllInstructions, InstructionGroup};
use crate::cdsl::settings::SettingGroup;
use crate::cdsl::xform::TransformGroups;

pub struct Definitions {
    pub settings: SettingGroup,
    pub all_instructions: AllInstructions,
    pub instructions: InstructionGroup,
    pub format_registry: FormatRegistry,
    pub transform_groups: TransformGroups,
}

pub fn define() -> Definitions {
    let mut all_instructions = AllInstructions::new();

    let format_registry = formats::define();
    let instructions = instructions::define(&mut all_instructions, &format_registry);
    let transform_groups = legalize::define(&instructions);

    Definitions {
        settings: settings::define(),
        all_instructions,
        instructions,
        format_registry,
        transform_groups,
    }
}
