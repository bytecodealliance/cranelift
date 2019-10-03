//! Stand-alone WebAssembly to Cranelift IR translator.
//!
//! This module defines the `FuncTranslator` type which can translate a single WebAssembly
//! function to Cranelift IR guided by a `FuncEnvironment` which provides information about the
//! WebAssembly module and the runtime environment.

use crate::environ::{FuncEnvironment, WasmResult};
use crate::state::ModuleTranslationState;
use cranelift_codegen::ir;
use wasmparser::{self, BinaryReader};

/// WebAssembly to Cranelift IR function translator.
///
/// A `FuncTranslator` is used to translate a binary WebAssembly function into Cranelift IR guided
/// by a `FuncEnvironment` object. A single translator instance can be reused to translate multiple
/// functions which will reduce heap allocation traffic.
#[deprecated(note = "Use `ModuleTranslationState::translate_func[_with_reader]` instead")]
pub struct FuncTranslator {}

#[allow(deprecated)]
impl FuncTranslator {
    /// Create a new translator.
    pub fn new() -> Self {
        Self {}
    }

    /// Deprecated: use `ModuleTranslationState::translate_func` instead.
    #[deprecated(note = "Use `ModuleTranslationState::translate_func` instead")]
    pub fn translate<FE: FuncEnvironment + ?Sized>(
        &mut self,
        module_translation_state: &ModuleTranslationState,
        code: &[u8],
        code_offset: usize,
        func: &mut ir::Function,
        environ: &mut FE,
    ) -> WasmResult<()> {
        module_translation_state.translate_func(code, code_offset, func, environ)
    }

    /// Deprecated: use `Moduletranslationstate::translate_func_with_reader` instead.
    #[deprecated(note = "Use `ModuleTranslationState::translate_func_with_reader` instead")]
    pub fn translate_from_reader<FE: FuncEnvironment + ?Sized>(
        &mut self,
        module_translation_state: &ModuleTranslationState,
        reader: BinaryReader,
        func: &mut ir::Function,
        environ: &mut FE,
    ) -> WasmResult<()> {
        module_translation_state.translate_func_with_reader(reader, func, environ)
    }
}
