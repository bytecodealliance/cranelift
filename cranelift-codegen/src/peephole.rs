//! This crate runs peephole optimizations that are platform specific.

use crate::ir::Function;
use crate::isa::TargetIsa;
use crate::timing;

/// Runs the peephole phase on the generated code. Optimizations carried over here are expected to
/// be platform-specific.
pub fn do_peephole(func: &mut Function, isa: &dyn TargetIsa) {
    if !isa.has_peephole_optimizations() {
        return;
    }
    let _tt = timing::peephole();
    isa.run_peephole(func);
}
