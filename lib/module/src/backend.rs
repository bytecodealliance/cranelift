//! Defines the `Backend` trait.

use cretonne::Context;
use cretonne::{ir, binemit};
use std::marker;
use DataContext;

/// A `Backend` implements the functionality needed to support a `Module`.
pub trait Backend
where
    Self: marker::Sized,
{
    /// The results of compiling a function.
    type CompiledFunction;

    /// The results of "compiling" a data object.
    type CompiledData;

    /// Define a function, producing the function body from the given `Context`.
    fn define_function(&mut self, name: &str, ctx: &Context) -> Self::CompiledFunction;

    /// Define a zero-initialized data object of the given size.
    fn define_data(&mut self, name: &str, ctx: &DataContext) -> Self::CompiledData;

    /// Write the address of `what` into the data for `data` at `offset`. `data` must refer to a
    /// defined data object.
    fn write_data_funcaddr(
        &mut self,
        data: &mut Self::CompiledData,
        offset: usize,
        what: ir::FuncRef,
    );

    /// Write the address of `what` plus `addend` into the data for `data` at `offset`. `data` must
    /// refer to a defined data object.
    fn write_data_dataaddr(
        &mut self,
        data: &mut Self::CompiledData,
        offset: usize,
        what: ir::GlobalVar,
        addend: binemit::Addend,
    );

    /// Perform all outstanding relocations on the given function. This requires all `DefinedHere`
    /// and `Export` entities referenced to be defined.
    fn finalize_function(&mut self, func: &Self::CompiledFunction);

    /// Perform all outstanding relocations on the given data object. This requires all
    /// `DefinedHere` and `Export` entities referenced to be defined.
    fn finalize_data(&mut self, data: &Self::CompiledData);
}
