//! Defines `Module` and related types.

// TODO: Should `ir::Function` really have a `name`?

// TODO: Factor out `ir::Function`'s `ext_funcs` and `global_vars` into a struct
// shared with `DataContext`?

use cretonne::{ir, binemit, Context};
use cretonne::entity::{PrimaryMap, EntityRef};
use std::collections::HashMap;
use data_context::DataContext;
use Backend;

/// A function identifier for use in the `Module` interface.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FuncId(u32);
entity_impl!(FuncId, "funcid");

/// A data object identifier for use in the `Module` interface.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DataId(u32);
entity_impl!(DataId, "dataid");

/// Linkage refers to where an entity is defined and who can see it.
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Linkage {
    /// Defined outside of a module.
    Import,
    /// Defined inside the module, but not visible outside it.
    Local,
    /// Defined inside the module, and visible outside it.
    Export,
}

impl Linkage {
    fn merge(a: Linkage, b: Linkage) -> Linkage {
        match a {
            Linkage::Export => Linkage::Export,
            Linkage::Local => {
                match b {
                    Linkage::Export => Linkage::Export,
                    _ => Linkage::Local,
                }
            }
            Linkage::Import => b,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum FuncOrDataId {
    Func(FuncId),
    Data(DataId),
}

struct ModuleFunction<B>
where
    B: Backend,
{
    name: String,
    linkage: Linkage,
    signature: ir::Signature,
    compiled: Option<B::CompiledFunction>,
    finalized: bool,
}

impl<B> ModuleFunction<B>
where
    B: Backend,
{
    fn merge(&mut self, linkage: Linkage) {
        self.linkage = Linkage::merge(self.linkage, linkage);
    }
}

struct ModuleData<B>
where
    B: Backend,
{
    name: String,
    linkage: Linkage,
    writable: bool,
    compiled: Option<B::CompiledData>,
    finalized: bool,
}

impl<B> ModuleData<B>
where
    B: Backend,
{
    fn merge(&mut self, linkage: Linkage, writable: bool) {
        self.linkage = Linkage::merge(self.linkage, linkage);
        self.writable = !self.writable && !writable;
    }
}

/// A `Module` is a utility for collecting functions and data objects, and linking them together.
pub struct Module<B>
where
    B: Backend,
{
    names: HashMap<String, FuncOrDataId>,
    functions: PrimaryMap<FuncId, ModuleFunction<B>>,
    data_objects: PrimaryMap<DataId, ModuleData<B>>,
    backend: B,
}

impl<B> Module<B>
where
    B: Backend,
{
    /// Create a new `Module`.
    pub fn new(backend: B) -> Self {
        Self {
            names: HashMap::new(),
            functions: PrimaryMap::new(),
            data_objects: PrimaryMap::new(),
            backend,
        }
    }

    /// Declare a function in this module.
    pub fn declare_function(
        &mut self,
        name: &str,
        linkage: Linkage,
        signature: &ir::Signature,
    ) -> FuncId {
        // TODO: Can we avoid allocating names so often?
        use std::collections::hash_map::Entry::*;
        match self.names.entry(name.to_owned()) {
            Occupied(entry) => {
                match *entry.get() {
                    FuncOrDataId::Func(id) => {
                        let existing = &mut self.functions[id];
                        existing.merge(linkage);
                        id
                    }
                    FuncOrDataId::Data(..) => unimplemented!(),
                }
            }
            Vacant(entry) => {
                let id = self.functions.push(ModuleFunction {
                    name: name.to_owned(),
                    linkage,
                    signature: signature.clone(),
                    compiled: None,
                    finalized: false,
                });
                entry.insert(FuncOrDataId::Func(id));
                id
            }
        }
    }

    /// Declare a data object in this module.
    pub fn declare_data(&mut self, name: &str, linkage: Linkage, writable: bool) -> DataId {
        // TODO: Can we avoid allocating names so often?
        use std::collections::hash_map::Entry::*;
        match self.names.entry(name.to_owned()) {
            Occupied(entry) => {
                match *entry.get() {
                    FuncOrDataId::Data(id) => {
                        let existing = &mut self.data_objects[id];
                        existing.merge(linkage, writable);
                        id
                    }

                    FuncOrDataId::Func(..) => unimplemented!(),
                }
            }
            Vacant(entry) => {
                let id = self.data_objects.push(ModuleData {
                    name: name.to_owned(),
                    linkage,
                    writable,
                    compiled: None,
                    finalized: false,
                });
                entry.insert(FuncOrDataId::Data(id));
                id
            }
        }
    }

    /// Use this when you're building the IR of a function to reference a function.
    ///
    /// TODO: Coalesce redundant decls.
    /// TODO: Look into ways to reduce the risk of using a FuncRef in the wrong function.
    pub fn declare_func_in_func(&self, func: FuncId, ctx: &mut Context) -> ir::FuncRef {
        let signature = ctx.func.import_signature(
            self.functions[func].signature.clone(),
        );
        ctx.func.import_function(ir::ExtFuncData {
            name: ir::ExternalName::user(0, func.index() as u32),
            signature,
        })
    }

    /// Use this when you're building the IR of a function to reference a data object.
    ///
    /// TODO: Same as above.
    pub fn declare_data_in_func(&self, data: DataId, ctx: &mut Context) -> ir::GlobalVar {
        ctx.func.create_global_var(ir::GlobalVarData::Sym {
            name: ir::ExternalName::user(0, data.index() as u32),
        })
    }

    /// TODO: Same as above.
    pub fn declare_func_in_data(&self, func: FuncId, ctx: &mut DataContext) -> ir::FuncRef {
        ctx.import_function(ir::ExternalName::user(0, func.index() as u32))
    }

    /// TODO: Same as above.
    pub fn declare_data_in_data(&self, data: DataId, ctx: &mut DataContext) -> ir::GlobalVar {
        ctx.import_global_var(ir::ExternalName::user(0, data.index() as u32))
    }

    /// Define a function, producing the function body from the given `Context`.
    pub fn define_function(&mut self, func: FuncId, ctx: &Context) {
        let info = &mut self.functions[func];
        debug_assert!(info.compiled.is_none());
        info.compiled = Some(self.backend.define_function(&info.name, ctx));
    }

    /// Define a function, producing the data contents from the given `DataContext`.
    pub fn define_data(&mut self, data: DataId, ctx: &DataContext) {
        let compiled = {
            let info = &mut self.data_objects[data];
            debug_assert!(info.compiled.is_none());
            Some(self.backend.define_data(&info.name, ctx))
        };
        self.data_objects[data].compiled = compiled;
    }

    /// Write the address of `what` into the data for `data` at `offset`. `data` must refer to a
    /// defined data object.
    pub fn write_data_funcaddr(&mut self, data: DataId, offset: usize, what: ir::FuncRef) {
        let info = &mut self.data_objects[data];
        self.backend.write_data_funcaddr(
            &mut info.compiled.as_mut().expect(
                "`data` must refer to a defined data object",
            ),
            offset,
            what,
        );
    }

    /// Write the address of `what` plus `addend` into the data for `data` at `offset`. `data` must
    /// refer to a defined data object.
    pub fn write_data_dataaddr(
        &mut self,
        data: DataId,
        offset: usize,
        what: ir::GlobalVar,
        addend: binemit::Addend,
    ) {
        let info = &mut self.data_objects[data];
        self.backend.write_data_dataaddr(
            &mut info.compiled.as_mut().expect(
                "`data` must refer to a defined data object",
            ),
            offset,
            what,
            addend,
        );
    }

    /// Perform all outstanding relocations on the given function. This requires all `DefinedHere`
    /// and `Export` entities referenced to be defined.
    pub fn finalize_function(&mut self, func: FuncId) {
        let info = &mut self.functions[func];
        finalize_function_info(info, &mut self.backend);
    }

    /// Perform all outstanding relocations on the given data object. This requires all
    /// `DefinedHere` and `Export` entities referenced to be defined.
    pub fn finalize_data(&mut self, data: DataId) {
        let info = &mut self.data_objects[data];
        finalize_data_info(info, &mut self.backend);
    }

    /// Finalize all functions and data objects.
    /// TODO: Make this consume self? And the loops do into_iter()? And make
    /// finalize_function/finalize_data too?
    pub fn finalize_all(&mut self) {
        for info in self.functions.values_mut() {
            finalize_function_info(info, &mut self.backend);
        }
        for info in self.data_objects.values_mut() {
            finalize_data_info(info, &mut self.backend);
        }
    }
}

fn finalize_function_info<B>(info: &mut ModuleFunction<B>, backend: &mut B)
where
    B: Backend,
{
    if !info.finalized {
        backend.finalize_function(info.compiled.as_ref().expect(
            "function must be compiled before it can be finalized",
        ));
        info.finalized = true;
    }
}

fn finalize_data_info<B>(info: &mut ModuleData<B>, backend: &mut B)
where
    B: Backend,
{
    if !info.finalized {
        backend.finalize_data(info.compiled.as_ref().expect(
            "data object must be compiled before it can be finalized",
        ));
        info.finalized = true;
    }
}
