//! Defines `Module` and related types.

// TODO: Should `ir::Function` really have a `name`?

// TODO: Factor out `ir::Function`'s `ext_funcs` and `global_vars` into a struct
// shared with `DataContext`?

use Backend;
use cretonne::entity::{EntityRef, PrimaryMap};
use cretonne::isa::TargetIsa;
use cretonne::result::{CtonError, CtonResult};
use cretonne::{binemit, ir, Context};
use data_context::DataContext;
use std::collections::HashMap;

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

    /// Test whether this linkage can have a definition.
    pub fn is_definable(&self) -> bool {
        match *self {
            Linkage::Import => false,
            Linkage::Local | Linkage::Export => true,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum FuncOrDataId {
    Func(FuncId),
    Data(DataId),
}

pub struct FunctionDeclaration {
    pub name: String,
    pub linkage: Linkage,
    pub signature: ir::Signature,
}

struct ModuleFunction<B>
where
    B: Backend,
{
    decl: FunctionDeclaration,
    compiled: Option<B::CompiledFunction>,
    finalized: bool,
}

impl<B> ModuleFunction<B>
where
    B: Backend,
{
    fn merge(&mut self, linkage: Linkage) {
        self.decl.linkage = Linkage::merge(self.decl.linkage, linkage);
    }
}

pub struct DataDeclaration {
    pub name: String,
    pub linkage: Linkage,
    pub writable: bool,
}

struct ModuleData<B>
where
    B: Backend,
{
    decl: DataDeclaration,
    compiled: Option<B::CompiledData>,
    finalized: bool,
}

impl<B> ModuleData<B>
where
    B: Backend,
{
    fn merge(&mut self, linkage: Linkage, writable: bool) {
        self.decl.linkage = Linkage::merge(self.decl.linkage, linkage);
        self.decl.writable = !self.decl.writable && !writable;
    }
}

struct ModuleContents<B>
where
    B: Backend,
{
    functions: PrimaryMap<FuncId, ModuleFunction<B>>,
    data_objects: PrimaryMap<DataId, ModuleData<B>>,
}

/// This provides a view to the state of a module which allows `ir::ExternalName`s to be translated
/// into `FunctionDeclaration`s and `DataDeclaration`s.
pub struct ModuleNamespace<'a, B: 'a>
where
    B: Backend,
{
    contents: &'a ModuleContents<B>,
}

impl<'a, B> ModuleNamespace<'a, B>
where
    B: Backend,
{
    /// Get the `FunctionDeclaration` for the function named by `name`.
    pub fn get_function_decl(&self, name: &ir::ExternalName) -> &FunctionDeclaration {
        if let ir::ExternalName::User { namespace, index } = *name {
            debug_assert!(namespace == 0);
            let func = FuncId::new(index as usize);
            &self.contents.functions[func].decl
        } else {
            panic!("unexpected ExternalName kind")
        }
    }

    /// Get the `DataDeclaration` for the function named by `name`.
    pub fn get_data_decl(&self, name: &ir::ExternalName) -> &DataDeclaration {
        if let ir::ExternalName::User { namespace, index } = *name {
            debug_assert!(namespace == 0);
            let data = DataId::new(index as usize);
            &self.contents.data_objects[data].decl
        } else {
            panic!("unexpected ExternalName kind")
        }
    }
}

/// A `Module` is a utility for collecting functions and data objects, and linking them together.
pub struct Module<'isa, B>
where
    B: Backend,
{
    names: HashMap<String, FuncOrDataId>,
    contents: ModuleContents<B>,
    backend: B,
    isa: &'isa TargetIsa,
}

impl<'isa, B> Module<'isa, B>
where
    B: Backend,
{
    /// Create a new `Module`.
    pub fn new(backend: B, isa: &'isa TargetIsa) -> Self {
        Self {
            names: HashMap::new(),
            contents: ModuleContents {
                functions: PrimaryMap::new(),
                data_objects: PrimaryMap::new(),
            },
            backend,
            isa,
        }
    }

    /// Declare a function in this module.
    pub fn declare_function(
        &mut self,
        name: &str,
        linkage: Linkage,
        signature: &ir::Signature,
    ) -> Result<FuncId, CtonError> {
        // TODO: Can we avoid allocating names so often?
        use std::collections::hash_map::Entry::*;
        match self.names.entry(name.to_owned()) {
            Occupied(entry) => {
                match *entry.get() {
                    FuncOrDataId::Func(id) => {
                        let existing = &mut self.contents.functions[id];
                        existing.merge(linkage);
                        if linkage != existing.decl.linkage {
                            self.backend.declare_function(name, existing.decl.linkage);
                        }
                        Ok(id)
                    }
                    FuncOrDataId::Data(..) => unimplemented!(),
                }
            }
            Vacant(entry) => {
                let id = self.contents.functions.push(ModuleFunction {
                    decl: FunctionDeclaration {
                        name: name.to_owned(),
                        linkage,
                        signature: signature.clone(),
                    },
                    compiled: None,
                    finalized: false,
                });
                entry.insert(FuncOrDataId::Func(id));
                self.backend.declare_function(name, linkage);
                Ok(id)
            }
        }
    }

    /// Declare a data object in this module.
    pub fn declare_data(
        &mut self,
        name: &str,
        linkage: Linkage,
        writable: bool,
    ) -> Result<DataId, CtonError> {
        // TODO: Can we avoid allocating names so often?
        use std::collections::hash_map::Entry::*;
        match self.names.entry(name.to_owned()) {
            Occupied(entry) => {
                match *entry.get() {
                    FuncOrDataId::Data(id) => {
                        let existing = &mut self.contents.data_objects[id];
                        existing.merge(linkage, writable);
                        if linkage != existing.decl.linkage || writable != existing.decl.writable {
                            self.backend.declare_data(
                                name,
                                existing.decl.linkage,
                                existing.decl.writable,
                            );
                        }
                        Ok(id)
                    }

                    FuncOrDataId::Func(..) => unimplemented!(),
                }
            }
            Vacant(entry) => {
                let id = self.contents.data_objects.push(ModuleData {
                    decl: DataDeclaration {
                        name: name.to_owned(),
                        linkage,
                        writable,
                    },
                    compiled: None,
                    finalized: false,
                });
                entry.insert(FuncOrDataId::Data(id));
                self.backend.declare_data(name, linkage, writable);
                Ok(id)
            }
        }
    }

    /// Use this when you're building the IR of a function to reference a function.
    ///
    /// TODO: Coalesce redundant decls and signatures.
    /// TODO: Look into ways to reduce the risk of using a FuncRef in the wrong function.
    pub fn declare_func_in_func(&self, func: FuncId, ctx: &mut Context) -> ir::FuncRef {
        let signature = ctx.func.import_signature(
            self.contents.functions[func].decl.signature.clone(),
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
    pub fn define_function(&mut self, func: FuncId, ctx: &mut Context) -> CtonResult {
        let compiled = {
            let code_size = ctx.compile(self.isa)?;

            let info = &self.contents.functions[func];
            debug_assert!(
                info.compiled.is_none(),
                "functions can be defined only once"
            );
            debug_assert!(
                info.decl.linkage.is_definable(),
                "imported functions cannot be defined"
            );
            Some(self.backend.define_function(
                &info.decl.name,
                ctx,
                &ModuleNamespace::<B> {
                    contents: &self.contents,
                },
                self.isa,
                code_size,
            )?)
        };
        self.contents.functions[func].compiled = compiled;
        Ok(())
    }

    /// Define a function, producing the data contents from the given `DataContext`.
    pub fn define_data(&mut self, data: DataId, ctx: &DataContext) -> CtonResult {
        let compiled = {
            let info = &mut self.contents.data_objects[data];
            debug_assert!(
                info.compiled.is_none(),
                "functions can be defined only once"
            );
            debug_assert!(
                info.decl.linkage.is_definable(),
                "imported functions cannot be defined"
            );
            Some(self.backend.define_data(&info.decl.name, ctx))
        };
        self.contents.data_objects[data].compiled = compiled;
        Ok(())
    }

    /// Write the address of `what` into the data for `data` at `offset`. `data` must refer to a
    /// defined data object.
    pub fn write_data_funcaddr(&mut self, data: DataId, offset: usize, what: ir::FuncRef) {
        let info = &mut self.contents.data_objects[data];
        debug_assert!(
            info.decl.linkage.is_definable(),
            "imported data cannot contain references"
        );
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
        let info = &mut self.contents.data_objects[data];
        debug_assert!(
            info.decl.linkage.is_definable(),
            "imported data cannot contain references"
        );
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
        let info = &mut self.contents.functions[func];
        finalize_function_info(info, &mut self.backend);
    }

    /// Perform all outstanding relocations on the given data object. This requires all
    /// `DefinedHere` and `Export` entities referenced to be defined.
    pub fn finalize_data(&mut self, data: DataId) {
        let info = &mut self.contents.data_objects[data];
        finalize_data_info(info, &mut self.backend);
    }

    /// Finalize all functions and data objects.
    pub fn finalize_all(mut self) -> B {
        // TODO: Could we use something like `into_iter()` here?
        for info in self.contents.functions.values_mut() {
            if info.decl.linkage.is_definable() {
                finalize_function_info(info, &mut self.backend);
            }
        }
        for info in self.contents.data_objects.values_mut() {
            if info.decl.linkage.is_definable() {
                finalize_data_info(info, &mut self.backend);
            }
        }
        self.backend
    }
}

fn finalize_function_info<B>(info: &mut ModuleFunction<B>, backend: &mut B)
where
    B: Backend,
{
    debug_assert!(
        info.decl.linkage.is_definable(),
        "imported data cannot be finalized"
    );
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
    debug_assert!(
        info.decl.linkage.is_definable(),
        "imported data cannot be finalized"
    );
    if !info.finalized {
        backend.finalize_data(info.compiled.as_ref().expect(
            "data object must be compiled before it can be finalized",
        ));
        info.finalized = true;
    }
}
