//! The Module data structure, which can hold multiple functions and data
//! objects.
//!
//! Currently, this is a very simple data structure that holds everything in
//! memory at once.

use cretonne::ir;
use cretonne::isa::TargetIsa;
use cretonne::binemit;
use std::fmt;
use ordermap::OrderMap;

/// A `Module` is a collection of functions and data. It also collects all the
/// imports from the things it contains into a single set.
///
/// This is currently very simplistic, and will likely evolve significantly.
/// Currently all defined symbols are exported.
pub struct Module {
    /// Functions, optionally with their compiled code.
    pub functions: Vec<CompiledFunction>,

    /// Data symbols.
    pub data_symbols: Vec<DataSymbol>,

    /// A set of imported symbols.
    pub imports: OrderMap<ir::ExternalName, ()>,

    /// A set of exported symbols.
    pub exports: OrderMap<ir::ExternalName, ()>,
}

impl Module {
    /// Create a new empty `Module`.
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            data_symbols: Vec::new(),
            imports: OrderMap::new(),
            exports: OrderMap::new(),
        }
    }

    /// Add cfunc to this module.
    pub fn add_function(&mut self, cfunc: CompiledFunction) {
        // Collect externally referenced symbols for the module.
        for func_ref in cfunc.il.dfg.ext_funcs.keys() {
            let name = &cfunc.il.dfg.ext_funcs[func_ref].name;
            // If this function is defined inside the module, don't list it
            // as an import.
            if !self.exports.contains_key(name) {
                self.imports.insert(name.clone(), ());
            }
        }
        for global_var in cfunc.il.global_vars.keys() {
            if let ir::GlobalVarData::Sym { ref name } = cfunc.il.global_vars[global_var] {
                // If this global is defined inside the module, don't list it
                // as an import.
                if !self.exports.contains_key(name) {
                    self.imports.insert(name.clone(), ());
                }
            }
        }

        // If the function was previously imported, it is no longer.
        self.imports.remove(&cfunc.il.name);

        // For now, all defined symbols are exported.
        self.exports.insert(cfunc.il.name.clone(), ());

        // Add it to the list.
        self.functions.push(cfunc);
    }

    /// Add data to this module.
    pub fn add_data(&mut self, data: DataSymbol) {
        // If the function was previously imported, it is no longer.
        self.imports.remove(&data.name);

        // For now, all defined symbols are exported.
        self.exports.insert(data.name.clone(), ());

        // Add it to the list.
        self.data_symbols.push(data);
    }

    /// Construct a `DisplayModule` for pretty-printing a `Module`.
    pub fn display<'a>(&'a self, isa: Option<&'a TargetIsa>) -> DisplayModule<'a> {
        DisplayModule { module: &self, isa }
    }
}

/// Support for pretty-printing `CompiledFunction`.
pub struct DisplayModule<'a> {
    module: &'a Module,
    isa: Option<&'a TargetIsa>,
}

impl<'a> fmt::Display for DisplayModule<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for func in &self.module.functions {
            write!(f, "{}", func.display(self.isa))?;
        }
        for sym in &self.module.data_symbols {
            write!(f, "data symbol: {}", sym)?;
        }
        Ok(())
    }
}

/// A function optionally paired with its compiled code.
pub struct CompiledFunction {
    /// The Cretonne IL for the function.
    pub il: ir::Function,

    /// The compiled code for the function.
    pub compilation: Option<Compilation>,
}

impl CompiledFunction {
    /// Construct a `DisplayCompiledFunction` for pretty-printing a
    /// `CompiledFunction`.
    pub fn display<'a>(&'a self, isa: Option<&'a TargetIsa>) -> DisplayCompiledFunction<'a> {
        DisplayCompiledFunction {
            compiled_func: &self,
            isa,
        }
    }
}

/// Support for pretty-printing `CompiledFunction`.
pub struct DisplayCompiledFunction<'a> {
    compiled_func: &'a CompiledFunction,
    isa: Option<&'a TargetIsa>,
}

impl<'a> fmt::Display for DisplayCompiledFunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.compiled_func.il.display(self.isa),
        )?;
        if let Some(ref compilation) = self.compiled_func.compilation {
            for byte in &compilation.body {
                write!(f, "0x{:02x}, ", byte)?;
            }
            write!(f, "\n")?;
            for &(ref reloc, ref name, ref offset) in &compilation.relocs {
                write!(f, "reloc: {}:{}@{}\n", reloc.0, name, offset)?;
            }
        }
        Ok(())
    }
}

/// Compiled code for a function, including relocations for the code.
pub struct Compilation {
    /// Encoded machine code.
    pub body: Vec<u8>,
    /// Relocations to apply.
    pub relocs: Vec<(binemit::Reloc, ir::ExternalName, binemit::CodeOffset)>,
}

/// A data symbol is a fixed-size data allocation identified by a symbol.
pub struct DataSymbol {
    /// The symbol identifying this data.
    pub name: ir::ExternalName,
    /// The initial values of the data bytes.
    pub contents: Vec<u8>,
}

impl fmt::Display for DataSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}: ",
            self.name,
        )?;
        for byte in &self.contents {
            write!(f, "0x{:02x}, ", byte)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use cretonne::ir;
    use super::*;

    #[test]
    fn basic() {
        let mut module = Module::new();

        let sig = ir::Signature::new(ir::CallConv::Native);
        let func0_name = ir::ExternalName::new("func0");
        let func1_name = ir::ExternalName::new("func1");
        let func2_name = ir::ExternalName::new("func2");
        let mut func0 = ir::Function::with_name_signature(func0_name.clone(), sig.clone());
        let mut func1 = ir::Function::with_name_signature(func1_name.clone(), sig.clone());
        let mut func2 = ir::Function::with_name_signature(func2_name.clone(), sig.clone());

        let sig0 = func0.import_signature(ir::Signature::new(ir::CallConv::Native));
        let sig1 = func1.import_signature(ir::Signature::new(ir::CallConv::Native));
        let sig2 = func2.import_signature(ir::Signature::new(ir::CallConv::Native));

        let name0 = ir::ExternalName::new("ext0");
        let name1 = ir::ExternalName::new("ext1");

        let _ext0 = func0.import_function(ir::ExtFuncData {
            name: name0.clone(),
            signature: sig0,
        });
        let _ext1 = func1.import_function(ir::ExtFuncData {
            name: name1.clone(),
            signature: sig1,
        });
        let _ext2 = func2.import_function(ir::ExtFuncData {
            name: name1.clone(),
            signature: sig2,
        });

        let gv_name0 = ir::ExternalName::new("gv_ext0");
        let gv_name1 = ir::ExternalName::new("gv_ext1");

        func0.create_global_var(ir::GlobalVarData::Sym { name: gv_name0.clone() });
        func1.create_global_var(ir::GlobalVarData::Sym { name: gv_name1.clone() });

        module.add_function(CompiledFunction {
            il: func0,
            compilation: None,
        });
        module.add_function(CompiledFunction {
            il: func1,
            compilation: None,
        });
        module.add_function(CompiledFunction {
            il: func2,
            compilation: None,
        });

        assert_eq!(module.imports.len(), 4);
        assert!(module.imports.contains_key(&name0));
        assert!(module.imports.contains_key(&name1));
        assert!(module.imports.contains_key(&gv_name0));
        assert!(module.imports.contains_key(&gv_name1));

        assert_eq!(module.exports.len(), 3);
        assert!(module.exports.contains_key(&func0_name));
        assert!(module.exports.contains_key(&func1_name));
        assert!(module.exports.contains_key(&func2_name));

        // Now define one of the names that was previously imported.
        let func_ext1 = ir::Function::with_name_signature(name1.clone(), sig);
        module.add_function(CompiledFunction {
            il: func_ext1,
            compilation: None,
        });
        assert_eq!(module.imports.len(), 3);
        assert!(!module.imports.contains_key(&name1));
    }
}
