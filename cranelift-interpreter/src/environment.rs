//! Implements the function environment (e.g. a name-to-function mapping) for interpretation.

use cranelift_codegen::ir::{FuncRef, Function};
use std::collections::HashMap;

#[derive(Default)]
pub struct Environment {
    functions: HashMap<FuncRef, Function>,
    function_name_to_index: Vec<String>,
}

impl From<Function> for Environment {
    fn from(f: Function) -> Self {
        let mut functions = HashMap::new();
        functions.insert(FuncRef::from_u32(0), f);
        Self {
            functions,
            function_name_to_index: vec![],
        }
    }
}

impl Environment {
    /// Add a function by name.
    pub fn add(&mut self, name: String, function: Function) {
        let func_ref = FuncRef::with_number(self.function_name_to_index.len() as u32)
            .expect("a valid function reference");
        self.function_name_to_index.push(name);
        self.functions.insert(func_ref, function);
    }

    /// Retrieve a function's index by name.
    pub fn index_of(&self, name: &str) -> Option<FuncRef> {
        self.function_name_to_index
            .iter()
            .position(|n| n == name)
            .map(|i| FuncRef::with_number(i as u32).expect("a valid function reference"))
        // TODO this may be more efficient as a hash map
    }

    /// Retrieve a function by its function reference.
    pub fn get_by_func_ref(&self, func_ref: FuncRef) -> Option<&Function> {
        self.functions.get(&func_ref)
    }

    /// Retrieve a function by its name.
    pub fn get_by_name(&self, name: &str) -> Option<&Function> {
        if let Some(fr) = self.index_of(name) {
            if let Some(func) = self.get_by_func_ref(fr) {
                return Some(func);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn addition() {
        let mut env = Environment::default();
        let a = "a";
        let f = Function::new();

        env.add(a.to_string(), f);
        assert!(env.get_by_name(a).is_some());
    }

    #[test]
    fn nonexistence() {
        let env = Environment::default();
        assert!(env.get_by_name("a").is_none());
    }
}
