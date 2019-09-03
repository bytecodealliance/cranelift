#![allow(non_upper_case_globals)]

use crate::cdsl::operands::{OperandKind, OperandKindBuilder as Builder, OperandKindFields};

lazy_static! {
    /// A reference to an extended basic block in the same function.
    /// This is primarliy used in control flow instructions.
    pub static ref Ebb: OperandKind = {
        create("ebb", "An extended basic block in the same function.")
            .default_member("destination")
            .build()
    };

    /// A reference to a stack slot declared in the function preamble.
    pub static ref StackSlot: OperandKind = {
        create("stack_slot", "A stack slot").build()
    };

    /// A reference to a global value.
    pub static ref GlobalValue: OperandKind = {
        create("global_value", "A global value.").build()
    };

    /// A reference to a function signature declared in the function preamble.
    /// This is used to provide the call signature in a call_indirect instruction.
    pub static ref SigRef: OperandKind = {
        create("sig_ref", "A function signature.").build()
    };

    /// A reference to an external function declared in the function preamble.
    /// This is used to provide the callee and signature in a call instruction.
    pub static ref FuncRef: OperandKind = {
        create("func_ref", "An external function.").build()
    };

    /// A reference to a jump table declared in the function preamble.
    pub static ref JumpTable: OperandKind = {
        create("jump_table", "A jump table.")
            .default_member("table")
            .build()
    };

    /// A reference to a heap declared in the function preamble.
    pub static ref Heap: OperandKind = {
        create("heap", "A heap.").build()
    };

    /// A reference to a table declared in the function preamble.
    pub static ref Table: OperandKind = {
        create("table", "A table.").build()
    };

    /// A variable-sized list of value operands. Use for Ebb and function call arguments.
    pub static ref VarArgs: OperandKind = {
        Builder::new("variable_args", OperandKindFields::VariableArgs)
            .doc(
                r#"
                A variable size list of `value` operands.

                Use this to represent arguments passed to a function call, arguments
                passed to an extended basic block, or a variable number of results
                returned from an instruction.
            "#,
            )
            .build()
    };
}

/// Small helper to initialize an OperandBuilder with the right kind, for a given name and doc.
fn create(name: &'static str, doc: &'static str) -> Builder {
    Builder::new(name, OperandKindFields::EntityRef).doc(doc)
}
