use crate::cdsl::formats::{FormatRegistry, InstructionFormatBuilder as Builder};

pub fn define() -> FormatRegistry {
    use crate::shared::entities::*;
    use crate::shared::immediates::*;

    let mut registry = FormatRegistry::new();

    registry.insert(Builder::new("Unary").value());
    registry.insert(Builder::new("UnaryImm").imm(&Imm64));
    registry.insert(Builder::new("UnaryImm128").imm(&Uimm128));
    registry.insert(Builder::new("UnaryIeee32").imm(&Ieee32));
    registry.insert(Builder::new("UnaryIeee64").imm(&Ieee64));
    registry.insert(Builder::new("UnaryBool").imm(&Boolean));
    registry.insert(Builder::new("UnaryGlobalValue").imm(&GlobalValue));

    registry.insert(Builder::new("Binary").value().value());
    registry.insert(Builder::new("BinaryImm").value().imm(&Imm64));

    // The select instructions are controlled by the second VALUE operand.
    // The first VALUE operand is the controlling flag which has a derived type.
    // The fma instruction has the same constraint on all inputs.
    registry.insert(
        Builder::new("Ternary")
            .value()
            .value()
            .value()
            .typevar_operand(1),
    );

    // Catch-all for instructions with many outputs and inputs and no immediate
    // operands.
    registry.insert(Builder::new("MultiAry").varargs());

    registry.insert(Builder::new("NullAry"));

    registry.insert(
        Builder::new("InsertLane")
            .value()
            .imm_with_name("lane", &Uimm8)
            .value(),
    );
    registry.insert(
        Builder::new("ExtractLane")
            .value()
            .imm_with_name("lane", &Uimm8),
    );

    registry.insert(Builder::new("IntCompare").imm(&IntCC).value().value());
    registry.insert(
        Builder::new("IntCompareImm")
            .imm(&IntCC)
            .value()
            .imm(&Imm64),
    );
    registry.insert(Builder::new("IntCond").imm(&IntCC).value());

    registry.insert(Builder::new("FloatCompare").imm(&FloatCC).value().value());
    registry.insert(Builder::new("FloatCond").imm(&FloatCC).value());;

    registry.insert(
        Builder::new("IntSelect")
            .imm(&IntCC)
            .value()
            .value()
            .value(),
    );

    registry.insert(Builder::new("Jump").imm(&Ebb).varargs());
    registry.insert(Builder::new("Branch").value().imm(&Ebb).varargs());
    registry.insert(
        Builder::new("BranchInt")
            .imm(&IntCC)
            .value()
            .imm(&Ebb)
            .varargs(),
    );
    registry.insert(
        Builder::new("BranchFloat")
            .imm(&FloatCC)
            .value()
            .imm(&Ebb)
            .varargs(),
    );
    registry.insert(
        Builder::new("BranchIcmp")
            .imm(&IntCC)
            .value()
            .value()
            .imm(&Ebb)
            .varargs(),
    );
    registry.insert(
        Builder::new("BranchTable")
            .value()
            .imm(&Ebb)
            .imm(&JumpTable),
    );
    registry.insert(
        Builder::new("BranchTableEntry")
            .value()
            .value()
            .imm(&Uimm8)
            .imm(&JumpTable),
    );
    registry.insert(Builder::new("BranchTableBase").imm(&JumpTable));
    registry.insert(Builder::new("IndirectJump").value().imm(&JumpTable));

    registry.insert(Builder::new("Call").imm(&FuncRef).varargs());
    registry.insert(Builder::new("CallIndirect").imm(&SigRef).value().varargs());
    registry.insert(Builder::new("FuncAddr").imm(&FuncRef));

    registry.insert(Builder::new("Load").imm(&MemFlags).value().imm(&Offset32));
    registry.insert(
        Builder::new("LoadComplex")
            .imm(&MemFlags)
            .varargs()
            .imm(&Offset32),
    );
    registry.insert(
        Builder::new("Store")
            .imm(&MemFlags)
            .value()
            .value()
            .imm(&Offset32),
    );
    registry.insert(
        Builder::new("StoreComplex")
            .imm(&MemFlags)
            .value()
            .varargs()
            .imm(&Offset32),
    );
    registry.insert(Builder::new("StackLoad").imm(&StackSlot).imm(&Offset32));
    registry.insert(
        Builder::new("StackStore")
            .value()
            .imm(&StackSlot)
            .imm(&Offset32),
    );

    // Accessing a WebAssembly heap.
    registry.insert(Builder::new("HeapAddr").imm(&Heap).value().imm(&Uimm32));

    // Accessing a WebAssembly table.
    registry.insert(Builder::new("TableAddr").imm(&Table).value().imm(&Offset32));

    registry.insert(
        Builder::new("RegMove")
            .value()
            .imm_with_name("src", &RegUnit)
            .imm_with_name("dst", &RegUnit),
    );
    registry.insert(
        Builder::new("CopySpecial")
            .imm_with_name("src", &RegUnit)
            .imm_with_name("dst", &RegUnit),
    );
    registry.insert(Builder::new("CopyToSsa").imm_with_name("src", &RegUnit));
    registry.insert(
        Builder::new("RegSpill")
            .value()
            .imm_with_name("src", &RegUnit)
            .imm_with_name("dst", &StackSlot),
    );
    registry.insert(
        Builder::new("RegFill")
            .value()
            .imm_with_name("src", &StackSlot)
            .imm_with_name("dst", &RegUnit),
    );

    registry.insert(Builder::new("Trap").imm(&TrapCode));
    registry.insert(Builder::new("CondTrap").value().imm(&TrapCode));
    registry.insert(
        Builder::new("IntCondTrap")
            .imm(&IntCC)
            .value()
            .imm(&TrapCode),
    );
    registry.insert(
        Builder::new("FloatCondTrap")
            .imm(&FloatCC)
            .value()
            .imm(&TrapCode),
    );

    registry
}
