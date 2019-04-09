"""
RISC-V Encoding recipes.

The encoding recipes defined here more or less correspond to the RISC-V native
instruction formats described in the reference:

    The RISC-V Instruction Set Manual
    Volume I: User-Level ISA
    Version 2.1
"""
from __future__ import absolute_import
from cdsl.isa import EncRecipe
from cdsl.predicates import IsSignedInt, IsUnsignedInt
from cdsl.registers import Stack
from base.formats import Binary, BinaryImm, MultiAry, IntCompare, IntCompareImm
from base.formats import Unary, UnaryImm, BranchIcmp, Branch, Jump
from base.formats import Call, CallIndirect, RegMove, Load, Store
from .registers import GPR, GPR8, SP, FPR, FPR8
from .settings import use_c

try:
    from typing import Tuple, Union, Sequence, TYPE_CHECKING  # noqa
    if TYPE_CHECKING:
        from cdsl.instructions import InstructionFormat # noqa
        from cdsl.predicates import PredNode
        from cdsl.registers import RegClass, Register
        OperandConstraint = Union[RegClass, Register, int, Stack]
        ConstraintSeq = Union[OperandConstraint, Tuple[OperandConstraint, ...]]
        BranchRange = Sequence[int]
        # A recipe predicate consisting of an ISA predicate and an instruction
        # predicate.
        RecipePred = Tuple[PredNode, PredNode]
except ImportError:
    pass

# The low 7 bits of a RISC-V instruction is the base opcode. All 32-bit
# instructions have 11 as the two low bits, with bits 6:2 determining the base
# opcode.
#
# Encbits for the 32-bit recipes are opcode[6:2] | (funct3 << 5) | ...
# The functions below encode the encbits.


def LOAD(funct3):
    # type: (int) -> int
    assert funct3 <= 0b111
    return 0b00000 | (funct3 << 5)


def STORE(funct3):
    # type: (int) -> int
    assert funct3 <= 0b111
    return 0b01000 | (funct3 << 5)


def BRANCH(funct3):
    # type: (int) -> int
    assert funct3 <= 0b111
    return 0b11000 | (funct3 << 5)


def JALR(funct3=0):
    # type: (int) -> int
    assert funct3 <= 0b111
    return 0b11001 | (funct3 << 5)


def JAL():
    # type: () -> int
    return 0b11011


def OPIMM(funct3, funct7=0):
    # type: (int, int) -> int
    assert funct3 <= 0b111
    return 0b00100 | (funct3 << 5) | (funct7 << 8)


def OPIMM32(funct3, funct7=0):
    # type: (int, int) -> int
    assert funct3 <= 0b111
    return 0b00110 | (funct3 << 5) | (funct7 << 8)


def OP(funct3, funct7):
    # type: (int, int) -> int
    assert funct3 <= 0b111
    assert funct7 <= 0b1111111
    return 0b01100 | (funct3 << 5) | (funct7 << 8)


def OP32(funct3, funct7):
    # type: (int, int) -> int
    assert funct3 <= 0b111
    assert funct7 <= 0b1111111
    return 0b01110 | (funct3 << 5) | (funct7 << 8)


def AIUPC():
    # type: () -> int
    return 0b00101


def LUI():
    # type: () -> int
    return 0b01101


def CR_OP(funct4):
    # type: (int) -> int
    assert funct4 <= 0b1111
    return funct4


def CS_OP(funct6, funct2):
    # type: (int, int) -> int
    assert funct6 <= 0b111111
    assert funct2 <= 0b11
    return funct2 | (funct6 << 2)


def CBshamt_OP(funct2, funct3):
    # type: (int, int) -> int
    assert funct2 <= 0b11
    assert funct3 <= 0b111
    return funct2 | (funct3 << 2)


def CB_OP(funct3):
    # type: (int) -> int
    assert funct3 <= 0b111
    return funct3


# R-type 32-bit instructions: These are mostly binary arithmetic instructions.
# The encbits are `opcode[6:2] | (funct3 << 5) | (funct7 << 8)
R = EncRecipe(
        'R', Binary, base_size=4, ins=(GPR, GPR), outs=GPR,
        emit='put_r(bits, in_reg0, in_reg1, out_reg0, sink);')

# R-type with an immediate shift amount instead of rs2.
Rshamt = EncRecipe(
        'Rshamt', BinaryImm, base_size=4, ins=GPR, outs=GPR,
        emit='put_rshamt(bits, in_reg0, imm.into(), out_reg0, sink);')

# R-type encoding of an integer comparison.
Ricmp = EncRecipe(
        'Ricmp', IntCompare, base_size=4, ins=(GPR, GPR), outs=GPR,
        emit='put_r(bits, in_reg0, in_reg1, out_reg0, sink);')

Ii = EncRecipe(
        'Ii', BinaryImm, base_size=4, ins=GPR, outs=GPR,
        instp=IsSignedInt(BinaryImm.imm, 12),
        emit='put_i(bits, in_reg0, imm.into(), out_reg0, sink);')

# I-type instruction with a hardcoded %x0 rs1.
Iz = EncRecipe(
        'Iz', UnaryImm, base_size=4, ins=(), outs=GPR,
        instp=IsSignedInt(UnaryImm.imm, 12),
        emit='put_i(bits, 0, imm.into(), out_reg0, sink);')

# I-type encoding of an integer comparison.
Iicmp = EncRecipe(
        'Iicmp', IntCompareImm, base_size=4, ins=GPR, outs=GPR,
        instp=IsSignedInt(IntCompareImm.imm, 12),
        emit='put_i(bits, in_reg0, imm.into(), out_reg0, sink);')

# I-type encoding for `jalr` as a return instruction. We won't use the
# immediate offset.
# The variable return values are not encoded.
Iret = EncRecipe(
        'Iret', MultiAry, base_size=4, ins=(), outs=(),
        emit='''
        // Return instructions are always a jalr to %x1.
        // The return address is provided as a special-purpose link argument.
        put_i(
            bits,
            1, // rs1 = %x1
            0, // no offset.
            0, // rd = %x0: no address written.
            sink,
        );
        ''')

# I-type encoding for `jalr` as a call_indirect.
Icall = EncRecipe(
        'Icall', CallIndirect, base_size=4, ins=GPR, outs=(),
        emit='''
        // call_indirect instructions are jalr with rd=%x1.
        put_i(
            bits,
            in_reg0,
            0, // no offset.
            1, // rd = %x1: link register.
            sink,
        );
        ''')


# Copy of a GPR is implemented as addi x, 0.
Icopy = EncRecipe(
        'Icopy', Unary, base_size=4, ins=GPR, outs=GPR,
        emit='put_i(bits, in_reg0, 0, out_reg0, sink);')

# Same for a GPR regmove.
Irmov = EncRecipe(
        'Irmov', RegMove, base_size=4, ins=GPR, outs=(),
        emit='put_i(bits, src, 0, dst, sink);')

# U-type instructions have a 20-bit immediate that targets bits 12-31.
U = EncRecipe(
        'U', UnaryImm, base_size=4, ins=(), outs=GPR,
        instp=IsSignedInt(UnaryImm.imm, 32, 12),
        emit='put_u(bits, imm.into(), out_reg0, sink);')

# UJ-type unconditional branch instructions.
UJ = EncRecipe(
        'UJ', Jump, base_size=4, ins=(), outs=(), branch_range=(0, 21),
        emit='''
        let dest = i64::from(func.offsets[destination]);
        let disp = dest - i64::from(sink.offset());
        put_uj(bits, disp, 0, sink);
        ''')

UJcall = EncRecipe(
        'UJcall', Call, base_size=4, ins=(), outs=(),
        emit='''
        sink.reloc_external(Reloc::RiscvCall,
                            &func.dfg.ext_funcs[func_ref].name,
                            0);
        // rd=%x1 is the standard link register.
        put_uj(bits, 0, 1, sink);
        ''')

# SB-type branch instructions.
SB = EncRecipe(
        'SB', BranchIcmp, base_size=4,
        ins=(GPR, GPR), outs=(),
        branch_range=(0, 13),
        emit='''
        let dest = i64::from(func.offsets[destination]);
        let disp = dest - i64::from(sink.offset());
        put_sb(bits, disp, in_reg0, in_reg1, sink);
        ''')

# SB-type branch instruction with rs2 fixed to zero.
SBzero = EncRecipe(
        'SBzero', Branch, base_size=4,
        ins=(GPR), outs=(),
        branch_range=(0, 13),
        emit='''
        let dest = i64::from(func.offsets[destination]);
        let disp = dest - i64::from(sink.offset());
        put_sb(bits, disp, in_reg0, 0, sink);
        ''')

# Spill of a GPR.
GPsp = EncRecipe(
        'GPsp', Unary, base_size=4,
        ins=GPR, outs=Stack(GPR),
        emit='unimplemented!();')

# Fill of a GPR.
GPfi = EncRecipe(
        'GPfi', Unary, base_size=4,
        ins=Stack(GPR), outs=GPR,
        emit='unimplemented!();')

# Recipes of the compressed instructions from the C extension. All instructions
# are 2 bytes and have a subset of the functionality of the normal instructions


def CompressedRecipe(
        name,               # type: str
        format,             # type: InstructionFormat
        ins,                # type: ConstraintSeq
        outs,               # type: ConstraintSeq
        branch_range=None,  # type: BranchRange
        instp=None,         # type: PredNode
        emit=None           # type: str
):
    # type: ( ... ) -> EncRecipe
    return EncRecipe(
        name, format, base_size=2,
        ins=ins, outs=outs,
        branch_range=branch_range,
        instp=instp,
        isap=use_c,
        emit=emit)


CR = CompressedRecipe(
        'CR', Binary,
        ins=(GPR, GPR), outs=0,
        emit='put_cr(bits, in_reg0, in_reg1, sink);')

CRrmov = CompressedRecipe(
        'CRrmov', RegMove,
        ins=GPR, outs=(),
        emit='put_cr(bits, dst, src, sink);')

CRcopy = CompressedRecipe(
        'CRcopy', Unary,
        ins=GPR, outs=GPR,
        emit='put_cr(bits, in_reg0, out_reg0, sink);')

CRicall = CompressedRecipe(
        'CRicall', CallIndirect,
        ins=GPR, outs=(),
        emit='put_cr(bits, in_reg0, 0, sink);')

CIshamt = CompressedRecipe(
        'CIshamt', BinaryImm,
        ins=GPR, outs=0,
        emit='put_ci_shamt(bits, in_reg0, imm.into(), sink);')

CI = CompressedRecipe(
        'CI', BinaryImm,
        ins=GPR, outs=0,
        emit='put_ci(bits, in_reg0, imm.into(), sink);',
        instp=IsSignedInt(BinaryImm.imm, 6))

CIW = CompressedRecipe(
        'CIW', BinaryImm,
        ins=SP, outs=GPR8,
        emit='put_ciw(bits, out_reg0, imm.into(), sink);',
        instp=IsSignedInt(BinaryImm.imm, 10, 2))

CIli = CompressedRecipe(
        'CIli', UnaryImm,
        ins=(), outs=GPR,
        emit='put_ci(bits, out_reg0, imm.into(), sink);',
        instp=IsSignedInt(UnaryImm.imm, 6))

CIlui = CompressedRecipe(
        'CIlui', UnaryImm,
        ins=(), outs=GPR,
        emit='''
        // Use of SP here encodes the ADDI16SP instruction, so the register
        // allocator should never give x2 to this recipe.
        use crate::isa::riscv::registers::RU;
        debug_assert!(out_reg0 != RU::x2 as RegUnit);
        put_cilui(bits, out_reg0, imm.into(), sink);
        ''',
        instp=IsSignedInt(UnaryImm.imm, 18, 12))

CIaddi16sp = CompressedRecipe(
        'CIaddi16sp', BinaryImm,
        ins=SP, outs=SP,
        emit='put_ci_addi16sp(bits, imm.into(), sink);',
        instp=IsSignedInt(BinaryImm.imm, 10, 4))

CS = CompressedRecipe(
        'CS', Binary,
        ins=(GPR8, GPR8), outs=0,
        emit='put_cs(bits, in_reg0, in_reg1, sink);')

CBshamt = CompressedRecipe(
        'CBshamt', BinaryImm,
        ins=GPR8, outs=0,
        emit='put_cb_shamt(bits, in_reg0, imm.into(), sink);',
        instp=IsSignedInt(BinaryImm.imm, 6))

CB = CompressedRecipe(
        'CB', Branch,
        ins=GPR8, outs=(),
        branch_range=(0, 9),
        emit='''
        let dest = i64::from(func.offsets[destination]);
        let disp = dest - i64::from(sink.offset());
        put_cb(bits, disp, in_reg0, sink);
        ''')

CJ = CompressedRecipe(
        'CJ', Jump,
        ins=(), outs=(),
        branch_range=(0, 12),
        emit='''
        let dest = i64::from(func.offsets[destination]);
        let disp = dest - i64::from(sink.offset());
        put_cj(bits, disp, sink);
        ''')

CJcall = CompressedRecipe(
        'CJcall', Call,
        ins=(), outs=(),
        branch_range=(0, 12),
        emit='''
        sink.reloc_external(Reloc::RiscvCall,
                            &func.dfg.ext_funcs[func_ref].name,
                            0);
        put_cj(bits, 0, sink);
        ''')


# Compressed integer loads/stores
CLw = CompressedRecipe(
        'CLw', Load,
        ins=GPR8, outs=GPR8,
        instp=IsUnsignedInt(Load.offset, 7, 2),
        emit='put_clw(bits, offset.into(), out_reg0, in_reg0, sink);')

CLd = CompressedRecipe(
        'CLd', Load,
        ins=GPR8, outs=GPR8,
        instp=IsUnsignedInt(Load.offset, 8, 3),
        emit='put_cld(bits, offset.into(), out_reg0, in_reg0, sink);')

CSw = CompressedRecipe(
        'CSw', Store,
        ins=(GPR8, GPR8), outs=(),
        instp=IsUnsignedInt(Store.offset, 7, 2),
        emit='put_csw(bits, offset.into(), in_reg0, in_reg1, sink);')

CSd = CompressedRecipe(
        'CSd', Store,
        ins=(GPR8, GPR8), outs=(),
        instp=IsUnsignedInt(Store.offset, 8, 3),
        emit='put_csd(bits, offset.into(), in_reg0, in_reg1, sink);')


# Compressed SP-relative integer loads/stores
CLwsp = CompressedRecipe(
        'CLwsp', Load,
        ins=SP, outs=GPR,
        instp=IsUnsignedInt(Load.offset, 8, 2),
        emit='put_clwsp(bits, offset.into(), out_reg0, sink);')

CLdsp = CompressedRecipe(
        'CLdsp', Load,
        ins=SP, outs=GPR,
        instp=IsUnsignedInt(Load.offset, 9, 3),
        emit='put_cldsp(bits, offset.into(), out_reg0, sink);')

CSwsp = CompressedRecipe(
        'CSwsp', Store,
        ins=(SP, GPR), outs=(),
        instp=IsUnsignedInt(Store.offset, 8, 2),
        emit='put_cswsp(bits, offset.into(), in_reg1, sink);')

CSdsp = CompressedRecipe(
        'CSdsp', Store,
        ins=(SP, GPR), outs=(),
        instp=IsUnsignedInt(Store.offset, 9, 3),
        emit='put_csdsp(bits, offset.into(), in_reg1, sink);')


# Compressed floating point loads/stores
CLfw = CompressedRecipe(
    'CLfw', Load,
    ins=FPR8, outs=FPR8,
    instp=IsUnsignedInt(Load.offset, 7, 2),
    emit='put_clw(bits, offset.into(), out_reg0, in_reg0, sink);')

CLfd = CompressedRecipe(
    'CLfd', Load,
    ins=FPR8, outs=FPR8,
    instp=IsUnsignedInt(Load.offset, 8, 3),
    emit='put_cld(bits, offset.into(), out_reg0, in_reg0, sink);')

CSfw = CompressedRecipe(
    'CSfw', Store,
    ins=(FPR8, FPR8), outs=(),
    instp=IsUnsignedInt(Store.offset, 7, 2),
    emit='put_csw(bits, offset.into(), in_reg0, in_reg1, sink);')

CSfd = CompressedRecipe(
    'CSfd', Store,
    ins=(FPR8, FPR8), outs=(),
    instp=IsUnsignedInt(Store.offset, 8, 3),
    emit='put_csd(bits, offset.into(), in_reg0, in_reg1, sink);')


# Compressed SP-relative floating point loads/stores
CLfwsp = CompressedRecipe(
    'CLfwsp', Load,
    ins=SP, outs=FPR,
    instp=IsUnsignedInt(Load.offset, 8, 2),
    emit='put_clwsp(bits, offset.into(), out_reg0, sink);')

CLfdsp = CompressedRecipe(
    'CLfdsp', Load,
    ins=SP, outs=FPR,
    instp=IsUnsignedInt(Load.offset, 9, 3),
    emit='put_cldsp(bits, offset.into(), out_reg0, sink);')

CSfwsp = CompressedRecipe(
    'CSfwsp', Store,
    ins=(SP, FPR), outs=(),
    instp=IsUnsignedInt(Store.offset, 8, 2),
    emit='put_cswsp(bits, offset.into(), in_reg1, sink);')

CSfdsp = CompressedRecipe(
    'CSfdsp', Store,
    ins=(SP, FPR), outs=(),
    instp=IsUnsignedInt(Store.offset, 9, 3),
    emit='put_csdsp(bits, offset.into(), in_reg1, sink);')
