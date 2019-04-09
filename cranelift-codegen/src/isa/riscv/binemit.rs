//! Emitting binary RISC-V machine code.

use crate::binemit::{bad_encoding, CodeSink, Reloc};
use crate::ir::{Function, Inst, InstructionData};
use crate::isa::{RegUnit, StackBaseMask, StackRef};
use crate::predicates::is_signed_int;
use crate::regalloc::RegDiversions;
use core::u32;

include!(concat!(env!("OUT_DIR"), "/binemit-riscv.rs"));

/// R-type instructions.
///
///   31     24  19  14     11 6
///   funct7 rs2 rs1 funct3 rd opcode
///       25  20  15     12  7      0
///
/// Encoding bits: `opcode[6:2] | (funct3 << 5) | (funct7 << 8)`.
fn put_r<CS: CodeSink + ?Sized>(bits: u16, rs1: RegUnit, rs2: RegUnit, rd: RegUnit, sink: &mut CS) {
    let bits = u32::from(bits);
    let opcode5 = bits & 0x1f;
    let funct3 = (bits >> 5) & 0x7;
    let funct7 = (bits >> 8) & 0x7f;
    let rs1 = u32::from(rs1) & 0x1f;
    let rs2 = u32::from(rs2) & 0x1f;
    let rd = u32::from(rd) & 0x1f;

    // 0-6: opcode
    let mut i = 0x3;
    i |= opcode5 << 2;
    i |= rd << 7;
    i |= funct3 << 12;
    i |= rs1 << 15;
    i |= rs2 << 20;
    i |= funct7 << 25;

    sink.put4(i);
}

/// R-type instructions with a shift amount instead of rs2.
///
///   31     25    19  14     11 6
///   funct7 shamt rs1 funct3 rd opcode
///       25    20  15     12  7      0
///
/// Both funct7 and shamt contribute to bit 25. In RV64, shamt uses it for shifts > 31.
///
/// Encoding bits: `opcode[6:2] | (funct3 << 5) | (funct7 << 8)`.
fn put_rshamt<CS: CodeSink + ?Sized>(
    bits: u16,
    rs1: RegUnit,
    shamt: i64,
    rd: RegUnit,
    sink: &mut CS,
) {
    let bits = u32::from(bits);
    let opcode5 = bits & 0x1f;
    let funct3 = (bits >> 5) & 0x7;
    let funct7 = (bits >> 8) & 0x7f;
    let rs1 = u32::from(rs1) & 0x1f;
    let shamt = shamt as u32 & 0x3f;
    let rd = u32::from(rd) & 0x1f;

    // 0-6: opcode
    let mut i = 0x3;
    i |= opcode5 << 2;
    i |= rd << 7;
    i |= funct3 << 12;
    i |= rs1 << 15;
    i |= shamt << 20;
    i |= funct7 << 25;

    sink.put4(i);
}

/// I-type instructions.
///
///   31  19  14     11 6
///   imm rs1 funct3 rd opcode
///    20  15     12  7      0
///
/// Encoding bits: `opcode[6:2] | (funct3 << 5)`
fn put_i<CS: CodeSink + ?Sized>(bits: u16, rs1: RegUnit, imm: i64, rd: RegUnit, sink: &mut CS) {
    let bits = u32::from(bits);
    let opcode5 = bits & 0x1f;
    let funct3 = (bits >> 5) & 0x7;
    let rs1 = u32::from(rs1) & 0x1f;
    let rd = u32::from(rd) & 0x1f;

    // 0-6: opcode
    let mut i = 0x3;
    i |= opcode5 << 2;
    i |= rd << 7;
    i |= funct3 << 12;
    i |= rs1 << 15;
    i |= (imm << 20) as u32;

    sink.put4(i);
}

/// U-type instructions.
///
///   31  11 6
///   imm rd opcode
///    12  7      0
///
/// Encoding bits: `opcode[6:2] | (funct3 << 5)`
fn put_u<CS: CodeSink + ?Sized>(bits: u16, imm: i64, rd: RegUnit, sink: &mut CS) {
    let bits = u32::from(bits);
    let opcode5 = bits & 0x1f;
    let rd = u32::from(rd) & 0x1f;

    // 0-6: opcode
    let mut i = 0x3;
    i |= opcode5 << 2;
    i |= rd << 7;
    i |= imm as u32 & 0xfffff000;

    sink.put4(i);
}

/// SB-type branch instructions.
///
///   31  24  19  14     11  6
///   imm rs2 rs1 funct3 imm opcode
///    25  20  15     12   7      0
///
/// Encoding bits: `opcode[6:2] | (funct3 << 5)`
fn put_sb<CS: CodeSink + ?Sized>(bits: u16, imm: i64, rs1: RegUnit, rs2: RegUnit, sink: &mut CS) {
    let bits = u32::from(bits);
    let opcode5 = bits & 0x1f;
    let funct3 = (bits >> 5) & 0x7;
    let rs1 = u32::from(rs1) & 0x1f;
    let rs2 = u32::from(rs2) & 0x1f;

    debug_assert!(is_signed_int(imm, 13, 1), "SB out of range {:#x}", imm);
    let imm = imm as u32;

    // 0-6: opcode
    let mut i = 0x3;
    i |= opcode5 << 2;
    i |= funct3 << 12;
    i |= rs1 << 15;
    i |= rs2 << 20;

    // The displacement is completely hashed up.
    i |= ((imm >> 11) & 0x1) << 7;
    i |= ((imm >> 1) & 0xf) << 8;
    i |= ((imm >> 5) & 0x3f) << 25;
    i |= ((imm >> 12) & 0x1) << 31;

    sink.put4(i);
}

/// UJ-type jump instructions.
///
///   31  11 6
///   imm rd opcode
///    12  7      0
///
/// Encoding bits: `opcode[6:2]`
fn put_uj<CS: CodeSink + ?Sized>(bits: u16, imm: i64, rd: RegUnit, sink: &mut CS) {
    let bits = u32::from(bits);
    let opcode5 = bits & 0x1f;
    let rd = u32::from(rd) & 0x1f;

    debug_assert!(is_signed_int(imm, 21, 1), "UJ out of range {:#x}", imm);
    let imm = imm as u32;

    // 0-6: opcode
    let mut i = 0x3;
    i |= opcode5 << 2;
    i |= rd << 7;

    // The displacement is completely hashed up.
    i |= imm & 0xff000;
    i |= ((imm >> 11) & 0x1) << 20;
    i |= ((imm >> 1) & 0x3ff) << 21;
    i |= ((imm >> 20) & 0x1) << 31;

    sink.put4(i);
}

/// CR-type instructions.
///
///   15     11     6   1
///   funct4 rd/rs1 rs2 op
///       12      7   2  0
///
/// Encoding bits: `funct4`.
fn put_cr<CS: CodeSink + ?Sized>(bits: u16, rs1: RegUnit, rs2: RegUnit, sink: &mut CS) {
    const OP: u16 = 0b10;
    let funct4 = bits & 0xF;
    let rs1 = rs1 & 0x1F;
    let rs2 = rs2 & 0x1F;

    let mut i = OP;
    i |= rs2 << 2;
    i |= rs1 << 7;
    i |= funct4 << 12;

    sink.put2(i);
}

/// CS-type instructions.
///
///   15     9      6      4   1
///   funct6 rd/rs1 funct2 rs2 op
///       10      7      5   2  0
///
/// Encoding bits: `funct2 | (funct6 << 2)`.
fn put_cs<CS: CodeSink + ?Sized>(bits: u16, rs1: RegUnit, rs2: RegUnit, sink: &mut CS) {
    const OP: u16 = 0b01;
    let funct2 = bits & 0x3;
    let funct6 = (bits >> 2) & 0x3F;
    let rs1 = rs1 & 0x7;
    let rs2 = rs2 & 0x7;

    let mut i = OP;
    i |= rs2 << 2;
    i |= funct2 << 5;
    i |= rs1 << 7;
    i |= funct6 << 10;

    sink.put2(i);
}

/// CBshamt-type instructions.
///
///   15     12       11     9      6          1
///   funct3 shamt[5] funct2 rd/rs1 shamt[4:0] op
///       13       12     10      7          2  0
///
/// Encoding bits: `funct2 | (funct3 << 2)`.
fn put_cb_shamt<CS: CodeSink + ?Sized>(bits: u16, rs1: RegUnit, shamt: i64, sink: &mut CS) {
    const OP: u16 = 0b01;
    let funct2 = bits & 0x3;
    let funct3 = (bits >> 2) & 0x7;
    let rs1 = rs1 & 0x7;
    let shamt = (shamt as u16) & 0x3F;

    let mut i = OP;
    i |= (shamt & 0x1F) << 2;
    i |= rs1 << 7;
    i |= funct2 << 10;
    i |= (shamt >> 5) << 12;
    i |= funct3 << 13;

    sink.put2(i);
}

/// CB-type instructions.
///
///   15     12            9   6                 1
///   funct3 offset[8|4:3] rs1 offset[7:6|2:1|5] op
///       13            10   7                 2  0
///
/// Encoding bits: `funct3`.
fn put_cb<CS: CodeSink + ?Sized>(bits: u16, disp: i64, rs1: RegUnit, sink: &mut CS) {
    const OP: u16 = 0b01;
    let funct3 = bits & 0x7;
    let rs1 = rs1 & 0x7;
    let offset = (disp as u16) & 0xFF;

    let mut i = OP;
    i |= ((offset >> 5) & 0b1) << 2;
    i |= ((offset >> 1) & 0b11) << 3;
    i |= ((offset >> 6) & 0b11) << 5;
    i |= rs1 << 7;
    i |= ((offset >> 3) & 0b11) << 10;
    i |= ((offset >> 8) & 0b1) << 12;
    i |= funct3 << 13;

    sink.put2(i);
}

/// CJ-type instructions.
///
///   15     12                            1
///   funct3 offset[11|4|9:8|10|6|7|3:1|5] op
///       13                             2  0
///
/// Encoding bits: `funct3`.
fn put_cj<CS: CodeSink + ?Sized>(bits: u16, disp: i64, sink: &mut CS) {
    const OP: u16 = 0b01;
    let funct3 = bits & 0x7;
    let offset = (disp as u16) & 0xFF;

    let mut i = OP;
    i |= ((offset >> 5) & 0b1) << 2;
    i |= ((offset >> 1) & 0b111) << 3;
    i |= ((offset >> 7) & 0b1) << 6;
    i |= ((offset >> 6) & 0b1) << 7;
    i |= ((offset >> 10) & 0b1) << 8;
    i |= ((offset >> 8) & 0b11) << 9;
    i |= ((offset >> 4) & 0b1) << 11;
    i |= ((offset >> 11) & 0b1) << 12;
    i |= funct3 << 13;

    sink.put2(i);
}

/// CIshamt-type instructions.
///
///   15     12       11     6          1
///   funct3 shamt[5] rd/rs1 shamt[4:0] op
///       13       12      7          2  0
///
/// Encoding bits: `funct3`.
fn put_ci_shamt<CS: CodeSink + ?Sized>(bits: u16, rs1: RegUnit, shamt: i64, sink: &mut CS) {
    const OP: u16 = 0b10;
    let funct3 = bits & 0x7;
    let rs1 = rs1 & 0x1F;
    let shamt = (shamt as u16) & 0x3F;

    let mut i = OP;
    i |= (shamt & 0x1F) << 2;
    i |= rs1 << 7;
    i |= (shamt >> 5) << 12;
    i |= funct3 << 13;

    sink.put2(i);
}

/// CIlui-type instructions.
///
///   15     12      11 6          1
///   funct3 imm[17] rd imm[16:12] op
///       13      12  7          2  0
///
/// Encoding bits: `funct3`.
fn put_cilui<CS: CodeSink + ?Sized>(bits: u16, rd: RegUnit, imm: i64, sink: &mut CS) {
    const OP: u16 = 0b01;
    let funct3 = bits & 0x7;
    let rd = rd & 0x1F;

    let mut i = OP;
    i |= (((imm >> 12) & 0x1F) as u16) << 2;
    i |= rd << 7;
    i |= (((imm >> 17) & 0b1) as u16) << 12;
    i |= funct3 << 13;

    sink.put2(i);
}

/// CIaddi16sp-type instructions.
///
///   15     12       11     6                1
///   funct3 nzimm[9] SP(x2) nzimm[4|6|8:7|5] op
///       13      12       7                2  0
///
/// Encoding bits: `funct3`.
fn put_ci_addi16sp<CS: CodeSink + ?Sized>(bits: u16, imm: i64, sink: &mut CS) {
    const OP: u16 = 0b01;
    const RD: u16 = 2;
    let funct3 = bits & 0x7;
    let imm = (imm as u16) & 0x3FF;

    let mut i = OP;
    i |= ((imm >> 5) & 0b1) << 2;
    i |= ((imm >> 7) & 0b11) << 3;
    i |= ((imm >> 6) & 0b1) << 5;
    i |= ((imm >> 4) & 0b1) << 6;
    i |= RD << 7;
    i |= ((imm >> 9) & 0b1) << 12;
    i |= funct3 << 13;

    sink.put2(i);
}

/// CIli-type instructions.
///
///   15     12     11 6        1
///   funct3 imm[5] rd imm[4:0] op
///       13     12  7        2  0
///
/// Encoding bits: `funct3`.
fn put_ci<CS: CodeSink + ?Sized>(bits: u16, rd: RegUnit, imm: i64, sink: &mut CS) {
    const OP: u16 = 0b01;
    let funct3 = bits & 0x7;
    let rd = rd & 0x1F;

    let mut i = OP;
    i |= ((imm & 0x1F) as u16) << 2;
    i |= rd << 7;
    i |= (((imm >> 5) & 0b1) as u16) << 12;
    i |= funct3 << 13;

    sink.put2(i);
}

/// CIW-type instructions.
///
///   15     12                  4  1
///   funct3 nzuimm[5:4|9:6|2|3] rd op
///       13                   5  2  0
///
/// Encoding bits: `funct3`.
fn put_ciw<CS: CodeSink + ?Sized>(bits: u16, rd: RegUnit, imm: i64, sink: &mut CS) {
    const OP: u16 = 0b00;
    let funct3 = bits & 0x7;
    let rd = rd & 0x7;
    let imm = (imm as u16) & 0x3F;

    let mut i = OP;
    i |= rd << 2;
    i |= ((imm >> 3) & 0b1) << 5;
    i |= ((imm >> 2) & 0b1) << 6;
    i |= ((imm >> 6) & 0b1111) << 7;
    i |= ((imm >> 4) & 0b11) << 11;
    i |= funct3 << 13;

    sink.put2(i);
}

/// CLd-type instructions.
///
///   15     12        9   6         4  1
///   funct3 uimm[5:3] rs1 uimm[7:6] rd op
///       13        10   7         5  2  0
///
/// Encoding bits: `funct3`.
fn put_cld<CS: CodeSink + ?Sized>(
    bits: u16,
    offset: i32,
    rd: RegUnit,
    rs1: RegUnit,
    sink: &mut CS,
) {
    const OP: u16 = 0b00;
    let funct3 = bits & 0x7;
    let rd = rd & 0x7;
    let rs1 = rs1 & 0x7;
    let offset = (offset as u16) & 0xFF;

    let mut i = OP;
    i |= rd << 2;
    i |= ((offset >> 6) & 0b11) << 5;
    i |= rs1 << 7;
    i |= ((offset >> 3) & 0b111) << 10;
    i |= funct3 << 13;

    sink.put2(i);
}

/// CLw-type instructions.
///
///   15     12        9   6         4  1
///   funct3 uimm[5:3] rs1 uimm[2|6] rd op
///       13        10   7         5  2  0
///
/// Encoding bits: `funct3`.
fn put_clw<CS: CodeSink + ?Sized>(
    bits: u16,
    offset: i32,
    rd: RegUnit,
    rs1: RegUnit,
    sink: &mut CS,
) {
    const OP: u16 = 0b00;
    let funct3 = bits & 0x7;
    let rd = rd & 0x7;
    let rs1 = rs1 & 0x7;
    let offset = (offset as u16) & 0x7F;

    let mut i = OP;
    i |= rd << 2;
    i |= ((offset >> 6) & 0b1) << 5;
    i |= ((offset >> 2) & 0b1) << 6;
    i |= rs1 << 7;
    i |= ((offset >> 3) & 0b111) << 10;
    i |= funct3 << 13;

    sink.put2(i);
}

/// CSd-type instructions.
///
///   15     12        9   6         4   1
///   funct3 uimm[5:3] rs1 uimm[7:6] rs2 op
///       13        10   7         5   2  0
///
/// Encoding bits: `funct3`.
fn put_csd<CS: CodeSink + ?Sized>(
    bits: u16,
    offset: i32,
    rs1: RegUnit,
    rs2: RegUnit,
    sink: &mut CS,
) {
    const OP: u16 = 0b00;
    let funct3 = bits & 0x7;
    let rs1 = rs1 & 0x7;
    let rs2 = rs2 & 0x7;
    let offset = (offset as u16) & 0xFF;

    let mut i = OP;
    i |= rs2 << 2;
    i |= ((offset >> 6) & 0b11) << 5;
    i |= rs1 << 7;
    i |= ((offset >> 3) & 0b111) << 10;
    i |= funct3 << 13;

    sink.put2(i);
}

/// CSw-type instructions.
///
///   15     12        9   6         4   1
///   funct3 uimm[5:3] rs1 uimm[2|6] rs2 op
///       13        10   7         5   2  0
///
/// Encoding bits: `funct3`.
fn put_csw<CS: CodeSink + ?Sized>(
    bits: u16,
    offset: i32,
    rs1: RegUnit,
    rs2: RegUnit,
    sink: &mut CS,
) {
    const OP: u16 = 0b00;
    let funct3 = bits & 0x7;
    let rs1 = rs1 & 0x7;
    let rs2 = rs2 & 0x7;
    let offset = (offset as u16) & 0x7F;

    let mut i = OP;
    i |= rs2 << 2;
    i |= ((offset >> 6) & 0b1) << 5;
    i |= ((offset >> 2) & 0b1) << 6;
    i |= rs1 << 7;
    i |= ((offset >> 3) & 0b111) << 10;
    i |= funct3 << 13;

    sink.put2(i);
}

/// CLwsp-type instructions.
///
///   15     12      11 6             1
///   funct3 uimm[5] rd uimm[4:2|7:6] op
///       13      12  7             2  0
///
/// Encoding bits: `funct3`.
fn put_clwsp<CS: CodeSink + ?Sized>(bits: u16, offset: i32, rd: RegUnit, sink: &mut CS) {
    const OP: u16 = 0b10;
    let funct3 = bits & 0x7;
    let rd = rd & 0x1F;
    let offset = (offset as u16) & 0xFF;

    let mut i = OP;
    i |= ((offset >> 6) & 0b11) << 2;
    i |= ((offset >> 2) & 0b111) << 4;
    i |= rd << 7;
    i |= ((offset >> 5) & 0b1) << 12;
    i |= funct3 << 13;

    sink.put2(i);
}

/// CLdsp-type instructions.
///
///   15     12      11 6             1
///   funct3 uimm[5] rd uimm[4:3|8:6] op
///       13      12  7             2  0
///
/// Encoding bits: `funct3`.
fn put_cldsp<CS: CodeSink + ?Sized>(bits: u16, offset: i32, rd: RegUnit, sink: &mut CS) {
    const OP: u16 = 0b10;
    let funct3 = bits & 0x7;
    let rd = rd & 0x1F;
    let offset = (offset as u16) & 0x1FF;

    let mut i = OP;
    i |= ((offset >> 6) & 0b111) << 2;
    i |= ((offset >> 3) & 0b11) << 5;
    i |= rd << 7;
    i |= ((offset >> 5) & 0b1) << 12;
    i |= funct3 << 13;

    sink.put2(i);
}

/// CSwsp-type instructions.
///
///   15     12            6   1
///   funct3 uimm[5:2|7:6] rs2 op
///       13             7   2  0
///
/// Encoding bits: `funct3`.
fn put_cswsp<CS: CodeSink + ?Sized>(bits: u16, offset: i32, rs2: RegUnit, sink: &mut CS) {
    const OP: u16 = 0b10;
    let funct3 = bits & 0x7;
    let rs2 = rs2 & 0x1F;
    let offset = (offset as u16) & 0xFF;

    let mut i = OP;
    i |= rs2 << 2;
    i |= ((offset >> 6) & 0b11) << 7;
    i |= ((offset >> 2) & 0b1111) << 9;
    i |= funct3 << 13;

    sink.put2(i);
}

/// CSdsp-type instructions.
///
///   15     12            6   1
///   funct3 uimm[5:3|8:6] rs2 op
///       13             7   2  0
///
/// Encoding bits: `funct3`.
fn put_csdsp<CS: CodeSink + ?Sized>(bits: u16, offset: i32, rs2: RegUnit, sink: &mut CS) {
    const OP: u16 = 0b10;
    let funct3 = bits & 0x7;
    let rs2 = rs2 & 0x1F;
    let offset = (offset as u16) & 0x1FF;

    let mut i = OP;
    i |= rs2 << 2;
    i |= ((offset >> 6) & 0b111) << 7;
    i |= ((offset >> 3) & 0b111) << 10;
    i |= funct3 << 13;

    sink.put2(i);
}
