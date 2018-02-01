"""
Custom legalization patterns for Intel.
"""
from __future__ import absolute_import
from cdsl.ast import Var
from cdsl.xform import Rtl, XFormGroup
from base.immediates import imm64, intcc, floatcc
from base.types import i32, i64
from base import legalize as shared
from base import instructions as insts
from . import instructions as x86
from .defs import ISA

intel_expand = XFormGroup(
        'intel_expand',
        """
        Legalize instructions by expansion.

        Use Intel-specific instructions if needed.
        """,
        isa=ISA, chain=shared.expand)

a = Var('a')
dead = Var('dead')
x = Var('x')
xhi = Var('xhi')
y = Var('y')
a1 = Var('a1')
a2 = Var('a2')

#
# Division and remainder.
#
intel_expand.legalize(
        a << insts.udiv(x, y),
        Rtl(
            xhi << insts.iconst(imm64(0)),
            (a, dead) << x86.udivmodx(x, xhi, y)
        ))

intel_expand.legalize(
        a << insts.urem(x, y),
        Rtl(
            xhi << insts.iconst(imm64(0)),
            (dead, a) << x86.udivmodx(x, xhi, y)
        ))

for ty in [i32, i64]:
    intel_expand.legalize(
            a << insts.sdiv.bind(ty)(x, y),
            Rtl(
                xhi << insts.sshr_imm(x, imm64(ty.lane_bits() - 1)),
                (a, dead) << x86.sdivmodx(x, xhi, y)
            ))

# The srem expansion requires custom code because srem INT_MIN, -1 is not
# allowed to trap.
intel_expand.custom_legalize(insts.srem, 'expand_srem')

# Floating point condition codes.
#
# The 8 condition codes in `supported_floatccs` are directly supported by a
# `ucomiss` or `ucomisd` instruction. The remaining codes need legalization
# patterns.

# Equality needs an explicit `ord` test which checks the parity bit.
intel_expand.legalize(
        a << insts.fcmp(floatcc.eq, x, y),
        Rtl(
            a1 << insts.fcmp(floatcc.ord, x, y),
            a2 << insts.fcmp(floatcc.ueq, x, y),
            a << insts.band(a1, a2)
        ))
intel_expand.legalize(
        a << insts.fcmp(floatcc.ne, x, y),
        Rtl(
            a1 << insts.fcmp(floatcc.uno, x, y),
            a2 << insts.fcmp(floatcc.one, x, y),
            a << insts.bor(a1, a2)
        ))

# Inequalities that need to be reversed.
for cc,               rev_cc in [
        (floatcc.lt,  floatcc.gt),
        (floatcc.le,  floatcc.ge),
        (floatcc.ugt, floatcc.ult),
        (floatcc.uge, floatcc.ule)]:
    intel_expand.legalize(
            a << insts.fcmp(cc, x, y),
            Rtl(
                a << insts.fcmp(rev_cc, y, x)
            ))

# We need to modify the CFG for min/max legalization.
intel_expand.custom_legalize(insts.fmin, 'expand_minmax')
intel_expand.custom_legalize(insts.fmax, 'expand_minmax')

# Conversions from unsigned need special handling.
intel_expand.custom_legalize(insts.fcvt_from_uint, 'expand_fcvt_from_uint')
# Conversions from float to int can trap.
intel_expand.custom_legalize(insts.fcvt_to_sint, 'expand_fcvt_to_sint')
intel_expand.custom_legalize(insts.fcvt_to_uint, 'expand_fcvt_to_uint')

# Count leading and trailing zeroes, for baseline x86_64
c_minus_one   = Var('c_minus_one')
c_thirty_one  = Var('c_thirty_one')
c_sixty_three = Var('c_sixty_three')
index1        = Var('index1')
r2flags       = Var('r2flags')
index2        = Var('index2')

intel_expand.legalize(
    a << insts.clz.i64(x),
    Rtl(
        c_minus_one << insts.iconst(imm64(-1)),
        c_sixty_three << insts.iconst(imm64(63)),
        (index1, r2flags) << x86.bsr(x),
        index2 << insts.selectif(intcc.eq, r2flags, c_minus_one, index1),
        a << insts.isub(c_sixty_three, index2),
    ))

intel_expand.legalize(
    a << insts.clz.i32(x),
    Rtl(
        c_minus_one << insts.iconst(imm64(-1)),
        c_thirty_one << insts.iconst(imm64(31)),
        (index1, r2flags) << x86.bsr(x),
        index2 << insts.selectif(intcc.eq, r2flags, c_minus_one, index1),
        a << insts.isub(c_thirty_one, index2),
    ))

intel_expand.legalize(
    a << insts.ctz.i64(x),
    Rtl(
        c_minus_one << insts.iconst(imm64(-1)),
        c_sixty_three << insts.iconst(imm64(63)),
        (index1, r2flags) << x86.bsf(x),
        index2 << insts.selectif(intcc.eq, r2flags, c_minus_one, index1),
        a << insts.isub(c_sixty_three, index2),
    ))

intel_expand.legalize(
    a << insts.ctz.i32(x),
    Rtl(
        c_minus_one << insts.iconst(imm64(-1)),
        c_thirty_one << insts.iconst(imm64(31)),
        (index1, r2flags) << x86.bsf(x),
        index2 << insts.selectif(intcc.eq, r2flags, c_minus_one, index1),
        a << insts.isub(c_thirty_one, index2),
    ))


# Population count for baseline x86_64
v1 = Var('v1')
v3 = Var('v3')
v4 = Var('v4')
v5 = Var('v5')
v6 = Var('v6')
v7 = Var('v7')
v8 = Var('v8')
v9 = Var('v9')
v10 = Var('v10')
v11 = Var('v11')
v12 = Var('v12')
v13 = Var('v13')
v14 = Var('v14')
v15 = Var('v15')
v16 = Var('v16')
c77 = Var('c77')
c0F = Var('c0F')
c01 = Var('c01')
intel_expand.legalize(
    v16 << insts.popcnt.i64(v1),
    Rtl(
        v3  << insts.ushr_imm (v1, 1),
        c77 << insts.iconst   (imm64(0x7777777777777777)),
        v4  << insts.band     (v3, c77),
        v5  << insts.isub     (v1, v4),
        v6  << insts.ushr_imm (v4, 1),
        v7  << insts.band     (v6, c77),
        v8  << insts.isub     (v5, v7),
        v9  << insts.ushr_imm (v7, 1),
        v10 << insts.band     (v9, c77),
        v11 << insts.isub     (v8, v10),
        v12 << insts.ushr_imm (v11, 4),
        v13 << insts.iadd     (v11, v12),
        c0F << insts.iconst   (imm64(0x0F0F0F0F0F0F0F0F)),
        v14 << insts.band     (v13, c0F),
        c01 << insts.iconst   (imm64(0x0101010101010101)),
        v15 << insts.imul     (v14, c01),
        v16 << insts.ushr_imm (v15, 56)
    ))
