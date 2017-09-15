"""
RISC-V register banks.
"""
from __future__ import absolute_import
from cdsl.registers import RegBank, RegClass
from .defs import ISA


# We include `x0`, a.k.a `zero` in the register bank. It will be reserved.
IntRegs = RegBank(
        'IntRegs', ISA,
        'General purpose registers',
        units=32, prefix='x')

FloatRegs = RegBank(
        'FloatRegs', ISA,
        'Floating point registers',
        units=32, prefix='f')

GPR = RegClass(IntRegs)
# The popular registers used in some compressed instructions; x8-x15
GPRC = GPR[8:15]
FPR = RegClass(FloatRegs)

RegClass.extract_names(globals())
