from __future__ import absolute_import
from base.instructions import vselect, vsplit, vconcat, TxN, iconst, iadd,\
        uextend, sextend, Int, bint, Bool, iadd_cin, iB, b1
from .typevar import TypeVar
from .tc import tc_rtl, TCError, TCNotSubtype, io_shape, TCOverspecified,\
    TCUnderspecified, tc_xform, TCDisagree, TCUnderconstrainedInput,\
    TCRedef
from .ast import Var
from .xform import Rtl, XForm
from unittest import TestCase


class TypeCheckingBaseTest(TestCase):
    def setUp(self):
        self.vm1 = Var("vm1")
        self.v0 = Var("v0")
        self.v1 = Var("v1")
        self.v2 = Var("v2")
        self.v3 = Var("v3")
        self.v4 = Var("v4")
        self.v5 = Var("v5")
        self.v6 = Var("v6")
        self.v7 = Var("v7")
        self.v8 = Var("v8")
        self.v9 = Var("v9")
        self.imm0 = Var("imm0")

        self.simd4_256 = TypeVar("simd4_256", "", ints=True, floats=True,
                                 bools=True, simd=(4, 256))

        self.simd8_64 = TypeVar("simd8_64", "", ints=True, floats=True,
                                bools=True, simd=(8, 64))

        self.isimd8_64 = TypeVar("isimd8_64", "", ints=True, simd=(8, 64))

        self.b4_256 = TypeVar("b4_256", "", ints=False, floats=False,
                              bools=(1, 1), simd=(4, 256))

        self.i16_32 = TypeVar("i16-32", "", ints=(16, 32), scalars=True,
                              simd=False)

        self.b1 = TypeVar.singleton(b1)

    def ppFail(self, r, m, msg):
        msg += "\n Partially inferred types: \n"
        for (k, v) in m.items():
            msg += "{} := {}\n".format(k, v)
        self.fail(msg)

    def runTC(self, arg, initialEnv, res):
        try:
            if isinstance(arg, Rtl):
                m = tc_rtl(arg, initialEnv)
            else:
                assert isinstance(arg, XForm)
                m = tc_xform(arg, initialEnv)
        except TCError as e:
            m = e

        if m == res:
            return  # Success

        if (isinstance(res, TCError)):
            if (not isinstance(m, TCError)):
                self.ppFail(arg, m, "Expected exception {} - instead no error."
                            .format(res))
            elif (m != res):
                self.ppFail(arg, {},
                            "Expected exception {} instead got exc {}"
                            .format(repr(res), repr(m)))
        elif (isinstance(res, dict)):
            if (isinstance(m, TCError)):
                self.ppFail(arg, {},
                            "Unexpected exception: {}".format(repr(m)))
            else:
                for (k, v) in res.items():
                    if k not in m:
                        self.ppFail(arg, m,
                                    ": Did not infer type for {}".format(k))

                    if not (m[k] == res[k]):
                        self.ppFail(arg, m,
                                       "Inferred wrong type for {}. Got {} expected {}" # noqa
                                       .format(k, m[k], res[k]))


class TestIOShape(TypeCheckingBaseTest):
    def test_vselect(self):
        r = Rtl(
                self.v0 << vselect(self.v1, self.v2, self.v3)
        )

        self.assertEqual(io_shape(r),
                         (set([self.v1, self.v2, self.v3]),  # Inputs
                          set([self.v2]),  # Control Vars
                          set([]),  # Free Non-Derived TVs
                          set([self.v0])))  # Defs

    def test_double_split(self):
        r = Rtl(
                self.v0 << vselect(self.v1, self.v2, self.v3),
                (self.v4, self.v5) << vsplit(self.v0),
                (self.v6, self.v7) << vsplit(self.v4),
        )

        self.assertEqual(io_shape(r),
                         (set([self.v1, self.v2, self.v3]),  # Inputs
                          set([self.v2]),  # Control Vars
                          set([]),  # Free Non-Derived TVs
                          set([self.v0, self.v4, self.v5, self.v6,
                               self.v7])))  # Defs

    def test_pattern3(self):
        r = Rtl(
                self.v0 << vselect(self.v1, self.v2, self.v3),
                self.v4 << vconcat(self.v0, self.v0),
                self.v5 << vconcat(self.v4, self.vm1),
                (self.v6, self.v7) << vsplit(self.v0),
                (self.v8, self.v9) << vsplit(self.v6),
        )

        self.assertEqual(io_shape(r),
                         (set([self.v1, self.v2, self.v3, self.vm1]),  # Inputs
                          set([self.v2]),  # Control Vars
                          set([]),  # Free Non-Derived TVs
                          set([self.v0, self.v4, self.v5, self.v6,
                               self.v7, self.v8, self.v9])))  # Defs

    def test_iconst(self):
        # for iconst the type-controlling variable
        # is the definition (v0 in this case)
        r = Rtl(
                self.v0 << iconst(self.imm0),
        )

        self.assertEqual(io_shape(r),
                         (set([self.imm0]),  # Inputs
                          set([self.v0]),  # Control Vars
                          set([]),  # Free Non-Derived TVs
                          set([self.v0])))  # Defs

    def test_pattern4(self):
        # A more complex pattern involving iconst
        # The v2 control variable comes from inputs,
        # while the v1 control variable comes from defs
        r = Rtl(
                self.v1 << iconst(self.imm0),
                self.v0 << vselect(self.v1, self.v2, self.v3),
                self.v4 << vconcat(self.v0, self.v0),
                self.v5 << vconcat(self.v4, self.vm1),
                (self.v6, self.v7) << vsplit(self.v0),
                (self.v8, self.v9) << vsplit(self.v6),
        )

        self.assertEqual(io_shape(r),
                         (set([self.imm0, self.v2, self.v3,
                               self.vm1]),  # Inputs
                          set([self.v2, self.v1]),  # Control Vars
                          set([]),  # Free Non-Derived TVs
                          set([self.v1, self.v0, self.v4, self.v5, self.v6,
                               self.v7, self.v8, self.v9])))  # Defs

    def test_pattern4_swap(self):
        # Same code as test_pattern4, but self.vm1 and self.v4 are
        # swapped on line 4. This adds self.vm1 to the set of
        # control-relevant variables. This is due to the fact
        # that our algorithm is rather naive and doesn't do unification.
        r = Rtl(
                self.v1 << iconst(self.imm0),
                self.v0 << vselect(self.v1, self.v2, self.v3),
                self.v4 << vconcat(self.v0, self.v0),
                self.v5 << vconcat(self.vm1, self.v4),
                (self.v6, self.v7) << vsplit(self.v0),
                (self.v8, self.v9) << vsplit(self.v6),
        )

        self.assertEqual(io_shape(r),
                         (set([self.imm0, self.v2, self.v3,
                               self.vm1]),  # Inputs
                          set([self.v2, self.v1, self.vm1]),  # Control Vars
                          set([]),  # Free Non-Derived TVs
                          set([self.v1, self.v0, self.v4, self.v5, self.v6,
                               self.v7, self.v8, self.v9])))  # Defs


class TestTC(TypeCheckingBaseTest):
    def test_overspecified(self):
        # Should fail when we specify more than the minimum neccessary types
        r = Rtl(
                self.v0 << vselect(self.v1, self.v2, self.v3)
        )

        self.runTC(r,
                   {self.v2: self.simd8_64,
                    self.v3: self.simd8_64,
                    self.v1: self.simd8_64.as_bool()},
                   TCOverspecified(r, set([self.v3, self.v1])))

        self.runTC(r,
                   {self.v2: self.simd8_64,
                    self.v1: self.simd8_64.as_bool()},
                   TCOverspecified(r, set([self.v1])))

    def test_underspecified(self):
        # Should fail when we specify more than the minimum neccessary types
        r = Rtl(
                self.v0 << vselect(self.v1, self.v2, self.v3)
        )

        self.runTC(r,
                   {},
                   TCUnderspecified(r, set([self.v2])))

    def test_vselect(self):
        # Make sure we infer the type of v0 to be same as v2
        r = Rtl(
                self.v0 << vselect(self.v1, self.v2, self.v3)
        )

        self.runTC(r,
                   {self.v2: self.simd8_64},
                   {})

        self.runTC(r,
                   {self.v2: self.simd8_64},
                   {self.v0: self.simd8_64,
                    self.v3: self.simd8_64,
                    self.v1: self.simd8_64.as_bool()})

        self.runTC(r,
                   {self.v2: TxN},
                   {self.v0: TxN,
                    self.v2: TxN,
                    self.v3: TxN,
                    self.v1: TxN.as_bool()})

    def test_bad_vselect(self):
        r = Rtl(
                self.v1 << iconst(self.imm0),
                self.v0 << vselect(self.v1, self.v2, self.v3)
        )

        # Make sure we flag an error if v1 was specified
        # to be anything but as_bool(T) where v2: T
        self.runTC(r,
                   {self.v2: TxN, self.v1: TxN},
                   TCNotSubtype((r, 0, False, 0), None, None))

    def test_bad_double_split(self):
        r = Rtl(
                self.v0 << vselect(self.v1, self.v2, self.v3),
                (self.v4, self.v5) << vsplit(self.v0),
                (self.v6, self.v7) << vsplit(self.v4),
        )

        # v0 here has the same type as v2 - TxN which is 2..256 simd.
        # Therefore v4 has the type half_vector(TxN) which is 1..128 simd.
        # However the last vsplit requires v4 to be TxN (at least 2 lanes)
        # This causes a subtyping error

        self.runTC(r,
                   {self.v2: TxN},
                   TCNotSubtype((r, 2, True, 0), None, None))

    def test_double_split(self):
        r = Rtl(
                self.v0 << vselect(self.v1, self.v2, self.v3),
                (self.v4, self.v5) << vsplit(self.v0),
                (self.v6, self.v7) << vsplit(self.v4),
        )

        # v0 here has the same type as v2 - TxN which is 2..256 simd.
        # Therefore v4 has the type half_vector(TxN) which is 1..128 simd.
        # However the last vsplit requires v4 to be TxN (at least 2 lanes)
        # This causes a subtyping error

        self.runTC(r,
                   {self.v2: self.simd4_256},
                   {self.v0: self.simd4_256,
                    self.v1: self.simd4_256.as_bool(),
                    self.v2: self.simd4_256,
                    self.v3: self.simd4_256,
                    self.v4: self.simd4_256.half_vector()})

    def test_bad_nonssa(self):
        r = Rtl(
                self.v0 << vselect(self.v1, self.v2, self.v3),
                self.v0 << vselect(self.v1, self.v2, self.v3),
        )

        # Should fail because we're redefining v0

        self.runTC(r,
                   {self.v2: self.simd4_256},
                   TCRedef((r, 1, False, 0)))

    def test_extend(self):
        r = Rtl(
                self.v0 << uextend(self.v2),
                self.v1 << sextend(self.v2),
        )

        self.runTC(r,
                   {self.v2: Int},
                   TCUnderspecified(r, set([self.v0, self.v1])))

        self.runTC(r,
                   {self.v0: Int, self.v1: Int},
                   {self.v0: Int,
                    self.v1: Int,
                    self.v2: Int})

        # Unfortunately we can't express the constraint the input
        # type of {u,s}extend is narrower than the output.
        # As a result the TC only infers that the input to v2 is Int
        self.runTC(r,
                   {self.v0: self.i16_32, self.v1: self.i16_32},
                   {self.v0: self.i16_32,
                    self.v1: self.i16_32,
                    self.v2: Int})

    def test_bint(self):
        r = Rtl(
                self.v0 << bint(self.v1),
        )

        self.runTC(r,
                   {},
                   TCUnderspecified(r, set([self.v0])))

        self.runTC(r,
                   {self.v0: self.i16_32,
                    self.v1: Bool},
                   {self.v0: self.i16_32,
                    self.v1: Bool})


def rewrite_env(env, xform):
    # type: (TypeEnv, XForm) -> TypeEnv
    return {xform.symtab[str(k)]: v for (k, v) in env.items()}


def rewrite_set(s, xform):
    # type: (Set[Var], XForm) -> Set[Var]
    return set([xform.symtab[str(x)] for x in s])


class TestTCXForm(TypeCheckingBaseTest):
    def runTCXForm(self, x, inEnv, res):
        inEnv = rewrite_env(inEnv, x)
        if (isinstance(res, dict)):
            res = rewrite_env(res, x)
        elif (isinstance(res, TCUnderspecified)):
            res = TCUnderspecified(res.loc, rewrite_set(res.missing, x))

        self.runTC(x, inEnv, res)

    def test_trivial(self):
        x = XForm(
                Rtl(
                    self.v0 << vselect(self.v1, self.v2, self.v3)
                ),
                Rtl(
                    self.v0 << vselect(self.v1, self.v2, self.v3)
                ))

        self.runTCXForm(x,
                        {self.v2: self.simd8_64},
                        {self.v0: self.simd8_64,
                         self.v3: self.simd8_64,
                         self.v1: self.simd8_64.as_bool()})

    def test_trivial_bad_diff_out(self):
        x = XForm(
                Rtl(
                    (self.v0, self.v1) << vsplit(self.v2)
                ),
                Rtl(
                    self.v0 << vconcat(self.v2, self.v2)
                ))

        self.runTCXForm(x,
                        {self.v2: self.simd8_64},
                        TCDisagree(x, self.v0, None, None))

    def test_trivial_unconstrained_input(self):
        x = XForm(
                Rtl(
                    self.v4 << vconcat(self.v2, self.v3)
                ),
                Rtl(
                    (self.v0, self.v1) << vsplit(self.v2)
                ))

        self.runTCXForm(x,
                        {self.v2: self.simd8_64},
                        TCUnderconstrainedInput(x, self.v3))

    def test_trivial_extra_dst(self):
        # Its not an error to have dest have extra outputs
        x = XForm(
                Rtl(
                    self.v0 << vselect(self.v1, self.v2, self.v3),
                ),
                Rtl(
                    self.v0 << vselect(self.v1, self.v2, self.v3),
                    self.v4 << iadd(self.v2, self.v3)
                ))

        self.runTCXForm(x,
                        {self.v2: self.isimd8_64},
                        {self.v0: self.isimd8_64,
                         self.v3: self.isimd8_64,
                         self.v1: self.isimd8_64.as_bool(),
                         self.v4: self.isimd8_64})

    def test_trivial_extra_inp(self):
        # Its not an error to have src have extra outputs
        x = XForm(
                Rtl(
                    self.v0 << vselect(self.v1, self.v2, self.v3),
                    self.v4 << iadd(self.v2, self.v3)
                ),
                Rtl(
                    self.v0 << vselect(self.v1, self.v2, self.v3),
                ))

        self.runTCXForm(x,
                        {self.v2: self.isimd8_64},
                        {self.v0: self.isimd8_64,
                         self.v3: self.isimd8_64,
                         self.v1: self.isimd8_64.as_bool(),
                         self.v4: self.isimd8_64})

    def test_bint(self):
        # Its not an error to have src have extra outputs
        x = XForm(
                Rtl(
                    self.v0 << iadd_cin(self.v1, self.v2, self.v3),
                ),
                Rtl(
                    self.v4 << iadd(self.v1, self.v2),
                    self.v5 << bint(self.v3),
                    self.v0 << iadd(self.v4, self.v5)
                ))

        self.runTCXForm(x,
                        {},
                        TCUnderspecified(x.src, set([self.v2])))

        self.runTCXForm(x,
                        {self.v2: iB, self.v3: self.b1},
                        TCUnderspecified(x.dst, set([self.v1, self.v5])))

        self.runTCXForm(x,
                        {self.v2: iB,
                         self.v1: iB,
                         self.v5: iB},
                        {})
