from __future__ import absolute_import
from base.instructions import vselect, vsplit, vconcat, TxN, iconst
from .typevar import TypeVar
from .tc import tc_rtl, TCError, resolve, TCNotSubtype, io_shape,\
        to_TypeMap
from .ast import Var
from .xform import Rtl
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

        self.b4_256 = TypeVar("b4_256", "", ints=False, floats=False,
                              bools=(1, 1), simd=(4, 256))


class TestIOShape(TypeCheckingBaseTest):
    def test_vselect(self):
        r = Rtl(
                self.v0 << vselect(self.v1, self.v2, self.v3)
        )

        self.assertEqual(io_shape(r),
                         (set([self.v1, self.v2, self.v3]),  # Inputs
                          set([self.v2]),  # Control Vars
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
                          set([self.v1, self.v0, self.v4, self.v5, self.v6,
                               self.v7, self.v8, self.v9])))  # Defs


class TestTypeChecking(TypeCheckingBaseTest):
    def ppFail(self, r, m, msg):
        msg += "\n Partially inferred types: \n"
        for (k, v) in m.items():
            msg += "{} := {}\n".format(k, v)
        self.fail(msg)

    def runTC(self, rtl, initialEnv, res):
        try:
            m = tc_rtl(rtl, to_TypeMap(initialEnv))
        except TCError as e:
            if e == res:
                return True
            else:
                if isinstance(res, TCError):
                    self.ppFail(rtl, {},
                                "Expected exception {} instead got exc {}"
                                .format(repr(res), repr(e)))
                else:
                    self.ppFail(rtl, {}, "Got unexpected exception {}"
                                .format(repr(e)))
                return False

        if (isinstance(res, TCError)):
            self.ppFail(rtl, m, "Expected exception {} - instead no error."
                        .format(res))
            return False
        elif (isinstance(res, dict)):
            res = {k.get_typevar(): v for (k, v) in res.items()}
            for (k, v) in res.items():
                if k not in m:
                    self.ppFail(rtl, m,
                                ": Did not infer type for {}".format(k))
                    return False

                if not (m[k] == res[k]):
                    self.ppFail(rtl, m,
                                   "Inferred wrong type for {}. Got {} expected {}" # noqa
                                   .format(k, m[k], res[k]))
                    return False
            return True
        else:
            for (k, tv) in m.items():
                print(k, "==", resolve(tv, m))
            return True

    def test_vselect(self):
        # Make sure we infer the type of v0 to be same as v2
        r = Rtl(
                self.v0 << vselect(self.v1, self.v2, self.v3)
        )

        self.runTC(r,
                   {self.v2: self.simd8_64,
                    self.v3: self.simd8_64,
                    self.v1: self.simd8_64.as_bool()},
                   {self.v0: self.simd8_64})

        self.runTC(r,
                   {self.v2: TxN, self.v3: TxN, self.v1: TxN.as_bool()},
                   {self.v0: TxN, self.v2: TxN, self.v3: TxN,
                    self.v1: TxN.as_bool()})

    def test_bad_vselect(self):
        r = Rtl(
                self.v0 << vselect(self.v1, self.v2, self.v3)
        )

        # Make sure we flag an error if v1 was specified
        # to be anything but as_bool(T) where v2: T
        self.runTC(r,
                   {self.v2: TxN, self.v3: TxN, self.v1: TxN},
                   TCNotSubtype((r, 0, True, 0), None, None))

        self.runTC(r,
                   {self.v2: TxN, self.v3: TxN, self.v1: self.b4_256},
                   TCNotSubtype((r, 0, True, 0), None, None))

    def test_bad_double_split(self):
        r1 = Rtl(
                self.v0 << vselect(self.v1, self.v2, self.v3),
                (self.v4, self.v5) << vsplit(self.v0),
                (self.v6, self.v7) << vsplit(self.v4),
        )

        # v0 here has the same type as v2 - TxN which is 2..256 simd.
        # Therefore v4 has the type half_vector(TxN) which is 1..128 simd.
        # However the last vsplit requires v4 to be TxN (at least 2 lanes)
        # This causes a subtyping error

        self.runTC(r1,
                   {self.v2: TxN, self.v3: TxN,
                    self.v1: TxN.as_bool()},
                   TCNotSubtype((r1, 2, True, 0), None, None))

    def test_double_split(self):
        r1 = Rtl(
                self.v0 << vselect(self.v1, self.v2, self.v3),
                (self.v4, self.v5) << vsplit(self.v0),
                (self.v6, self.v7) << vsplit(self.v4),
        )

        # v0 here has the same type as v2 - TxN which is 2..256 simd.
        # Therefore v4 has the type half_vector(TxN) which is 1..128 simd.
        # However the last vsplit requires v4 to be TxN (at least 2 lanes)
        # This causes a subtyping error

        self.runTC(r1,
                   {self.v2: self.simd4_256,
                    self.v3: self.simd4_256,
                    self.v1: self.simd4_256.as_bool()},
                   {self.v0: self.simd4_256,
                    self.v4: self.simd4_256.half_vector()})
