from __future__ import absolute_import
from unittest import TestCase
from doctest import DocTestSuite
from . import typevar
from .typevar import TypeSet, TypeVar
from base.types import i32


def load_tests(loader, tests, ignore):
    tests.addTests(DocTestSuite(typevar))
    return tests


class TestTypeSet(TestCase):
    def test_invalid(self):
        with self.assertRaises(AssertionError):
            TypeSet(lanes=(2, 1))
        with self.assertRaises(AssertionError):
            TypeSet(ints=(32, 16))
        with self.assertRaises(AssertionError):
            TypeSet(floats=(32, 16))
        with self.assertRaises(AssertionError):
            TypeSet(bools=(32, 16))
        with self.assertRaises(AssertionError):
            TypeSet(ints=(32, 33))

    def test_hash(self):
        a = TypeSet(lanes=True, ints=True, floats=True)
        b = TypeSet(lanes=True, ints=True, floats=True)
        c = TypeSet(lanes=True, ints=(8, 16), floats=True)
        self.assertEqual(a, b)
        self.assertNotEqual(a, c)
        s = set()
        s.add(a)
        self.assertTrue(a in s)
        self.assertTrue(b in s)
        self.assertFalse(c in s)

    def test_hash_modified(self):
        a = TypeSet(lanes=True, ints=True, floats=True)
        s = set()
        s.add(a)
        a.max_int = 32
        # Can't rehash after modification.
        with self.assertRaises(AssertionError):
            a in s

    def test_derived_constructors(self):
        a = TypeSet(lanes=(2, 4), ints=(16, 32), floats=None, bools=(16, 32))
        a_hlf = TypeSet(lanes=(2, 4), ints=(8, 16), floats=None, bools=(8, 16))
        a_dbl = TypeSet(lanes=(2, 4), ints=(32, 64), floats=None,
                        bools=(32, 64))
        a_dbl_v = TypeSet(lanes=(4, 8), ints=(16, 32), floats=None,
                          bools=(16, 32))
        a_hlf_v = TypeSet(lanes=(1, 2), ints=(16, 32), floats=None,
                          bools=(16, 32))
        a_bool = TypeSet(lanes=(2, 4), ints=False, floats=False, bools=(1, 1))
        a_lane = TypeSet(lanes=(1, 1), ints=(16, 32), floats=None,
                         bools=(16, 32))

        f64 = TypeSet(lanes=(2, 4), ints=None, floats=(64, 64), bools=None)
        f32 = TypeSet(lanes=(2, 4), ints=None, floats=(32, 32), bools=None)
        f64_hv = TypeSet(lanes=(1, 2), ints=None, floats=(64, 64), bools=None)
        f32_dv = TypeSet(lanes=(4, 8), ints=None, floats=(32, 32), bools=None)
        large_simd = TypeSet(lanes=(1, 256), ints=True, floats=True)
        f32_bool = TypeSet(lanes=(2, 4), ints=None, floats=None, bools=(1, 1))

        # Check half/double width/vector works correctly
        self.assertEqual(a.half_width(), a_hlf)
        self.assertEqual(a.double_width(), a_dbl)
        self.assertEqual(a.half_vector(), a_hlf_v)
        self.assertEqual(a.double_vector(), a_dbl_v)

        self.assertEqual(f64.half_width(), f32)
        self.assertEqual(f32.double_width(), f64)
        self.assertEqual(f64.half_vector(), f64_hv)
        self.assertEqual(f32.double_vector(), f32_dv)

        # Check half/double width/vector fails when going outside the full
        # range
        with self.assertRaises(AssertionError):
            a_dbl.double_width()
        with self.assertRaises(AssertionError):
            a_hlf.half_width()
        with self.assertRaises(AssertionError):
            f64.double_width()
        with self.assertRaises(AssertionError):
            f32.half_width()
        with self.assertRaises(AssertionError):
            a_hlf_v.half_vector()
        with self.assertRaises(AssertionError):
            f64_hv.half_vector()
        with self.assertRaises(AssertionError):
            large_simd.double_vector()

        # Check as_bool works. No edge cases here I think?
        self.assertEqual(a_dbl.as_bool(), a_bool)
        self.assertEqual(f32.as_bool(), f32_bool)

        # Check lane_of. TODO: What happens when called on scalar?
        self.assertEqual(a.lane_of(), a_lane)


class TestTypeVar(TestCase):
    def test_functions(self):
        x = TypeVar('x', 'all ints', ints=True)
        with self.assertRaises(AssertionError):
            x.double_width()
        with self.assertRaises(AssertionError):
            x.half_width()

        x2 = TypeVar('x2', 'i16 and up', ints=(16, 64))
        with self.assertRaises(AssertionError):
            x2.double_width()
        self.assertEqual(str(x2.half_width()), '`half_width(x2)`')
        self.assertEqual(x2.half_width().rust_expr(), 'x2.half_width()')
        self.assertEqual(
                x2.half_width().double_width().rust_expr(),
                'x2.half_width().double_width()')

        x3 = TypeVar('x3', 'up to i32', ints=(8, 32))
        self.assertEqual(str(x3.double_width()), '`double_width(x3)`')
        with self.assertRaises(AssertionError):
            x3.half_width()

    def test_singleton(self):
        x = TypeVar.singleton(i32)
        self.assertEqual(str(x), '`i32`')
        self.assertEqual(x.type_set.min_int, 32)
        self.assertEqual(x.type_set.max_int, 32)
        self.assertEqual(x.type_set.min_lanes, 1)
        self.assertEqual(x.type_set.max_lanes, 1)

        x = TypeVar.singleton(i32.by(4))
        self.assertEqual(str(x), '`i32x4`')
        self.assertEqual(x.type_set.min_int, 32)
        self.assertEqual(x.type_set.max_int, 32)
        self.assertEqual(x.type_set.min_lanes, 4)
        self.assertEqual(x.type_set.max_lanes, 4)
