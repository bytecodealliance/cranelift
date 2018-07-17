//! Cranelift ValueType hierarchy

// Temporary disabled: Unused at the moment.
// use std::collections::HashMap;

use base::types as base_types;

static _RUST_NAME_PREFIX: &'static str = "ir::types::";

// ValueType variants (i8, i32, ...) are provided in `base::types.rs`.

/// A concrete SSA value type.
///
/// All SSA values have a type that is described by an instance of `ValueType`
/// or one of its subclasses.
pub enum ValueType {
    BV(BVType),
    Lane(LaneType),
    Special(SpecialType),
    Vector(VectorType),
}

impl ValueType {
    /// Iterate through all of the lane types.
    pub fn all_lane_types() -> LaneTypeIterator {
        LaneTypeIterator::new()
    }

    /// Iterate through all of the special types (neither lanes nor vectors).
    pub fn all_special_types() -> SpecialTypeIterator {
        SpecialTypeIterator::new()
    }

    /// Return a string containing the documentation comment for this type.
    pub fn doc(&self) -> String {
        match self {
            &ValueType::BV(ref b) => b.doc(),
            &ValueType::Lane(l) => l.doc(),
            &ValueType::Special(s) => s.doc(),
            &ValueType::Vector(ref v) => v.doc(),
        }
    }

    /// Return the number of bits in a lane.
    pub fn lane_bits(&self) -> u64 {
        match self {
            &ValueType::BV(ref b) => b.lane_bits(),
            &ValueType::Lane(l) => l.lane_bits(),
            &ValueType::Special(s) => s.lane_bits(),
            &ValueType::Vector(ref v) => v.lane_bits(),
        }
    }

    /// Return the number of lanes.
    pub fn lane_count(&self) -> u64 {
        match self {
            &ValueType::Vector(ref v) => v.lane_count(),
            _ => 1,
        }
    }

    /// Find the number of bytes that this type occupies in memory.
    pub fn membytes(&self) -> u64 {
        self.width() / 8
    }

    /// Get the name of this type.
    pub fn name(&self) -> String {
        match self {
            &ValueType::BV(ref b) => b.name(),
            &ValueType::Lane(l) => l.name(),
            &ValueType::Special(s) => s.name(),
            &ValueType::Vector(ref v) => v.name(),
        }
    }

    /// Find the unique number associated with this type.
    pub fn number(&self) -> Option<u8> {
        match self {
            &ValueType::BV(_) => None,
            &ValueType::Lane(l) => Some(l.number()),
            &ValueType::Special(s) => Some(s.number()),
            &ValueType::Vector(ref v) => Some(v.number()),
        }
    }

    /// Return the name of this type for other Rust source files.
    pub fn _rust_name(&self) -> String {
        format!("{}{}", _RUST_NAME_PREFIX, self.name().to_uppercase())
    }

    /// Return true iff:
    ///     1. self and other have equal number of lanes
    ///     2. each lane in self has at least as many bits as a lane in other
    pub fn _wider_or_equal(&self, rhs: &ValueType) -> bool {
        (self.lane_count() == rhs.lane_count()) && (self.lane_bits() >= rhs.lane_bits())
    }

    /// Return the total number of bits of an instance of this type.
    pub fn width(&self) -> u64 {
        self.lane_count() * self.lane_bits()
    }
}

/// Create a ValueType from a given bitvector type.
impl From<BVType> for ValueType {
    fn from(bv: BVType) -> Self {
        ValueType::BV(bv)
    }
}

/// Create a ValueType from a given lane type.
impl From<LaneType> for ValueType {
    fn from(lane: LaneType) -> Self {
        ValueType::Lane(lane)
    }
}

/// Create a ValueType from a given special type.
impl From<SpecialType> for ValueType {
    fn from(spec: SpecialType) -> Self {
        ValueType::Special(spec)
    }
}

/// Create a ValueType from a given vector type.
impl From<VectorType> for ValueType {
    fn from(vector: VectorType) -> Self {
        ValueType::Vector(vector)
    }
}

/// A concrete scalar type that can appear as a vector lane too.
#[derive(Debug, Clone, Copy)]
pub struct LaneType {
    bits: u64,
    tag: LaneTypeTag,
}

impl LaneType {
    /// Return a string containing the documentation comment for this lane type.
    pub fn doc(&self) -> String {
        match self.tag {
            LaneTypeTag::BoolType(_) => format!("A boolean type with {} bits.", self.bits),
            LaneTypeTag::FloatType(base_types::Float::F32) => String::from(
                "A 32-bit floating point type represented in the IEEE 754-2008
                *binary32* interchange format. This corresponds to the :c:type:`float`
                type in most C implementations.",
            ),
            LaneTypeTag::FloatType(base_types::Float::F64) => String::from(
                "A 64-bit floating point type represented in the IEEE 754-2008
                *binary64* interchange format. This corresponds to the :c:type:`double`
                type in most C implementations.",
            ),
            LaneTypeTag::IntType(_) if self.bits < 32 => format!(
                "An integer type with {} bits.
                WARNING: arithmetic on {}bit integers is incomplete",
                self.bits, self.bits
            ),
            LaneTypeTag::IntType(_) => format!("An integer type with {} bits.", self.bits),
        }
    }

    /// Return the number of bits in a lane.
    pub fn lane_bits(&self) -> u64 {
        self.bits
    }

    /// Get the name of this lane type.
    pub fn name(&self) -> String {
        match self.tag {
            LaneTypeTag::BoolType(_) => format!("b{}", self.bits),
            LaneTypeTag::FloatType(_) => format!("f{}", self.bits),
            LaneTypeTag::IntType(_) => format!("i{}", self.bits),
        }
    }

    /// Find the unique number associated with this lane type.
    pub fn number(&self) -> u8 {
        match self.tag {
            LaneTypeTag::BoolType(b) => b.number(),
            LaneTypeTag::FloatType(f) => f.number(),
            LaneTypeTag::IntType(i) => i.number(),
        }
    }
}

/// Create a LaneType from a given bool variant.
impl From<base_types::Bool> for LaneType {
    fn from(b: base_types::Bool) -> Self {
        let bits = b as u64;
        let tag = LaneTypeTag::BoolType(b);
        Self { bits, tag }
    }
}

/// Create a LaneType from a given float variant.
impl From<base_types::Float> for LaneType {
    fn from(f: base_types::Float) -> Self {
        let bits = f as u64;
        let tag = LaneTypeTag::FloatType(f);
        Self { bits, tag }
    }
}

/// Create a LaneType from a given int variant.
impl From<base_types::Int> for LaneType {
    fn from(i: base_types::Int) -> Self {
        let bits = i as u64;
        let tag = LaneTypeTag::IntType(i);
        Self { bits, tag }
    }
}

/// Tags used to specify the kinds of elements in a lane type.
#[derive(Debug, Clone, Copy)]
pub enum LaneTypeTag {
    BoolType(base_types::Bool),
    FloatType(base_types::Float),
    IntType(base_types::Int),
}

/// An iterator for different lane types.
pub struct LaneTypeIterator {
    bool_iter: base_types::BoolIterator,
    int_iter: base_types::IntIterator,
    float_iter: base_types::FloatIterator,
}

impl LaneTypeIterator {
    /// Create a new lane type iterator.
    fn new() -> Self {
        Self {
            bool_iter: base_types::BoolIterator::new(),
            int_iter: base_types::IntIterator::new(),
            float_iter: base_types::FloatIterator::new(),
        }
    }
}

impl Iterator for LaneTypeIterator {
    type Item = LaneType;
    fn next(&mut self) -> Option<Self::Item> {
        if let b @ Some(_) = self.bool_iter.next() {
            b.map(LaneType::from)
        } else if let i @ Some(_) = self.int_iter.next() {
            i.map(LaneType::from)
        } else if let f @ Some(_) = self.float_iter.next() {
            f.map(LaneType::from)
        } else {
            None
        }
    }
}

/// A concrete SIMD vector type.
///
/// A vector type has a lane type which is an instance of `LaneType`,
/// and a positive number of lanes.
pub struct VectorType {
    base: LaneType,
    lanes: u64,
}

impl VectorType {
    /// Initialize a new integer type with `n` bits.
    pub fn new(base: LaneType, lanes: u64) -> VectorType {
        VectorType { base, lanes }
    }

    pub fn doc(&self) -> String {
        format!(
            "A SIMD vector with {} lanes containing a `{}` each.",
            self.lanes,
            self.base.name()
        )
    }

    /// Get the name of this type.
    pub fn name(&self) -> String {
        format!("{}X{}", self.base.name(), self.lanes,)
    }

    /// Find the unique number associated with this type.
    pub fn number(&self) -> u8 {
        let b = f64::from(self.base.number());
        let l = (self.lanes as f64).log2();
        let num = 16_f64 * l + b;
        num as u8
    }

    /// Return the number of bits in a lane.
    pub fn lane_bits(&self) -> u64 {
        self.base.lane_bits()
    }

    /// Return the number of lanes.
    pub fn lane_count(&self) -> u64 {
        self.lanes
    }
}

/// A flat bitvector type. Used for semantics description only.
pub struct BVType {
    bits: u64,
}

impl BVType {
    /// Initialize a new bitvector type with `n` bits.
    pub fn _new(bits: u64) -> Self {
        Self { bits }
    }

    /// Return a string containing the documentation comment for this bitvector type.
    pub fn doc(&self) -> String {
        format!("A bitvector type with {} bits.", self.bits)
    }

    /// Return the number of bits in a lane.
    pub fn lane_bits(&self) -> u64 {
        self.bits
    }

    /// Get the name of this bitvector type.
    pub fn name(&self) -> String {
        format!("bv{}", self.bits)
    }
}

/// A concrete scalar type that is neither a vector nor a lane type.
///
/// Special types cannot be used to form vectors.
#[derive(Debug, Clone, Copy)]
pub struct SpecialType {
    tag: SpecialTypeTag,
}

impl SpecialType {
    pub fn doc(&self) -> String {
        match self.tag {
            SpecialTypeTag::Flag(base_types::Flag::IFlags) => String::from(
                "CPU flags representing the result of an integer comparison. These flags
                can be tested with an :type:`intcc` condition code.",
            ),
            SpecialTypeTag::Flag(base_types::Flag::FFlags) => String::from(
                "CPU flags representing the result of a floating point comparison. These
                flags can be tested with a :type:`floatcc` condition code.",
            ),
        }
    }

    pub fn lane_bits(&self) -> u64 {
        match self.tag {
            SpecialTypeTag::Flag(_) => 0,
        }
    }

    pub fn name(&self) -> String {
        match self.tag {
            SpecialTypeTag::Flag(base_types::Flag::IFlags) => "iflags".to_string(),
            SpecialTypeTag::Flag(base_types::Flag::FFlags) => "fflags".to_string(),
        }
    }

    pub fn number(&self) -> u8 {
        self.tag.number()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SpecialTypeTag {
    Flag(base_types::Flag),
}

impl SpecialTypeTag {
    pub fn number(&self) -> u8 {
        match *self {
            SpecialTypeTag::Flag(f) => f.number(),
        }
    }
}

pub struct SpecialTypeIterator {
    flag_iter: base_types::FlagIterator,
}

impl SpecialTypeIterator {
    fn new() -> Self {
        Self {
            flag_iter: base_types::FlagIterator::new(),
        }
    }
}

impl Iterator for SpecialTypeIterator {
    type Item = ValueType;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(f) = self.flag_iter.next() {
            let next = SpecialType {
                tag: SpecialTypeTag::Flag(f),
            };
            Some(ValueType::Special(next))
        } else {
            None
        }
    }
}
