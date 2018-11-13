use super::types::{SpecialType, ValueType};
use indexmap::IndexSet;
use std::ops::Range;

const MAX_LANES: usize = 256;
const MAX_BITS: usize = 64;
const MAX_BITVEC: usize = MAX_BITS * MAX_LANES;

pub enum Interval {
    All,
    Range(Range<usize>),
}

#[derive(Clone, Debug)]
struct IntervalIter {
    next: usize,
    range: Range<usize>,
}

impl IntervalIter {
    fn empty() -> Self {
        Self {
            next: 0,
            range: (0..0),
        }
    }
}

impl Iterator for IntervalIter {
    type Item = usize;
    fn next(&mut self) -> Option<usize> {
        let next = self.next;
        if next <= self.range.end && next > 0 {
            self.next *= 2;
            Some(next)
        } else {
            None
        }
    }
}

impl Interval {
    fn decode(
        interval: Option<Interval>,
        full_range: Range<usize>,
        default: Option<usize>,
    ) -> Range<usize> {
        match interval {
            Some(Interval::All) => full_range,
            Some(Interval::Range(range)) => range,
            None => match default {
                Some(default) => (default..default),
                None => (0..0),
            },
        }
    }

    fn to_iter(range: Range<usize>) -> IntervalIter {
        if let (0, 0) = (range.start, range.end) {
            return IntervalIter::empty();
        }

        let (start, end) = (range.start, range.end);

        assert!(start.is_power_of_two());
        assert!(end.is_power_of_two());
        assert!(start <= end);

        IntervalIter { next: start, range }
    }
}

#[derive(Clone, Debug)]
pub enum SpecialSpec {
    All,
    List(Vec<SpecialType>),
}

impl SpecialSpec {
    fn to_set(spec: Option<SpecialSpec>) -> IndexSet<SpecialType> {
        match spec {
            Some(SpecialSpec::All) => ValueType::all_special_types().collect(),
            Some(SpecialSpec::List(vec)) => vec.into_iter().collect(),
            None => IndexSet::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeSet {
    lanes: IndexSet<usize>,
    ints: IndexSet<usize>,
    floats: IndexSet<usize>,
    bools: IndexSet<usize>,
    bitvecs: IndexSet<usize>,
    specials: IndexSet<SpecialType>,
}

impl TypeSet {
    pub fn build() -> TypeSetBuilder {
        Default::default()
    }

    pub fn intersect(&self, other: &TypeSet) -> TypeSet {
        TypeSet {
            lanes: &self.lanes & &other.lanes,
            ints: &self.ints & &other.ints,
            floats: &self.floats & &other.floats,
            bools: &self.bools & &other.bools,
            bitvecs: &self.bitvecs & &other.bitvecs,
            specials: &self.specials & &other.specials,
        }
    }

    pub fn is_subset(&self, other: &TypeSet) -> bool {
        self.lanes.is_subset(&other.lanes)
            && self.ints.is_subset(&other.ints)
            && self.floats.is_subset(&other.floats)
            && self.bools.is_subset(&other.bools)
            && self.bitvecs.is_subset(&other.bitvecs)
            && self.specials.is_subset(&other.specials)
    }

    pub fn as_bool(&self) -> TypeSet {
        let mut new = self.clone();
        new.ints.clear();
        new.floats.clear();
        new.bitvecs.clear();

        if self
            .lanes
            .difference(&[1usize].to_vec().into_iter().collect::<IndexSet<usize>>())
            .count()
            > 0
        {
            new.bools = self
                .ints
                .union(&self.floats)
                .map(|i| *i)
                .collect::<IndexSet<usize>>()
                .union(&self.bools)
                .map(|i| *i)
                .collect::<IndexSet<usize>>();
        }

        if self.lanes.contains(&1) {
            new.bools.insert(1);
        }

        new
    }
}

#[derive(Default)]
pub struct TypeSetBuilder {
    lanes: Option<Interval>,
    ints: Option<Interval>,
    floats: Option<Interval>,
    bools: Option<Interval>,
    bitvecs: Option<Interval>,
    specials: Option<SpecialSpec>,
}

impl TypeSetBuilder {
    pub fn lanes(mut self, interval: Interval) -> Self {
        self.lanes = Some(interval);
        self
    }

    pub fn ints(mut self, interval: Interval) -> Self {
        self.ints = Some(interval);
        self
    }

    pub fn floats(mut self, interval: Interval) -> Self {
        self.floats = Some(interval);
        self
    }

    pub fn bools(mut self, interval: Interval) -> Self {
        self.bools = Some(interval);
        self
    }

    pub fn bitvecs(mut self, interval: Interval) -> Self {
        self.bitvecs = Some(interval);
        self
    }

    pub fn specials(mut self, specials: SpecialSpec) -> Self {
        self.specials = Some(specials);
        self
    }

    pub fn finish(self) -> TypeSet {
        TypeSet {
            lanes: Interval::to_iter(Interval::decode(self.lanes, 1..MAX_LANES, Some(1))).collect(),
            ints: Interval::to_iter(Interval::decode(self.ints, 8..MAX_BITS, None)).collect(),
            floats: Interval::to_iter(Interval::decode(self.floats, 32..64, None)).collect(),
            bools: Interval::to_iter(Interval::decode(self.bools, 1..MAX_BITS, None))
                .filter(|&bits| {
                    bits == 1 || (bits >= 8 && bits <= MAX_BITS && bits.is_power_of_two())
                }).collect(),
            bitvecs: Interval::to_iter(Interval::decode(self.bitvecs, 1..MAX_BITVEC, None))
                .collect(),
            specials: SpecialSpec::to_set(self.specials),
        }
    }
}

pub struct TypeVar {
    name: String,
    doc: &'static str,
    type_set: TypeSet,
    base: Option<Box<TypeVar>>,
    derived_func: Option<&'static str>,
}

impl TypeVar {
    pub fn build(name: impl Into<String>) -> TypeVarBuilder {
        TypeVarBuilder {
            name: name.into(),
            doc: "",
            type_set_builder: TypeSet::build(),
            derived_func: None,
            scalars: true,
            simd: None,
            base: None,
        }
    }
}

pub struct TypeVarBuilder {
    name: String,
    doc: &'static str,
    base: Option<Box<TypeVar>>,
    type_set_builder: TypeSetBuilder,
    derived_func: Option<&'static str>,
    scalars: bool,
    simd: Option<Interval>,
}

impl TypeVarBuilder {
    pub fn doc(mut self, doc: &'static str) -> Self {
        self.doc = doc;
        self
    }

    pub fn ints(mut self, interval: Interval) -> Self {
        self.type_set_builder = self.type_set_builder.ints(interval);
        self
    }

    pub fn floats(mut self, interval: Interval) -> Self {
        self.type_set_builder = self.type_set_builder.floats(interval);
        self
    }

    pub fn bools(mut self, interval: Interval) -> Self {
        self.type_set_builder = self.type_set_builder.bools(interval);
        self
    }

    pub fn scalars(mut self, b: bool) -> Self {
        self.scalars = b;
        self
    }

    pub fn simd(mut self, interval: Interval) -> Self {
        self.simd = Some(interval);
        self
    }

    pub fn bitvecs(mut self, interval: Interval) -> Self {
        self.type_set_builder = self.type_set_builder.bitvecs(interval);
        self
    }

    pub fn base(mut self, base: TypeVar, derived_func: &'static str) -> Self {
        self.name = format!("{}({})", derived_func, base.name);
        self.base = Some(Box::new(base));
        self.derived_func = Some(derived_func);
        self
    }

    pub fn specials(mut self, specials: SpecialSpec) -> Self {
        self.type_set_builder = self.type_set_builder.specials(specials);
        self
    }

    pub fn finish(self) -> TypeVar {
        let min_lanes = if self.scalars { 1 } else { 2 };
        let type_set = self
            .type_set_builder
            .lanes(Interval::Range(Interval::decode(
                self.simd,
                min_lanes..MAX_LANES,
                Some(1),
            ))).finish();

        TypeVar {
            name: self.name,
            doc: self.doc,
            type_set,
            derived_func: self.derived_func,
            base: self.base,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_set_works() {
        let set = TypeSet::build().ints(Interval::Range(8..32)).finish();

        assert_eq!(set.lanes, indexset!{1});
        assert_eq!(set.ints, indexset!{8, 16, 32});

        let set = TypeSet::build().ints(Interval::All).finish();

        assert_eq!(set.lanes, indexset!{1});
        assert_eq!(set.ints, indexset!{8, 16, 32, 64});

        let set = TypeSet::build().floats(Interval::All).finish();

        assert_eq!(set.lanes, indexset!{1});
        assert_eq!(set.floats, indexset!{32, 64});

        let set = TypeSet::build().bools(Interval::All).finish();

        assert_eq!(set.lanes, indexset!{1});
        assert_eq!(set.bools, indexset!{1, 8, 16, 32, 64});

        let set = TypeSet::build()
            .lanes(Interval::All)
            .ints(Interval::All)
            .finish();

        assert_eq!(set.lanes, indexset!{1, 2, 4, 8, 16, 32, 64, 128, 256});
        assert_eq!(set.ints, indexset!{8, 16, 32, 64});
    }
}
