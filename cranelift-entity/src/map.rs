//! Densely numbered entity references as mapping keys.

use crate::iter::{Iter, IterMut};
use crate::keys::Keys;
use crate::EntityRef;
use alloc::vec::Vec;
use core::cmp::min;
use core::marker::PhantomData;
use core::ops::{Index, IndexMut};
use core::slice;
#[cfg(feature = "enable-serde")]
use serde::{
    de::{Deserializer, SeqAccess, Visitor},
    ser::{SerializeSeq, Serializer},
    Deserialize, Serialize,
};

/// A mapping `K -> V` for densely indexed entity references.
///
/// The `SecondaryMap` data structure uses the dense index space to implement a map with a vector.
/// Unlike `PrimaryMap`, an `SecondaryMap` can't be used to allocate entity references. It is used
/// to associate secondary information with entities.
///
/// The map does not track if an entry for a key has been inserted or not. Instead it behaves as if
/// all keys have a default entry from the beginning.
#[derive(Debug, Clone)]
pub struct SecondaryMap<K, V>
where
    K: EntityRef,
    V: Clone,
{
    elems: Vec<V>,
    default: V,
    unused: PhantomData<K>,
}

/// Shared `SecondaryMap` implementation for all value types.
impl<K, V> SecondaryMap<K, V>
where
    K: EntityRef,
    V: Clone,
{
    /// Create a new empty map.
    pub fn new() -> Self
    where
        V: Default,
    {
        Self {
            elems: Vec::new(),
            default: Default::default(),
            unused: PhantomData,
        }
    }

    /// Create a new, empty map with the specified capacity.
    ///
    /// The map will be able to hold exactly `capacity` elements without reallocating.
    pub fn with_capacity(capacity: usize) -> Self
    where
        V: Default,
    {
        Self {
            elems: Vec::with_capacity(capacity),
            default: Default::default(),
            unused: PhantomData,
        }
    }

    /// Create a new empty map with a specified default value.
    ///
    /// This constructor does not require V to implement Default.
    pub fn with_default(default: V) -> Self {
        Self {
            elems: Vec::new(),
            default,
            unused: PhantomData,
        }
    }

    /// Returns the number of elements the map can hold without reallocating.
    pub fn capacity(&self) -> usize {
        self.elems.capacity()
    }

    /// Get the element at `k` if it exists.
    #[inline(always)]
    pub fn get(&self, k: K) -> Option<&V> {
        self.elems.get(k.index())
    }

    /// Is this map completely empty?
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.elems.is_empty()
    }

    /// Remove all entries from this map.
    #[inline(always)]
    pub fn clear(&mut self) {
        self.elems.clear()
    }

    /// Iterate over all the keys and values in this map.
    pub fn iter(&self) -> Iter<K, V> {
        Iter::new(self.elems.iter())
    }

    /// Iterate over all the keys and values in this map, mutable edition.
    pub fn iter_mut(&mut self) -> IterMut<K, V> {
        IterMut::new(self.elems.iter_mut())
    }

    /// Iterate over all the keys in this map.
    pub fn keys(&self) -> Keys<K> {
        Keys::with_len(self.elems.len())
    }

    /// Iterate over all the values in this map.
    pub fn values(&self) -> slice::Iter<V> {
        self.elems.iter()
    }

    /// Iterate over all the values in this map, mutable edition.
    pub fn values_mut(&mut self) -> slice::IterMut<V> {
        self.elems.iter_mut()
    }

    /// Resize the map to have `n` entries by adding default entries as needed.
    pub fn resize(&mut self, n: usize) {
        self.elems.resize(n, self.default.clone());
    }
}

/// Immutable indexing into an `SecondaryMap`.
///
/// All keys are permitted. Untouched entries have the default value.
impl<K, V> Index<K> for SecondaryMap<K, V>
where
    K: EntityRef,
    V: Clone,
{
    type Output = V;

    #[inline(always)]
    fn index(&self, k: K) -> &V {
        self.elems.get(k.index()).unwrap_or(&self.default)
    }
}

/// Mutable indexing into an `SecondaryMap`.
///
/// The map grows as needed to accommodate new keys.
impl<K, V> IndexMut<K> for SecondaryMap<K, V>
where
    K: EntityRef,
    V: Clone,
{
    #[inline(always)]
    fn index_mut(&mut self, k: K) -> &mut V {
        let i = k.index();
        if i >= self.elems.len() {
            self.elems.resize(i + 1, self.default.clone());
        }
        &mut self.elems[i]
    }
}

impl<K, V> PartialEq for SecondaryMap<K, V>
where
    K: EntityRef,
    V: Clone + PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        let min_size = min(self.elems.len(), other.elems.len());
        self.default == other.default
            && self.elems[..min_size] == other.elems[..min_size]
            && self.elems[min_size..].iter().all(|e| *e == self.default)
            && other.elems[min_size..].iter().all(|e| *e == other.default)
    }
}

impl<K, V> Eq for SecondaryMap<K, V>
where
    K: EntityRef,
    V: Clone + PartialEq + Eq,
{
}

#[cfg(feature = "enable-serde")]
impl<K, V> Serialize for SecondaryMap<K, V>
where
    K: EntityRef,
    V: Clone + PartialEq + Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // skip default elements at the end
        let mut elems_cnt = self.elems.len();
        while elems_cnt > 0 && self.elems[elems_cnt - 1] == self.default {
            elems_cnt -= 1;
        }

        // cnt + default elem + all elems (worst case) + elem presence bitmap
        // assuming worst case we don't need to know exact number up front
        let pack_size = 64; // if changed, adjust partial (bit)mask size and deserialization
        let packs_num = (elems_cnt + (pack_size - 1)) / pack_size;
        let all_elems_cnt = 2 + elems_cnt + packs_num;

        // layout:
        // - elems_cnt
        // - default elem
        // - pack: 64-bit mask, then elements different than default
        // - pack again, and again
        // We use packs, so deserialization doesn't have to allocate temporary buffer
        // for the entire bitmask
        let mut seq = serializer.serialize_seq(Some(all_elems_cnt))?;
        seq.serialize_element(&elems_cnt)?;
        seq.serialize_element(&self.default)?;
        for pack_no in 0..packs_num {
            let base_idx = pack_no * pack_size;
            let pack_size = min(pack_size, elems_cnt - base_idx);
            let mut mask = 0u64;
            for i in 0..pack_size {
                if self.elems[base_idx + i] != self.default {
                    mask |= 1u64 << i;
                }
            }
            seq.serialize_element(&mask)?;
            for i in 0..pack_size {
                if mask & 1 != 0 {
                    seq.serialize_element(&self.elems[base_idx + i])?;
                }
                mask >>= 1;
            }
        }
        seq.end()
    }
}

#[cfg(feature = "enable-serde")]
impl<'de, K, V> Deserialize<'de> for SecondaryMap<K, V>
where
    K: EntityRef,
    V: Clone + Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        use alloc::fmt;
        struct SecondaryMapVisitor<K, V> {
            unused: PhantomData<fn(K) -> V>,
        }

        impl<'de, K, V> Visitor<'de> for SecondaryMapVisitor<K, V>
        where
            K: EntityRef,
            V: Clone + Deserialize<'de>,
        {
            type Value = SecondaryMap<K, V>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct SecondaryMap")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                use serde::de::Error;

                let elems_cnt: usize = seq
                    .next_element()?
                    .ok_or(Error::custom("expected elems_cnt"))?;
                let default_val = seq
                    .next_element()?
                    .ok_or(Error::custom("expected default_val"))?;
                let mut m = SecondaryMap::with_default(default_val);
                m.resize(elems_cnt); // preallocate memory

                let pack_size = 64; // if changed, adjust partial (bit)mask size and serialization
                let mut mask = 0u64;
                for idx in 0..elems_cnt {
                    if idx % pack_size == 0 {
                        mask = seq.next_element()?.ok_or(Error::custom("expected mask"))?;
                    }
                    if mask & 1 == 1 {
                        m[K::new(idx)] =
                            seq.next_element()?.ok_or(Error::custom("expected elem"))?;
                    }
                    mask >>= 1;
                }

                Ok(m)
            }
        }

        deserializer.deserialize_seq(SecondaryMapVisitor {
            unused: PhantomData {},
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // `EntityRef` impl for testing.
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct E(u32);

    impl EntityRef for E {
        fn new(i: usize) -> Self {
            E(i as u32)
        }
        fn index(self) -> usize {
            self.0 as usize
        }
    }

    #[test]
    fn basic() {
        let r0 = E(0);
        let r1 = E(1);
        let r2 = E(2);
        let mut m = SecondaryMap::new();

        let v: Vec<E> = m.keys().collect();
        assert_eq!(v, []);

        m[r2] = 3;
        m[r1] = 5;

        assert_eq!(m[r1], 5);
        assert_eq!(m[r2], 3);

        let v: Vec<E> = m.keys().collect();
        assert_eq!(v, [r0, r1, r2]);

        let shared = &m;
        assert_eq!(shared[r0], 0);
        assert_eq!(shared[r1], 5);
        assert_eq!(shared[r2], 3);
    }
}
