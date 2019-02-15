//! Frame layout item changes.

use crate::ir::entities::Inst;
use crate::isa::RegUnit;
use std::boxed::Box;

#[cfg(not(feature = "std"))]
use hashmap_core::HashMap;
#[cfg(feature = "std")]
use std::collections::HashMap;

/// Change in the frame layout information.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum FrameLayoutChange {
    /// Base CFA pointer moved to different register/offset.
    CallFrameAddressAt {
        /// CFA register.
        reg: RegUnit,
        /// CFA offset.
        offset: isize,
    },
    /// Register saved at.
    RegAt {
        /// Saved register.
        reg: RegUnit,
        /// Offset in the frame (offset from CFA).
        cfa_offset: isize,
    },
    /// Return address saved at.
    ReturnAddressAt {
        /// Offset in the frame (offset from CFA).
        cfa_offset: isize,
    },
}

/// Set of frame layout changes.
pub type FrameLayoutChanges = Box<[FrameLayoutChange]>;

/// Frame items layout for (prologue/epilogue) instructions.
#[derive(Debug, Clone)]
pub struct FrameLayout {
    /// Initial frame layout.
    pub initial: FrameLayoutChanges,

    /// Instruction frame layout (changes). The map will not be dense,
    /// a HashMap is used instead of a SecondaryMap.
    pub instructions: HashMap<Inst, FrameLayoutChanges>,
}

impl FrameLayout {
    /// Creates instance of FrameLayout.
    pub fn new() -> Self {
        FrameLayout {
            initial: vec![].into_boxed_slice(),
            instructions: HashMap::new(),
        }
    }

    /// Clear the structure.
    pub fn clear(&mut self) {
        self.initial = vec![].into_boxed_slice();
        self.instructions.clear();
    }
}
