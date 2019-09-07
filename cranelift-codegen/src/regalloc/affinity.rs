//! Value affinity for register allocation.
//!
//! An SSA value's affinity is a hint used to guide the register allocator. It specifies the class
//! of allocation that is likely to cause the least amount of fixup moves in order to satisfy
//! instruction operand constraints.
//!
//! For values that want to be in registers, the affinity hint includes a register class or
//! subclass. This is just a hint, and the register allocator is allowed to pick a register from a
//! larger register class instead.

use crate::ir::{AbiParam, ArgumentLoc};
use crate::isa::{ConstraintKind, OperandConstraint, RegClass, RegClassIndex, RegInfo, RegUnit};
use core::fmt;

/// Preferred register allocation for an SSA value.
#[derive(Clone, Copy, Debug)]
pub enum Affinity {
    /// No affinity.
    ///
    /// This indicates a value that is not defined or used by any real instructions. It is a ghost
    /// value that won't appear in the final program.
    Unassigned,

    /// This value should be placed in a spill slot on the stack.
    Stack,

    /// This value prefers a register from the given register class.
    RegClass(RegClassIndex),

    /// This value prefers a specific register unit
    RegUnit(RegUnit),
}

impl Default for Affinity {
    fn default() -> Self {
        Self::Unassigned
    }
}

impl Affinity {
    /// Create an affinity that satisfies a single constraint.
    ///
    /// This will never create an `Affinity::Unassigned`.
    /// Use the `Default` implementation for that.
    pub fn new(constraint: &OperandConstraint) -> Self {
        match constraint.kind {
            ConstraintKind::Stack => Affinity::Stack,
            ConstraintKind::FixedReg(unit) | ConstraintKind::FixedTied(unit) => {
                Affinity::RegUnit(unit)
            }
            _ => Affinity::RegClass(constraint.regclass.into()),
        }
    }

    /// Create an affinity that matches an ABI argument for `isa`.
    pub fn abi(arg: &AbiParam) -> Self {
        match arg.location {
            ArgumentLoc::Unassigned => Self::Unassigned,
            ArgumentLoc::Reg(unit) => Self::RegUnit(unit),
            ArgumentLoc::Stack(_) => Self::Stack,
        }
    }

    /// Is this the `Unassigned` affinity?
    pub fn is_unassigned(self) -> bool {
        match self {
            Self::Unassigned => true,
            _ => false,
        }
    }

    /// Is this the `Reg` affinity?
    pub fn is_reg(self) -> bool {
        match self {
            Self::RegUnit(_) | Self::RegClass(_) => true,
            _ => false,
        }
    }

    /// Is this the `Stack` affinity?
    pub fn is_stack(self) -> bool {
        match self {
            Self::Stack => true,
            _ => false,
        }
    }

    pub fn rc_in(&self, reginfo: &RegInfo) -> Option<RegClass> {
        match self {
            Affinity::RegUnit(reg) => Some(reginfo.toprc_containing_regunit(*reg)),
            Affinity::RegClass(rci) => Some(reginfo.rc(*rci)),
            _ => None,
        }
    }

    pub fn toprc_in(&self, reginfo: &RegInfo) -> Option<RegClass> {
        match self {
            Affinity::RegUnit(reg) => Some(reginfo.toprc_containing_regunit(*reg)),
            Affinity::RegClass(rci) => Some(reginfo.toprc(*rci)),
            _ => None,
        }
    }

    /// Merge an operand constraint into this affinity.
    ///
    /// Note that this does not guarantee that the register allocator will pick a register that
    /// satisfies the constraint.
    pub fn merge(&mut self, constraint: &OperandConstraint, reginfo: &RegInfo) {
        match *self {
            Self::Unassigned => *self = Self::new(constraint),
            Self::RegClass(rc) => {
                match constraint.kind {
                    ConstraintKind::RegClass => {
                        // If the preferred register class is a subclass of the constraint, there's no need
                        // to change anything.
                        if !constraint.regclass.has_subclass(rc) {
                            // If the register classes don't overlap, `intersect` returns `Unassigned`, and
                            // we just keep our previous affinity.
                            if let Some(subclass) =
                                constraint.regclass.intersect_index(reginfo.rc(rc))
                            {
                                // This constraint shrinks our preferred register class.
                                *self = Self::RegClass(subclass);
                            }
                        }
                    }
                    ConstraintKind::FixedReg(reg) | ConstraintKind::FixedTied(reg) => {
                        // If the constraint register indicates a more precise affinity, update it
                        if reginfo.rc(rc).contains(reg) {
                            *self = Affinity::RegUnit(reg);
                        }
                    }
                    _ => {}
                }
            }
            // Either the constraint is a stack constraint, and we would just keep this affinity,
            // or it's a register constraint. If it's a register constraint, it's either the same,
            // and we wouldn't change the affinity, or it's a different constraint, which has no
            // intersection with this register, and wouldn't change the affinity.
            //
            // In all cases, we wouldn't change the affinity.
            Self::RegUnit(_) => {}
            Self::Stack => {}
        }
    }

    /// Compute the maximal intersection between this affinity and some other affinity.
    ///
    /// In cases where the two affinities conflict, this affinity wins out.
    pub fn intersect(&mut self, other: &Affinity, reginfo: &RegInfo) {
        match *self {
            Affinity::Unassigned => {
                // Unassigned is the least picky affinity, we can accept anything more precise.
                *self = other.clone();
            }
            Affinity::RegClass(curr_rci) => {
                // Register class affinities can be narrowed, if a narrower class exists.
                match other {
                    Affinity::RegUnit(new_unit) => {
                        // if the other affinity is for a compatible register, we can use it
                        if reginfo.rc(curr_rci).contains(*new_unit) {
                            *self = Affinity::RegUnit(*new_unit);
                        }
                    }
                    Affinity::RegClass(new_rci) => {
                        // or if the affinity is for a register class more precise than the
                        // current, we can also use it
                        if let Some(subclass) =
                            reginfo.rc(curr_rci).intersect_index(reginfo.rc(*new_rci))
                        {
                            *self = Affinity::RegClass(subclass);
                        }
                    }
                    // other cases are either wider or equivalent
                    _ => {}
                }
            }
            // Either this is already a RegUnit affinity, and either the same or not useful, or
            // this is a Stack affinity, and the new affinity is conflicting or a no-more-precise
            // Stack affinity
            _ => {}
        }
    }

    /// Return an object that can display this value affinity, using the register info from the
    /// target ISA.
    pub fn display<'a, R: Into<Option<&'a RegInfo>>>(self, regs: R) -> DisplayAffinity<'a> {
        DisplayAffinity(self, regs.into())
    }
}

/// Displaying an `Affinity` correctly requires the associated `RegInfo` from the target ISA.
pub struct DisplayAffinity<'a>(Affinity, Option<&'a RegInfo>);

impl<'a> fmt::Display for DisplayAffinity<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            Affinity::Unassigned => write!(f, "unassigned"),
            Affinity::Stack => write!(f, "stack"),
            Affinity::RegClass(rci) => match self.1 {
                Some(regs) => write!(f, "{}", regs.rc(rci)),
                None => write!(f, "{}", rci),
            },
            Affinity::RegUnit(unit) => match self.1 {
                Some(regs) => write!(f, "{}", regs.display_regunit(unit)),
                None => write!(f, "{}", unit),
            },
        }
    }
}
