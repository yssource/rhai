//! Module defining script options.

use std::ops::{Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, Not, Sub, SubAssign};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

/// A type representing the access mode of a function.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum FnAccess {
    /// Public function.
    Public,
    /// Private function.
    Private,
}

/// _(internals)_ A type that holds a configuration option with bit-flags.
/// Exported under the `internals` feature only.
///
/// Functionality-wise, this type is a naive and simplistic implementation of
/// [`bit_flags`](https://crates.io/crates/bitflags).  It is re-implemented to avoid pulling in yet
/// one more dependency.
#[derive(PartialEq, Eq, Copy, Clone, Hash, Default)]
pub struct OptionFlags(u8);

impl OptionFlags {
    /// Does this [`OptionFlags`] contain a particular option flag?
    #[inline(always)]
    #[must_use]
    pub const fn contains(self, flag: Self) -> bool {
        self.0 & flag.0 != 0
    }
}

impl Not for OptionFlags {
    type Output = Self;

    /// Return the negation of the [`OptionFlags`].
    #[inline(always)]
    fn not(self) -> Self::Output {
        Self(!self.0) & AST_OPTION_FLAGS::AST_OPTION_ALL
    }
}

impl Add for OptionFlags {
    type Output = Self;

    /// Return the union of two [`OptionFlags`].
    #[inline(always)]
    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl AddAssign for OptionFlags {
    /// Add the option flags in one [`OptionFlags`] to another.
    #[inline(always)]
    fn add_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0
    }
}

impl BitOr for OptionFlags {
    type Output = Self;

    /// Return the union of two [`OptionFlags`].
    #[inline(always)]
    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl BitOrAssign for OptionFlags {
    /// Add the option flags in one [`OptionFlags`] to another.
    #[inline(always)]
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0
    }
}

impl Sub for OptionFlags {
    type Output = Self;

    /// Return the difference of two [`OptionFlags`].
    #[inline(always)]
    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 & !rhs.0)
    }
}

impl SubAssign for OptionFlags {
    /// Remove the option flags in one [`OptionFlags`] from another.
    #[inline(always)]
    fn sub_assign(&mut self, rhs: Self) {
        self.0 &= !rhs.0
    }
}

impl BitAnd for OptionFlags {
    type Output = Self;

    /// Return the intersection of two [`OptionFlags`].
    #[inline(always)]
    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & !rhs.0)
    }
}

impl BitAndAssign for OptionFlags {
    /// Keep only the intersection of one [`OptionFlags`] with another.
    #[inline(always)]
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= !rhs.0
    }
}

/// _(internals)_ Option bit-flags for [`AST`][super::AST] nodes.
/// Exported under the `internals` feature only.
#[allow(non_snake_case)]
pub mod AST_OPTION_FLAGS {
    use super::OptionFlags;

    /// _(internals)_ No options for the [`AST`][crate::AST] node.
    /// Exported under the `internals` feature only.
    pub const AST_OPTION_NONE: OptionFlags = OptionFlags(0b0000_0000);
    /// _(internals)_ The [`AST`][crate::AST] node is constant.
    /// Exported under the `internals` feature only.
    pub const AST_OPTION_CONSTANT: OptionFlags = OptionFlags(0b0000_0001);
    /// _(internals)_ The [`AST`][crate::AST] node is exported to the outside (i.e. public).
    /// Exported under the `internals` feature only.
    pub const AST_OPTION_EXPORTED: OptionFlags = OptionFlags(0b0000_0010);
    /// _(internals)_ The [`AST`][crate::AST] node is in negated mode
    /// (meaning whatever information is the opposite).
    /// Exported under the `internals` feature only.
    pub const AST_OPTION_NEGATED: OptionFlags = OptionFlags(0b0000_0100);
    /// _(internals)_ The [`AST`][crate::AST] node breaks out of normal control flow.
    /// Exported under the `internals` feature only.
    pub const AST_OPTION_BREAK: OptionFlags = OptionFlags(0b0000_1000);
    /// _(internals)_ Mask of all options.
    /// Exported under the `internals` feature only.
    pub(crate) const AST_OPTION_ALL: OptionFlags = OptionFlags(
        AST_OPTION_CONSTANT.0 | AST_OPTION_EXPORTED.0 | AST_OPTION_NEGATED.0 | AST_OPTION_BREAK.0,
    );

    impl std::fmt::Debug for OptionFlags {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            fn write_option(
                options: &OptionFlags,
                f: &mut std::fmt::Formatter<'_>,
                num_flags: &mut usize,
                flag: OptionFlags,
                name: &str,
            ) -> std::fmt::Result {
                if options.contains(flag) {
                    if *num_flags > 0 {
                        f.write_str("+")?;
                    }
                    f.write_str(name)?;
                    *num_flags += 1;
                }
                Ok(())
            }

            let num_flags = &mut 0;

            f.write_str("(")?;
            write_option(self, f, num_flags, AST_OPTION_CONSTANT, "Constant")?;
            write_option(self, f, num_flags, AST_OPTION_EXPORTED, "Exported")?;
            write_option(self, f, num_flags, AST_OPTION_NEGATED, "Negated")?;
            write_option(self, f, num_flags, AST_OPTION_BREAK, "Break")?;
            f.write_str(")")?;

            Ok(())
        }
    }
}
