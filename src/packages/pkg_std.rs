#[cfg(feature = "no_std")]
use std::prelude::v1::*;

use crate::def_package;

def_package! {
    /// Standard package containing all built-in features.
    ///
    /// # Contents
    ///
    /// * [`CorePackage`][super::CorePackage]
    /// * [`BasicMathPackage`][super::BasicMathPackage]
    /// * [`BasicArrayPackage`][super::BasicArrayPackage]
    /// * [`BasicBlobPackage`][super::BasicBlobPackage]
    /// * [`BasicMapPackage`][super::BasicMapPackage]
    /// * [`BasicTimePackage`][super::BasicTimePackage]
    /// * [`MoreStringPackage`][super::MoreStringPackage]
    crate::StandardPackage => |lib| {
        lib.standard = true;

        super::CorePackage::init(lib);
        super::BasicMathPackage::init(lib);
        #[cfg(not(feature = "no_index"))]
        super::BasicArrayPackage::init(lib);
        #[cfg(not(feature = "no_index"))]
        super::BasicBlobPackage::init(lib);
        #[cfg(not(feature = "no_object"))]
        super::BasicMapPackage::init(lib);
        #[cfg(not(feature = "no_std"))]
        super::BasicTimePackage::init(lib);
        super::MoreStringPackage::init(lib);
    }
}
