#[cfg(feature = "no_std")]
use std::prelude::v1::*;

use crate::def_package;

def_package! {
    /// Core package containing basic facilities.
    ///
    /// # Contents
    ///
    /// * [`LanguageCorePackage`][super::LanguageCorePackage]
    /// * [`ArithmeticPackage`][super::ArithmeticPackage]
    /// * [`LogicPackage`][super::LogicPackage]
    /// * [`BasicStringPackage`][super::BasicStringPackage]
    /// * [`BasicIteratorPackage`][super::BasicIteratorPackage]
    /// * [`BasicFnPackage`][super::BasicFnPackage]
    crate::CorePackage => |lib| {
        lib.standard = true;

        super::LanguageCorePackage::init(lib);
        super::ArithmeticPackage::init(lib);
        super::LogicPackage::init(lib);
        super::BasicStringPackage::init(lib);
        super::BasicIteratorPackage::init(lib);
        super::BasicFnPackage::init(lib);
    }
}
