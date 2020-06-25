use super::arithmetic::ArithmeticPackage;
use super::fn_basic::BasicFnPackage;
use super::iter_basic::BasicIteratorPackage;
use super::logic::LogicPackage;
use super::string_basic::BasicStringPackage;

use crate::def_package;

def_package!(crate:CorePackage:"_Core_ package containing basic facilities.", lib, {
    ArithmeticPackage::init(lib);
    LogicPackage::init(lib);
    BasicStringPackage::init(lib);
    BasicIteratorPackage::init(lib);
    BasicFnPackage::init(lib);
});
