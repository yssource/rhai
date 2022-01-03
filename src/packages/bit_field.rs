#![allow(non_snake_case)]

use crate::plugin::*;
use crate::{def_package, ExclusiveRange, InclusiveRange, Position, RhaiResultOf, ERR, INT};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

def_package! {
    /// Package of basic bit-field utilities.
    crate::BitFieldPackage => |lib| {
        lib.standard = true;

        combine_with_exported_module!(lib, "bit_field", bit_field_functions);
    }
}

#[export_module]
mod bit_field_functions {
    const BITS: usize = std::mem::size_of::<INT>() * 8;

    #[rhai_fn(return_raw)]
    pub fn get_bit(value: INT, index: INT) -> RhaiResultOf<bool> {
        if index >= 0 {
            let offset = index as usize;

            if offset >= BITS {
                Err(ERR::ErrorBitFieldBounds(BITS, index, Position::NONE).into())
            } else {
                Ok((value & (1 << offset)) != 0)
            }
        } else if let Some(abs_index) = index.checked_abs() {
            let offset = abs_index as usize;

            // Count from end if negative
            if offset > BITS {
                Err(ERR::ErrorBitFieldBounds(BITS, index, Position::NONE).into())
            } else {
                Ok((value & (1 << (BITS - offset))) != 0)
            }
        } else {
            Err(ERR::ErrorBitFieldBounds(BITS, index, Position::NONE).into())
        }
    }
    #[rhai_fn(return_raw)]
    pub fn set_bit(value: &mut INT, index: INT, new_value: bool) -> RhaiResultOf<()> {
        if index >= 0 {
            let offset = index as usize;

            if offset >= BITS {
                Err(ERR::ErrorBitFieldBounds(BITS, index, Position::NONE).into())
            } else {
                let mask = 1 << offset;
                if new_value {
                    *value |= mask;
                } else {
                    *value &= !mask;
                }
                Ok(())
            }
        } else if let Some(abs_index) = index.checked_abs() {
            let offset = abs_index as usize;

            // Count from end if negative
            if offset > BITS {
                Err(ERR::ErrorBitFieldBounds(BITS, index, Position::NONE).into())
            } else {
                let mask = 1 << offset;
                if new_value {
                    *value |= mask;
                } else {
                    *value &= !mask;
                }
                Ok(())
            }
        } else {
            Err(ERR::ErrorBitFieldBounds(BITS, index, Position::NONE).into())
        }
    }
    #[rhai_fn(name = "get_bits", return_raw)]
    pub fn get_bits_range(value: INT, range: ExclusiveRange) -> RhaiResultOf<INT> {
        let from = INT::max(range.start, 0);
        let to = INT::max(range.end, from);
        get_bits(value, from, to - from)
    }
    #[rhai_fn(name = "get_bits", return_raw)]
    pub fn get_bits_range_inclusive(value: INT, range: InclusiveRange) -> RhaiResultOf<INT> {
        let from = INT::max(*range.start(), 0);
        let to = INT::max(*range.end(), from - 1);
        get_bits(value, from, to - from + 1)
    }
    #[rhai_fn(return_raw)]
    pub fn get_bits(value: INT, index: INT, bits: INT) -> RhaiResultOf<INT> {
        if bits < 1 {
            return Ok(0);
        }

        let offset = if index >= 0 {
            let offset = index as usize;

            if offset >= BITS {
                return Err(ERR::ErrorBitFieldBounds(BITS, index, Position::NONE).into());
            }

            offset
        } else if let Some(abs_index) = index.checked_abs() {
            let offset = abs_index as usize;

            // Count from end if negative
            if offset > BITS {
                return Err(ERR::ErrorBitFieldBounds(BITS, index, Position::NONE).into());
            }
            BITS - offset
        } else {
            return Err(ERR::ErrorBitFieldBounds(BITS, index, Position::NONE).into());
        };

        let bits = if offset + bits as usize > BITS {
            BITS - offset
        } else {
            bits as usize
        };

        let mut base = 1;
        let mut mask = 0;

        for _ in 0..bits {
            mask |= base;
            base <<= 1;
        }

        Ok(((value & (mask << index)) >> index) & mask)
    }
    #[rhai_fn(name = "set_bits", return_raw)]
    pub fn set_bits_range(
        value: &mut INT,
        range: ExclusiveRange,
        new_value: INT,
    ) -> RhaiResultOf<()> {
        let from = INT::max(range.start, 0);
        let to = INT::max(range.end, from);
        set_bits(value, from, to - from, new_value)
    }
    #[rhai_fn(name = "set_bits", return_raw)]
    pub fn set_bits_range_inclusive(
        value: &mut INT,
        range: InclusiveRange,
        new_value: INT,
    ) -> RhaiResultOf<()> {
        let from = INT::max(*range.start(), 0);
        let to = INT::max(*range.end(), from - 1);
        set_bits(value, from, to - from + 1, new_value)
    }
    #[rhai_fn(return_raw)]
    pub fn set_bits(value: &mut INT, index: INT, bits: INT, new_value: INT) -> RhaiResultOf<()> {
        if bits < 1 {
            return Ok(());
        }

        let offset = if index >= 0 {
            let offset = index as usize;

            if offset >= BITS {
                return Err(ERR::ErrorBitFieldBounds(BITS, index, Position::NONE).into());
            }

            offset
        } else if let Some(abs_index) = index.checked_abs() {
            let offset = abs_index as usize;

            // Count from end if negative
            if offset > BITS {
                return Err(ERR::ErrorBitFieldBounds(BITS, index, Position::NONE).into());
            }
            BITS - offset
        } else {
            return Err(ERR::ErrorBitFieldBounds(BITS, index, Position::NONE).into());
        };

        let bits = if offset + bits as usize > BITS {
            BITS - offset
        } else {
            bits as usize
        };

        let mut base = 1;
        let mut mask = 0;

        for _ in 0..bits {
            mask |= base;
            base <<= 1;
        }

        *value &= !(mask << index);
        *value |= (new_value & mask) << index;

        Ok(())
    }
}
