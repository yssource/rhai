#![allow(non_snake_case)]

use crate::eval::calc_index;
use crate::plugin::*;
use crate::{
    def_package, ExclusiveRange, InclusiveRange, Position, RhaiResultOf, ERR, INT, UNSIGNED_INT,
};
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
    pub fn get_bit(value: INT, bit: INT) -> RhaiResultOf<bool> {
        let bit = calc_index(BITS, bit, true, || {
            ERR::ErrorBitFieldBounds(BITS, bit, Position::NONE)
        })?;

        Ok((value & (1 << bit)) != 0)
    }
    #[rhai_fn(return_raw)]
    pub fn set_bit(value: &mut INT, bit: INT, new_value: bool) -> RhaiResultOf<()> {
        let bit = calc_index(BITS, bit, true, || {
            ERR::ErrorBitFieldBounds(BITS, bit, Position::NONE)
        })?;

        let mask = 1 << bit;
        if new_value {
            *value |= mask;
        } else {
            *value &= !mask;
        }

        Ok(())
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
    pub fn get_bits(value: INT, bit: INT, bits: INT) -> RhaiResultOf<INT> {
        if bits <= 0 {
            return Ok(0);
        }

        let bit = calc_index(BITS, bit, true, || {
            ERR::ErrorBitFieldBounds(BITS, bit, Position::NONE)
        })?;

        let bits = if bit + bits as usize > BITS {
            BITS - bit
        } else {
            bits as usize
        };

        if bit == 0 && bits == BITS {
            return Ok(value);
        }

        // 2^bits - 1
        let mask = ((2 as UNSIGNED_INT).pow(bits as u32) - 1) as crate::INT;

        Ok(((value & (mask << bit)) >> bit) & mask)
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
    pub fn set_bits(value: &mut INT, bit: INT, bits: INT, new_value: INT) -> RhaiResultOf<()> {
        if bits <= 0 {
            return Ok(());
        }

        let bit = calc_index(BITS, bit, true, || {
            ERR::ErrorBitFieldBounds(BITS, bit, Position::NONE)
        })?;

        let bits = if bit + bits as usize > BITS {
            BITS - bit
        } else {
            bits as usize
        };

        if bit == 0 && bits == BITS {
            *value = new_value;
            return Ok(());
        }

        // 2^bits - 1
        let mask = ((2 as UNSIGNED_INT).pow(bits as u32) - 1) as crate::INT;

        *value &= !(mask << bit);
        *value |= (new_value & mask) << bit;

        Ok(())
    }
}
