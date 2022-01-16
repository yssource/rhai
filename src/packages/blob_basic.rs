#![cfg(not(feature = "no_index"))]
#![allow(non_snake_case)]

use crate::eval::{calc_index, calc_offset_len};
use crate::plugin::*;
use crate::{
    def_package, Blob, Dynamic, ExclusiveRange, InclusiveRange, NativeCallContext, Position,
    RhaiResultOf, INT,
};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{any::TypeId, mem};

#[cfg(not(feature = "no_float"))]
use crate::FLOAT;

const INT_BYTES: usize = mem::size_of::<INT>();

#[cfg(not(feature = "no_float"))]
const FLOAT_BYTES: usize = mem::size_of::<FLOAT>();

def_package! {
    /// Package of basic BLOB utilities.
    crate::BasicBlobPackage => |lib| {
        lib.standard = true;

        combine_with_exported_module!(lib, "blob", blob_functions);
        combine_with_exported_module!(lib, "parse_int", parse_int_functions);
        combine_with_exported_module!(lib, "write_int", write_int_functions);
        combine_with_exported_module!(lib, "write_string", write_string_functions);

        #[cfg(not(feature = "no_float"))]
        {
            combine_with_exported_module!(lib, "parse_float", parse_float_functions);
            combine_with_exported_module!(lib, "write_float", write_float_functions);
        }

        // Register blob iterator
        lib.set_iterable::<Blob>();
    }
}

#[export_module]
pub mod blob_functions {
    /// Return a new, empty BLOB.
    pub const fn blob() -> Blob {
        Blob::new()
    }
    /// Return a new BLOB of the specified length, filled with zeros.
    ///
    /// If `len` ≤ 0, an empty BLOB is returned.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b = blob(10);
    ///
    /// print(b);       // prints "[0000000000000000 0000]"
    /// ```
    #[rhai_fn(name = "blob", return_raw)]
    pub fn blob_with_capacity(ctx: NativeCallContext, len: INT) -> RhaiResultOf<Blob> {
        blob_with_capacity_and_value(ctx, len, 0)
    }
    /// Return a new BLOB of the specified length, filled with copies of the initial `value`.
    ///
    /// If `len` ≤ 0, an empty BLOB is returned.
    ///
    /// Only the lower 8 bits of the initial `value` are used; all other bits are ignored.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b = blob(10, 0x42);
    ///
    /// print(b);       // prints "[4242424242424242 4242]"
    /// ```
    #[rhai_fn(name = "blob", return_raw)]
    pub fn blob_with_capacity_and_value(
        ctx: NativeCallContext,
        len: INT,
        value: INT,
    ) -> RhaiResultOf<Blob> {
        let len = if len < 0 { 0 } else { len as usize };
        let _ctx = ctx;

        // Check if blob will be over max size limit
        #[cfg(not(feature = "unchecked"))]
        if _ctx.engine().max_array_size() > 0 && len > _ctx.engine().max_array_size() {
            return Err(
                crate::ERR::ErrorDataTooLarge("Size of BLOB".to_string(), Position::NONE).into(),
            );
        }

        let mut blob = Blob::new();
        blob.resize(len, (value & 0x000000ff) as u8);
        Ok(blob)
    }
    /// Return the length of the BLOB.
    #[rhai_fn(name = "len", get = "len", pure)]
    pub fn len(blob: &mut Blob) -> INT {
        blob.len() as INT
    }
    /// Add a new byte `value` to the end of the BLOB.
    ///
    /// Only the lower 8 bits of the `value` are used; all other bits are ignored.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b = blob();
    ///
    /// b.push(0x42);
    ///
    /// print(b);       // prints "[42]"
    /// ```
    pub fn push(blob: &mut Blob, value: INT) {
        let value = (value & 0x000000ff) as u8;
        blob.push(value);
    }
    /// Add another BLOB to the end of the BLOB.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b1 = blob(5, 0x42);
    /// let b2 = blob(3, 0x11);
    ///
    /// b1.push(b2);
    ///
    /// print(b1);      // prints "[4242424242111111]"
    /// ```
    pub fn append(blob: &mut Blob, y: Blob) {
        if !y.is_empty() {
            if blob.is_empty() {
                *blob = y;
            } else {
                blob.extend(y);
            }
        }
    }
    /// Add another BLOB to the end of the BLOB, returning it as a new BLOB.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b1 = blob(5, 0x42);
    /// let b2 = blob(3, 0x11);
    ///
    /// print(b1 + b2);     // prints "[4242424242111111]"
    ///
    /// print(b1);          // prints "[4242424242]"
    /// ```
    #[rhai_fn(name = "+")]
    pub fn concat(blob1: Blob, blob2: Blob) -> Blob {
        if !blob2.is_empty() {
            if blob1.is_empty() {
                blob2
            } else {
                let mut blob = blob1;
                blob.extend(blob2);
                blob
            }
        } else {
            blob1
        }
    }
    /// Add a byte `value` to the BLOB at a particular `index` position.
    ///
    /// * If `index` < 0, position counts from the end of the BLOB (`-1` is the last byte).
    /// * If `index` < -length of BLOB, the byte value is added to the beginning of the BLOB.
    /// * If `index` ≥ length of BLOB, the byte value is appended to the end of the BLOB.
    ///
    /// Only the lower 8 bits of the `value` are used; all other bits are ignored.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b = blob(5, 0x42);
    ///
    /// b.insert(2, 0x18);
    ///
    /// print(b);       // prints "[4242184242]"
    /// ```
    pub fn insert(blob: &mut Blob, index: INT, value: INT) {
        let value = (value & 0x000000ff) as u8;

        if blob.is_empty() {
            blob.push(value);
            return;
        }

        let (index, _) = calc_offset_len(blob.len(), index, 0);

        if index >= blob.len() {
            blob.push(value);
        } else {
            blob.insert(index, value);
        }
    }
    /// Pad the BLOB to at least the specified length with copies of a specified byte `value`.
    ///
    /// If `len` ≤ length of BLOB, no padding is done.
    ///
    /// Only the lower 8 bits of the `value` are used; all other bits are ignored.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b = blob(3, 0x42);
    ///
    /// b.pad(5, 0x18)
    ///
    /// print(b);               // prints "[4242421818]"
    ///
    /// b.pad(3, 0xab)
    ///
    /// print(b);               // prints "[4242421818]"
    /// ```
    #[rhai_fn(return_raw)]
    pub fn pad(ctx: NativeCallContext, blob: &mut Blob, len: INT, value: INT) -> RhaiResultOf<()> {
        if len <= 0 {
            return Ok(());
        }

        let value = (value & 0x000000ff) as u8;
        let _ctx = ctx;

        // Check if blob will be over max size limit
        #[cfg(not(feature = "unchecked"))]
        if _ctx.engine().max_array_size() > 0 && (len as usize) > _ctx.engine().max_array_size() {
            return Err(
                crate::ERR::ErrorDataTooLarge("Size of BLOB".to_string(), Position::NONE).into(),
            );
        }

        if len as usize > blob.len() {
            blob.resize(len as usize, value);
        }

        Ok(())
    }
    /// Remove the last byte from the BLOB and return it.
    ///
    /// If the BLOB is empty, zero is returned.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b = blob();
    ///
    /// b += 1; b += 2; b += 3; b += 4; b += 5;
    ///
    /// print(b.pop());         // prints 5
    ///
    /// print(b);               // prints "[01020304]"
    /// ```
    pub fn pop(blob: &mut Blob) -> INT {
        if blob.is_empty() {
            0
        } else {
            blob.pop().map_or_else(|| 0, |v| v as INT)
        }
    }
    /// Remove the first byte from the BLOB and return it.
    ///
    /// If the BLOB is empty, zero is returned.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b = blob();
    ///
    /// b += 1; b += 2; b += 3; b += 4; b += 5;
    ///
    /// print(b.shift());       // prints 1
    ///
    /// print(b);               // prints "[02030405]"
    /// ```
    pub fn shift(blob: &mut Blob) -> INT {
        if blob.is_empty() {
            0
        } else {
            blob.remove(0) as INT
        }
    }
    /// Remove the byte at the specified `index` from the BLOB and return it.
    ///
    /// * If `index` < 0, position counts from the end of the BLOB (`-1` is the last byte).
    /// * If `index` < -length of BLOB, zero is returned.
    /// * If `index` ≥ length of BLOB, zero is returned.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b = blob();
    ///
    /// b += 1; b += 2; b += 3; b += 4; b += 5;
    ///
    /// print(x.remove(1));     // prints 2
    ///
    /// print(x);               // prints "[01030405]"
    ///
    /// print(x.remove(-2));    // prints 4
    ///
    /// print(x);               // prints "[010305]"
    /// ```
    pub fn remove(blob: &mut Blob, index: INT) -> INT {
        let index = match calc_index(blob.len(), index, true, || Err(())) {
            Ok(n) => n,
            Err(_) => return 0,
        };

        blob.remove(index) as INT
    }
    /// Clear the BLOB.
    pub fn clear(blob: &mut Blob) {
        if !blob.is_empty() {
            blob.clear();
        }
    }
    /// Cut off the BLOB at the specified length.
    ///
    /// * If `len` ≤ 0, the BLOB is cleared.
    /// * If `len` ≥ length of BLOB, the BLOB is not truncated.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b = blob();
    ///
    /// b += 1; b += 2; b += 3; b += 4; b += 5;
    ///
    /// b.truncate(3);
    ///
    /// print(b);           // prints "[010203]"
    ///
    /// b.truncate(10);
    ///
    /// print(b);           // prints "[010203]"
    /// ```
    pub fn truncate(blob: &mut Blob, len: INT) {
        if !blob.is_empty() {
            if len >= 0 {
                blob.truncate(len as usize);
            } else {
                blob.clear();
            }
        }
    }
    /// Cut off the head of the BLOB, leaving a tail of the specified length.
    ///
    /// * If `len` ≤ 0, the BLOB is cleared.
    /// * If `len` ≥ length of BLOB, the BLOB is not modified.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b = blob();
    ///
    /// b += 1; b += 2; b += 3; b += 4; b += 5;
    ///
    /// b.chop(3);
    ///
    /// print(b);           // prints "[030405]"
    ///
    /// b.chop(10);
    ///
    /// print(b);           // prints "[030405]"
    /// ```
    pub fn chop(blob: &mut Blob, len: INT) {
        if !blob.is_empty() {
            if len <= 0 {
                blob.clear();
            } else if (len as usize) < blob.len() {
                blob.drain(0..blob.len() - len as usize);
            }
        }
    }
    /// Reverse the BLOB.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b = blob();
    ///
    /// b += 1; b += 2; b += 3; b += 4; b += 5;
    ///
    /// print(b);           // prints "[0102030405]"
    ///
    /// b.reverse();
    ///
    /// print(b);           // prints "[0504030201]"
    /// ```
    pub fn reverse(blob: &mut Blob) {
        if !blob.is_empty() {
            blob.reverse();
        }
    }
    /// Replace an exclusive range of the BLOB with another BLOB.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b1 = blob(10, 0x42);
    /// let b2 = blob(5, 0x18);
    ///
    /// b1.splice(1..4, b2);
    ///
    /// print(b1);      // prints "[4218181818184242 42424242]"
    /// ```
    #[rhai_fn(name = "splice")]
    pub fn splice_range(blob: &mut Blob, range: ExclusiveRange, replace: Blob) {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        splice(blob, start, end - start, replace)
    }
    /// Replace an inclusive range of the BLOB with another BLOB.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b1 = blob(10, 0x42);
    /// let b2 = blob(5, 0x18);
    ///
    /// b1.splice(1..=4, b2);
    ///
    /// print(b1);      // prints "[4218181818184242 424242]"
    /// ```
    #[rhai_fn(name = "splice")]
    pub fn splice_range_inclusive(blob: &mut Blob, range: InclusiveRange, replace: Blob) {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        splice(blob, start, end - start + 1, replace)
    }
    /// Replace a portion of the BLOB with another BLOB.
    ///
    /// * If `start` < 0, position counts from the end of the BLOB (`-1` is the last byte).
    /// * If `start` < -length of BLOB, position counts from the beginning of the BLOB.
    /// * If `start` ≥ length of BLOB, the other BLOB is appended to the end of the BLOB.
    /// * If `len` ≤ 0, the other BLOB is inserted into the BLOB at the `start` position without replacing anything.
    /// * If `start` position + `len` ≥ length of BLOB, entire portion of the BLOB after the `start` position is replaced.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b1 = blob(10, 0x42);
    /// let b2 = blob(5, 0x18);
    ///
    /// b1.splice(1, 3, b2);
    ///
    /// print(b1);      // prints "[4218181818184242 42424242]"
    ///
    /// b1.splice(-5, 4, b2);
    ///
    /// print(b1);      // prints "[4218181818184218 1818181842]"
    /// ```
    pub fn splice(blob: &mut Blob, start: INT, len: INT, replace: Blob) {
        if blob.is_empty() {
            *blob = replace;
            return;
        }

        let (start, len) = calc_offset_len(blob.len(), start, len);

        if len == 0 {
            blob.extend(replace);
        } else {
            blob.splice(start..start + len, replace);
        }
    }
    /// Copy an exclusive range of the BLOB and return it as a new BLOB.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b = blob();
    ///
    /// b += 1; b += 2; b += 3; b += 4; b += 5;
    ///
    /// print(b.extract(1..3));     // prints "[0203]"
    ///
    /// print(b);                   // prints "[0102030405]"
    /// ```
    #[rhai_fn(name = "extract")]
    pub fn extract_range(blob: &mut Blob, range: ExclusiveRange) -> Blob {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        extract(blob, start, end - start)
    }
    /// Copy an inclusive range of the BLOB and return it as a new BLOB.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b = blob();
    ///
    /// b += 1; b += 2; b += 3; b += 4; b += 5;
    ///
    /// print(b.extract(1..=3));    // prints "[020304]"
    ///
    /// print(b);                   // prints "[0102030405]"
    /// ```
    #[rhai_fn(name = "extract")]
    pub fn extract_range_inclusive(blob: &mut Blob, range: InclusiveRange) -> Blob {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        extract(blob, start, end - start + 1)
    }
    /// Copy a portion of the BLOB and return it as a new BLOB.
    ///
    /// * If `start` < 0, position counts from the end of the BLOB (`-1` is the last byte).
    /// * If `start` < -length of BLOB, position counts from the beginning of the BLOB.
    /// * If `start` ≥ length of BLOB, an empty BLOB is returned.
    /// * If `len` ≤ 0, an empty BLOB is returned.
    /// * If `start` position + `len` ≥ length of BLOB, entire portion of the BLOB after the `start` position is copied and returned.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b = blob();
    ///
    /// b += 1; b += 2; b += 3; b += 4; b += 5;
    ///
    /// print(b.extract(1, 3));     // prints "[020303]"
    ///
    /// print(b.extract(-3, 2));    // prints "[0304]"
    ///
    /// print(b);                   // prints "[0102030405]"
    /// ```
    pub fn extract(blob: &mut Blob, start: INT, len: INT) -> Blob {
        if blob.is_empty() || len <= 0 {
            return Blob::new();
        }

        let (start, len) = calc_offset_len(blob.len(), start, len);

        if len == 0 {
            Blob::new()
        } else {
            blob[start..start + len].to_vec()
        }
    }
    /// Copy a portion of the BLOB beginning at the `start` position till the end and return it as
    /// a new BLOB.
    ///
    /// * If `start` < 0, position counts from the end of the BLOB (`-1` is the last byte).
    /// * If `start` < -length of BLOB, the entire BLOB is copied and returned.
    /// * If `start` ≥ length of BLOB, an empty BLOB is returned.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b = blob();
    ///
    /// b += 1; b += 2; b += 3; b += 4; b += 5;
    ///
    /// print(b.extract(2));        // prints "[030405]"
    ///
    /// print(b.extract(-3));       // prints "[030405]"
    ///
    /// print(b);                   // prints "[0102030405]"
    /// ```
    #[rhai_fn(name = "extract")]
    pub fn extract_tail(blob: &mut Blob, start: INT) -> Blob {
        extract(blob, start, INT::MAX)
    }
    /// Cut off the BLOB at `index` and return it as a new BLOB.
    ///
    /// * If `index` < 0, position counts from the end of the BLOB (`-1` is the last byte).
    /// * If `index` is zero, the entire BLOB is cut and returned.
    /// * If `index` < -length of BLOB, the entire BLOB is cut and returned.
    /// * If `index` ≥ length of BLOB, nothing is cut from the BLOB and an empty BLOB is returned.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b1 = blob();
    ///
    /// b1 += 1; b1 += 2; b1 += 3; b1 += 4; b1 += 5;
    ///
    /// let b2 = b1.split(2);
    ///
    /// print(b2);          // prints "[030405]"
    ///
    /// print(b1);          // prints "[0102]"
    /// ```
    #[rhai_fn(name = "split")]
    pub fn split_at(blob: &mut Blob, index: INT) -> Blob {
        if blob.is_empty() {
            return Blob::new();
        }

        let (index, len) = calc_offset_len(blob.len(), index, INT::MAX);

        if index == 0 {
            if len > blob.len() {
                mem::take(blob)
            } else {
                let mut result = Blob::new();
                result.extend(blob.drain(blob.len() - len..));
                result
            }
        } else if index >= blob.len() {
            Blob::new()
        } else {
            let mut result = Blob::new();
            result.extend(blob.drain(index as usize..));
            result
        }
    }
    /// Remove all bytes in the BLOB within an exclusive range and return them as a new BLOB.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b1 = blob();
    ///
    /// b1 += 1; b1 += 2; b1 += 3; b1 += 4; b1 += 5;
    ///
    /// let b2 = b1.drain(1..3);
    ///
    /// print(b1);      // prints "[010405]"
    ///
    /// print(b2);      // prints "[0203]"
    ///
    /// let b3 = b1.drain(2..3);
    ///
    /// print(b1);      // prints "[0104]"
    ///
    /// print(b3);      // prints "[05]"
    /// ```
    #[rhai_fn(name = "drain")]
    pub fn drain_range(blob: &mut Blob, range: ExclusiveRange) -> Blob {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        drain(blob, start, end - start)
    }
    /// Remove all bytes in the BLOB within an inclusive range and return them as a new BLOB.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b1 = blob();
    ///
    /// b1 += 1; b1 += 2; b1 += 3; b1 += 4; b1 += 5;
    ///
    /// let b2 = b1.drain(1..=2);
    ///
    /// print(b1);      // prints "[010405]"
    ///
    /// print(b2);      // prints "[0203]"
    ///
    /// let b3 = b1.drain(2..=2);
    ///
    /// print(b1);      // prints "[0104]"
    ///
    /// print(b3);      // prints "[05]"
    /// ```
    #[rhai_fn(name = "drain")]
    pub fn drain_range_inclusive(blob: &mut Blob, range: InclusiveRange) -> Blob {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        drain(blob, start, end - start + 1)
    }
    /// Remove all bytes within a portion of the BLOB and return them as a new BLOB.
    ///
    /// * If `start` < 0, position counts from the end of the BLOB (`-1` is the last byte).
    /// * If `start` < -length of BLOB, position counts from the beginning of the BLOB.
    /// * If `start` ≥ length of BLOB, nothing is removed and an empty BLOB is returned.
    /// * If `len` ≤ 0, nothing is removed and an empty BLOB is returned.
    /// * If `start` position + `len` ≥ length of BLOB, entire portion of the BLOB after the `start` position is removed and returned.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b1 = blob();
    ///
    /// b1 += 1; b1 += 2; b1 += 3; b1 += 4; b1 += 5;
    ///
    /// let b2 = b1.drain(1, 2);
    ///
    /// print(b1);      // prints "[010405]"
    ///
    /// print(b2);      // prints "[0203]"
    ///
    /// let b3 = b1.drain(-1, 1);
    ///
    /// print(b3);      // prints "[0104]"
    ///
    /// print(z);       // prints "[5]"
    /// ```
    pub fn drain(blob: &mut Blob, start: INT, len: INT) -> Blob {
        if blob.is_empty() || len <= 0 {
            return Blob::new();
        }

        let (start, len) = calc_offset_len(blob.len(), start, len);

        if len == 0 {
            Blob::new()
        } else {
            blob.drain(start..start + len).collect()
        }
    }
    /// Remove all bytes in the BLOB not within an exclusive range and return them as a new BLOB.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b1 = blob();
    ///
    /// b1 += 1; b1 += 2; b1 += 3; b1 += 4; b1 += 5;
    ///
    /// let b2 = b1.retain(1..4);
    ///
    /// print(b1);      // prints "[020304]"
    ///
    /// print(b2);      // prints "[0105]"
    ///
    /// let b3 = b1.retain(1..3);
    ///
    /// print(b1);      // prints "[0304]"
    ///
    /// print(b2);      // prints "[01]"
    /// ```
    #[rhai_fn(name = "retain")]
    pub fn retain_range(blob: &mut Blob, range: ExclusiveRange) -> Blob {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        retain(blob, start, end - start)
    }
    /// Remove all bytes in the BLOB not within an inclusive range and return them as a new BLOB.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b1 = blob();
    ///
    /// b1 += 1; b1 += 2; b1 += 3; b1 += 4; b1 += 5;
    ///
    /// let b2 = b1.retain(1..=3);
    ///
    /// print(b1);      // prints "[020304]"
    ///
    /// print(b2);      // prints "[0105]"
    ///
    /// let b3 = b1.retain(1..=2);
    ///
    /// print(b1);      // prints "[0304]"
    ///
    /// print(b2);      // prints "[01]"
    /// ```
    #[rhai_fn(name = "retain")]
    pub fn retain_range_inclusive(blob: &mut Blob, range: InclusiveRange) -> Blob {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        retain(blob, start, end - start + 1)
    }
    /// Remove all bytes not within a portion of the BLOB and return them as a new BLOB.
    ///
    /// * If `start` < 0, position counts from the end of the BLOB (`-1` is the last byte).
    /// * If `start` < -length of BLOB, position counts from the beginning of the BLOB.
    /// * If `start` ≥ length of BLOB, all elements are removed returned.
    /// * If `len` ≤ 0, all elements are removed and returned.
    /// * If `start` position + `len` ≥ length of BLOB, entire portion of the BLOB before the `start` position is removed and returned.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let b1 = blob();
    ///
    /// b1 += 1; b1 += 2; b1 += 3; b1 += 4; b1 += 5;
    ///
    /// let b2 = b1.retain(1, 2);
    ///
    /// print(b1);      // prints "[0203]"
    ///
    /// print(b2);      // prints "[010405]"
    ///
    /// let b3 = b1.retain(-1, 1);
    ///
    /// print(b1);      // prints "[03]"
    ///
    /// print(b3);      // prints "[02]"
    /// ```
    pub fn retain(blob: &mut Blob, start: INT, len: INT) -> Blob {
        if blob.is_empty() || len <= 0 {
            return Blob::new();
        }

        let (start, len) = calc_offset_len(blob.len(), start, len);

        if len == 0 {
            mem::take(blob)
        } else {
            let mut drained: Blob = blob.drain(..start).collect();
            drained.extend(blob.drain(len..));

            drained
        }
    }
}

#[export_module]
mod parse_int_functions {
    #[inline]
    fn parse_int(blob: &mut Blob, start: INT, len: INT, is_le: bool) -> INT {
        if blob.is_empty() || len <= 0 {
            return 0;
        }
        let (start, len) = calc_offset_len(blob.len(), start, len);

        if len == 0 {
            return 0;
        }

        let len = usize::min(len, INT_BYTES);

        let mut buf = [0_u8; INT_BYTES];

        buf[..len].copy_from_slice(&blob[start..][..len]);

        if is_le {
            INT::from_le_bytes(buf)
        } else {
            INT::from_be_bytes(buf)
        }
    }

    #[rhai_fn(name = "parse_le_int")]
    pub fn parse_le_int_range(blob: &mut Blob, range: ExclusiveRange) -> INT {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        parse_le_int(blob, start, end - start)
    }
    #[rhai_fn(name = "parse_le_int")]
    pub fn parse_le_int_range_inclusive(blob: &mut Blob, range: InclusiveRange) -> INT {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        parse_le_int(blob, start, end - start + 1)
    }
    pub fn parse_le_int(blob: &mut Blob, start: INT, len: INT) -> INT {
        parse_int(blob, start, len, true)
    }
    #[rhai_fn(name = "parse_be_int")]
    pub fn parse_be_int_range(blob: &mut Blob, range: ExclusiveRange) -> INT {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        parse_be_int(blob, start, end - start)
    }
    #[rhai_fn(name = "parse_be_int")]
    pub fn parse_be_int_range_inclusive(blob: &mut Blob, range: InclusiveRange) -> INT {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        parse_be_int(blob, start, end - start + 1)
    }
    pub fn parse_be_int(blob: &mut Blob, start: INT, len: INT) -> INT {
        parse_int(blob, start, len, false)
    }
}

#[cfg(not(feature = "no_float"))]
#[export_module]
mod parse_float_functions {
    #[inline]
    fn parse_float(blob: &mut Blob, start: INT, len: INT, is_le: bool) -> FLOAT {
        if blob.is_empty() || len <= 0 {
            return 0.0;
        }

        let (start, len) = calc_offset_len(blob.len(), start, len);

        if len == 0 {
            return 0.0;
        }

        let len = usize::min(len, FLOAT_BYTES);

        let mut buf = [0_u8; FLOAT_BYTES];

        buf[..len].copy_from_slice(&blob[start..][..len]);

        if is_le {
            FLOAT::from_le_bytes(buf)
        } else {
            FLOAT::from_be_bytes(buf)
        }
    }

    #[rhai_fn(name = "parse_le_float")]
    pub fn parse_le_float_range(blob: &mut Blob, range: ExclusiveRange) -> FLOAT {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        parse_le_float(blob, start, end - start)
    }
    #[rhai_fn(name = "parse_le_float")]
    pub fn parse_le_float_range_inclusive(blob: &mut Blob, range: InclusiveRange) -> FLOAT {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        parse_le_float(blob, start, end - start + 1)
    }
    pub fn parse_le_float(blob: &mut Blob, start: INT, len: INT) -> FLOAT {
        parse_float(blob, start, len, true)
    }
    #[rhai_fn(name = "parse_be_float")]
    pub fn parse_be_float_range(blob: &mut Blob, range: ExclusiveRange) -> FLOAT {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        parse_be_float(blob, start, end - start)
    }
    #[rhai_fn(name = "parse_be_float")]
    pub fn parse_be_float_range_inclusive(blob: &mut Blob, range: InclusiveRange) -> FLOAT {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        parse_be_float(blob, start, end - start + 1)
    }
    pub fn parse_be_float(blob: &mut Blob, start: INT, len: INT) -> FLOAT {
        parse_float(blob, start, len, false)
    }
}

#[export_module]
mod write_int_functions {
    #[inline]
    fn write_int(blob: &mut Blob, start: INT, len: INT, value: INT, is_le: bool) {
        if blob.is_empty() || len <= 0 {
            return;
        }

        let (start, len) = calc_offset_len(blob.len(), start, len);

        if len == 0 {
            return;
        }

        let len = usize::min(len, INT_BYTES);

        let buf = if is_le {
            value.to_le_bytes()
        } else {
            value.to_be_bytes()
        };

        blob[start..][..len].copy_from_slice(&buf[..len]);
    }
    #[rhai_fn(name = "write_le")]
    pub fn write_le_int_range(blob: &mut Blob, range: ExclusiveRange, value: INT) {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        write_le_int(blob, start, end - start, value)
    }
    #[rhai_fn(name = "write_le")]
    pub fn write_le_int_range_inclusive(blob: &mut Blob, range: InclusiveRange, value: INT) {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        write_le_int(blob, start, end - start + 1, value)
    }
    #[rhai_fn(name = "write_le")]
    pub fn write_le_int(blob: &mut Blob, start: INT, len: INT, value: INT) {
        write_int(blob, start, len, value, true)
    }
    #[rhai_fn(name = "write_be")]
    pub fn write_be_int_range(blob: &mut Blob, range: ExclusiveRange, value: INT) {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        write_be_int(blob, start, end - start, value)
    }
    #[rhai_fn(name = "write_be")]
    pub fn write_be_int_range_inclusive(blob: &mut Blob, range: InclusiveRange, value: INT) {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        write_be_int(blob, start, end - start + 1, value)
    }
    #[rhai_fn(name = "write_be")]
    pub fn write_be_int(blob: &mut Blob, start: INT, len: INT, value: INT) {
        write_int(blob, start, len, value, false)
    }
}

#[cfg(not(feature = "no_float"))]
#[export_module]
mod write_float_functions {
    #[inline]
    fn write_float(blob: &mut Blob, start: INT, len: INT, value: FLOAT, is_le: bool) {
        if blob.is_empty() || len <= 0 {
            return;
        }

        let (start, len) = calc_offset_len(blob.len(), start, len);

        if len == 0 {
            return;
        }

        let len = usize::min(len, FLOAT_BYTES);
        let buf = if is_le {
            value.to_le_bytes()
        } else {
            value.to_be_bytes()
        };

        blob[start..][..len].copy_from_slice(&buf[..len]);
    }
    #[rhai_fn(name = "write_le")]
    pub fn write_le_float_range(blob: &mut Blob, range: ExclusiveRange, value: FLOAT) {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        write_le_float(blob, start, end - start, value)
    }
    #[rhai_fn(name = "write_le")]
    pub fn write_le_float_range_inclusive(blob: &mut Blob, range: InclusiveRange, value: FLOAT) {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        write_le_float(blob, start, end - start + 1, value)
    }
    #[rhai_fn(name = "write_le")]
    pub fn write_le_float(blob: &mut Blob, start: INT, len: INT, value: FLOAT) {
        write_float(blob, start, len, value, true)
    }
    #[rhai_fn(name = "write_be")]
    pub fn write_be_float_range(blob: &mut Blob, range: ExclusiveRange, value: FLOAT) {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        write_be_float(blob, start, end - start, value)
    }
    #[rhai_fn(name = "write_be")]
    pub fn write_be_float_range_inclusive(blob: &mut Blob, range: InclusiveRange, value: FLOAT) {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        write_be_float(blob, start, end - start + 1, value)
    }
    #[rhai_fn(name = "write_be")]
    pub fn write_be_float(blob: &mut Blob, start: INT, len: INT, value: FLOAT) {
        write_float(blob, start, len, value, false)
    }
}

#[export_module]
mod write_string_functions {
    #[inline]
    fn write_string(blob: &mut Blob, start: INT, len: INT, string: &str, ascii_only: bool) {
        if len <= 0 || blob.is_empty() || string.is_empty() {
            return;
        }

        let (start, len) = calc_offset_len(blob.len(), start, len);

        if len == 0 {
            return;
        }

        let len = usize::min(len, string.len());

        if ascii_only {
            string
                .chars()
                .filter(char::is_ascii)
                .take(len)
                .map(|ch| ch as u8)
                .enumerate()
                .for_each(|(i, x)| blob[start + i] = x);
        } else {
            blob[start..][..len].copy_from_slice(&string.as_bytes()[..len]);
        }
    }
    #[rhai_fn(name = "write_utf8")]
    pub fn write_utf8_string(blob: &mut Blob, start: INT, len: INT, string: &str) {
        write_string(blob, start, len, string, false)
    }
    #[rhai_fn(name = "write_utf8")]
    pub fn write_utf8_string_range(blob: &mut Blob, range: ExclusiveRange, string: &str) {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        write_string(blob, start, end - start, string, false)
    }
    #[rhai_fn(name = "write_utf8")]
    pub fn write_utf8_string_range_inclusive(blob: &mut Blob, range: InclusiveRange, string: &str) {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        write_string(blob, start, end - start + 1, string, false)
    }
    #[rhai_fn(name = "write_ascii")]
    pub fn write_ascii_string(blob: &mut Blob, start: INT, len: INT, string: &str) {
        write_string(blob, start, len, string, true)
    }
    #[rhai_fn(name = "write_ascii")]
    pub fn write_ascii_string_range(blob: &mut Blob, range: ExclusiveRange, string: &str) {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        write_string(blob, start, end - start, string, true)
    }
    #[rhai_fn(name = "write_ascii")]
    pub fn write_ascii_string_range_inclusive(
        blob: &mut Blob,
        range: InclusiveRange,
        string: &str,
    ) {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        write_string(blob, start, end - start + 1, string, true)
    }
}
