use super::str::ImmutableStringDeserializer;
use crate::any::{Dynamic, Union};
use crate::result::EvalAltResult;
use crate::token::Position;
use crate::utils::ImmutableString;

use serde::de::{DeserializeSeed, Deserializer, Error, MapAccess, SeqAccess, Visitor};
use serde::Deserialize;

#[cfg(not(feature = "no_index"))]
use crate::engine::Array;
#[cfg(not(feature = "no_object"))]
use crate::engine::Map;

use crate::stdlib::{any::type_name, fmt};

#[cfg(not(feature = "no_std"))]
#[cfg(not(target_arch = "wasm32"))]
use crate::stdlib::time::Instant;

#[cfg(not(feature = "no_std"))]
#[cfg(target_arch = "wasm32")]
use instant::Instant;

pub struct DynamicDeserializer<'a> {
    value: &'a Dynamic,
}

impl<'a> DynamicDeserializer<'a> {
    pub fn from_dynamic(value: &'a Dynamic) -> Self {
        Self { value }
    }
    pub fn type_error<R, T>(&self) -> Result<T, Box<EvalAltResult>> {
        self.type_error_str(type_name::<R>())
    }
    pub fn type_error_str<T>(&self, name: &str) -> Result<T, Box<EvalAltResult>> {
        Err(Box::new(EvalAltResult::ErrorMismatchOutputType(
            name.into(),
            self.value.type_name().into(),
            Position::none(),
        )))
    }
}

pub fn from_dynamic<'de, T: Deserialize<'de>>(
    value: &'de Dynamic,
) -> Result<T, Box<EvalAltResult>> {
    T::deserialize(&mut DynamicDeserializer::from_dynamic(value))
}

impl Error for Box<EvalAltResult> {
    fn custom<T: fmt::Display>(err: T) -> Self {
        Box::new(EvalAltResult::ErrorRuntime(
            err.to_string(),
            Position::none(),
        ))
    }
}

impl<'de> Deserializer<'de> for &mut DynamicDeserializer<'de> {
    type Error = Box<EvalAltResult>;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        match &self.value.0 {
            Union::Unit(_) => self.deserialize_unit(visitor),
            Union::Bool(_) => self.deserialize_bool(visitor),
            Union::Str(_) => self.deserialize_str(visitor),
            Union::Char(_) => self.deserialize_char(visitor),
            #[cfg(not(feature = "only_i32"))]
            Union::Int(_) => self.deserialize_i64(visitor),
            #[cfg(feature = "only_i32")]
            Union::Int(_) => self.deserialize_i32(visitor),
            #[cfg(not(feature = "no_float"))]
            Union::Float(_) => self.deserialize_f64(visitor),
            #[cfg(not(feature = "no_index"))]
            Union::Array(_) => self.deserialize_seq(visitor),
            #[cfg(not(feature = "no_object"))]
            Union::Map(_) => self.deserialize_map(visitor),
            Union::FnPtr(_) => unimplemented!(),

            #[cfg(not(feature = "no_std"))]
            Union::Variant(value) if value.is::<Instant>() => unimplemented!(),

            Union::Variant(value) if value.is::<i8>() => self.deserialize_i8(visitor),
            Union::Variant(value) if value.is::<i16>() => self.deserialize_i16(visitor),
            Union::Variant(value) if value.is::<i32>() => self.deserialize_i32(visitor),
            Union::Variant(value) if value.is::<i64>() => self.deserialize_i64(visitor),
            Union::Variant(value) if value.is::<u8>() => self.deserialize_u8(visitor),
            Union::Variant(value) if value.is::<u16>() => self.deserialize_u16(visitor),
            Union::Variant(value) if value.is::<u32>() => self.deserialize_u32(visitor),
            Union::Variant(value) if value.is::<u64>() => self.deserialize_u64(visitor),

            Union::Variant(_) => self.type_error_str("any"),
        }
    }

    fn deserialize_bool<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        visitor.visit_bool(
            self.value
                .as_bool()
                .or_else(|_| self.type_error::<bool, _>())?,
        )
    }

    fn deserialize_i8<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.value
            .downcast_ref::<i8>()
            .map_or_else(|| self.type_error::<i8, _>(), |&x| visitor.visit_i8(x))
    }

    fn deserialize_i16<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.value
            .downcast_ref::<i16>()
            .map_or_else(|| self.type_error::<i16, _>(), |&x| visitor.visit_i16(x))
    }

    fn deserialize_i32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.value
            .downcast_ref::<i32>()
            .map_or_else(|| self.type_error::<i32, _>(), |&x| visitor.visit_i32(x))
    }

    fn deserialize_i64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.value
            .downcast_ref::<i64>()
            .map_or_else(|| self.type_error::<i64, _>(), |&x| visitor.visit_i64(x))
    }

    fn deserialize_u8<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.value
            .downcast_ref::<u8>()
            .map_or_else(|| self.type_error::<u8, _>(), |&x| visitor.visit_u8(x))
    }

    fn deserialize_u16<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.value
            .downcast_ref::<u16>()
            .map_or_else(|| self.type_error::<u16, _>(), |&x| visitor.visit_u16(x))
    }

    fn deserialize_u32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.value
            .downcast_ref::<u32>()
            .map_or_else(|| self.type_error::<u32, _>(), |&x| visitor.visit_u32(x))
    }

    fn deserialize_u64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.value
            .downcast_ref::<u64>()
            .map_or_else(|| self.type_error::<u64, _>(), |&x| visitor.visit_u64(x))
    }

    fn deserialize_f32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        #[cfg(not(feature = "no_float"))]
        {
            self.value
                .downcast_ref::<f32>()
                .map_or_else(|| self.type_error::<f32, _>(), |&x| visitor.visit_f32(x))
        }
        #[cfg(feature = "no_float")]
        self.type_error_str("f32")
    }

    fn deserialize_f64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        #[cfg(not(feature = "no_float"))]
        {
            self.value
                .downcast_ref::<f64>()
                .map_or_else(|| self.type_error::<f64, _>(), |&x| visitor.visit_f64(x))
        }
        #[cfg(feature = "no_float")]
        self.type_error_str("f64")
    }

    fn deserialize_char<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.value
            .downcast_ref::<char>()
            .map_or_else(|| self.type_error::<char, _>(), |&x| visitor.visit_char(x))
    }

    fn deserialize_str<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.value.downcast_ref::<ImmutableString>().map_or_else(
            || self.type_error::<ImmutableString, _>(),
            |x| visitor.visit_borrowed_str(x.as_str()),
        )
    }

    fn deserialize_string<V: Visitor<'de>>(
        self,
        visitor: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error_str("bytes array")
    }

    fn deserialize_byte_buf<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error_str("bytes array")
    }

    fn deserialize_option<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error_str("bytes array")
    }

    fn deserialize_unit<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.value
            .downcast_ref::<()>()
            .map_or_else(|| self.type_error::<(), _>(), |_| visitor.visit_unit())
    }

    fn deserialize_unit_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        #[cfg(not(feature = "no_index"))]
        {
            self.value.downcast_ref::<Array>().map_or_else(
                || self.type_error::<Array, _>(),
                |arr| visitor.visit_seq(IterateArray::new(arr.iter())),
            )
        }
        #[cfg(feature = "no_index")]
        self.type_error_str("array")
    }

    fn deserialize_tuple<V: Visitor<'de>>(
        self,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Box<EvalAltResult>> {
        #[cfg(not(feature = "no_object"))]
        {
            self.value.downcast_ref::<Map>().map_or_else(
                || self.type_error::<Map, _>(),
                |map| visitor.visit_map(IterateMap::new(map.keys(), map.values())),
            )
        }
        #[cfg(feature = "no_object")]
        self.type_error_str("map")
    }

    fn deserialize_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        _: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error_str("num")
    }

    fn deserialize_identifier<V: Visitor<'de>>(
        self,
        visitor: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V: Visitor<'de>>(
        self,
        visitor: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        self.deserialize_any(visitor)
    }
}

struct IterateArray<'a, ITER: Iterator<Item = &'a Dynamic>> {
    iter: ITER,
}

impl<'a, ITER: Iterator<Item = &'a Dynamic>> IterateArray<'a, ITER> {
    pub fn new(iter: ITER) -> Self {
        Self { iter }
    }
}

impl<'a: 'de, 'de, ITER: Iterator<Item = &'a Dynamic>> SeqAccess<'de> for IterateArray<'a, ITER> {
    type Error = Box<EvalAltResult>;

    fn next_element_seed<T: DeserializeSeed<'de>>(
        &mut self,
        seed: T,
    ) -> Result<Option<T::Value>, Box<EvalAltResult>> {
        match self.iter.next() {
            None => Ok(None),
            Some(item) => seed
                .deserialize(&mut DynamicDeserializer::from_dynamic(item))
                .map(Some),
        }
    }
}

struct IterateMap<
    'a,
    KEYS: Iterator<Item = &'a ImmutableString>,
    VALUES: Iterator<Item = &'a Dynamic>,
> {
    keys: KEYS,
    values: VALUES,
}

impl<'a, KEYS: Iterator<Item = &'a ImmutableString>, VALUES: Iterator<Item = &'a Dynamic>>
    IterateMap<'a, KEYS, VALUES>
{
    pub fn new(keys: KEYS, values: VALUES) -> Self {
        Self { keys, values }
    }
}

impl<
        'a: 'de,
        'de,
        KEYS: Iterator<Item = &'a ImmutableString>,
        VALUES: Iterator<Item = &'a Dynamic>,
    > MapAccess<'de> for IterateMap<'a, KEYS, VALUES>
{
    type Error = Box<EvalAltResult>;

    fn next_key_seed<K: DeserializeSeed<'de>>(
        &mut self,
        seed: K,
    ) -> Result<Option<K::Value>, Box<EvalAltResult>> {
        match self.keys.next() {
            None => Ok(None),
            Some(item) => seed
                .deserialize(&mut ImmutableStringDeserializer::from_str(item))
                .map(Some),
        }
    }

    fn next_value_seed<V: DeserializeSeed<'de>>(
        &mut self,
        seed: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        seed.deserialize(&mut DynamicDeserializer::from_dynamic(
            self.values.next().unwrap(),
        ))
    }
}
