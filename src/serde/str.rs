use crate::result::EvalAltResult;
use crate::token::Position;
use crate::utils::ImmutableString;

use serde::de::{Deserializer, Visitor};

use crate::stdlib::any::type_name;

pub struct ImmutableStringDeserializer<'a> {
    value: &'a ImmutableString,
}

impl<'a> ImmutableStringDeserializer<'a> {
    pub fn from_str(value: &'a ImmutableString) -> Self {
        Self { value }
    }
    pub fn type_error<R, T>(&self) -> Result<T, Box<EvalAltResult>> {
        self.type_error_str(type_name::<R>())
    }
    pub fn type_error_str<T>(&self, name: &str) -> Result<T, Box<EvalAltResult>> {
        Err(Box::new(EvalAltResult::ErrorMismatchOutputType(
            name.into(),
            "string".into(),
            Position::none(),
        )))
    }
}

impl<'de> Deserializer<'de> for &mut ImmutableStringDeserializer<'de> {
    type Error = Box<EvalAltResult>;

    fn deserialize_any<V: Visitor<'de>>(self, v: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.deserialize_str(v)
    }
    fn deserialize_bool<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error::<bool, _>()
    }
    fn deserialize_i8<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error::<i8, _>()
    }
    fn deserialize_i16<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error::<i16, _>()
    }
    fn deserialize_i32<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error::<i32, _>()
    }
    fn deserialize_i64<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error::<i64, _>()
    }
    fn deserialize_u8<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error::<u8, _>()
    }
    fn deserialize_u16<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error::<u16, _>()
    }
    fn deserialize_u32<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error::<u32, _>()
    }
    fn deserialize_u64<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error::<u64, _>()
    }
    fn deserialize_f32<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error_str("f32")
    }
    fn deserialize_f64<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error_str("f64")
    }
    fn deserialize_char<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error::<char, _>()
    }
    fn deserialize_str<V: Visitor<'de>>(self, v: V) -> Result<V::Value, Box<EvalAltResult>> {
        v.visit_borrowed_str(self.value.as_str())
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
        self.type_error_str("option")
    }
    fn deserialize_unit<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error::<(), _>()
    }
    fn deserialize_unit_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        v: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        self.deserialize_unit(v)
    }
    fn deserialize_newtype_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        v: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        v.visit_newtype_struct(self)
    }
    fn deserialize_seq<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error_str("array")
    }
    fn deserialize_tuple<V: Visitor<'de>>(
        self,
        _len: usize,
        v: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        self.deserialize_seq(v)
    }
    fn deserialize_tuple_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _len: usize,
        v: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        self.deserialize_seq(v)
    }
    fn deserialize_map<V: Visitor<'de>>(self, _: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error_str("map")
    }
    fn deserialize_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        v: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        self.deserialize_map(v)
    }
    fn deserialize_enum<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        _: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        self.type_error_str("enum")
    }
    fn deserialize_identifier<V: Visitor<'de>>(self, v: V) -> Result<V::Value, Box<EvalAltResult>> {
        self.deserialize_str(v)
    }
    fn deserialize_ignored_any<V: Visitor<'de>>(
        self,
        v: V,
    ) -> Result<V::Value, Box<EvalAltResult>> {
        self.deserialize_any(v)
    }
}
