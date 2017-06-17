use std::collections::HashMap;
use serde;
use {Data, StrVal, Bool, VecVal, Map, OptVal};
use encoder::{Encoder, EncoderResult, Error};
use encoder::Error::*;

use serde::Serialize;

pub use serde::Serialize as Encodable;

impl serde::ser::Error for Error {
    fn custom<T: Into<String>>(msg: T) -> Self {
        Error::Serialization(msg.into())
    }
}

impl serde::Serializer for Encoder {
    type Error = Error;

    // FIXME: Serialization could likely be made more efficient by taking advantage of
    // better associated-types here.

    // Sequence-like
    type SeqState = Vec<Data>;
    type TupleState = Self::SeqState;
    type TupleStructState = Self::SeqState;
    type TupleVariantState = Self::SeqState;

    // Map-like
    type MapState = HashMap<String, Data>;
    type StructState = Self::MapState;
    type StructVariantState = Self::MapState;

    fn serialize_bool(&mut self, v: bool) -> EncoderResult {
        self.push_ok(Bool(v))
    }

    fn serialize_char(&mut self, v: char) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_u8(&mut self, v: u8) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_i8(&mut self, v: i8) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_u16(&mut self, v: u16) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_i16(&mut self, v: i16) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_u32(&mut self, v: u32) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_i32(&mut self, v: i32) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_i64(&mut self, v: i64) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_u64(&mut self, v: u64) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_usize(&mut self, v: usize) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_isize(&mut self, v: isize) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_f32(&mut self, v: f32) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_f64(&mut self, v: f64) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_str(&mut self, v: &str) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_unit_struct(&mut self, _name: &'static str) -> Result<(), Error> {
        // FIXME: Perhaps this could be relaxed to just 'do nothing'
        Err(UnsupportedType)
    }

    fn serialize_unit_variant(&mut self,
                               _name: &'static str,
                               _variant_index: usize,
                               _variant: &'static str,
                               ) -> Result<(), Self::Error>
    {
        // FIXME: Perhaps this could be relaxed to just 'do nothing'
        Err(UnsupportedType)
    }

    fn serialize_unit(&mut self) -> EncoderResult {
        Err(UnsupportedType)
    }

    fn serialize_none(&mut self) -> EncoderResult {
        self.push_ok(OptVal(None))
    }

    fn serialize_some<V>(&mut self, value: V) -> EncoderResult
    where V: Serialize
    {
        try!(value.serialize(self));

        let val = match self.data.pop() {
            Some(OptVal(_)) => return Err(NestedOptions),
            Some(d) => d,
            _ => {
                return Err(UnsupportedType);
            }
        };
        self.push_ok(OptVal(Some(Box::new(val))))
    }

    fn serialize_struct(&mut self,
                        _name: &'static str,
                        len: usize) -> Result<Self::StructState, Error> {
        self.serialize_map(Some(len))
    }

    fn serialize_struct_elt<V>(&mut self,
                               state: &mut Self::StructState,
                               key: &'static str,
                               value: V) -> Result<(), Error>
    where V: Serialize {
        try!(self.serialize_map_key(state, key));
        self.serialize_map_value(state, value)
    }

    fn serialize_struct_end(&mut self, state: Self::StructState) -> Result<(), Error> {
        self.serialize_map_end(state)
    }

    fn serialize_struct_variant(&mut self,
                                _name: &'static str,
                                _id: usize,
                                _variant: &'static str,
                                len: usize) -> Result<Self::StructVariantState, Error> {
        self.serialize_map(Some(len))
    }

    fn serialize_struct_variant_elt<V>(&mut self,
                                       state: &mut Self::StructVariantState,
                                       key: &'static str, value: V) -> Result<(), Error>
    where V: Serialize {
        try!(self.serialize_map_key(state, key));
        self.serialize_map_value(state, value)
    }

    fn serialize_struct_variant_end(&mut self,
                                    state: Self::StructVariantState) -> Result<(), Error> {
        self.serialize_map_end(state)
    }

    fn serialize_newtype_struct<T: Serialize>(&mut self,
                                          _name: &'static str,
                                          value: T)
                                          -> Result<(), Self::Error> {
        // Ignore newtype name
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: Serialize>(&mut self,
                                           _name: &'static str,
                                           _variant_index: usize,
                                           _variant: &'static str,
                                           value: T)
                                           -> Result<(), Self::Error> {
        // Ignore newtype name
        value.serialize(self)
    }

    fn serialize_bytes(&mut self, v: &[u8]) -> Result<(), Error> {
        let mut state = try!(self.serialize_seq(Some(v.len())));
        for c in v {
            try!(self.serialize_seq_elt(&mut state, c));
        }
        self.serialize_seq_end(state)
    }

    fn serialize_seq_fixed_size(&mut self, len: usize) -> Result<Self::SeqState, Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_seq(&mut self, len: Option<usize>) -> Result<Self::SeqState, Error> {
        Ok(Vec::with_capacity(len.unwrap_or(0)))
    }

    fn serialize_seq_elt<T>(&mut self, state: &mut Self::SeqState, value: T) -> EncoderResult
    where T: Serialize {
        try!(value.serialize(self));
        let data = match self.data.pop() {
            Some(d) => d,
            _ => {
                return Err(UnsupportedType);
            }
        };

        state.push(data);
        Ok(())
    }

    fn serialize_seq_end(&mut self, state: Self::SeqState) -> Result<(), Error> {
        self.push_ok(VecVal(state))
    }

    fn serialize_tuple(&mut self, len: usize) -> Result<Self::SeqState, Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_elt<T>(&mut self, state: &mut Self::SeqState, value: T) -> Result<(), Error>
    where T: Serialize {
        self.serialize_seq_elt(state, value)
    }

    fn serialize_tuple_end(&mut self, state: Self::SeqState) -> Result<(), Error> {
        self.serialize_seq_end(state)
    }

    fn serialize_tuple_struct(&mut self,
                              _name: &'static str,
                              len: usize) -> Result<Self::SeqState, Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct_elt<T>(&mut self,
                                     state: &mut Self::SeqState,
                                     value: T) -> Result<(), Error>
    where T: Serialize {
        self.serialize_seq_elt(state, value)
    }

    fn serialize_tuple_struct_end(&mut self, state: Self::SeqState) -> Result<(), Error> {
        self.serialize_seq_end(state)
    }
    fn serialize_tuple_variant(&mut self,
                               _name: &'static str,
                               _id: usize,
                               _variant: &'static str,
                               len: usize) -> Result<Self::SeqState, Error> {
        self.serialize_seq(Some(len))
    }
    fn serialize_tuple_variant_elt<T>(&mut self,
                                      state: &mut Self::SeqState,
                                      value: T) -> Result<(), Error>
    where T: serde::ser::Serialize {
        self.serialize_seq_elt(state, value)
    }
    fn serialize_tuple_variant_end(&mut self, state: Self::SeqState) -> Result<(), Error> {
        self.serialize_seq_end(state)
    }

    fn serialize_map(&mut self,
                 len: Option<usize>)
                 -> Result<Self::MapState, Self::Error> {
        Ok(HashMap::with_capacity(len.unwrap_or(0)))
    }

    fn serialize_map_key<T: Serialize>(&mut self,
                                   _state: &mut Self::MapState,
                                   key: T)
                                   -> Result<(), Self::Error> {
        key.serialize(self)
    }

    fn serialize_map_value<T: Serialize>(&mut self,
                                        state: &mut Self::MapState,
                                        value: T)
                                        -> Result<(), Self::Error> {
        // FIXME: With the new serde API (0.8), `serialize_map_key` and `serialize_map_value` may
        // have other method calls interleaved between them which will fail here or else give wrong
        // results.
        let key = match self.data.pop() {
            Some(StrVal(s)) => s,
            _ => {
                return Err(KeyIsNotString);
            }
        };

        try!(value.serialize(self));
        let value = match self.data.pop() {
            Some(p) => p,
            None => bug!("Nothing to pop!"),
        };

        state.insert(key, value);

        Ok(())
    }

    fn serialize_map_end(&mut self,
                        state: Self::MapState)
                        -> Result<(), Self::Error> {
        self.push_ok(Map(state))
    }
}
