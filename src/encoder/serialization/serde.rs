use std::collections::HashMap;
use serde;
use {StrVal, Bool, VecVal, Map, OptVal};
use encoder::{Encoder, EncoderResult, Error};
use encoder::Error::*;

pub use serde::Serialize as Encodable;

impl serde::ser::Error for Error {
    fn custom<T: Into<String>>(msg: T) -> Self {
        Error::Serialization(msg.into())
    }
}

impl serde::Serializer for Encoder {
    type Error = Error;

    fn serialize_bool(&mut self, v: bool) -> EncoderResult {
        self.push_ok(Bool(v))
    }

    fn serialize_i64(&mut self, v: i64) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_u64(&mut self, v: u64) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_f64(&mut self, v: f64) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_str(&mut self, v: &str) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn serialize_unit(&mut self) -> EncoderResult {
        Err(UnsupportedType)
    }

    fn serialize_none(&mut self) -> EncoderResult {
        self.push_ok(OptVal(None))
    }

    fn serialize_some<V>(&mut self, value: V) -> EncoderResult
    where V: serde::ser::Serialize
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

    fn serialize_seq<V>(&mut self, mut visitor: V) -> EncoderResult
    where V: serde::ser::SeqVisitor {
        let len = visitor.len().unwrap_or(0);
        self.push(VecVal(Vec::with_capacity(len)));

        while let Some(()) = try!(visitor.visit(self)) {}

        Ok(())
    }

    fn serialize_seq_elt<T>(&mut self, value: T) -> EncoderResult
    where T: serde::ser::Serialize {
        let mut v = match self.data.pop() {
            Some(VecVal(v)) => v,
            _ => {
                return Err(UnsupportedType);
            }
        };
        try!(value.serialize(self));
        let data = match self.data.pop() {
            Some(d) => d,
            _ => {
                return Err(UnsupportedType);
            }
        };
        v.push(data);
        self.push_ok(VecVal(v))
    }

    fn serialize_map<V>(&mut self, mut visitor: V) -> EncoderResult
    where V: serde::ser::MapVisitor
    {
        let len = visitor.len().unwrap_or(0);

        self.push(Map(HashMap::with_capacity(len)));

        while let Some(()) = try!(visitor.visit(self)) {}

        Ok(())
    }

    fn serialize_map_elt<K, V>(&mut self, key: K, value: V) -> EncoderResult
    where K: serde::Serialize,
          V: serde::Serialize
    {
        try!(key.serialize(self));
        let key = match self.data.pop() {
            Some(StrVal(s)) => s,
            _ => {
                return Err(KeyIsNotString);
            }
        };

        let mut m = match self.data.pop() {
            Some(Map(m)) => m,
            _ => bug!("Expected a map"),
        };

        try!(value.serialize(self));

        let value = match self.data.pop() {
            Some(p) => p,
            None => bug!("Nothing to pop!"),
        };
        m.insert(key, value);
        self.push(Map(m));
        Ok(())
    }
}
