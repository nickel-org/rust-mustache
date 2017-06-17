use std::collections::HashMap;
use std::fmt;
use std::error;
use rustc_serialize;

use super::{Data, StrVal, Bool, VecVal, Map, OptVal};

#[derive(Default)]
pub struct Encoder {
     data: Vec<Data>,
}

impl Encoder {
    pub fn new() -> Encoder {
        Encoder::default()
    }

    pub fn stack(&self) -> &[Data] {
        &self.data
    }
}

/// Error type to represent encoding failure.
///
/// This type is not intended to be matched exhaustively as new variants
/// may be added in future without a version bump.
#[derive(Debug)]
pub enum Error {
    NestedOptions,
    UnsupportedType,
    MissingElements,
    KeyIsNotString,
    NoDataToEncode,
    MultipleRootsFound,

    #[doc(hidden)]
    __Nonexhaustive,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::error::Error;
        self.description().fmt(f)
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::NestedOptions => "nested Option types are not supported",
            Error::UnsupportedType => "unsupported type",
            Error::MissingElements => "no elements in value",
            Error::KeyIsNotString => "key is not a string",
            Error::NoDataToEncode => "the encodable type created no data",
            Error::MultipleRootsFound => {
                "the encodable type emitted data that was not tree-like in structure"
            }
            Error::__Nonexhaustive => unreachable!(),
        }
    }
}

pub type EncoderResult = Result<(), Error>;

impl Encoder {
    fn push(&mut self, data: Data) {
        self.data.push(data);
    }

    fn push_ok(&mut self, data: Data) -> EncoderResult {
        self.push(data);
        Ok(())
    }

    fn push_str_ok<T: ToString>(&mut self, data: T) -> EncoderResult {
        self.push(StrVal(data.to_string()));
        Ok(())
    }
}

impl rustc_serialize::Encoder for Encoder {
    type Error = Error;

    fn emit_nil(&mut self) -> EncoderResult {
        Err(Error::UnsupportedType)
    }

    fn emit_isize(&mut self, v: isize) -> EncoderResult {
        self.push_str_ok(v)
    }
    fn emit_usize(&mut self, v: usize) -> EncoderResult {
        self.push_str_ok(v)
    }
    fn emit_u64(&mut self, v: u64) -> EncoderResult {
        self.push_str_ok(v)
    }
    fn emit_u32(&mut self, v: u32) -> EncoderResult {
        self.push_str_ok(v)
    }
    fn emit_u16(&mut self, v: u16) -> EncoderResult {
        self.push_str_ok(v)
    }
    fn emit_u8(&mut self, v: u8) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn emit_i64(&mut self, v: i64) -> EncoderResult {
        self.push_str_ok(v)
    }
    fn emit_i32(&mut self, v: i32) -> EncoderResult {
        self.push_str_ok(v)
    }
    fn emit_i16(&mut self, v: i16) -> EncoderResult {
        self.push_str_ok(v)
    }
    fn emit_i8(&mut self, v: i8) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn emit_bool(&mut self, v: bool) -> EncoderResult {
        self.push_ok(Bool(v))
    }

    fn emit_f64(&mut self, v: f64) -> EncoderResult {
        self.push_str_ok(v)
    }
    fn emit_f32(&mut self, v: f32) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn emit_char(&mut self, v: char) -> EncoderResult {
        self.push_str_ok(v)
    }
    fn emit_str(&mut self, v: &str) -> EncoderResult {
        self.push_str_ok(v)
    }

    fn emit_enum<F>(&mut self, _name: &str, _f: F) -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        Err(Error::UnsupportedType)
    }

    fn emit_enum_variant<F>(&mut self, _name: &str, _id: usize, _len: usize, _f: F) -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        Err(Error::UnsupportedType)
    }

    fn emit_enum_variant_arg<F>(&mut self, _a_idx: usize, _f: F) -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        Err(Error::UnsupportedType)
    }

    fn emit_enum_struct_variant<F>(&mut self,
                                   _v_name: &str,
                                   _v_id: usize,
                                   _len: usize,
                                   _f: F)
                                   -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        Err(Error::UnsupportedType)
    }

    fn emit_enum_struct_variant_field<F>(&mut self,
                                         _f_name: &str,
                                         _f_idx: usize,
                                         _f: F)
                                         -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        Err(Error::UnsupportedType)
    }

    fn emit_struct<F>(&mut self, _name: &str, _len: usize, f: F) -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        self.push(Map(HashMap::new()));
        f(self)
    }

    fn emit_struct_field<F>(&mut self, name: &str, _idx: usize, f: F) -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        let mut m = match self.data.pop() {
            Some(Map(m)) => m,
            _ => {
                return Err(Error::UnsupportedType);
            }
        };
        try!(f(self));
        let data = match self.data.pop() {
            Some(d) => d,
            _ => {
                return Err(Error::UnsupportedType);
            }
        };
        m.insert(name.to_string(), data);
        self.push_ok(Map(m))
    }

    fn emit_tuple<F>(&mut self, len: usize, f: F) -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        self.emit_seq(len, f)
    }

    fn emit_tuple_arg<F>(&mut self, idx: usize, f: F) -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        self.emit_seq_elt(idx, f)
    }

    fn emit_tuple_struct<F>(&mut self, _name: &str, len: usize, f: F) -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        self.emit_seq(len, f)
    }

    fn emit_tuple_struct_arg<F>(&mut self, idx: usize, f: F) -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        self.emit_seq_elt(idx, f)
    }

    // Specialized types:
    fn emit_option<F>(&mut self, f: F) -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        f(self)
    }

    fn emit_option_none(&mut self) -> EncoderResult {
        self.push_ok(OptVal(None))
    }

    fn emit_option_some<F>(&mut self, f: F) -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        try!(f(self));
        let val = match self.data.pop() {
            Some(OptVal(_)) => return Err(Error::NestedOptions),
            Some(d) => d,
            _ => {
                return Err(Error::UnsupportedType);
            }
        };
        self.push_ok(OptVal(Some(Box::new(val))))
    }

    fn emit_seq<F>(&mut self, _len: usize, f: F) -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        self.push(VecVal(Vec::new()));
        f(self)
    }

    fn emit_seq_elt<F>(&mut self, _idx: usize, f: F) -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        let mut v = match self.data.pop() {
            Some(VecVal(v)) => v,
            _ => {
                return Err(Error::UnsupportedType);
            }
        };
        try!(f(self));
        let data = match self.data.pop() {
            Some(d) => d,
            _ => {
                return Err(Error::UnsupportedType);
            }
        };
        v.push(data);
        self.push_ok(VecVal(v))
    }

    fn emit_map<F>(&mut self, _len: usize, f: F) -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        self.push(Map(HashMap::new()));
        f(self)
    }

    fn emit_map_elt_key<F>(&mut self, _idx: usize, f: F) -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        try!(f(self));
        let last = match self.data.last() {
            Some(d) => d,
            None => {
                return Err(Error::MissingElements);
            }
        };
        match *last {
            StrVal(_) => Ok(()),
            _ => Err(Error::KeyIsNotString),
        }
    }

    fn emit_map_elt_val<F>(&mut self, _idx: usize, f: F) -> EncoderResult
    where F: FnOnce(&mut Encoder) -> EncoderResult
    {
        let k = match self.data.pop() {
            Some(StrVal(s)) => s,
            _ => {
                return Err(Error::KeyIsNotString);
            }
        };
        let mut m = match self.data.pop() {
            Some(Map(m)) => m,
            _ => bug!("Expected a map"),
        };
        try!(f(self));
        let popped = match self.data.pop() {
            Some(p) => p,
            None => bug!("Nothing to pop!"),
        };
        m.insert(k, popped);
        self.push(Map(m));
        Ok(())
    }
}

pub fn encode<T: rustc_serialize::Encodable>(data: &T) -> Result<Data, Error> {
    let mut encoder = Encoder::new();
    try!(data.encode(&mut encoder));
    match encoder.data.len() {
        1 => Ok(encoder.data.pop().unwrap()),
        // These two errors are really down to either the type implementing Encodable wrong
        // or using some data which is incompatible with the way mustache *currently* works.
        // It does feel like `bug!` more than Error but maybe it covers some usecases so
        // lets not panic here.
        0 => Err(Error::NoDataToEncode),
        _ => Err(Error::MultipleRootsFound),
    }
}

#[cfg(test)]
mod tests {
    use super::{encode, Error};
    use rustc_serialize::{Encodable, Encoder};

    struct NoData;

    impl Encodable for NoData {
        fn encode<S: Encoder>(&self, _: &mut S) -> Result<(), S::Error> {
            Ok(())
        }
    }

    struct Multiroot;

    impl Encodable for Multiroot {
        fn encode<S: Encoder>(&self, s: &mut S) -> Result<(), S::Error> {
            try!(s.emit_u32(4));
            s.emit_u32(1)
        }
    }

    #[test]
    fn encodable_with_no_data() {
        assert_let!(Err(Error::NoDataToEncode) = encode(&NoData));
    }

    #[test]
    fn encodable_with_multiple_roots() {
        assert_let!(Err(Error::MultipleRootsFound) = encode(&Multiroot));
    }
}
