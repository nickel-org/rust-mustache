use std::fmt;
use std::error;

use super::{Data, StrVal};
use self::Error::*;

#[cfg(feature = "serde")]
mod serialization {
    mod serde;
    pub use serde::Serialize as Encodable;
}

#[cfg(not(feature = "serde"))]
mod serialization {
    mod rustc_serialize;
    pub use rustc_serialize::Encodable;
}

/// Shared reexport between serde and rustc_serialize.
///
/// It shares a name so that projects can refer to a single trait bound
/// while maintaining compatability with both serialization libraries.
pub use self::serialization::Encodable;

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

    /// This variant is currently only used by serde.
    Serialization(String),

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
            NestedOptions => "nested Option types are not supported",
            UnsupportedType => "unsupported type",
            MissingElements => "no elements in value",
            KeyIsNotString => "key is not a string",
            NoDataToEncode => "the encodable type created no data",
            MultipleRootsFound => {
                "the encodable type emitted data that was not tree-like in structure"
            }
            Serialization(ref e) => e,
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

#[cfg(feature = "serde")]
pub fn encode<T: Encodable>(data: &T) -> Result<Data, Error> {
    let mut encoder = Encoder::new();
    try!(data.serialize(&mut encoder));
    match encoder.data.len() {
        1 => Ok(encoder.data.pop().unwrap()),
        // These two errors are really down to either the type implementing Encodable wrong
        // or using some data which is incompatible with the way mustache *currently* works.
        // It does feel like `bug!` more than Error but maybe it covers some usecases so
        // lets not panic here.
        0 => Err(NoDataToEncode),
        _ => Err(MultipleRootsFound),
    }
}

#[cfg(not(feature = "serde"))]
pub fn encode<T: Encodable>(data: &T) -> Result<Data, Error> {
    let mut encoder = Encoder::new();
    try!(data.encode(&mut encoder));
    match encoder.data.len() {
        1 => Ok(encoder.data.pop().unwrap()),
        // These two errors are really down to either the type implementing Encodable wrong
        // or using some data which is incompatible with the way mustache *currently* works.
        // It does feel like `bug!` more than Error but maybe it covers some usecases so
        // lets not panic here.
        0 => Err(NoDataToEncode),
        _ => Err(MultipleRootsFound),
    }
}

#[cfg(test)]
mod tests {
    use super::{encode, Error};

    struct NoData;
    struct Multiroot;

    #[test]
    fn encodable_with_no_data() {
        assert_let!(Err(Error::NoDataToEncode) = encode(&NoData));
    }

    #[test]
    fn encodable_with_multiple_roots() {
        assert_let!(Err(Error::MultipleRootsFound) = encode(&Multiroot));
    }

    #[cfg(not(feature = "serde"))]
    mod serialization {
        use super::{NoData, Multiroot};
        use rustc_serialize::{Encodable, Encoder};

        impl Encodable for NoData {
            fn encode<S: Encoder>(&self, _: &mut S) -> Result<(), S::Error> {
                Ok(())
            }
        }

        impl Encodable for Multiroot {
            fn encode<S: Encoder>(&self, s: &mut S) -> Result<(), S::Error> {
                try!(s.emit_u32(4));
                s.emit_u32(1)
            }
        }
    }

    #[cfg(feature = "serde")]
    mod serialization {
        use super::{NoData, Multiroot};
        use serde::ser::{Serialize, Serializer};

        impl Serialize for NoData {
            fn serialize<S: Serializer>(&self, _: &mut S) -> Result<(), S::Error> {
                Ok(())
            }
        }

        impl Serialize for Multiroot {
            fn serialize<S: Serializer>(&self, s: &mut S) -> Result<(), S::Error> {
                try!(s.serialize_u32(4));
                s.serialize_u32(1)
            }
        }
    }
}
