use super::encode;

use mustache::EncoderError;
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
    assert_let!(Err(EncoderError::NoDataToEncode) = encode(&NoData));
}

#[test]
fn encodable_with_multiple_roots() {
    assert_let!(Err(EncoderError::MultipleRootsFound) = encode(&Multiroot));
}
