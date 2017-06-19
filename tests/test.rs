extern crate mustache;
extern crate rustc_serialize;
extern crate tempdir;

#[macro_use]
mod macros;

mod builder;
mod encoder;
mod template;

use mustache::{Data, EncoderError};
use rustc_serialize::Encodable;

fn encode<T: Encodable>(data: &T) -> Result<Data, EncoderError> {
    let mut encoder = mustache::Encoder::new();
    data.encode(&mut encoder)?;
    encoder.finish()
}
