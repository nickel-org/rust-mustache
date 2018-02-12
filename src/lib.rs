#![cfg_attr(feature = "unstable", feature(external_doc))]
#![cfg_attr(feature = "unstable", doc(include = "../README.md"))]

extern crate log;
extern crate serde;

use std::str;
use std::path::{PathBuf, Path};
use std::result;

#[macro_use]
mod macros;

mod builder;
mod compiler;
mod context;
mod data;
mod encoder;
mod error;
mod parser;
mod template;

pub use builder::{MapBuilder, VecBuilder};
pub use context::Context;
pub use data::Data;
pub use encoder::Encoder;
pub use encoder::Error as EncoderError;
pub use encoder::{SerializeVec, SerializeTupleVariant, SerializeMap, SerializeStructVariant};
pub use error::{Error, Result};
pub use parser::Error as ParserError;
pub use template::Template;

pub fn to_data<T>(value: T) -> result::Result<Data, encoder::Error>
where
    T: serde::Serialize,
{
    value.serialize(Encoder)
}

/// Compiles a template from an `Iterator<char>`.
pub fn compile_iter<T: Iterator<Item = char>>(iter: T) -> Result<Template> {
    Context::new(PathBuf::from(".")).compile(iter)
}

/// Compiles a template from a path.
/// returns None if the file cannot be read OR the file is not UTF-8 encoded
pub fn compile_path<U: AsRef<Path>>(path: U) -> Result<Template> {
    let path = path.as_ref();

    match path.file_name() {
        Some(filename) => {
            let template_dir = path.parent().unwrap_or(Path::new("."));
            // FIXME: Should work with OsStrings, this will not use provided extension if
            // the extension is not utf8 :(
            let extension = path.extension().and_then(|ext| ext.to_str()).unwrap_or("mustache");

            let context = Context {
                template_path: template_dir.to_path_buf(),
                template_extension: extension.to_string(),
            };
            context.compile_path(filename)
        }
        None => Err(Error::NoFilename),
    }
}

/// Compiles a template from a string.
pub fn compile_str(template: &str) -> Result<Template> {
    compile_iter(template.chars())
}
