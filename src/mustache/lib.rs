#![crate_id = "github.com/erickt/rust-mustache#mustache:0.3.0"]

#![license = "MIT/ASL2"]
#![crate_type = "dylib"]
#![crate_type = "rlib"]

#![feature(phase)]

extern crate collections;
extern crate serialize;

#[phase(syntax, link)]
extern crate log;

use std::fmt;
use std::io::File;
use std::str;
use collections::HashMap;

pub use builder::{MapBuilder, VecBuilder};
pub use encoder::{Encoder, EncoderResult};
pub use encoder::{Error, InvalidStr, IoError};
pub use template::Template;

pub mod builder;
pub mod encoder;

mod compiler;
mod parser;
mod template;

pub enum Data<'a> {
    Str(~str),
    Bool(bool),
    Vec(Vec<Data<'a>>),
    Map(HashMap<~str, Data<'a>>),
    Fun(|~str|: 'a -> ~str),
}

impl<'a> Eq for Data<'a> {
    #[inline]
    fn eq(&self, other: &Data<'a>) -> bool {
        match (self, other) {
            (&Str(ref v0), &Str(ref v1)) => v0 == v1,
            (&Bool(ref v0), &Bool(ref v1)) => v0 == v1,
            (&Vec(ref v0), &Vec(ref v1)) => v0 == v1,
            (&Map(ref v0), &Map(ref v1)) => v0 == v1,
            (&Fun(_), &Fun(_)) => fail!("cannot compare closures"),
            (_, _) => false,
        }
    }
}

impl<'a> fmt::Show for Data<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Str(ref v) => write!(f.buf, "Str({})", v),
            Bool(v) => write!(f.buf, "Bool({})", v),
            Vec(ref v) => write!(f.buf, "Vec({})", v),
            Map(ref v) => write!(f.buf, "Map({})", v),
            Fun(_) => write!(f.buf, "Fun(...)"),
        }
    }
}

/// Represents the shared metadata needed to compile and render a mustache
/// template.
#[deriving(Clone)]
pub struct Context {
    pub template_path: Path,
    pub template_extension: ~str,
}

impl Context {
    /// Configures a mustache context the specified path to the templates.
    pub fn new(path: Path) -> Context {
        Context {
            template_path: path,
            template_extension: ~"mustache",
        }
    }

    /// Compiles a template from a string
    pub fn compile<IT: Iterator<char>>(&self, reader: IT) -> Template {
        let compiler = compiler::Compiler::new(self.clone(), reader);
        let (tokens, partials) = compiler.compile();

        template::new(self.clone(), tokens, partials)
    }

    /// Compiles a template from a path.
    pub fn compile_path(&self, path: Path) -> Result<Template, Error> {
        // FIXME(#6164): This should use the file decoding tools when they are
        // written. For now we'll just read the file and treat it as UTF-8file.
        let mut path = self.template_path.join(path);
        path.set_extension(self.template_extension.clone());

        let s = match File::open(&path).read_to_end() {
            Ok(s) => s,
            Err(err) => { return Err(IoError(err)); }
        };

        // TODO: maybe allow UTF-16 as well?
        let template = match str::from_utf8(s.as_slice()) {
            Some(string) => string,
            None => { return Err(InvalidStr); }
        };

        Ok(self.compile(template.chars()))
    }
}

/// Compiles a template from an `Iterator<char>`.
pub fn compile_iter<T: Iterator<char>>(iter: T) -> Template {
    Context::new(Path::new(".")).compile(iter)
}

/// Compiles a template from a path.
/// returns None if the file cannot be read OR the file is not UTF-8 encoded
pub fn compile_path(path: Path) -> Result<Template, Error> {
    Context::new(Path::new(".")).compile_path(path)
}

/// Compiles a template from a string.
pub fn compile_str(template: &str) -> Template {
    compile_iter(template.chars())
}
