use template::{self, Template};
use compiler;
use {Result, Error};

use std::fmt;
use std::fs::File;
use std::str;
use std::io::Read;
use std::path::{Path, PathBuf};

/// Represents the shared metadata needed to compile and render a mustache
/// template.
#[derive(Clone)]
pub struct Context {
    pub template_path: PathBuf,
    pub template_extension: String,
}

impl fmt::Debug for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "Context {{ template_path: {:?}, template_extension: {} }}",
               &*self.template_path,
               self.template_extension)
    }
}

impl Context {
    /// Configures a mustache context the specified path to the templates.
    pub fn new(path: PathBuf) -> Context {
        Context {
            template_path: path,
            template_extension: "mustache".to_string(),
        }
    }

    /// Compiles a template from a string
    pub fn compile<IT: Iterator<Item = char>>(&self, reader: IT) -> Result<Template> {
        let compiler = compiler::Compiler::new(self.clone(), reader);
        let (tokens, partials) = try!(compiler.compile());

        Ok(template::new(self.clone(), tokens, partials))
    }

    /// Compiles a template from a path.
    pub fn compile_path<U: AsRef<Path>>(&self, path: U) -> Result<Template> {
        // FIXME(#6164): This should use the file decoding tools when they are
        // written. For now we'll just read the file and treat it as UTF-8file.
        let mut path = self.template_path.join(path.as_ref());
        path.set_extension(&self.template_extension);
        let mut s = vec![];
        let mut file = try!(File::open(&path));
        try!(file.read_to_end(&mut s));

        // TODO: maybe allow UTF-16 as well?
        let template = match str::from_utf8(&*s) {
            Ok(string) => string,
            _ => {
                return Err(Error::InvalidStr);
            }
        };

        self.compile(template.chars())
    }
}