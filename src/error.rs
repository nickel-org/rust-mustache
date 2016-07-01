use std::error::Error as StdError;
use std::io::Error as StdIoError;
use std::fmt;

use parser;
use encoder;

#[derive(Debug)]
pub enum Error {
    InvalidStr,
    NoFilename,
    Io(StdIoError),
    Parser(parser::Error),
    Encoder(encoder::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.description().fmt(f)
    }
}

impl StdError for Error {
    fn description(&self) -> &str {
        match *self {
            Error::InvalidStr => "invalid str",
            Error::NoFilename => "a filename must be provided",
            Error::Io(ref err) => err.description(),
            Error::Parser(ref err) => err.description(),
            Error::Encoder(ref err) => err.description(),
        }
    }
}

impl From<StdIoError> for Error {
    fn from(err: StdIoError) -> Error {
        Error::Io(err)
    }
}

impl From<parser::Error> for Error {
    fn from(err: parser::Error) -> Error {
        Error::Parser(err)
    }
}

impl From<encoder::Error> for Error {
    fn from(err: encoder::Error) -> Error {
        Error::Encoder(err)
    }
}
