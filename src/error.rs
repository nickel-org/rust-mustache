use std::error::Error as StdError;
use std::fmt;
use std::io::Error as StdIoError;
use std::result;

use parser;
use encoder;

/// Error type for any error within this library.
///
/// This type is not intended to be matched exhaustively as new variants
/// may be added in future without a version bump.
#[derive(Debug)]
pub enum Error {
    InvalidStr,
    NoFilename,
    Io(StdIoError),
    Parser(parser::Error),
    Encoder(encoder::Error),

    #[doc(hidden)]
    __Nonexhaustive,
}

pub type Result<T> = result::Result<T, Error>;

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
            Error::__Nonexhaustive => unreachable!(),
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
