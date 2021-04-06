use std::fmt;
use std::io::Error as StdIoError;
use std::result::Result as StdResult;
use std::error::Error as StdError;

use parser;
use encoder;

/// Error type for any error within this library.
///
/// This type is not intended to be matched exhaustively as new variants
/// may be added in future without a version bump.
#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    InvalidStr,
    NoFilename,
    IncompleteSection,
    Io(StdIoError),
    Parser(parser::Error),
    Encoder(encoder::Error),
}

pub type Result<T> = StdResult<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Error::InvalidStr => "invalid str".to_string(),
            Error::NoFilename => "a filename must be provided".to_string(),
            Error::IncompleteSection => "a section wasn't completed".to_string(), // Is there a better way to put this?
            Error::Io(ref err) => err.to_string(),
            Error::Parser(ref err) => err.to_string(),
            Error::Encoder(ref err) => err.to_string(),
        })
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

impl StdError for Error { }
