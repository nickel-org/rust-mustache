use std::collections::HashMap;
use std::cell::RefCell;
use std::fmt;

// for bug!
use log::error;

pub enum Data {
    Null,
    String(String),
    Bool(bool),
    Vec(Vec<Data>),
    Map(HashMap<String, Data>),
    Fun(RefCell<Box<dyn FnMut(String) -> String + Send>>),
}

impl PartialEq for Data {
    #[inline]
    fn eq(&self, other: &Data) -> bool {
        match (self, other) {
            (&Data::Null, &Data::Null) => true,
            (&Data::String(ref v0), &Data::String(ref v1)) => v0 == v1,
            (&Data::Bool(ref v0), &Data::Bool(ref v1)) => v0 == v1,
            (&Data::Vec(ref v0), &Data::Vec(ref v1)) => v0 == v1,
            (&Data::Map(ref v0), &Data::Map(ref v1)) => v0 == v1,
            (&Data::Fun(_), &Data::Fun(_)) => {
                bug!("Cannot compare closures");
                false
            },
            (_, _) => false,
        }
    }
}

impl fmt::Debug for Data {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Data::Null => write!(f, "Null"),
            Data::String(ref v) => write!(f, "StrVal({})", v),
            Data::Bool(v) => write!(f, "Bool({:?})", v),
            Data::Vec(ref v) => write!(f, "VecVal({:?})", v),
            Data::Map(ref v) => write!(f, "Map({:?})", v),
            Data::Fun(_) => write!(f, "Fun(...)"),
        }
    }
}
