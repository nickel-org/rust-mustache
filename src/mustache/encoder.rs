extern crate serialize;
extern crate collections;

use std::str;
use std::vec_ng::Vec;
use collections::hashmap::HashMap;

/// Represents template data.
#[deriving(Clone)]
pub enum Data {
    Str(~str),
    Bool(bool),
    Vec(Vec<Data>),
    Map(HashMap<~str, Data>),
    //Fun(fn(~str) -> ~str),
}

pub struct Encoder {
    data: Vec<Data>,
}

impl Encoder {
    pub fn new() -> Encoder {
        Encoder { data: Vec::new() }
    }
}

impl serialize::Encoder for Encoder {
    fn emit_nil(&mut self) { fail!() }

    fn emit_uint(&mut self, v: uint) { self.emit_str(v.to_str()); }
    fn emit_u64(&mut self, v: u64)   { self.emit_str(v.to_str()); }
    fn emit_u32(&mut self, v: u32)   { self.emit_str(v.to_str()); }
    fn emit_u16(&mut self, v: u16)   { self.emit_str(v.to_str()); }
    fn emit_u8(&mut self, v: u8)     { self.emit_str(v.to_str()); }

    fn emit_int(&mut self, v: int) { self.emit_str(v.to_str()); }
    fn emit_i64(&mut self, v: i64) { self.emit_str(v.to_str()); }
    fn emit_i32(&mut self, v: i32) { self.emit_str(v.to_str()); }
    fn emit_i16(&mut self, v: i16) { self.emit_str(v.to_str()); }
    fn emit_i8(&mut self, v: i8)   { self.emit_str(v.to_str()); }

    fn emit_bool(&mut self, v: bool) { self.data.push(Bool(v)); }

    fn emit_f64(&mut self, v: f64) {
        self.emit_str(v.to_str());
    }

    fn emit_f32(&mut self, v: f32) {
        self.emit_str(v.to_str());
    }

    fn emit_char(&mut self, v: char) {
        self.emit_str(str::from_char(v));
    }

    fn emit_str(&mut self, v: &str) {
        // copying emit_owned_str
        self.data.push(Str(v.to_owned()));
    }

    fn emit_enum(&mut self, _name: &str, _f: |&mut Encoder|) {
        fail!()
    }

    fn emit_enum_variant(&mut self, _name: &str, _id: uint, _len: uint, _f: |&mut Encoder|) {
        fail!()
    }

    fn emit_enum_variant_arg(&mut self, _a_idx: uint, _f: |&mut Encoder|) {
        fail!()
    }

    fn emit_enum_struct_variant(&mut self,
                                _v_name: &str,
                                _v_id: uint,
                                _len: uint,
                                _f: |&mut Encoder|) {
        fail!()
    }

    fn emit_enum_struct_variant_field(&mut self,
                                      _f_name: &str,
                                      _f_idx: uint,
                                      _f: |&mut Encoder|) {
        fail!()
    }

    fn emit_struct(&mut self, _name: &str, _len: uint, f: |&mut Encoder|) {
        self.data.push(Map(HashMap::new()));
        f(self);
    }

    fn emit_struct_field(&mut self, name: &str, _idx: uint, f: |&mut Encoder|) {
        let mut m = match self.data.pop() {
            Some(Map(m)) => m,
            _ => fail!(),
        };
        f(self);
        let data = match self.data.pop() {
            Some(d) => d,
            _ => fail!(),
        };
        m.insert(name.to_owned(), data);
        self.data.push(Map(m));
    }

    fn emit_tuple(&mut self, len: uint, f: |&mut Encoder|) {
        self.emit_seq(len, f)
    }

    fn emit_tuple_arg(&mut self, idx: uint, f: |&mut Encoder|) {
        self.emit_seq_elt(idx, f)
    }

    fn emit_tuple_struct(&mut self, _name: &str, len: uint, f: |&mut Encoder|) {
        self.emit_seq(len, f)
    }

    fn emit_tuple_struct_arg(&mut self, idx: uint, f: |&mut Encoder|) {
        self.emit_seq_elt(idx, f)
    }

    // Specialized types:
    fn emit_option(&mut self, _f: |&mut Encoder|) {
        fail!()
    }

    fn emit_option_none(&mut self) {
        fail!()
    }

    fn emit_option_some(&mut self, _f: |&mut Encoder|) {
        fail!()
    }

    fn emit_seq(&mut self, _len: uint, f: |&mut Encoder|) {
        self.data.push(Vec(Vec::new()));
        f(self);
    }

    fn emit_seq_elt(&mut self, _idx: uint, f: |&mut Encoder|) {
        let mut v = match self.data.pop() {
            Some(Vec(v)) => v,
            _ => fail!(),
        };
        f(self);
        let data = match self.data.pop() {
            Some(d) => d,
            _ => fail!(),
        };
        v.push(data);
        self.data.push(Vec(v));
    }

    fn emit_map(&mut self, _len: uint, f: |&mut Encoder|) {
        self.data.push(Map(HashMap::new()));
        f(self);
    }

    fn emit_map_elt_key(&mut self, _idx: uint, f: |&mut Encoder|) {
        f(self);
        let last = match self.data.last() {
            Some(d) => d,
            None => fail!("error: No elements in self.data"),
        };
        match *last {
            Str(_) => {}
            _ => fail!("error: key is not a string"),
        }
    }

    fn emit_map_elt_val(&mut self, _idx: uint, f: |&mut Encoder|) {
        let k = match self.data.pop() {
            Some(Str(s)) => s,
            _ => fail!(),
        };
        let mut m = match self.data.pop() {
            Some(Map(m)) => m,
            _ => fail!(),
        };
        f(self);
        let popped = match self.data.pop() {
            Some(p) => p,
            None => fail!("Error: Nothing to pop!"),
        };
        m.insert(k, popped);
        self.data.push(Map(m));
    }
}
