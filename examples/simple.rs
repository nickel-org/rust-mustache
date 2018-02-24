extern crate mustache;

use std::str;
use std::collections::HashMap;

fn main() {
    let template = mustache::compile_str("This is a {{type}} example!").expect("Failed to compile");

    let mut data = HashMap::new();
    data.insert("type", "simple");

    let mut bytes = vec![];

    template
        .render(&mut bytes, &data)
        .expect("Failed to render");

    let message = str::from_utf8(&bytes).unwrap();
    println!("{}", message);
}
