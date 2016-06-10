extern crate rustc_serialize;
extern crate mustache;

use std::str;
use std::collections::HashMap;

#[derive(RustcEncodable)]
struct User {
    name: String
}

fn main() {
    let template ="{{#users}}\
                   Hello {{name}}!
                   {{/users}}";

    let template = mustache::compile_str(template).expect("Failed to compile");

    let users = vec![
        User { name: "Harry".into() },
        User { name: "Samantha".into() }
    ];

    let mut data = HashMap::new();
    data.insert("users", users);

    let mut bytes = vec![];
    template.render(&mut bytes, &data).expect("Failed to render");

    assert_eq!(str::from_utf8(&bytes), Ok("Hello Harry!\nHello Samantha!\n"))
}
