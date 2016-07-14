#[cfg(feature = "serde")]
extern crate serde;

#[cfg(not(feature = "serde"))]
extern crate rustc_serialize;
extern crate mustache;

use std::str;
use std::collections::HashMap;

#[cfg_attr(not(feature = "serde"), derive(RustcEncodable))]
struct User {
    name: String
}

#[cfg(feature = "serde")]
impl serde::ser::Serialize for User {
    fn serialize<S>(&self, serializer: &mut S) -> Result<(), S::Error>
    where S: serde::ser::Serializer {
        let mut map = HashMap::new();
        map.insert("name", &self.name);
        map.serialize(serializer)
    }
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

    let string = str::from_utf8(&bytes);
    assert_eq!(string, Ok("Hello Harry!\nHello Samantha!\n"));
    println!("{}", string.unwrap());
}
