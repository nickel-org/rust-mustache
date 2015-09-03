#[macro_use]
extern crate nickel;
extern crate rustc_serialize;

use nickel::*;

#[derive(RustcEncodable, RustcDecodable)]
struct Foo {
  name: String,
  lastname: Option<String>
}

fn main() {
  let mut server = Nickel::new();

  server.get("**", middleware! { |_, res|
    let foo = Foo { name: "foo".to_owned(), lastname: None };
    return res.render("index.html", &foo);
  });

  server.listen("0.0.0.0:8080");
}
