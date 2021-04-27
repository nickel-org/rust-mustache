Mustache [![Ohloh statistics](http://www.ohloh.net/p/rust-mustache/widgets/project_thin_badge.gif)](https://www.ohloh.net/p/rust-mustache) [![Build Status](http://travis-ci.org/nickel-org/rust-mustache.png?branch=master)](https://travis-ci.org/nickel-org/rust-mustache) [![](http://meritbadge.herokuapp.com/mustache)](https://crates.io/crates/mustache)
========

Inspired by [ctemplate][1] and [et][2], [Mustache][3] is a framework-agnostic way
to render logic-free views.

As ctemplates says, "It emphasizes separating logic from presentation: it is
impossible to embed application logic in this template language."

rust-mustache is a rust implementation of Mustache.

## Documentation

The different Mustache tags are documented at [mustache(5)][4].

Documentation for this library is [here][5].

## Install

Install it through Cargo!

```toml
[dependencies]
mustache = "*"
```

# Basic example

```rust
#[macro_use]
extern crate serde_derive;
extern crate mustache;

use std::io;
use mustache::MapBuilder;

#[derive(Serialize)]
struct Planet {
    name: String,
}

fn main() {
    // First the string needs to be compiled.
    let template = mustache::compile_str("hello {{name}}").unwrap();

    // You can either use an encodable type to print "hello Mercury".
    let planet = Planet { name: "Mercury".into() };
    template.render(&mut io::stdout(), &planet).unwrap();
    println!("");

    // ... or you can use a builder to print "hello Venus".
    let data = MapBuilder::new()
        .insert_str("name", "Venus")
        .build();

    template.render_data(&mut io::stdout(), &data).unwrap();
    println!("");

    // ... you can even use closures.
    let mut planets = vec!("Jupiter", "Mars", "Earth");

    let data = MapBuilder::new()
        .insert_fn("name", move |_| {
            planets.pop().unwrap().into()
        })
        .build();

    // prints "hello Earth"
    template.render_data(&mut io::stdout(), &data).unwrap();
    println!("");

    // prints "hello Mars"
    template.render_data(&mut io::stdout(), &data).unwrap();
    println!("");

    // prints "hello Jupiter"
    template.render_data(&mut io::stdout(), &data).unwrap();
    println!("");
}
```

## Testing

Simply clone and run:

```bash
# If you want to run the test cases, you'll need the spec as well.
git submodule init
git submodule update

cargo test

# If you want to test the readme example, we're currently using the unstable feature to do so.
cargo +nightly test --features unstable
```

## Releasing

If cutting a new release, please follow something along the lines of the below:

```bash
# Assuming master is the current release commit, ideally it will be a commit
# announcing the release and the only change would be the version number.

# Ensure everything looks good
cargo publish --dry-run

# Actually publish
cargo publish

# Tag the release, prefix it with 'v' for easy tag searching, i.e. git tag --list 'v*'
git tag vX.Y.Z

git push --tags origin master
```

[1]: http://code.google.com/p/google-ctemplate/
[2]: http://www.ivan.fomichev.name/2008/05/erlang-template-engine-prototype.html
[3]: https://mustache.github.io/
[4]: https://mustache.github.io/mustache.5.html
[5]: https://docs.rs/mustache

# License

See LICENSE File
