Mustache [![Ohloh statistics](http://www.ohloh.net/p/rust-mustache/widgets/project_thin_badge.gif)](https://www.ohloh.net/p/rust-mustache)
========

[![Build Status](http://travis-ci.org/erickt/rust-mustache.png?branch=master)](https://travis-ci.org/erickt/rust-mustache)
========

Inspired by [ctemplate][1] and [et][2], [Mustache][3] is a framework-agnostic way
to render logic-free views.

As ctemplates says, "It emphasizes separating logic from presentation: it is
impossible to embed application logic in this template language."

rust-mustache is a rust implementation of Mustache.

Documentation
=============

The different Mustache tags are documented at [mustache(5)][4].

Install It
==========

```ignore
    git clone https://github.com/erickt/rust-mustache.git
    cd rust-mustache
    make 
```

If you want to run the test cases, you'll need the spec as well.

```ignore
    git submodule init
    git submodule update
    make test
```

Use It
======

```rust
    extern crate mustache;
    extern crate serialize;

    use std::io;
    use mustache::MapBuilder;

    #[deriving(Encodable)]
    struct Planet {
        name: ~str,
    }

    fn main() {
        // First the string needs to be compiled.
        let template = mustache::compile_str("hello {{name}}");

        // You can either use an encodable type to print "hello Mercury".
        let planet = Planet { name: ~"Mercury" };
        template.render(&mut io::stdout(), &planet).unwrap();
        println!("");

        // ... or you can use a builder to print "hello Venus".
        let data = MapBuilder::new()
            .insert_str(~"name", ~"Venus")
            .build();

        template.render_data(&mut io::stdout(), &data);
        println!("");

        // ... you can even use closures.
        let mut planets = vec!(~"Jupiter", ~"Mars", ~"Earth");

        let data = MapBuilder::new()
            .insert_fn(~"name", |_| {
                planets.pop().unwrap()
            })
            .build();

        // prints "hello Earth"
        template.render_data(&mut io::stdout(), &data);
        println!("");

        // prints "hello Mars"
        template.render_data(&mut io::stdout(), &data);
        println!("");

        // prints "hello Jupiter"
        template.render_data(&mut io::stdout(), &data);
        println!("");
    }
```

[1]: http://code.google.com/p/google-ctemplate/
[2]: http://www.ivan.fomichev.name/2008/05/erlang-template-engine-prototype.html
[3]: http://defunkt.github.com/mustache/
[4]: http://mustache.github.com/mustache.5.html
