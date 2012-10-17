Mustache
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

    cargo install mustache

Use It
======

    extern mod std;
    use mustache;

    #[auto_serialize]
    struct Name { name: ~str }

    fn main() {
        let name = Name { name: ~"world" };
        let s = mustache::render_str("hello {{name}}!", ctx);
        io::println(s);
    }

[1]: http://code.google.com/p/google-ctemplate/
[2]: http://www.ivan.fomichev.name/2008/05/erlang-template-engine-prototype.html
[3]: http://defunkt.github.com/mustache/
[4]: http://mustache.github.com/mustache.5.html
