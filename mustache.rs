use std;

import result::{ok, err};
import std::io;
import std::map::{map, new_str_hash};

export token;
export compile;

enum token {
    text(str),
    etag(str),
    utag(str),
    section({ name: str, inverted: bool, children: [token] }),
    incomplete_section(str, bool),
    partial(str),
}

enum data {
    str(str),
    bool(bool),
    vec([context]),
    fun(fn@(str) -> str),
}

type context = map<str, data>;

mod parser {
    enum state { TEXT, OTAG, TAG, CTAG }

    type t = {
        mutable state: state,
        mutable otag: str,
        mutable ctag: str,
        mutable content: str,
        mutable tag_position: uint,
        mutable tokens: [token],
    };
}

fn compile(src: str) -> [token] {
    let parser : parser::t = {
        mutable state: parser::TEXT,
        mutable otag: "{{",
        mutable ctag: "}}",
        mutable content: "",
        mutable tag_position: 0u,
        mutable tokens: [],
    };

    let curly_brace_tag = false;

    str::bytes_iter(src) { |c|
        alt parser.state {
          parser::TEXT {
            if c == parser.otag[0] {
                if str::len_bytes(parser.otag) > 1u {
                    parser.tag_position = 1u;
                    parser.state = parser::OTAG;
                } else {
                    add_text(parser);
                    parser.state = parser::TAG;
                }
            } else {
                unsafe { str::unsafe::push_byte(parser.content, c) };
            }
          }
          parser::OTAG {
            if c == parser.otag[parser.tag_position] {
                if parser.tag_position == str::len_bytes(parser.otag) - 1u {
                    add_text(parser);
                    curly_brace_tag = false;
                    parser.state = parser::TAG;
                } else {
                    parser.tag_position += 1u;
                }
            } else {
                // We don't have a tag, so add all the tag parts we've seen
                // so far to the string.
                parser.state = parser::TEXT;
                not_otag(parser);
                unsafe { str::unsafe::push_byte(parser.content, c) };
            }
          }
          parser::TAG {
              if parser.content == "" && c == '{' as u8 {
                  curly_brace_tag = true;
                  unsafe { str::unsafe::push_byte(parser.content, c) };
              } else if curly_brace_tag && c == '}' as u8 {
                  curly_brace_tag = false;
                  unsafe { str::unsafe::push_byte(parser.content, c) };
              } else if c == parser.ctag[0u] {
                  if str::len_bytes(parser.ctag) > 1u {
                      parser.tag_position = 1u;
                      parser.state = parser::CTAG;
                  } else {
                      add_tag(parser);
                      parser.state = parser::TEXT;
                  }
              } else {
                  unsafe { str::unsafe::push_byte(parser.content, c) };
              }
          }
          parser::CTAG {
              if c == parser.ctag[parser.tag_position] {
                  if parser.tag_position == str::len_bytes(parser.ctag) - 1u {
                      add_tag(parser);
                      parser.state = parser::TEXT;
                  } else {
                      parser.state = parser::TAG;
                      not_ctag(parser);
                      unsafe { str::unsafe::push_byte(parser.content, c) };
                  }
              }
          }
      }
    };

    alt parser.state {
      parser::TEXT { add_text(parser) }
      parser::OTAG { not_otag(parser); add_text(parser) }
      parser::TAG { fail "unclosed tag" }
      parser::CTAG { not_ctag(parser); add_text(parser) }
    }

    // Check that we don't have any incomplete sections.
    vec::iter(copy parser.tokens) { |token|
        alt token {
          incomplete_section(name, _) {
              fail #fmt("Unclosed mustache section %s", name);
          }
          _ {}
        }
    };

    parser.tokens
}

fn add_text(parser: parser::t) {
    if parser.content != "" {
        vec::push(parser.tokens, text(parser.content));
        parser.content = "";
    }
}

fn add_tag(parser: parser::t) {
    let content = parser.content;
    let content_len = str::len_bytes(content);

    alt content[0] as char {
      '!' {} // ignore comments
      '&' {
          let name = check_content(str::slice(content, 1u, content_len));
          vec::push(parser.tokens, utag(name)); }
      '{' {
        if str::ends_with(content, "}") {
            let name = check_content(str::slice(content, 1u, content_len - 1u));
            vec::push(parser.tokens, utag(name));
        } else {
            log(error, parser.content);
            log(error, content);
            fail "unbalanced \"{\" in tag";
        }
      }
      '#' {
          let name = check_content(str::slice(content, 1u, content_len));
          vec::push(parser.tokens, incomplete_section(name, false));
      }
      '^' {
          let name = check_content(str::slice(content, 1u, content_len));
          vec::push(parser.tokens, incomplete_section(name, true));
      }
      '/' {
          let name = check_content(str::slice(content, 1u, content_len));
          let children = [];

          while true {
              if vec::len(parser.tokens) == 0u {
                  fail "closing unopened section";
              }

              let last = vec::pop(parser.tokens);

              alt last {
                incomplete_section(section_name, inverted) {
                    if section_name == name {
                        vec::push(parser.tokens,
                            section({
                                name: name,
                                inverted: inverted,
                                children: vec::reversed(children),
                            }));
                        break;
                    } else {
                        fail "Unclosed section";
                    }
                }
                _ { vec::push(children, last); }
              }
          }
      }
      '>' {
          let name = check_content(str::slice(content, 1u, content_len));
          vec::push(parser.tokens, partial(name));
      }
      '=' {
          if (content_len > 2u && str::ends_with(content, "=")) {
              let s = check_content(str::slice(content, 1u, content_len - 1u));
              let tags = str::splitn_char(s, ' ', 2u);

              if vec::len(tags) == 2u {
                  parser.otag = tags[0];
                  parser.ctag = tags[1];
              } else {
                  fail "invalid change delimiter tag content";
              }
          } else {
              fail "invalid change delimiter tag content";
          }
      }
      _ {
          vec::push(parser.tokens, etag(check_content(content)));
      }
    }

    parser.content = "";
}

fn not_otag(parser: parser::t) {
    let i = 0u;
    while i < parser.tag_position {
        unsafe { str::unsafe::push_byte(parser.content, parser.otag[i]) };
        i += 1u;
    }
}

fn not_ctag(parser: parser::t) {
    let i = 0u;
    while i < parser.tag_position {
        unsafe { str::unsafe::push_byte(parser.content, parser.ctag[i]) };
        i += 1u;
    }
}

fn check_content(content: str) -> str {
    let trimmed = str::trim(content);
    if str::len_bytes(trimmed) == 0u {
        fail "empty tag";
    }
    trimmed
}

fn from_str(template: str, context: context) -> str {
    render(compile(template), context)
}

fn from_file(path: str, context: context) -> str {
    alt io::read_whole_file_str(path) {
      ok(template) { from_str(template, context) }
      err(e) { fail }
    }
}

fn render(tokens: [token], context: context) -> str {
    let output = vec::map(tokens) { |token|
        alt token {
          text(s) { s }
          etag(name) {
            alt context.find(name) {
              none { "" }
              some(tag) { render_etag(tag) }
            }
          }
          utag(name) {
            alt context.find(name) {
              none { "" }
              some(tag) { render_utag(tag) }
            }
          }
          section({name, inverted, children}) {
            alt context.find(name) {
              none {
                if inverted {
                    render(children, new_str_hash())
                } else { "" }
              }
              some(tag) {
                render_section(tag, inverted, children) }
            }
          }
          partial(name) { from_file(name + ".mustache", context) }
          _ { fail }
        }
    };

    str::concat(output)
}

fn render_etag(data: data) -> str {
    let escaped = "";
    str::chars_iter(render_utag(data)) { |c|
        alt c {
          '<' { escaped += "&lt;" }
          '>' { escaped += "&gt;" }
          '&' { escaped += "&amp;" }
          '"' { escaped += "&quot;" }
          '\'' { escaped += "&#39;" }
          _ { str::push_char(escaped, c); }
        }
    }
    escaped
}

fn render_utag(data: data) -> str {
    alt data {
      str(s) { s }
      _ { fail }
    }
}

fn render_section(data: data, inverted: bool, children: [token]) -> str {
    alt data {
      bool(b) {
        if inverted {
            render(children, new_str_hash())
        } else { "" }
      }
      vec(ctxs) {
        if inverted {
            if vec::is_empty(ctxs) {
                render(children, new_str_hash())
            } else { "" }
        } else {
            str::concat(vec::map(ctxs) { |ctx| render(children, ctx) })
        }
      }
      fun(f) { fail }
      _ { fail }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_compile_texts() {
        assert compile("hello world") == [text("hello world")];
        assert compile("hello {world") == [text("hello {world")];
        assert compile("hello world}") == [text("hello world}")];
        assert compile("hello world}}") == [text("hello world}}")];
    }

    #[test]
    fn test_compile_etags() {
        assert compile("{{ name }}") == [etag("name")];

        assert compile("before {{name}} after") == [
            text("before "),
            etag("name"),
            text(" after")
        ];

        assert compile("before {{name}}") == [
            text("before "),
            etag("name")
        ];

        assert compile("{{name}} after") == [
            etag("name"),
            text(" after")
        ];
    }

    #[test]
    fn test_compile_utags() {
        assert compile("{{{name}}}") == [utag("name")];

        assert compile("before {{{name}}} after") == [
            text("before "),
            utag("name"),
            text(" after")
        ];

        assert compile("before {{{name}}}") == [
            text("before "),
            utag("name")
        ];

        assert compile("{{{name}}} after") == [
            utag("name"),
            text(" after")
        ];
    }

    #[test]
    fn test_compile_sections() {
        assert compile("{{# name}}{{/name}}") == [
            section({name: "name", inverted: false, children: []})
        ];

        assert compile("before {{^name}}{{/name}} after") == [
            text("before "),
            section({name: "name", inverted: true, children: []}),
            text(" after")
        ];

        assert compile("before {{#name}}{{/name}}") == [
            text("before "),
            section({name: "name", inverted: false, children: []})
        ];

        assert compile("{{#name}}{{/name}} after") == [
            section({name: "name", inverted: false, children: []}),
            text(" after")
        ];

        assert compile("before {{#a}} 1 {{^b}} 2 {{/b}} {{/a}} after") == [
            text("before "),
            section({name: "a", inverted: false, children: [
                text(" 1 "),
                section({name: "b", inverted: true, children: [
                    text(" 2 ")
                ]}),
                text(" ")
            ]}),
            text(" after")
        ];
    }

    #[test]
    fn test_compile_partials() {
        assert compile("{{> name}}") == [partial("name")];

        assert compile("before {{>name}} after") == [
            text("before "),
            partial("name"),
            text(" after")
        ];

        assert compile("before {{> name}}") == [
            text("before "),
            partial("name")
        ];

        assert compile("{{>name}} after") == [
            partial("name"),
            text(" after")
        ];
    }

    #[test]
    fn test_compile_delimiters() {
        assert compile("before {{=<% %>=}}<%name%> after") == [
            text("before "),
            etag("name"),
            text(" after")
        ];
    }

    #[test]
    fn test_render_texts() {
        let ctx = new_str_hash();
        ctx.insert("name", str("world"));

        assert render(compile("hello world"), ctx) == "hello world";
        assert render(compile("hello {world"), ctx) == "hello {world";
        assert render(compile("hello world}"), ctx) == "hello world}";
        assert render(compile("hello {world}"), ctx) == "hello {world}";
        assert render(compile("hello world}}"), ctx) == "hello world}}";
    }

    #[test]
    fn test_render_etags() {
        let ctx = new_str_hash();
        ctx.insert("name", str("world"));

        assert render(compile("hello {{name}}"), ctx) == "hello world";
    }

    #[test]
    fn test_render_utags() {
        let ctx = new_str_hash();
        ctx.insert("name", str("world"));

        assert render(compile("hello {{{name}}}"), ctx) == "hello world";
    }

    #[test]
    fn test_render_sections() {
        let ctx0 = new_str_hash();
        let template = compile("0{{#a}}1 {{n}} 3{{/a}}5");

        assert render(template, ctx0) == "05";

        ctx0.insert("a", vec([]));
        assert render(template, ctx0) == "05";

        let ctx1 = new_str_hash();
        ctx0.insert("a", vec([ctx1]));

        assert render(template, ctx0) == "01  35";

        ctx1.insert("n", str("a"));
        assert render(template, ctx0) == "01 a 35";
    }

    #[test]
    fn test_render_inverted_sections() {
        let template = "0{{^a}}1 3{{/a}}5";

        let ctx0 = new_str_hash();
        assert from_str(template, ctx0) == "01 35";

        ctx0.insert("a", vec([]));
        assert from_str(template, ctx0) == "01 35";

        let ctx1 = new_str_hash();
        ctx0.insert("a", vec([ctx1]));
        assert from_str(template, ctx0) == "05";

        ctx1.insert("n", str("a"));
        assert from_str(template, ctx0) == "05";
    }

    #[test]
    fn test_render_partial() {
        let path = "base.mustache";

        let ctx0 = new_str_hash();
        assert from_file(path, ctx0) == "<h2>Names</h2>\n\n";

        ctx0.insert("names", vec([]));
        assert from_file(path, ctx0) == "<h2>Names</h2>\n\n";

        let ctx1 = new_str_hash();
        ctx0.insert("names", vec([ctx1]));
        assert from_file(path, ctx0) ==
            "<h2>Names</h2>\n\n" +
            "  <strong></strong>\n\n\n";

        ctx1.insert("name", str("a"));
        assert from_file(path, ctx0) ==
            "<h2>Names</h2>\n\n" +
            "  <strong>a</strong>\n\n\n";

        let ctx2 = new_str_hash();
        ctx2.insert("name", str("<b>"));
        ctx0.insert("names", vec([ctx1, ctx2]));
        assert from_file(path, ctx0) ==
            "<h2>Names</h2>\n\n" +
            "  <strong>a</strong>\n\n\n" +
            "  <strong>&lt;b&gt;</strong>\n\n\n";
    }
}
