use std;

import result::{ok, err};
import std::io;
import std::io::{reader_util};
import std::map::{map, new_str_hash};

export token;
export compile_reader;
export compile_str;

enum token {
    text(str),
    etag(str, str),
    utag(str, str),
    section(str, bool, [token], str),
    incomplete_section(str, bool, str),
    partial(str, str),
}

enum data {
    str(str),
    bool(bool),
    vec([context]),
    dict(context),
    fun(fn@(str) -> str),
}

type context = map<str, data>;

type parser = {
    rdr: io::reader,
    mut ch: char,
    mut line: uint,
    mut col: uint,
    mut content: str,
    mut state: parser::state,
    mut otag: str,
    mut ctag: str,
    mut otag_chars: [char],
    mut ctag_chars: [char],
    mut tag_position: uint,
    mut tokens: [token],
};

mod parser {
    enum state { TEXT, OTAG, TAG, CTAG }
}

impl parser for parser {
    fn eof() -> bool { self.ch == -1 as char }

    fn bump() {
        self.ch = self.rdr.read_char();

        if self.ch == '\n' {
            self.line += 1u;
            self.col = 1u;
        } else {
            self.col += 1u;
        }
    }

    fn parse() -> [token] {
        let curly_brace_tag = false;

        while !self.eof() {
            alt self.state {
              parser::TEXT {
                if self.ch == self.otag_chars[0] {
                    if vec::len(self.otag_chars) > 1u {
                        self.tag_position = 1u;
                        self.state = parser::OTAG;
                    } else {
                        self.add_text();
                        self.state = parser::TAG;
                    }
                } else {
                    unsafe { str::push_char(self.content, self.ch) };
                }
              }
              parser::OTAG {
                if self.ch == self.otag_chars[self.tag_position] {
                    if self.tag_position == vec::len(self.otag_chars) - 1u {
                        self.add_text();
                        curly_brace_tag = false;
                        self.state = parser::TAG;
                    } else {
                        self.tag_position += 1u;
                    }
                } else {
                    // We don't have a tag, so add all the tag parts we've seen
                    // so far to the string.
                    self.state = parser::TEXT;
                    self.not_otag();
                    unsafe { str::push_char(self.content, self.ch) };
                }
              }
              parser::TAG {
                  if self.content == "" && self.ch == '{' {
                      curly_brace_tag = true;
                      unsafe { str::push_char(self.content, self.ch) };
                  } else if curly_brace_tag && self.ch == '}' {
                      curly_brace_tag = false;
                      unsafe { str::push_char(self.content, self.ch) };
                  } else if self.ch == self.ctag_chars[0u] {
                      if vec::len(self.ctag_chars) > 1u {
                          self.tag_position = 1u;
                          self.state = parser::CTAG;
                      } else {
                          self.add_tag();
                          self.state = parser::TEXT;
                      }
                  } else {
                      unsafe { str::push_char(self.content, self.ch) };
                  }
              }
              parser::CTAG {
                  if self.ch == self.ctag_chars[self.tag_position] {
                      if self.tag_position == vec::len(self.ctag_chars) - 1u {
                          self.add_tag();
                          self.state = parser::TEXT;
                      } else {
                          self.state = parser::TAG;
                          self.not_ctag();
                          unsafe { str::push_char(self.content, self.ch) };
                      }
                  }
              }
            }

            self.bump();
        }

        alt self.state {
          parser::TEXT { self.add_text() }
          parser::OTAG { self.not_otag(); self.add_text() }
          parser::TAG { fail "unclosed tag" }
          parser::CTAG { self.not_ctag(); self.add_text() }
        }

        // Check that we don't have any incomplete sections.
        vec::iter(copy self.tokens) { |token|
            alt token {
              incomplete_section(name, _, _) {
                  fail #fmt("Unclosed mustache section %s", name);
              }
              _ {}
            }
        };

        self.tokens
    }

    fn add_text() {
        if self.content != "" {
            vec::push(self.tokens, text(self.content));
            self.content = "";
        }
    }

    fn add_tag() {
        let src = self.otag + self.content + self.ctag;
        let content = self.content;
        let content_len = str::len(content);

        alt content[0] as char {
          '!' {} // ignore comments
          '&' {
            let name = str::slice(content, 1u, content_len);
            let name = self.check_content(name);
            vec::push(self.tokens, utag(name, src)); }
          '{' {
            if str::ends_with(content, "}") {
                let name = str::slice(content, 1u, content_len - 1u);
                let name = self.check_content(name);
                vec::push(self.tokens, utag(name, src));
            } else {
                log(error, self.content);
                log(error, content);
                fail "unbalanced \"{\" in tag";
            }
          }
          '#' {
            let name = self.check_content(str::slice(content, 1u, content_len));
            vec::push(
                self.tokens,
                incomplete_section(name, false, src));
          }
          '^' {
            let name = self.check_content(str::slice(content, 1u, content_len));
            vec::push(
                self.tokens,
                incomplete_section(name, true, src));
          }
          '/' {
            let name = self.check_content(str::slice(content, 1u, content_len));
            let children = [];

            while true {
                if vec::len(self.tokens) == 0u {
                    fail "closing unopened section";
                }

                let last = vec::pop(self.tokens);

                alt last {
                  incomplete_section(section_name, inverted, section_src) {
                    let children = vec::reversed(children);

                    // Collect all the children's sources.
                    let srcs = [section_src];
                    vec::iter(children) { |child: token|
                        alt child {
                        text(s)
                        | etag(_, s)
                        | utag(_, s)
                        | section(_, _, _, s)
                        | incomplete_section(_, _, s)
                        | partial(_, s) {
                            vec::push(srcs, s);
                          }
                        }
                    }
                    vec::push(srcs, self.otag + self.content + self.ctag);

                    if section_name == name {
                        vec::push(
                            self.tokens,
                            section(name, inverted, children, str::concat(srcs)));
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
            let name = self.check_content(str::slice(content, 1u, content_len));
            vec::push(self.tokens, partial(name, src));
          }
          '=' {
            if (content_len > 2u && str::ends_with(content, "=")) {
                let s = self.check_content(str::slice(content, 1u, content_len - 1u));
                let tags = str::splitn_char(s, ' ', 2u);

                if vec::len(tags) == 2u {
                    self.otag = tags[0];
                    self.otag_chars = str::chars(self.otag);

                    self.ctag = tags[1];
                    self.ctag_chars = str::chars(self.ctag);
                } else {
                    fail "invalid change delimiter tag content";
                }
            } else {
                fail "invalid change delimiter tag content";
            }
          }
          _ {
            let src = self.otag + self.content + self.ctag;
            vec::push(self.tokens, etag(self.check_content(content), src));
          }
        }

        self.content = "";
    }

    fn not_otag() {
        let i = 0u;
        while i < self.tag_position {
            unsafe { str::push_char(self.content, self.otag_chars[i]) };
            i += 1u;
        }
    }

    fn not_ctag() {
        let i = 0u;
        while i < self.tag_position {
            unsafe { str::push_char(self.content, self.ctag_chars[i]) };
            i += 1u;
        }
    }

    fn check_content(content: str) -> str {
        let trimmed = str::trim(content);
        if str::len(trimmed) == 0u {
            fail "empty tag";
        }
        trimmed
    }
}

fn compile_reader(rdr: io::reader) -> [token] {
    let parser :parser = {
        rdr: rdr,
        mut ch: rdr.read_char(),
        mut line: 1u,
        mut col: 1u,
        mut content: "",
        mut state: parser::TEXT,
        mut otag: "{{",
        mut ctag: "}}",
        mut otag_chars: ['{', '{'],
        mut ctag_chars: ['}', '}'],
        mut tag_position: 0u,
        mut tokens: [],
    };

    /*
    if self.pos < self.len {
        let {ch, next} = str::char_range_at(src, 0u);
        self.ch = ch;
        parser.pos = next;
    }
    */

    parser.parse()
}

fn compile_str(src: str) -> [token] {
    io::with_str_reader(src, compile_reader)
}

fn from_str(template: str, context: context) -> str {
    render(compile_str(template), context)
}

fn from_file(path: str, context: context) -> str {
    alt io::read_whole_file_str(path) {
      ok(template) { from_str(template, context) }
      err(e) { fail }
    }
}

fn render(tokens: [token], context: context) -> str {
    render_helper(tokens, [context])
}

fn render_helper(tokens: [token], stack: [context]) -> str {
    fn find(stack: [context], name: str) -> option<data> {
        let i = vec::len(stack);
        while i > 0u {
            alt stack[i - 1u].find(name) {
              some(value) { ret some(value); }
              none { }
            }
            i -= 1u;
        }

        ret none;
    }

    let output = vec::map(tokens) { |token|
        alt token {
          text(s) { s }
          etag(name, _) {
            alt find(stack, name) {
              none { "" }
              some(tag) { render_etag(tag) }
            }
          }
          utag(name, _) {
            alt find(stack, name) {
              none { "" }
              some(tag) { render_utag(tag) }
            }
          }
          section(name, inverted, children, src) {
            alt find(stack, name) {
              none {
                if inverted {
                    render_helper(children, stack)
                } else { "" }
              }
              some(tag) {
                render_section(tag, inverted, children, src, stack) }
            }
          }
          //partial(name) { from_file(name + ".mustache", context) }
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

fn render_section(data: data,
                  inverted: bool,
                  children: [token],
                  src: str,
                  stack: [context]
                  ) -> str {
    alt data {
      bool(b) {
        if b || inverted {
            render_helper(children, stack)
        } else { "" }
      }
      vec(ctxs) {
        if inverted {
            if vec::is_empty(ctxs) {
                render_helper(children, stack)
            } else { "" }
        } else {
            str::concat(vec::map(ctxs) { |ctx|
                render_helper(children, stack + [ctx])
            })
        }
      }
      dict(d) { render_helper(children, stack + [d]) }
      fun(f) { f(src) }
      _ { fail }
    }
}

#[cfg(test)]
mod tests {
    import std::json;

    #[test]
    fn test_compile_texts() {
        assert compile_str("hello world") == [text("hello world")];
        assert compile_str("hello {world") == [text("hello {world")];
        assert compile_str("hello world}") == [text("hello world}")];
        assert compile_str("hello world}}") == [text("hello world}}")];
    }

    #[test]
    fn test_compile_etags() {
        assert compile_str("{{ name }}") == [
            etag("name", "{{ name }}")
        ];

        assert compile_str("before {{name}} after") == [
            text("before "),
            etag("name", "{{name}}"),
            text(" after")
        ];

        assert compile_str("before {{name}}") == [
            text("before "),
            etag("name", "{{name}}")
        ];

        assert compile_str("{{name}} after") == [
            etag("name", "{{name}}"),
            text(" after")
        ];
    }

    #[test]
    fn test_compile_utags() {
        assert compile_str("{{{name}}}") == [
            utag("name", "{{{name}}}")
        ];

        assert compile_str("before {{{name}}} after") == [
            text("before "),
            utag("name", "{{{name}}}"),
            text(" after")
        ];

        assert compile_str("before {{{name}}}") == [
            text("before "),
            utag("name", "{{{name}}}")
        ];

        assert compile_str("{{{name}}} after") == [
            utag("name", "{{{name}}}"),
            text(" after")
        ];
    }

    #[test]
    fn test_compile_sections() {
        assert compile_str("{{# name}}{{/name}}") == [
            section(
                "name",
                false,
                [],
                "{{# name}}{{/name}}"
            )
        ];

        assert compile_str("before {{^name}}{{/name}} after") == [
            text("before "),
            section(
                "name",
                true,
                [],
                "{{^name}}{{/name}}"
            ),
            text(" after")
        ];

        assert compile_str("before {{#name}}{{/name}}") == [
            text("before "),
            section(
                "name",
                false,
                [],
                "{{#name}}{{/name}}"
            )
        ];

        assert compile_str("{{#name}}{{/name}} after") == [
            section(
                "name",
                false,
                [],
                "{{#name}}{{/name}}"
            ),
            text(" after")
        ];

        assert compile_str("before {{#a}} 1 {{^b}} 2 {{/b}} {{/a}} after") == [
            text("before "),
            section(
                "a",
                false,
                [
                    text(" 1 "),
                    section(
                        "b",
                        true,
                        [text(" 2 ")],
                        "{{^b}} 2 {{/b}}"
                    ),
                    text(" ")
                ],
                "{{#a}} 1 {{^b}} 2 {{/b}} {{/a}}"
            ),
            text(" after")
        ];
    }

    #[test]
    fn test_compile_partials() {
        assert compile_str("{{> name}}") == [
            partial("name", "{{> name}}")
        ];

        assert compile_str("before {{>name}} after") == [
            text("before "),
            partial("name", "{{>name}}"),
            text(" after")
        ];

        assert compile_str("before {{> name}}") == [
            text("before "),
            partial("name", "{{> name}}")
        ];

        assert compile_str("{{>name}} after") == [
            partial("name", "{{>name}}"),
            text(" after")
        ];
    }

    #[test]
    fn test_compile_delimiters() {
        assert compile_str("before {{=<% %>=}}<%name%> after") == [
            text("before "),
            etag("name", "<%name%>"),
            text(" after")
        ];
    }

    #[test]
    fn test_render_texts() {
        let ctx = new_str_hash();
        ctx.insert("name", str("world"));

        assert render(compile_str("hello world"), ctx) == "hello world";
        assert render(compile_str("hello {world"), ctx) == "hello {world";
        assert render(compile_str("hello world}"), ctx) == "hello world}";
        assert render(compile_str("hello {world}"), ctx) == "hello {world}";
        assert render(compile_str("hello world}}"), ctx) == "hello world}}";
    }

    #[test]
    fn test_render_etags() {
        let ctx = new_str_hash();
        ctx.insert("name", str("world"));

        assert render(compile_str("hello {{name}}"), ctx) == "hello world";
    }

    #[test]
    fn test_render_utags() {
        let ctx = new_str_hash();
        ctx.insert("name", str("world"));

        assert render(compile_str("hello {{{name}}}"), ctx) == "hello world";
    }

    #[test]
    fn test_render_sections() {
        let ctx0 = new_str_hash();
        let template = compile_str("0{{#a}}1 {{n}} 3{{/a}}5");

        assert render(template, ctx0) == "05";

        ctx0.insert("a", vec([]));
        assert render(template, ctx0) == "05";

        let ctx1 = new_str_hash();
        ctx0.insert("a", vec([ctx1]));

        assert render(template, ctx0) == "01  35";

        ctx1.insert("n", str("a"));
        assert render(template, ctx0) == "01 a 35";

        ctx0.insert("a", fun({|text|
            assert text == "1 {{n}} 3";
            "foo"
        }));
        assert render(template, ctx0) == "0foo5";
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

    /*
    fn parse_spec_tests(src: str) -> [json::json] {
        alt io::read_whole_file_str(src) {
          err(e) { fail e }
          ok(s) {
            alt json::from_str(s) {
              err(e) { fail #fmt("%s:%u:%u: %s", src, e.line, e.col, e.msg) }
              ok(json) {
                alt json {
                  json::dict(d) {
                    alt d.find("tests") {
                      some(json::list(tests)) { tests }
                      _ { fail #fmt("%s: tests key not a list", src) }
                    }
                  }
                  _ { fail #fmt("%s: JSON value not a map", src) }
                }
              }
            }
          }
        }
    }

    fn convert_json_map(map: map<str, json::json>) -> context {
        let ctx = new_str_hash();
        map.items { |key, value|
            alt value {
              json::num(n) { ctx.insert(key, str(float::to_str(n, 6u))); }
              json::string(s) { ctx.insert(key, str(s)); }
              json::boolean(b) { ctx.insert(key, bool(b)); }
              json::list(v) {
                let value = vec::map(v) { |item|
                    alt item {
                      json::dict(m) { convert_json_map(m) }
                      _ { fail }
                    }
                };
                ctx.insert(key, vec(value));
              }
              json::dict(d) {
                ctx.insert(key, dict(convert_json_map(d)));
              }
              _ { fail #fmt("%?", value) }
            }
        };
        ctx
    }

    fn run_test(test: json::json) {
        let test = alt test {
          json::dict(m) { m }
          _ { fail }
        };

        let ctx = alt test.get("data") {
          json::dict(m) { convert_json_map(m) }
          _ { fail }
        };

        let template = alt test.get("template") {
          json::string(s) { s }
          _ { fail }
        };

        let expected = alt test.get("expected") {
          json::string(s) { s }
          _ { fail }
        };

        io::println(#fmt("context:  %?", ctx));
        io::println(#fmt("template: %?", template));
        io::println(#fmt("expected: %?", expected));
        io::println(#fmt("result:   %?", from_str(template, ctx)));
        io::println(#fmt(""));
        assert from_str(template, ctx) == expected;
    }

    #[test]
    fn test_specs() {
        //vec::iter(parse_spec_tests("spec/specs/comments.json"), run_test);
        //vec::iter(parse_spec_tests("spec/specs/delimiters.json"), run_test);
        //vec::iter(parse_spec_tests("spec/specs/interpolation.json"), run_test);
        //vec::iter(parse_spec_tests("spec/specs/inverted.json"), run_test);
        //vec::iter(parse_spec_tests("spec/specs/partials.json"), run_test);
        vec::iter(parse_spec_tests("spec/specs/sections.json"), run_test);
        //vec::iter(parse_spec_tests("spec/specs/~lambdas.json"), run_test);
    }
    */
}
