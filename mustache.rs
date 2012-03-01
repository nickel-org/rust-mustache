use std;

import result::{ok, err};
import std::io;
import std::io::{reader_util, writer_util};
import std::map::{map, new_str_hash};

export compile_reader;
export compile_str;

enum token {
    text(str),
    etag([str], str),
    utag([str], str),
    section([str], bool, [token], str, str, str),
    incomplete_section([str], bool, str, bool),
    partial(str, str),
}

enum token_class {
    normal,
    standalone,
    whitespace(str, uint),
    newline_whitespace(str, uint),
}

type partial_map = map<str, [token]>;

type template = {
    tokens: [token],
    partials: partial_map,
};

enum data {
    str(str),
    bool(bool),
    vec([data]),
    dict(context),
    fun(fn@(str) -> str),
}

type context = map<str, data>;

type parser = {
    rdr: io::reader,
    mut ch: char,
    mut lookahead: option<char>,
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
    mut partials: [str],
};

mod parser {
    enum state { TEXT, OTAG, TAG, CTAG }
}

impl parser for parser {
    fn eof() -> bool { self.ch == -1 as char }

    fn bump() {
        alt self.lookahead {
          none { self.ch = self.rdr.read_char(); }
          some(ch) { self.ch = ch; self.lookahead = none; }
        }

        io::println(#fmt("bump: %s", str::from_char(self.ch)));

        if self.ch == '\n' {
            self.line += 1u;
            self.col = 1u;
        } else {
            self.col += 1u;
        }
    }

    fn peek() -> char {
        alt self.lookahead {
          none {
            let ch = self.rdr.read_char();
            self.lookahead = some(ch);
            ch
          }
          some(ch) { ch }
        }
    }

    fn parse() -> ([token], [str]) {
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
                self.bump();
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
                self.bump();
              }
              parser::TAG {
                if self.content == "" && self.ch == '{' {
                    curly_brace_tag = true;
                    unsafe { str::push_char(self.content, self.ch) };
                    self.bump();
                } else if curly_brace_tag && self.ch == '}' {
                    curly_brace_tag = false;
                    unsafe { str::push_char(self.content, self.ch) };
                    self.bump();
                } else if self.ch == self.ctag_chars[0u] {
                    if vec::len(self.ctag_chars) > 1u {
                        self.tag_position = 1u;
                        self.state = parser::CTAG;
                        self.bump();
                    } else {
                        self.add_tag();
                        self.state = parser::TEXT;
                    }
                } else {
                    unsafe { str::push_char(self.content, self.ch) };
                    self.bump();
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
                        self.bump();
                    }
                }
              }
            }
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
              incomplete_section(name, _, _, _) {
                  fail #fmt("Unclosed mustache section %s",
                    str::connect(name, "."));
              }
              _ {}
            }
        };

        (self.tokens, self.partials)
    }

    fn add_text() {
        if self.content != "" {
            vec::push(self.tokens, text(self.content));
            self.content = "";
        }
    }

    // This function classifies whether or not a token is standalone, or if it
    // has trailing whitespace. It's looking for this pattern:
    //
    //   ("\n" | "\r\n") whitespace* token ("\n" | "\r\n")
    //
    fn classify_token() -> token_class {
        // Exit early if the next character is not '\n' or '\r\n'.
        if !self.eof() &&
                self.ch != '\n' &&
                (self.ch == '\r' && self.peek() != '\n') {
            ret normal;
        }

        // If the last token ends with a newline (or there is no previous
        // token), then this token is standalone.
        alt vec::last(self.tokens) {
          none | some(incomplete_section(_, _, _, true)) {
              standalone
          }

          some(text(s)) if s != "" {
              io::println(#fmt("classify_token: %?", s));

            // Look for the last newline character that may have whitespace
            // following it.
            alt str::rfind(s, {|c| c == '\n' || !char::is_whitespace(c)}) {
              none {
                // It's all whitespace.
                //whitespace(s, 0u)
                normal
              }
              some(pos) {
                if str::char_at(s, pos) == '\n' {
                    standalone
                  /*
                    if pos == str::len(s) {
                        standalone
                    } else {
                        whitespace(s, pos + 1u)
                    }
                    */
                } else {
                    normal
                }
              }
            }
          }
          _ { normal }
        }
    }

    /*
    fn eat_whitespace() -> bool {
        // If the next character is a newline, and the last token ends with a
        // newline and whitespace, clear out the whitespace.

        alt self.classify_token() {
          normal { false }
          standalone {
              io::println("eating");
              if self.ch == '\r' { self.bump(); }
              self.bump();
              true
          }
          whitespace(s, pos) | newline_whitespace(s, pos) {
              io::println("eating");

              if self.ch == '\r' { self.bump(); }
              self.bump();

              // Trim the whitespace from the last token.
              vec::pop(self.tokens);
              vec::push(self.tokens, text(str::slice(s, 0u, pos)));

              true
          }
        }
    }
    */

    fn eat_whitespace() -> bool {
        // If the next character is a newline, and the last token ends with a
        // newline and whitespace, clear out the whitespace.

        if self.eof() ||
           self.ch == '\n' ||
           (self.ch == '\r' && self.peek() == '\n') {
            // Find the last text token.
            /*
            let token = vec::rfind(self.tokens) { |token|
                alt token {
                  text(_) { true }
                  _ { false }
                }
            };
            */

            alt vec::last(self.tokens) {
              none | some(incomplete_section(_, _, _, true)) {
                //io::println("eating newline");
                // Skip over the next newline...
                if self.ch == '\r' { self.bump(); }
                self.bump();
                true
              }

              some(text(s)) {
                // Look for the last newline non-whitespace
                // character.
                let pos = str::rfind(s) { |c|
                    c == '\n' || !char::is_whitespace(c)
                };
                //io::println(#fmt("pos:  %?", pos));

                // If we found a newline, then we can skip this
                // newline.
                let pos = alt pos {
                  none { 0u }
                  some(i) { i }
                };

                let c = str::char_at(s, pos);

                if char::is_whitespace(c) {
                    // Skip over the next newline...
                    if self.ch == '\r' { self.bump(); }
                    self.bump();

                    // And trim the whitespace in the last token.
                    vec::pop(self.tokens);

                    let s = if c == '\n' {
                        str::slice(s, 0u, pos + 1u)
                    } else {
                        str::slice(s, 0u, pos)
                    };

                    vec::push(self.tokens, text(s));
                    true

                    /*
                } else if str::char_at(s, i) == '\r' &&
                              str::len(s) > i + 1u &&
                              str::char_at(s, i + 1u) == '\n' {
                        self.bump();
                        self.bump();
                    */
                } else {
                    false
                }
              }
              _ {
                //io::println(#fmt("not text: %?", vec::last(self.tokens)));
                false
              }
            }
        } else {
            false
        }
    }

    fn eat_newline() {
        self.eat_whitespace();
    }

    fn add_tag() {
        self.bump();

        let tag = self.otag + self.content + self.ctag;
        let content = self.content;
        let content_len = str::len(content);

        alt content[0] as char {
          '!' {
            // ignore comments
            self.eat_whitespace();
          }
          '&' {
            let name = str::slice(content, 1u, content_len);
            let name = self.check_content(name);
            let name = str::split_char_nonempty(name, '.');
            vec::push(self.tokens, utag(name, tag)); }
          '{' {
            if str::ends_with(content, "}") {
                let name = str::slice(content, 1u, content_len - 1u);
                let name = self.check_content(name);
                let name = str::split_char_nonempty(name, '.');
                vec::push(self.tokens, utag(name, tag));
            } else {
                log(error, self.content);
                log(error, content);
                fail "unbalanced \"{\" in tag";
            }
          }
          '#' {
            let newlined = self.eat_whitespace();
            #error("incomplete: %b", newlined);

            let name = self.check_content(str::slice(content, 1u, content_len));
            let name = str::split_char_nonempty(name, '.');
            vec::push(self.tokens, incomplete_section(name, false, tag, newlined));
          }
          '^' {
            let newlined = self.eat_whitespace();

            let name = self.check_content(str::slice(content, 1u, content_len));
            let name = str::split_char_nonempty(name, '.');
            vec::push(self.tokens, incomplete_section(name, true, tag, newlined));
          }
          '/' {
            let newlined = self.eat_whitespace();
            #error("complete: %b", newlined);

            let name = self.check_content(str::slice(content, 1u, content_len));
            let name = str::split_char_nonempty(name, '.');
            let children = [];

            while true {
                if vec::len(self.tokens) == 0u {
                    fail "closing unopened section";
                }

                let last = vec::pop(self.tokens);

                alt last {
                  incomplete_section(section_name, inverted, otag, _) {
                    let children = vec::reversed(children);

                    // Collect all the children's sources.
                    let srcs = [];
                    vec::iter(children) { |child: token|
                        alt child {
                          text(s) | etag(_, s) | utag(_, s) | partial(_, s) {
                            vec::push(srcs, s);
                          }
                          section(_, _, _, otag, src, ctag) {
                            vec::push(srcs, otag);
                            vec::push(srcs, src);
                            vec::push(srcs, ctag);
                          }
                          _ { fail; }
                        }
                    }

                    if section_name == name {
                        vec::push(
                            self.tokens,
                            section(
                                name,
                                inverted,
                                children,
                                otag,
                                str::concat(srcs),
                                tag));
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
            self.eat_newline();

            // We can't inline the tokens directly as we may have a recursive
            // partial. So instead, we'll cache the partials we used and look
            // them up later.
            let name = str::slice(content, 1u, content_len);
            let name = self.check_content(name);
            vec::push(self.tokens, partial(name, tag));
            vec::push(self.partials, name);
          }
          '=' {
            self.eat_whitespace();

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
            let name = self.check_content(content);
            let name = str::split_char_nonempty(name, '.');
            vec::push(self.tokens, etag(name, tag));
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

fn compile_reader(rdr: io::reader) -> template {
    let partials = new_str_hash();
    let tokens = compile_helper(rdr, partials);

    { tokens: tokens, partials: partials }
}

fn compile_helper(rdr: io::reader, partials: partial_map) -> [token] {
    let parser = {
        rdr: rdr,
        mut ch: rdr.read_char(),
        mut lookahead: none,
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
        mut partials: [],
    };

    let (tokens, partial_names) = parser.parse();

    // Compile the partials if we haven't done so already.
    vec::iter(partial_names) { |name|
        let path = name + ".mustache";

        if !partials.contains_key(name) {
            // Insert a placeholder so we don't recurse off to infinity.
            partials.insert(name, []);

            alt io::file_reader(path) {
              err(e) {}
              ok(rdr) { partials.insert(name, compile_helper(rdr, partials)); }
            }
        }
    }

    tokens
}

fn compile_file(path: str) -> template {
    alt io::file_reader(path) {
      ok(rdr) { compile_reader(rdr) }
      err(e) { fail e; }
    }
}

fn compile_str(src: str) -> template {
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

fn render(template: template, context: context) -> str {
    render_helper(template.tokens, template.partials, [dict(context)])
}

fn render_helper(tokens: [token], partials: partial_map, stack: [data])
  -> str {
    fn find(stack: [data], path: [str]) -> option<data> {
        // If we have an empty path, we just want the top value in our stack.
        if vec::is_empty(path) {
            ret alt vec::last(stack) {
              none { none }
              some(value) { some(value) }
            };
        }

        // Otherwise, find the stack that has the first part of our path.
        let value = none;

        let i = vec::len(stack);
        while i > 0u {
            alt stack[i - 1u] {
              dict(ctx) {
                alt ctx.find(path[0u]) {
                  some(v) { value = some(v); break; }
                  none {}
                }
                i -= 1u;
              }
              _ { fail #fmt("%? %?", stack, path) }
            }
        }

        // Walk the rest of the path to find our final value.
        let value = value;

        let i = 1u;
        let len = vec::len(path);

        while i < len {
            alt value {
              some(dict(v)) { value = v.find(path[i]); }
              _ { break; }
            }
            i += 1u;
        }

        value
    }

    let output = vec::map(tokens) { |token|
        alt token {
          text(value) { value }
          etag(path, _) {
            alt find(stack, path) {
              none { "" }
              some(value) { render_etag(value) }
            }
          }
          utag(path, _) {
            alt find(stack, path) {
              none { "" }
              some(value) { render_utag(value) }
            }
          }
          section(path, true, children, _, _, _) {
            alt find(stack, path) {
              none { render_helper(children, partials, stack) }
              some(value) {
                render_inverted_section(value, children, partials, stack)
              }
            }
          }
          section(path, false, children, _, src, _) {
            alt find(stack, path) {
              none { "" }
              some(value) {
                render_section(value, children, src, partials, stack)
              }
            }
          }
          partial(name, _) {
            alt partials.find(name) {
              none { "" }
              some(tokens) { render_helper(tokens, partials, stack) }
            }
          }
          _ { fail }
        }
    };

    str::concat(output)
}

fn render_etag(value: data) -> str {
    let escaped = "";
    str::chars_iter(render_utag(value)) { |c|
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

fn render_utag(value: data) -> str {
    alt value {
      str(s) { s }
      _ { fail }
    }
}

fn render_inverted_section(value: data,
                           children: [token],
                           partials: partial_map,
                           stack: [data]) -> str {
    alt value {
      bool(false) { render_helper(children, partials, stack) }
      vec(xs) if vec::is_empty(xs) { render_helper(children, partials, stack) }
      _ { "" }
    }
}

fn render_section(value: data,
                  children: [token],
                  src: str,
                  partials: partial_map,
                  stack: [data]) -> str {
    alt value {
      bool(true) { render_helper(children, partials, stack) }
      bool(false) { "" }
      vec(vs) {
        str::concat(vec::map(vs) { |v|
            render_helper(children, partials, stack + [v])
        })
      }
      dict(_) {
          render_helper(children, partials, stack + [value])
      }
      fun(f) { f(src) }
      _ { fail }
    }
}

#[cfg(test)]
mod tests {
    import std::fs;
    import std::json;

    /*
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
    */

    #[test]
    fn test_compile_sections() {
        /*
        assert compile_str("{{# name}}{{/name}}") == [
            section(
                "name",
                false,
                [],
                "{{# name}}",
                "",
                "{{/name}}"
            )
        ];

        assert compile_str("before {{^name}}{{/name}} after") == [
            text("before "),
            section(
                "name",
                true,
                [],
                "{{^name}}",
                "",
                "{{/name}}"
            ),
            text(" after")
        ];

        assert compile_str("before {{#name}}{{/name}}") == [
            text("before "),
            section(
                "name",
                false,
                [],
                "{{#name}}",
                "",
                "{{/name}}"
            )
        ];

        assert compile_str("{{#name}}{{/name}} after") == [
            section(
                "name",
                false,
                [],
                "{{#name}}",
                "",
                "{{/name}}"
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
                        "{{^b}}",
                        " 2 ",
                        "{{/b}}"
                    ),
                    text(" ")
                ],
                "{{#a}}",
                " 1 {{^b}} 2 {{/b}} ",
                "{{/a}}"
            ),
            text(" after")
        ];
        */
    }

    /*
    #[test]
    fn test_compile_partials() {
        assert compile_str("{{> test}}") == [
            etag("foo", "{{foo}}"),
            text("bar\n")
        ];

        assert compile_str("before {{>test}} after") == [
            text("before "),
            etag("foo", "{{foo}}"),
            text("bar\n"),
            text(" after")
        ];

        assert compile_str("before {{> test}}") == [
            text("before "),
            etag("foo", "{{foo}}"),
            text("bar\n")
        ];

        assert compile_str("{{>test}} after") == [
            etag("foo", "{{foo}}"),
            text("bar\n"),
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
    */

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
        map.items { |key, value| ctx.insert(key, convert_json(value)); };
        ctx
    }

    fn convert_json(value: json::json) -> data {
        alt value {
          json::num(n) {
            // We have to cheat and use %? because %f doesn't convert 3.3 to
            // 3.3.
            str(#fmt("%?", n))
          }
          json::string(s) { str(s) }
          json::boolean(b) { bool(b) }
          json::list(v) { vec(vec::map(v, convert_json)) }
          json::dict(d) { dict(convert_json_map(d)) }
          _ { fail #fmt("%?", value) }
        }
    }

    fn write_partials(value: json::json) -> [str] {
        let files = [];

        alt value {
          json::dict(d) {
            d.items { |key, value|
                alt value {
                  json::string(s) {
                    let file = key + ".mustache";
                    alt io::file_writer(file, [io::create, io::truncate]) {
                      ok(wr) { vec::push(files, file); wr.write_str(s); }
                      err(e) { fail e; }
                    }
                  }
                  _ { fail; }
                }
            }
          }
          _ { fail; }
        }

        files
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

        let partials = alt test.find("partials") {
          some(value) { write_partials(value) }
          none { [] }
        };

            fn to_list(x: json::json) -> json::json {
                alt x {
                  json::dict(d) {
                    let xs = [];
                    d.items { |k,v|
                        let k = json::string(k);
                        let v = to_list(v);
                        vec::push(xs, json::list([k, v]));
                    }
                    json::list(xs)
                  }
                  json::list(xs) { json::list(vec::map(xs, to_list)) }
                  _ { x }
                }
            }


        if from_str(template, ctx) != expected {
            io::println(#fmt("desc:     %?", test.get("desc")));
            io::println(#fmt("context:  %?", to_list(test.get("data"))));
            io::println(#fmt("partials: %?", partials));
            io::println(#fmt("template:\n%?", template));
            io::println(#fmt("expected:\n%?", expected));
            io::println(#fmt("result:  \n%?", from_str(template, ctx)));
            io::println(#fmt(""));
        }
        assert from_str(template, ctx) == expected;

        vec::iter(partials) { |file| fs::remove_file(file); }
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
}
