use std;

import result::{ok, err};
import io::{reader_util, writer_util};
import dvec::{dvec, extensions};
import std::map::{hashmap, str_hash};

export context;
export default_context;
export data;
export to_mustache;
export template;
export compile_reader;
export compile_file;
export compile_str;
export render_reader;
export render_file;
export render_str;

#[doc = "
Represents the shared metadata needed to compile and render a mustache
template.
"]
type context = {
    template_path: @str,
    template_extension: @str,
};

#[doc = "
Configures a mustache context with the partial template path and extension.
"]
fn context(+template_path: str, +template_extension: str) -> context {
    {
        template_path: @template_path,
        template_extension: @template_extension,
    }
}

#[doc = "
Configures a mustache context looking for partial files in the current
directory.
"]
fn default_context() -> context { context(".", ".mustache") }

impl context for context {
    #[doc = "Compiles a template from an io::reader."]
    fn compile_reader(rdr: io::reader) -> template {
        let partials = str_hash();
        let tokens = compile_helper({
            rdr: rdr,
            partials: partials,
            otag: @"{{",
            ctag: @"}}",
            template_path: self.template_path,
            template_extension: self.template_extension
        });

        {
            ctx: self,
            tokens: tokens,
            partials: partials
        }
    }

    #[doc = "Compiles a template from a file."]
    fn compile_file(file: str) -> template {
        let path = path::connect(*self.template_path,
                                 file + *self.template_extension);

        alt io::file_reader(path) {
          ok(rdr) { self.compile_reader(rdr) }
          err(e) { fail e; }
        }
    }

    #[doc = "Compiles a template from a string."]
    fn compile_str(src: str) -> template {
        io::with_str_reader(src, |rdr| self.compile_reader(rdr))
    }

    #[doc = "Renders a template from an io::reader."]
    fn render_reader(rdr: io::reader, data: hashmap<str, data>) -> str {
        self.compile_reader(rdr).render(data)
    }

    #[doc = "Renders a template from a file."]
    fn render_file(file: str, data: hashmap<str, data>) -> str {
        self.compile_file(file).render(data)
    }

    #[doc = "Renders a template from a string."]
    fn render_str(template: str, data: hashmap<str, data>) -> str {
        self.compile_str(template).render(data)
    }
}

#[doc = "Compiles a template from an io::reader."]
fn compile_reader(rdr: io::reader) -> template {
    default_context().compile_reader(rdr)
}

#[doc = "Compiles a template from a file."]
fn compile_file(file: str) -> template {
    default_context().compile_file(file)
}

#[doc = "Compiles a template from a string."]
fn compile_str(template: str) -> template {
    default_context().compile_str(template)
}

#[doc = "Renders a template from an io::reader."]
fn render_reader(rdr: io::reader, data: hashmap<str, data>) -> str {
    default_context().compile_reader(rdr).render(data)
}

#[doc = "Renders a template from a file."]
fn render_file(file: str, data: hashmap<str, data>) -> str {
    default_context().compile_file(file).render(data)
}

#[doc = "Renders a template from a string."]
fn render_str(template: str, data: hashmap<str, data>) -> str {
    default_context().compile_str(template).render(data)
}

#[doc = "Represents template data."]
enum data {
    str(@str),
    bool(bool),
    vec(@~[data]),
    map(hashmap<str, data>),
    fun(fn@(@str) -> str),
}

iface to_mustache {
    fn to_mustache() -> data;
}

impl of to_mustache for data {
    fn to_mustache() -> data { self }
}

impl of to_mustache for str {
    fn to_mustache() -> data { str(@copy self) }
}

impl of to_mustache for @str {
    fn to_mustache() -> data { str(self) }
}

impl of to_mustache for bool {
    fn to_mustache() -> data { bool(self) }
}

impl of to_mustache for uint {
    fn to_mustache() -> data { str(@uint::str(self)) }
}

impl of to_mustache for int {
    fn to_mustache() -> data { str(@int::str(self)) }
}

impl <T: to_mustache> of to_mustache for ~[T] {
    fn to_mustache() -> data { vec(@self.map(|x| x.to_mustache())) }
}

impl <T: to_mustache copy> of to_mustache for hashmap<str, T> {
    fn to_mustache() -> data {
        let m = str_hash();
        for self.each |k, v| { m.insert(copy k, v.to_mustache()); }
        map(m)
    }
}

impl of to_mustache for fn@(@str) -> str {
    fn to_mustache() -> data { fun(self) }
}

impl <T: to_mustache> of to_mustache for option<T> {
    fn to_mustache() -> data {
        alt self {
          none { bool(false) }
          some(v) { v.to_mustache() }
        }
    }
}

type template = {
    ctx: context,
    tokens: @~[token],
    partials: hashmap<str, @~[token]>
};

impl template for template {
    fn render(data: hashmap<str, data>) -> str {
        render_helper({
            ctx: self.ctx,
            tokens: self.tokens,
            partials: self.partials,
            stack: ~[map(data)],
            indent: @""
        })
    }
}

enum token {
    text(@str),
    etag(@~[str], @str),
    utag(@~[str], @str),
    section(@~[str], bool, @~[token], @str, @str, @str, @str, @str),
    incomplete_section(@~[str], bool, @str, bool),
    partial(@str, @str, @str),
}

enum token_class {
    normal,
    standalone,
    whitespace(@str, uint),
    newline_whitespace(@str, uint),
}

type parser = {
    rdr: io::reader,
    mut ch: char,
    mut lookahead: option<char>,
    mut line: uint,
    mut col: uint,
    mut content: str,
    mut state: parser::state,
    mut otag: @str,
    mut ctag: @str,
    mut otag_chars: ~[char],
    mut ctag_chars: ~[char],
    mut tag_position: uint,
    tokens: dvec<token>,
    partials: dvec<@str>,
};

mod parser {
    enum state { TEXT, OTAG, TAG, CTAG }
}

impl parser for parser {
    fn eof() -> bool { self.ch == -1 as char }

    fn bump() {
        let mut lookahead = none;
        lookahead <-> self.lookahead;

        alt lookahead {
          none { self.ch = self.rdr.read_char(); }
          some(ch) { self.ch = ch; }
        }

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

    fn parse() {
        let mut curly_brace_tag = false;

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
        for self.tokens.each |token| {
            alt token {
              incomplete_section(name, _, _, _) {
                  fail #fmt("Unclosed mustache section %s",
                    str::connect(*name, "."));
              }
              _ {}
            }
        };
    }

    fn add_text() {
        if self.content != "" {
            let mut content = "";
            content <-> self.content;

            self.tokens.push(text(@content));
        }
    }

    // This function classifies whether or not a token is standalone, or if it
    // has trailing whitespace. It's looking for this pattern:
    //
    //   ("\n" | "\r\n") whitespace* token ("\n" | "\r\n")
    //
    fn classify_token() -> token_class {
        // Exit early if the next character is not '\n' or '\r\n'.
        if self.eof() ||
           self.ch == '\n' ||
           (self.ch == '\r' || self.peek() == '\n') {

            // If the last token ends with a newline (or there is no previous
            // token), then this token is standalone.
            if self.tokens.len() == 0u { ret standalone; }

            alt self.tokens[self.tokens.len() - 1u] {
              incomplete_section(_, _, _, true) { standalone }

              text(s) if *s != "" {
                // Look for the last newline character that may have whitespace
                // following it.
                alt str::rfind(*s,
                              { |c| c == '\n' || !char::is_whitespace(c) }) {
                  // It's all whitespace.
                  none {
                    if self.tokens.len() == 1u {
                        whitespace(s, 0u)
                    } else {
                        normal
                    }
                  }
                  some(pos) {
                    if str::char_at(*s, pos) == '\n' {
                        if pos == (*s).len() - 1u {
                            standalone
                        } else {
                            whitespace(s, pos + 1u)
                        }
                    } else { normal }
                  }
                }
              }
              _ { normal }
            }
        } else { normal }
    }

    fn eat_whitespace() -> bool {
        // If the next character is a newline, and the last token ends with a
        // newline and whitespace, clear out the whitespace.

        alt self.classify_token() {
          normal { false }
          standalone {
              if self.ch == '\r' { self.bump(); }
              self.bump();
              true
          }
          whitespace(s, pos) | newline_whitespace(s, pos) {
              if self.ch == '\r' { self.bump(); }
              self.bump();

              // Trim the whitespace from the last token.
              self.tokens.pop();
              self.tokens.push(text(@(*s).slice(0u, pos)));

              true
          }
        }
    }

    fn add_tag() {
        self.bump();

        let tag = @(*self.otag + self.content + *self.ctag);

        // Move the content to avoid a copy.
        let mut content = "";
        content <-> self.content;
        let len = content.len();

        alt content[0] as char {
          '!' {
            // ignore comments
            self.eat_whitespace();
          }
          '&' {
            let name = content.slice(1u, len);
            let name = self.check_content(name);
            let name = @str::split_char_nonempty(name, '.');
            self.tokens.push(utag(name, tag));
          }
          '{' {
            if str::ends_with(content, "}") {
                let name = content.slice(1u, len - 1u);
                let name = self.check_content(name);
                let name = @str::split_char_nonempty(name, '.');
                self.tokens.push(utag(name, tag));
            } else { fail "unbalanced \"{\" in tag"; }
          }
          '#' {
            let newlined = self.eat_whitespace();

            let name = self.check_content(content.slice(1u, len));
            let name = @str::split_char_nonempty(name, '.');
            self.tokens.push(incomplete_section(name, false, tag, newlined));
          }
          '^' {
            let newlined = self.eat_whitespace();

            let name = self.check_content(content.slice(1u, len));
            let name = @str::split_char_nonempty(name, '.');
            self.tokens.push(incomplete_section(name, true, tag, newlined));
          }
          '/' {
            self.eat_whitespace();

            let name = self.check_content(content.slice(1u, len));
            let name = @str::split_char_nonempty(name, '.');
            let mut children = ~[];

            loop {
                if self.tokens.len() == 0u {
                    fail "closing unopened section";
                }

                let last = self.tokens.pop();

                alt last {
                  incomplete_section(section_name, inverted, osection, _) {
                    let children = vec::reversed(children);

                    // Collect all the children's sources.
                    let srcs = dvec();
                    for children.each |child: token| {
                        alt child {
                          text(s)
                          | etag(_, s)
                          | utag(_, s)
                          | partial(_, _, s) {
                            srcs.push(s);
                          }
                          section(_, _, _, _, osection, src, csection, _) {
                            srcs.push(osection);
                            srcs.push(src);
                            srcs.push(csection);
                          }
                          _ { fail; }
                        }
                    }

                    if section_name == name {
                        // Cache the combination of all the sources in the
                        // section. It's unfortunate, but we need to do this in
                        // case the user uses a function to instantiate the
                        // tag.
                        let mut src = "";
                        for srcs.each |s| { src += *s; }

                        self.tokens.push(
                            section(
                                name,
                                inverted,
                                @children,
                                self.otag,
                                osection,
                                @src,
                                tag,
                                self.ctag));
                        break;
                    } else {
                        fail "Unclosed section";
                    }
                  }
                  _ { vec::push(children, last); }
                }
            }
          }
          '>' { self.add_partial(content, tag); }
          '=' {
            self.eat_whitespace();

            if (len > 2u && str::ends_with(content, "=")) {
                let s = self.check_content(content.slice(1u, len - 1u));

                let pos = str::find_from(s, 0u, char::is_whitespace);
                let pos = alt pos {
                  none { fail "invalid change delimiter tag content"; }
                  some(pos) { pos }
                };

                self.otag = @s.slice(0u, pos);
                self.otag_chars = str::chars(*self.otag);

                let pos = str::find_from(s, pos, |c| !char::is_whitespace(c));
                let pos = alt pos {
                  none { fail "invalid change delimiter tag content"; }
                  some(pos) { pos }
                };

                self.ctag = @s.slice(pos, s.len());
                self.ctag_chars = str::chars(*self.ctag);
            } else {
                fail "invalid change delimiter tag content";
            }
          }
          _ {
            let name = self.check_content(content);
            let name = @str::split_char_nonempty(name, '.');
            self.tokens.push(etag(name, tag));
          }
        }
    }

    fn add_partial(content: str, tag: @str) {
        let token_class = self.classify_token();
        let indent = alt token_class {
          normal { "" }
          standalone {
            if self.ch == '\r' { self.bump(); }
            self.bump();
            ""
          }
          whitespace(s, pos) | newline_whitespace(s, pos) {
            if self.ch == '\r' { self.bump(); }
            self.bump();

            let ws = (*s).slice(pos, (*s).len());

            // Trim the whitespace from the last token.
            self.tokens.pop();
            self.tokens.push(text(@(*s).slice(0u, pos)));

            ws
          }
        };

        // We can't inline the tokens directly as we may have a recursive
        // partial. So instead, we'll cache the partials we used and look them
        // up later.
        let name = content.slice(1u, content.len());
        let name = @self.check_content(name);

        self.tokens.push(partial(name, @indent, tag));
        self.partials.push(name);
    }

    fn not_otag() {
        let mut i = 0u;
        while i < self.tag_position {
            unsafe { str::push_char(self.content, self.otag_chars[i]) };
            i += 1u;
        }
    }

    fn not_ctag() {
        let mut i = 0u;
        while i < self.tag_position {
            unsafe { str::push_char(self.content, self.ctag_chars[i]) };
            i += 1u;
        }
    }

    fn check_content(content: str) -> str {
        let trimmed = content.trim();
        if trimmed.len() == 0u {
            fail "empty tag";
        }
        trimmed
    }
}

type compile_context = {
    rdr: io::reader,
    partials: hashmap<str, @~[token]>,
    otag: @str,
    ctag: @str,
    template_path: @str,
    template_extension: @str,
};

fn compile_helper(ctx: compile_context) -> @~[token] {
    let parser: parser = {
        rdr: ctx.rdr,
        mut ch: ctx.rdr.read_char(),
        mut lookahead: none,
        mut line: 1u,
        mut col: 1u,
        mut content: "",
        mut state: parser::TEXT,
        mut otag: ctx.otag,
        mut ctag: ctx.ctag,
        mut otag_chars: str::chars(*ctx.otag),
        mut ctag_chars: str::chars(*ctx.ctag),
        mut tag_position: 0u,
        tokens: dvec(),
        partials: dvec(),
    };

    parser.parse();

    // Compile the partials if we haven't done so already.
    for parser.partials.each |name| {
        let path = path::connect(*ctx.template_path,
                                 *name + *ctx.template_extension);

        if !ctx.partials.contains_key(*name) {
            // Insert a placeholder so we don't recurse off to infinity.
            ctx.partials.insert(copy *name, @~[]);

            alt io::file_reader(path) {
              err(e) {}
              ok(rdr) {
                let tokens = compile_helper({
                    rdr: rdr,
                    partials: ctx.partials,
                    otag: @"{{",
                    ctag: @"}}",
                    template_path: ctx.template_path,
                    template_extension: ctx.template_extension
                });

                ctx.partials.insert(copy *name, tokens);
              }
            }
        }
    }

    // Destructure the parser so we get get at the tokens without a copy.
    let { tokens, _ } = parser;

    @vec::from_mut(dvec::unwrap(tokens))
}

type render_context = {
    ctx: context,
    tokens: @~[token],
    partials: hashmap<str, @~[token]>,
    stack: ~[data],
    indent: @str,
};

fn render_helper(ctx: render_context) -> str {
    fn find(stack: ~[data], path: ~[str]) -> option<data> {
        // If we have an empty path, we just want the top value in our stack.
        if vec::is_empty(path) {
            ret alt vec::last_opt(stack) {
              none { none }
              some(value) { some(value) }
            };
        }

        // Otherwise, find the stack that has the first part of our path.
        let mut value = none;

        let mut i = vec::len(stack);
        while i > 0u {
            alt stack[i - 1u] {
              map(ctx) {
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
        let mut value = value;

        let mut i = 1u;
        let len = vec::len(path);

        while i < len {
            alt copy value {
              some(map(v)) { value = v.find(path[i]); }
              _ { break; }
            }
            i += 1u;
        }

        value
    }

    let mut output = "";
    
    for (*ctx.tokens).each |token| {
        alt token {
          text(value) {
            // Indent the lines.
            if *ctx.indent == "" {
                output += *value;
            } else {
                let mut pos = 0u;
                let len = (*value).len();

                while pos < len {
                    let line = alt str::find_char_from(*value, '\n', pos) {
                      none {
                        let line = (*value).slice(pos, len);
                        pos = len;
                        line
                      }
                      some(i) {
                        let line = (*value).slice(pos, i + 1u);
                        pos = i + 1u;
                        line
                      }
                    };

                    if str::char_at(line, 0u) != '\n' {
                        output += *ctx.indent;
                    }

                    output += line;
                }
            }
          }
          etag(path, _) {
            alt find(ctx.stack, *path) {
              none { }
              some(value) {
                output += *ctx.indent + render_etag(value, ctx);
              }
            }
          }
          utag(path, _) {
            alt find(ctx.stack, *path) {
              none { }
              some(value) {
                output += *ctx.indent + render_utag(value, ctx);
              }
            }
          }
          section(path, true, children, _, _, _, _, _) {
            let ctx = { tokens: children with ctx };

            output += alt find(ctx.stack, *path) {
              none { render_helper(ctx) }
              some(value) { render_inverted_section(value, ctx) }
            };
          }
          section(path, false, children, otag, _, src, _, ctag) {
            alt find(ctx.stack, *path) {
              none { }
              some(value) {
                output += render_section(value, src, otag, ctag, {
                    tokens: children
                    with ctx
                });
              }
            }
          }
          partial(name, ind, _) {
            alt ctx.partials.find(*name) {
              none { }
              some(tokens) {
                output += render_helper({
                    tokens: tokens,
                    indent: @(*ctx.indent + *ind)
                    with ctx
                });
              }
            }
          }
          _ { fail }
        };
    };

    output
}

fn render_etag(value: data, ctx: render_context) -> str {
    let mut escaped = "";
    do str::chars_iter(render_utag(value, ctx)) |c| {
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

fn render_utag(value: data, ctx: render_context) -> str {
    alt value {
      str(s) { copy *s }
      fun(f) {
          // etags and utags use the default delimiter.
          render_fun(ctx, @"", @"{{", @"}}", f)
      }
      _ { fail }
    }
}

fn render_inverted_section(value: data, ctx: render_context) -> str {
    alt value {
      bool(false) { render_helper(ctx) }
      vec(xs) if (*xs).is_empty() { render_helper(ctx) }
      _ { "" }
    }
}

fn render_section(value: data,
                  src: @str,
                  otag: @str,
                  ctag: @str,
                  ctx: render_context) -> str {
    alt value {
      bool(true) { render_helper(ctx) }
      bool(false) { "" }
      vec(vs) {
        str::concat(do (*vs).map |v| {
            render_helper({ stack: ctx.stack + ~[v] with ctx })
        })
      }
      map(_) { render_helper({ stack: ctx.stack + ~[value] with ctx }) }
      fun(f) { render_fun(ctx, src, otag, ctag, f) }
      _ { fail }
    }
}

fn render_fun(ctx: render_context,
              src: @str,
              otag: @str,
              ctag: @str,
              f: fn(@str) -> str) -> str {
    let tokens = do io::with_str_reader(f(src)) |rdr| {
        compile_helper({
            rdr: rdr,
            partials: ctx.partials,
            otag: otag,
            ctag: ctag,
            template_path: ctx.ctx.template_path,
            template_extension: ctx.ctx.template_extension
        })
    };

    render_helper({ tokens: tokens with ctx })
}

#[cfg(test)]
mod tests {
    import std::json;
    import std::json::to_str;

    #[test]
    fn test_compile_texts() {
        assert compile_str("hello world").tokens == @~[text(@"hello world")];
        assert compile_str("hello {world").tokens == @~[text(@"hello {world")];
        assert compile_str("hello world}").tokens == @~[text(@"hello world}")];
        assert compile_str("hello world}}").tokens == @~[text(@"hello world}}")];
    }

    #[test]
    fn test_compile_etags() {
        assert compile_str("{{ name }}").tokens == @~[
            etag(@~["name"], @"{{ name }}")
        ];

        assert compile_str("before {{name}} after").tokens == @~[
            text(@"before "),
            etag(@~["name"], @"{{name}}"),
            text(@" after")
        ];

        assert compile_str("before {{name}}").tokens == @~[
            text(@"before "),
            etag(@~["name"], @"{{name}}")
        ];

        assert compile_str("{{name}} after").tokens == @~[
            etag(@~["name"], @"{{name}}"),
            text(@" after")
        ];
    }

    #[test]
    fn test_compile_utags() {
        assert compile_str("{{{name}}}").tokens == @~[
            utag(@~["name"], @"{{{name}}}")
        ];

        assert compile_str("before {{{name}}} after").tokens == @~[
            text(@"before "),
            utag(@~["name"], @"{{{name}}}"),
            text(@" after")
        ];

        assert compile_str("before {{{name}}}").tokens == @~[
            text(@"before "),
            utag(@~["name"], @"{{{name}}}")
        ];

        assert compile_str("{{{name}}} after").tokens == @~[
            utag(@~["name"], @"{{{name}}}"),
            text(@" after")
        ];
    }

    #[test]
    fn test_compile_sections() {
        assert compile_str("{{# name}}{{/name}}").tokens == @~[
            section(
                @~["name"],
                false,
                @~[],
                @"{{",
                @"{{# name}}",
                @"",
                @"{{/name}}",
                @"}}"
            )
        ];

        assert compile_str("before {{^name}}{{/name}} after").tokens == @~[
            text(@"before "),
            section(
                @~["name"],
                true,
                @~[],
                @"{{",
                @"{{^name}}",
                @"",
                @"{{/name}}",
                @"}}"
            ),
            text(@" after")
        ];

        assert compile_str("before {{#name}}{{/name}}").tokens == @~[
            text(@"before "),
            section(
                @~["name"],
                false,
                @~[],
                @"{{",
                @"{{#name}}",
                @"",
                @"{{/name}}",
                @"}}"
            )
        ];

        assert compile_str("{{#name}}{{/name}} after").tokens == @~[
            section(
                @~["name"],
                false,
                @~[],
                @"{{",
                @"{{#name}}",
                @"",
                @"{{/name}}",
                @"}}"
            ),
            text(@" after")
        ];

        assert compile_str(
                "before {{#a}} 1 {{^b}} 2 {{/b}} {{/a}} after").tokens == @~[
            text(@"before "),
            section(
                @~["a"],
                false,
                @~[
                    text(@" 1 "),
                    section(
                        @~["b"],
                        true,
                        @~[text(@" 2 ")],
                        @"{{",
                        @"{{^b}}",
                        @" 2 ",
                        @"{{/b}}",
                        @"}}"
                    ),
                    text(@" ")
                ],
                @"{{",
                @"{{#a}}",
                @" 1 {{^b}} 2 {{/b}} ",
                @"{{/a}}",
                @"}}"
            ),
            text(@" after")
        ];
    }

    #[test]
    fn test_compile_partials() {
        assert compile_str("{{> test}}").tokens == @~[
            partial(@"test", @"", @"{{> test}}")
        ];

        assert compile_str("before {{>test}} after").tokens == @~[
            text(@"before "),
            partial(@"test", @"", @"{{>test}}"),
            text(@" after")
        ];

        assert compile_str("before {{> test}}").tokens == @~[
            text(@"before "),
            partial(@"test", @"", @"{{> test}}")
        ];

        assert compile_str("{{>test}} after").tokens == @~[
            partial(@"test", @"", @"{{>test}}"),
            text(@" after")
        ];
    }

    #[test]
    fn test_compile_delimiters() {
        assert compile_str("before {{=<% %>=}}<%name%> after").tokens == @~[
            text(@"before "),
            etag(@~["name"], @"<%name%>"),
            text(@" after")
        ];
    }

    #[test]
    fn test_render_texts() {
        let ctx = str_hash();
        ctx.insert("name", str(@"world"));

        assert render_str("hello world", ctx) == "hello world";
        assert render_str("hello {world", ctx) == "hello {world";
        assert render_str("hello world}", ctx) == "hello world}";
        assert render_str("hello {world}", ctx) == "hello {world}";
        assert render_str("hello world}}", ctx) == "hello world}}";
    }

    #[test]
    fn test_render_etags() {
        let ctx = str_hash();
        ctx.insert("name", str(@"world"));

        assert render_str("hello {{name}}", ctx) == "hello world";
    }

    #[test]
    fn test_render_utags() {
        let ctx = str_hash();
        ctx.insert("name", str(@"world"));

        assert render_str("hello {{{name}}}", ctx) == "hello world";
    }

    #[test]
    fn test_render_sections() {
        let ctx0 = str_hash();
        let template = compile_str("0{{#a}}1 {{n}} 3{{/a}}5");

        assert template.render(ctx0) == "05";

        ctx0.insert("a", vec(@~[]));
        assert template.render(ctx0) == "05";

        let ctx1: hashmap<str, data> = str_hash();
        ctx0.insert("a", (~[ctx1]).to_mustache());

        assert template.render(ctx0) == "01  35";

        let ctx1 = str_hash();
        ctx1.insert("n", "a".to_mustache());
        ctx0.insert("a", (~[ctx1]).to_mustache());
        assert template.render(ctx0) == "01 a 35";

        ctx0.insert("a", { |_text| "foo" }.to_mustache());
        assert template.render(ctx0) == "0foo5";
    }

    #[test]
    fn test_render_inverted_sections() {
        let template = "0{{^a}}1 3{{/a}}5";

        let ctx0 = str_hash();
        assert render_str(template, ctx0) == "01 35";

        ctx0.insert("a", vec(@~[]));
        assert render_str(template, ctx0) == "01 35";

        let ctx1 = str_hash();
        ctx0.insert("a", (~[ctx1]).to_mustache());
        assert render_str(template, ctx0) == "05";

        ctx1.insert("n", "a".to_mustache());
        assert render_str(template, ctx0) == "05";
    }

    #[test]
    fn test_render_partial() {
        let path = "base";

        let ctx0 = str_hash();
        assert render_file(path, ctx0) == "<h2>Names</h2>\n";

        ctx0.insert("names", vec(@~[]));
        assert render_file(path, ctx0) == "<h2>Names</h2>\n";

        let ctx1 = str_hash();
        ctx0.insert("names", vec(@~[map(ctx1)]));
        assert render_file(path, ctx0) ==
            "<h2>Names</h2>\n" +
            "  <strong></strong>\n\n";

        ctx1.insert("name", str(@"a"));
        assert render_file(path, ctx0) ==
            "<h2>Names</h2>\n" +
            "  <strong>a</strong>\n\n";

        let ctx2 = str_hash();
        ctx2.insert("name", str(@"<b>"));
        ctx0.insert("names", vec(@~[map(ctx1), map(ctx2)]));
        assert render_file(path, ctx0) ==
            "<h2>Names</h2>\n" +
            "  <strong>a</strong>\n\n" +
            "  <strong>&lt;b&gt;</strong>\n\n";
    }

    fn parse_spec_tests(src: str) -> @~[json::json] {
        alt io::read_whole_file_str(src) {
          err(e) { fail e }
          ok(s) {
            alt json::from_str(s) {
              err(e) { fail e.to_str() }
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

    fn convert_json_map(map: hashmap<str, json::json>) -> hashmap<str, data> {
        let d = str_hash();
        for map.each |key, value| { d.insert(copy key, convert_json(value)); }
        d
    }

    fn convert_json(value: json::json) -> data {
        alt value {
          json::num(n) {
            // We have to cheat and use %? because %f doesn't convert 3.3 to
            // 3.3.
            str(@#fmt("%?", n))
          }
          json::string(s) { str(s) }
          json::boolean(b) { bool(b) }
          json::list(v) { vec(@(*v).map(convert_json)) }
          json::dict(d) { map(convert_json_map(d)) }
          _ { fail #fmt("%?", value) }
        }
    }

    fn write_partials(value: json::json) -> ~[str] {
        let mut files = ~[];

        alt value {
          json::dict(d) {
            for d.each |key, value| {
                alt value {
                  json::string(s) {
                    let file = key + ".mustache";
                    alt io::file_writer(file, ~[io::create, io::truncate]) {
                      ok(wr) { vec::push(files, file); wr.write_str(*s); }
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

    fn run_test(test: hashmap<str, json::json>, data: hashmap<str, data>) {
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
          none { ~[] }
        };

        let result = render_str(*template, data);

        if result != *expected {
            fn to_list(x: json::json) -> json::json {
                alt x {
                  json::dict(d) {
                    let mut xs = ~[];
                    for d.each |k, v| {
                        let k = json::string(@copy k);
                        let v = to_list(v);
                        vec::push(xs, json::list(@~[k, v]));
                    }
                    json::list(@xs)
                  }
                  json::list(xs) { json::list(@vec::map(*xs, to_list)) }
                  _ { x }
                }
            }

            io::println(#fmt("desc:     %?", test.get("desc")));
            io::println(#fmt("context:  %?", to_list(test.get("data"))));
            io::println(#fmt("partials: %?", partials));
            io::println(#fmt("partials: %?", test.find("partials")));
            io::println("");
            io::println(#fmt("template:\n%?", template));
            io::println(#fmt("expected:\n%?", expected));
            io::println(#fmt("result:  \n%?", result));
            io::println("");
            io::println(#fmt("template:\n%s", *template));
            io::println(#fmt("expected:\n%s", *expected));
            io::println(#fmt("result:  \n%s", result));
        }
        assert result == *expected;

        for partials.each |file| { os::remove_file(file); }
    }

    fn run_tests(spec: str) {
        for (*parse_spec_tests(spec)).each |json| {
            let test = alt json {
              json::dict(m) { m }
              _ { fail }
            };

            let data = alt test.get("data") {
              json::dict(m) { convert_json_map(m) }
              _ { fail }
            };

            run_test(test, data);
        }
    }

    #[test]
    fn test_spec_comments() {
        run_tests("spec/specs/comments.json");
    }

    #[test]
    fn test_spec_delimiters() {
        run_tests("spec/specs/delimiters.json");
    }

    #[test]
    fn test_spec_interpolation() {
        run_tests("spec/specs/interpolation.json");
    }

    #[test]
    fn test_spec_inverted() {
        run_tests("spec/specs/inverted.json");
    }

    #[test]
    fn test_spec_partials() {
        run_tests("spec/specs/partials.json");
    }

    #[test]
    fn test_spec_sections() {
        run_tests("spec/specs/sections.json");
    }

    #[test]
    fn test_spec_lambdas() {
        for (*parse_spec_tests("spec/specs/~lambdas.json")).each |json| {
            let test = alt json {
              json::dict(m) { m }
              _ { fail }
            };

            // Replace the lambda with rust code.
            let ctx = alt test.get("data") {
              json::dict(m) { convert_json_map(m) }
              _ { fail }
            };

            let f = alt test.get("name") {
              json::string(@"Interpolation") {
                  |_text| {"world" }
              }
              json::string(@"Interpolation - Expansion") {
                  |_text| {"{{planet}}" }
              }
              json::string(@"Interpolation - Alternate Delimiters") {
                  |_text| {"|planet| => {{planet}}" }
              }
              json::string(@"Interpolation - Multiple Calls") {
                  let calls = @mut 0;
                  |_text| {*calls += 1; int::str(*calls) }
              }
              json::string(@"Escaping") {
                  |_text| {">" }
              }
              json::string(@"Section") {
                  |text: @str| {if *text == "{{x}}" { "yes" } else { "no" } }
              }
              json::string(@"Section - Expansion") {
                  |text: @str| {*text + "{{planet}}" + *text }
              }
              json::string(@"Section - Alternate Delimiters") {
                  |text: @str| {*text + "{{planet}} => |planet|" + *text }
              }
              json::string(@"Section - Multiple Calls") {
                  |text: @str| {"__" + *text + "__" }
              }
              json::string(@"Inverted Section") {
                  |_text| {"" }
              }
              value { fail #fmt("%?", value) }
            };

            ctx.insert("lambda", fun(f));

            run_test(test, ctx);
        }
    }
}
