use std;

export token;
export compile;

enum token {
    text(str),
    etag(str),
    utag(str),
    section({ name: str, inverted: bool, children: @[token] }),
    incomplete_section(str, bool),
    partial(str),
}

enum state { TEXT, OTAG, TAG, CTAG }

type parser = {
    mutable state: state,
    mutable otag: str,
    mutable ctag: str,
    mutable content: str,
    mutable tag_position: uint,
    mutable tokens: [token],
};

fn compile(src: str) -> [token] {
    let parser : parser = {
        mutable state: TEXT,
        mutable otag: "{{",
        mutable ctag: "}}",
        mutable content: "",
        mutable tag_position: 0u,
        mutable tokens: [],
    };

    let curly_brace_tag = false;

    str::bytes_iter(src) { |c|
        alt parser.state {
          TEXT {
            if c == parser.otag[0] {
                if str::byte_len(parser.otag) > 1u {
                    parser.tag_position = 1u;
                    parser.state = OTAG;
                } else {
                    add_text(parser);
                    parser.state = TAG;
                }
            } else {
                str::push_byte(parser.content, c);
            }
          }
          OTAG {
            if c == parser.otag[parser.tag_position] {
                if parser.tag_position == str::byte_len(parser.otag) - 1u {
                    add_text(parser);
                    curly_brace_tag = false;
                    parser.state = TAG;
                } else {
                    parser.tag_position += 1u;
                }
            } else {
                // We don't have a tag, so add all the tag parts we've seen
                // so far to the string.
                parser.state = TEXT;
                not_otag(parser);
                str::push_byte(parser.content, c);
            }
          }
          TAG {
              if parser.content == "" && c == '{' as u8 {
                  curly_brace_tag = true;
                  str::push_byte(parser.content, c);
              } else if curly_brace_tag && c == '}' as u8 {
                  curly_brace_tag = false;
                  str::push_byte(parser.content, c);
              } else if c == parser.ctag[0u] {
                  if str::byte_len(parser.ctag) > 1u {
                      parser.tag_position = 1u;
                      parser.state = CTAG;
                  } else {
                      add_tag(parser);
                      parser.state = TEXT;
                  }
              } else {
                  str::push_byte(parser.content, c);
              }
          }
          CTAG {
              if c == parser.ctag[parser.tag_position] {
                  if parser.tag_position == str::byte_len(parser.ctag) - 1u {
                      add_tag(parser);
                      parser.state = TEXT;
                  } else {
                      parser.state = TAG;
                      not_ctag(parser);
                      str::push_byte(parser.content, c);
                  }
              }
          }
      }
    };

    alt parser.state {
      TEXT { add_text(parser) }
      OTAG { not_otag(parser); add_text(parser) }
      TAG { fail "unclosed tag" }
      CTAG { not_ctag(parser); add_text(parser) }
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

fn add_text(parser: parser) {
    if parser.content != "" {
        vec::push(parser.tokens, text(parser.content));
        parser.content = "";
    }
}

fn add_tag(parser: parser) {
    let content = parser.content;
    let content_len = str::byte_len(content);

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
                                children: @vec::reversed(children),
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
              let tags = str::splitn(s, ' ' as u8, 2u);

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

fn not_otag(parser: parser) {
    let i = 0u;
    while i < parser.tag_position {
        str::push_byte(parser.content, parser.otag[i]);
        i += 1u;
    }
}

fn not_ctag(parser: parser) {
    let i = 0u;
    while i < parser.tag_position {
        str::push_byte(parser.content, parser.ctag[i]);
        i += 1u;
    }
}

fn check_content(content: str) -> str {
    let trimmed = str::trim(content);
    if str::byte_len(trimmed) == 0u {
        fail "empty tag";
    }
    trimmed
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
            section({name: "name", inverted: false, children: @[]})
        ];

        assert compile("before {{^name}}{{/name}} after") == [
            text("before "),
            section({name: "name", inverted: true, children: @[]}),
            text(" after")
        ];

        assert compile("before {{#name}}{{/name}}") == [
            text("before "),
            section({name: "name", inverted: false, children: @[]})
        ];

        assert compile("{{#name}}{{/name}} after") == [
            section({name: "name", inverted: false, children: @[]}),
            text(" after")
        ];

        assert compile("before {{#a}} 1 {{^b}} 2 {{/b}} {{/a}} after") == [
            text("before "),
            section({name: "a", inverted: false, children: @[
                text(" 1 "),
                section({name: "b", inverted: true, children: @[
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
}
