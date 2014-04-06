use std::io::{File, FileNotFound};
use std::str;
use collections::HashMap;

use parser::{Parser, Token};
use super::Context;

pub struct Compiler<T> {
    ctx: Context,
    reader: T,
    partials: HashMap<~str, Vec<Token>>,
    otag: ~str,
    ctag: ~str,
}

impl<T: Iterator<char>> Compiler<T> {
    /// Construct a default compiler.
    pub fn new(ctx: Context, reader: T) -> Compiler<T> {
        Compiler {
            ctx: ctx,
            reader: reader,
            partials: HashMap::new(),
            otag: ~"{{",
            ctag: ~"}}",
        }
    }

    /// Construct a default compiler.
    pub fn new_with(
        ctx: Context,
        reader: T,
        partials: HashMap<~str, Vec<Token>>,
        otag: ~str,
        ctag: ~str
    ) -> Compiler<T> {
        Compiler {
            ctx: ctx,
            reader: reader,
            partials: partials,
            otag: otag,
            ctag: ctag,
        }
    }

    /// Compiles a template into a series of tokens.
    pub fn compile(mut self) -> (Vec<Token>, HashMap<~str, Vec<Token>>) {
        let (tokens, partials) = {
            let parser = Parser::new(&mut self.reader, self.otag, self.ctag);
            parser.parse()
        };

        // Compile the partials if we haven't done so already.
        for name in partials.move_iter() {
            let path = self.ctx.template_path.join(name + "." + self.ctx.template_extension);

            if !self.partials.contains_key(&name) {
                // Insert a placeholder so we don't recurse off to infinity.
                self.partials.insert(name.to_owned(), Vec::new());

                match File::open(&path).read_to_end() {
                    Ok(contents) => {
                        let string = match str::from_utf8_owned(contents) {
                            Some(string) => string,
                            None => { fail!("Failed to parse file as UTF-8"); }
                        };

                        let compiler = Compiler {
                            ctx: self.ctx.clone(),
                            reader: string.chars(),
                            partials: self.partials.clone(),
                            otag: ~"{{",
                            ctag: ~"}}",
                        };

                        let (tokens, _) = compiler.compile();

                        self.partials.insert(name, tokens);
                    },
                    Err(e) => {
                        // Ignore missing files.
                        if e.kind == FileNotFound {
                            debug!("failed to read file {}", path.display());
                        } else {
                            fail!("error reading file: {}", e);
                        }
                    }
                }
            }
        }

        let Compiler { partials, .. } = self;

        (tokens, partials)
    }
}

#[cfg(test)]
mod tests {
    use parser::{Token, Text, ETag, UTag, Section, IncompleteSection, Partial};
    use super::Compiler;
    use super::super::Context;

    fn compile_str(template: &str) -> Vec<Token> {
        let ctx = Context::new(Path::new("."));
        let (tokens, _) = Compiler::new(ctx, template.chars()).compile();
        tokens
    }

    fn token_to_str(token: &Token) -> ~str {
        match *token {
            // recursive enums crash %?
            Section(ref name,
                    inverted,
                    ref children,
                    ref otag,
                    ref osection,
                    ref src,
                    ref tag,
                    ref ctag) => {
                let name = name.iter().map(|e| format!("{:?}", *e)).collect::<Vec<~str>>();
                let children = children.iter().map(|x| token_to_str(x)).collect::<Vec<~str>>();
                format!("Section(vec!({}), {:?}, vec!({}), {:?}, {:?}, {:?}, {:?}, {:?})",
                        name.connect(", "),
                        inverted,
                        children.connect(", "),
                        otag,
                        osection,
                        src,
                        tag,
                        ctag)
            }
            ETag(ref name, ref tag) => {
                let name = name.iter().map(|e| format!("{:?}", *e)).collect::<Vec<~str>>();
                format!("ETag(vec!({:?}), {:?})", name.connect(", "), *tag)
            }
            UTag(ref name, ref tag) => {
                let name = name.iter().map(|e| format!("{:?}", *e)).collect::<Vec<~str>>();
                format!("UTag(vec!({:?}), {:?})", name.connect(", "), *tag)
            }
            IncompleteSection(ref name, ref inverted, ref osection, ref newlined) => {
                let name = name.iter().map(|e| format!("{:?}", *e)).collect::<Vec<~str>>();
                format!("IncompleteSection(vec!({:?}), {:?}, {:?}, {:?})",
                        name.connect(", "),
                        *inverted,
                        *osection,
                        *newlined)
            }
            _ => {
                format!("{:?}", token)
            }
        }
    }

    fn check_tokens(actual: Vec<Token>, expected: &[Token]) {
        // TODO: equality is currently broken for enums
        let actual: Vec<~str> = actual.iter().map(token_to_str).collect();
        let expected = expected.iter().map(token_to_str).collect();

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_compile_texts() {
        check_tokens(compile_str("hello world"), [
            Text(~"hello world")
        ]);
        check_tokens(compile_str("hello {world"), [
            Text(~"hello {world")
        ]);
        check_tokens(compile_str("hello world}"), [
            Text(~"hello world}")
        ]);
        check_tokens(compile_str("hello world}}"), [
            Text(~"hello world}}")
        ]);
    }

    #[test]
    fn test_compile_etags() {
        check_tokens(compile_str("{{ name }}"), [
            ETag(vec!(~"name"), ~"{{ name }}")
        ]);

        check_tokens(compile_str("before {{name}} after"), [
            Text(~"before "),
            ETag(vec!(~"name"), ~"{{name}}"),
            Text(~" after")
        ]);

        check_tokens(compile_str("before {{name}}"), [
            Text(~"before "),
            ETag(vec!(~"name"), ~"{{name}}")
        ]);

        check_tokens(compile_str("{{name}} after"), [
            ETag(vec!(~"name"), ~"{{name}}"),
            Text(~" after")
        ]);
    }

    #[test]
    fn test_compile_utags() {
        check_tokens(compile_str("{{{name}}}"), [
            UTag(vec!(~"name"), ~"{{{name}}}")
        ]);

        check_tokens(compile_str("before {{{name}}} after"), [
            Text(~"before "),
            UTag(vec!(~"name"), ~"{{{name}}}"),
            Text(~" after")
        ]);

        check_tokens(compile_str("before {{{name}}}"), [
            Text(~"before "),
            UTag(vec!(~"name"), ~"{{{name}}}")
        ]);

        check_tokens(compile_str("{{{name}}} after"), [
            UTag(vec!(~"name"), ~"{{{name}}}"),
            Text(~" after")
        ]);
    }

    #[test]
    fn test_compile_sections() {
        check_tokens(compile_str("{{# name}}{{/name}}"), [
            Section(
                vec!(~"name"),
                false,
                Vec::new(),
                ~"{{",
                ~"{{# name}}",
                ~"",
                ~"{{/name}}",
                ~"}}"
            )
        ]);

        check_tokens(compile_str("before {{^name}}{{/name}} after"), [
            Text(~"before "),
            Section(
                vec!(~"name"),
                true,
                Vec::new(),
                ~"{{",
                ~"{{^name}}",
                ~"",
                ~"{{/name}}",
                ~"}}"
            ),
            Text(~" after")
        ]);

        check_tokens(compile_str("before {{#name}}{{/name}}"), [
            Text(~"before "),
            Section(
                vec!(~"name"),
                false,
                Vec::new(),
                ~"{{",
                ~"{{#name}}",
                ~"",
                ~"{{/name}}",
                ~"}}"
            )
        ]);

        check_tokens(compile_str("{{#name}}{{/name}} after"), [
            Section(
                vec!(~"name"),
                false,
                Vec::new(),
                ~"{{",
                ~"{{#name}}",
                ~"",
                ~"{{/name}}",
                ~"}}"
            ),
            Text(~" after")
        ]);

        check_tokens(compile_str(
                "before {{#a}} 1 {{^b}} 2 {{/b}} {{/a}} after"), [
            Text(~"before "),
            Section(
                vec!(~"a"),
                false,
                vec!(
                    Text(~" 1 "),
                    Section(
                        vec!(~"b"),
                        true,
                        vec!(Text(~" 2 ")),
                        ~"{{",
                        ~"{{^b}}",
                        ~" 2 ",
                        ~"{{/b}}",
                        ~"}}"
                    ),
                    Text(~" ")
                ),
                ~"{{",
                ~"{{#a}}",
                ~" 1 {{^b}} 2 {{/b}} ",
                ~"{{/a}}",
                ~"}}"
            ),
            Text(~" after")
        ]);
    }

    #[test]
    fn test_compile_partials() {
        check_tokens(compile_str("{{> test}}"), [
            Partial(~"test", ~"", ~"{{> test}}")
        ]);

        check_tokens(compile_str("before {{>test}} after"), [
            Text(~"before "),
            Partial(~"test", ~"", ~"{{>test}}"),
            Text(~" after")
        ]);

        check_tokens(compile_str("before {{> test}}"), [
            Text(~"before "),
            Partial(~"test", ~"", ~"{{> test}}")
        ]);

        check_tokens(compile_str("{{>test}} after"), [
            Partial(~"test", ~"", ~"{{>test}}"),
            Text(~" after")
        ]);
    }

    #[test]
    fn test_compile_delimiters() {
        check_tokens(compile_str("before {{=<% %>=}}<%name%> after"), [
            Text(~"before "),
            ETag(vec!(~"name"), ~"<%name%>"),
            Text(~" after")
        ]);
    }
}
