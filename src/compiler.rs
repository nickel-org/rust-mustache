use std::collections::HashMap;
use std::io::ErrorKind::NotFound;
use std::io::Read;
use std::fs::File;

use parser_internals::{Parser, Token};
use super::Context;

use Result;

pub type PartialsMap = HashMap<String, Vec<Token>>;

/// `Compiler` is a object that compiles a string into a `Vec<Token>`.
pub struct Compiler<T> {
    ctx: Context,
    reader: T,
    partials: PartialsMap,
    otag: String,
    ctag: String,
}

impl<T: Iterator<Item = char>> Compiler<T> {
    /// Construct a default compiler.
    pub fn new(ctx: Context, reader: T) -> Compiler<T> {
        Compiler {
            ctx: ctx,
            reader: reader,
            partials: HashMap::new(),
            otag: "{{".to_string(),
            ctag: "}}".to_string(),
        }
    }

    /// Construct a default compiler.
    pub fn new_with(ctx: Context,
                    reader: T,
                    partials: PartialsMap,
                    otag: String,
                    ctag: String)
                    -> Compiler<T> {
        Compiler {
            ctx: ctx,
            reader: reader,
            partials: partials,
            otag: otag,
            ctag: ctag,
        }
    }

    /// Compiles a template into a series of tokens.
    pub fn compile(mut self) -> Result<(Vec<Token>, PartialsMap)> {
        let (tokens, partials) = {
            let parser = Parser::new(&mut self.reader, &self.otag, &self.ctag);
            try!(parser.parse())
        };

        // Compile the partials if we haven't done so already.
        for name in partials.into_iter() {
            let path =
                self.ctx.template_path.join(&(name.clone() + "." + &self.ctx.template_extension));

            if !self.partials.contains_key(&name) {
                // Insert a placeholder so we don't recurse off to infinity.
                self.partials.insert(name.to_string(), Vec::new());

                match File::open(&path) {
                    Ok(mut file) => {
                        let mut string = String::new();
                        try!(file.read_to_string(&mut string));

                        let compiler = Compiler {
                            ctx: self.ctx.clone(),
                            reader: string.chars(),
                            partials: self.partials.clone(),
                            otag: "{{".to_string(),
                            ctag: "}}".to_string(),
                        };

                        let (tokens, subpartials) = try!(compiler.compile());

                        // Include subpartials
                        self.partials.extend(subpartials.into_iter());

                        // Set final compiled tokens for *this* partial
                        self.partials.insert(name, tokens);
                    }
                    // Ignore missing files.
                    Err(ref e) if e.kind() == NotFound => {},
                    Err(e) => return Err(e.into()),
                }
            }
        }

        let Compiler { partials, .. } = self;

        Ok((tokens, partials))
    }
}

#[cfg(test)]
mod tests {
    use parser_internals::{Token, Text, EscapedTag, UnescapedTag, Section, Partial};
    use super::Compiler;
    use super::super::Context;
    use std::path::PathBuf;

    fn compile_str(template: &str) -> Vec<Token> {
        let ctx = Context::new(PathBuf::from("."));
        let (tokens, _) = Compiler::new(ctx, template.chars())
                                   .compile()
                                   .expect("Failed to compile");
        tokens
    }

    fn check_tokens(actual: Vec<Token>, expected: &[Token]) {
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_compile_texts() {
        check_tokens(compile_str("hello world"),
                     &[Text("hello world".to_string())]);
        check_tokens(compile_str("hello {world"),
                     &[Text("hello {world".to_string())]);
        check_tokens(compile_str("hello world}"),
                     &[Text("hello world}".to_string())]);
        check_tokens(compile_str("hello world}}"),
                     &[Text("hello world}}".to_string())]);
    }

    #[test]
    fn test_compile_etags() {
        check_tokens(compile_str("{{ name }}"),
                     &[EscapedTag(vec!["name".to_string()], "{{ name }}".to_string())]);

        check_tokens(compile_str("before {{name}} after"),
                     &[Text("before ".to_string()),
                       EscapedTag(vec!["name".to_string()], "{{name}}".to_string()),
                       Text(" after".to_string())]);

        check_tokens(compile_str("before {{name}}"),
                     &[Text("before ".to_string()),
                       EscapedTag(vec!["name".to_string()], "{{name}}".to_string())]);

        check_tokens(compile_str("{{name}} after"),
                     &[EscapedTag(vec!["name".to_string()], "{{name}}".to_string()),
                       Text(" after".to_string())]);
    }

    #[test]
    fn test_compile_utags() {
        check_tokens(compile_str("{{{name}}}"),
                     &[UnescapedTag(vec!["name".to_string()], "{{{name}}}".to_string())]);

        check_tokens(compile_str("before {{{name}}} after"),
                     &[Text("before ".to_string()),
                       UnescapedTag(vec!["name".to_string()], "{{{name}}}".to_string()),
                       Text(" after".to_string())]);

        check_tokens(compile_str("before {{{name}}}"),
                     &[Text("before ".to_string()),
                       UnescapedTag(vec!["name".to_string()], "{{{name}}}".to_string())]);

        check_tokens(compile_str("{{{name}}} after"),
                     &[UnescapedTag(vec!["name".to_string()], "{{{name}}}".to_string()),
                       Text(" after".to_string())]);
    }

    #[test]
    fn test_compile_sections() {
        check_tokens(compile_str("{{# name}}{{/name}}"),
                     &[Section(vec!["name".to_string()],
                               false,
                               Vec::new(),
                               "{{".to_string(),
                               "{{# name}}".to_string(),
                               "".to_string(),
                               "{{/name}}".to_string(),
                               "}}".to_string())]);

        check_tokens(compile_str("before {{^name}}{{/name}} after"),
                     &[Text("before ".to_string()),
                       Section(vec!["name".to_string()],
                               true,
                               Vec::new(),
                               "{{".to_string(),
                               "{{^name}}".to_string(),
                               "".to_string(),
                               "{{/name}}".to_string(),
                               "}}".to_string()),
                       Text(" after".to_string())]);

        check_tokens(compile_str("before {{#name}}{{/name}}"),
                     &[Text("before ".to_string()),
                       Section(vec!["name".to_string()],
                               false,
                               Vec::new(),
                               "{{".to_string(),
                               "{{#name}}".to_string(),
                               "".to_string(),
                               "{{/name}}".to_string(),
                               "}}".to_string())]);

        check_tokens(compile_str("{{#name}}{{/name}} after"),
                     &[Section(vec!["name".to_string()],
                               false,
                               Vec::new(),
                               "{{".to_string(),
                               "{{#name}}".to_string(),
                               "".to_string(),
                               "{{/name}}".to_string(),
                               "}}".to_string()),
                       Text(" after".to_string())]);

        check_tokens(compile_str("before {{#a}} 1 {{^b}} 2 {{/b}} {{/a}} after"),
                     &[Text("before ".to_string()),
                       Section(vec!["a".to_string()],
                               false,
                               vec![Text(" 1 ".to_string()),
                                    Section(vec!["b".to_string()],
                                            true,
                                            vec![Text(" 2 ".to_string())],
                                            "{{".to_string(),
                                            "{{^b}}".to_string(),
                                            " 2 ".to_string(),
                                            "{{/b}}".to_string(),
                                            "}}".to_string()),
                                    Text(" ".to_string())],
                               "{{".to_string(),
                               "{{#a}}".to_string(),
                               " 1 {{^b}} 2 {{/b}} ".to_string(),
                               "{{/a}}".to_string(),
                               "}}".to_string()),
                       Text(" after".to_string())]);
    }

    #[test]
    fn test_compile_partials() {
        check_tokens(compile_str("{{> test}}"),
                     &[Partial("test".to_string(), "".to_string(), "{{> test}}".to_string())]);

        check_tokens(compile_str("before {{>test}} after"),
                     &[Text("before ".to_string()),
                       Partial("test".to_string(), "".to_string(), "{{>test}}".to_string()),
                       Text(" after".to_string())]);

        check_tokens(compile_str("before {{> test}}"),
                     &[Text("before ".to_string()),
                       Partial("test".to_string(), "".to_string(), "{{> test}}".to_string())]);

        check_tokens(compile_str("{{>test}} after"),
                     &[Partial("test".to_string(), "".to_string(), "{{>test}}".to_string()),
                       Text(" after".to_string())]);
    }

    #[test]
    fn test_compile_delimiters() {
        check_tokens(compile_str("before {{=<% %>=}}<%name%> after"),
                     &[Text("before ".to_string()),
                       EscapedTag(vec!["name".to_string()], "<%name%>".to_string()),
                       Text(" after".to_string())]);
    }
}
