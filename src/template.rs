use std::io::Write;
use std::collections::HashMap;
use std::mem;
use std::str;
use rustc_serialize::Encodable;

use encoder;
use compiler::Compiler;
use parser_internals::Token;
use parser_internals::Token::*;

use super::{Context, Data, Bool, StrVal, VecVal, Map, Fun, OptVal, Result};

/// `Template` represents a compiled mustache file.
#[derive(Debug, Clone)]
pub struct Template {
    ctx: Context,
    tokens: Vec<Token>,
    partials: HashMap<String, Vec<Token>>,
}

/// Construct a `Template`. This is not part of the impl of Template so it is
/// not exported outside of mustache.
pub fn new(ctx: Context, tokens: Vec<Token>, partials: HashMap<String, Vec<Token>>) -> Template {
    Template {
        ctx: ctx,
        tokens: tokens,
        partials: partials,
    }
}

impl Template {
    /// Renders the template with the `Encodable` data.
    pub fn render<W: Write, T: Encodable>(&self, wr: &mut W, data: &T) -> Result {
        let data = try!(encoder::encode(data));
        self.render_data(wr, &data)
    }

    /// Renders the template with the `Data`.
    pub fn render_data<W: Write>(&self, wr: &mut W, data: &Data) -> Result {
        let mut render_ctx = RenderContext::new(self);
        let mut stack = vec![data];

        render_ctx.render(wr, &mut stack, &self.tokens)
    }
}

struct RenderContext<'a> {
    template: &'a Template,
    indent: String,
    line_start: bool,
}

impl<'a> RenderContext<'a> {
    fn new(template: &'a Template) -> RenderContext<'a> {
        RenderContext {
            template: template,
            indent: "".to_string(),
            line_start: true,
        }
    }

    fn render<W: Write>(&mut self, wr: &mut W, stack: &mut Vec<&Data>, tokens: &[Token]) -> Result {
        for token in tokens.iter() {
            try!(self.render_token(wr, stack, token));
        }

        Ok(())
    }

    fn render_token<W: Write>(&mut self, wr: &mut W, stack: &mut Vec<&Data>, token: &Token) -> Result {
        match *token {
            Text(ref value) => {
                self.render_text(wr, value)
            }
            EscapedTag(ref path, _) => {
                self.render_etag(wr, stack, path)
            }
            UnescapedTag(ref path, _) => {
                self.render_utag(wr, stack, path)
            }
            Section(ref path, true, ref children, _, _, _, _, _) => {
                self.render_inverted_section(wr, stack, path, children)
            }
            Section(ref path, false, ref children, ref otag, _, ref src, _, ref ctag) => {
                self.render_section(wr, stack, path, children, src, otag, ctag)
            }
            Partial(ref name, ref indent, _) => {
                self.render_partial(wr, stack, name, indent)
            }
            IncompleteSection(..) => {
                bug!("render_token should not encounter IncompleteSections")
            }
        }
    }

    fn write_tracking_newlines<W: Write>(&mut self, wr: &mut W, value: &str) -> Result {
        try!(wr.write_all(value.as_bytes()));
        self.line_start = match value.chars().last() {
            None => self.line_start, // None == ""
            Some('\n') => true,
            _ => false,
        };

        Ok(())
    }

    fn write_indent<W: Write>(&mut self, wr: &mut W) -> Result {
        if self.line_start {
            try!(wr.write_all(self.indent.as_bytes()));
        }

        Ok(())
    }

    fn render_text<W: Write>(&mut self, wr: &mut W, value: &str) -> Result {
        // Indent the lines.
        if self.indent.is_empty() {
            return self.write_tracking_newlines(wr, value);
        } else {
            let mut pos = 0;
            let len = value.len();

            while pos < len {
                let v = &value[pos..];
                let line = match v.find('\n') {
                    None => {
                        let line = v;
                        pos = len;
                        line
                    }
                    Some(i) => {
                        let line = &v[..i + 1];
                        pos += i + 1;
                        line
                    }
                };

                if line.as_bytes()[0] != b'\n' {
                    try!(self.write_indent(wr));
                }

                try!(self.write_tracking_newlines(wr, line));
            }
        }

        Ok(())
    }

    fn render_etag<W: Write>(&mut self, wr: &mut W, stack: &mut Vec<&Data>, path: &[String]) -> Result {
        let mut bytes = vec![];

        try!(self.render_utag(&mut bytes, stack, path));

        for b in bytes {
            match b {
                b'<' => {
                    try!(wr.write_all(b"&lt;"));
                }
                b'>' => {
                    try!(wr.write_all(b"&gt;"));
                }
                b'&' => {
                    try!(wr.write_all(b"&amp;"));
                }
                b'"' => {
                    try!(wr.write_all(b"&quot;"));
                }
                b'\'' => {
                    try!(wr.write_all(b"&#39;"));
                }
                _ => {
                    try!(wr.write_all(&[b]));
                }
            }
        }

        Ok(())
    }

    fn render_utag<W: Write>(&mut self, wr: &mut W, stack: &mut Vec<&Data>, path: &[String]) -> Result {
        match self.find(path, stack) {
            None => {}
            Some(mut value) => {
                try!(self.write_indent(wr));

                // Currently this doesn't allow Option<Option<Foo>>, which
                // would be un-nameable in the view anyway, so I'm unsure if it's
                // a real problem. Having {{foo}} render only when `foo = Some(Some(val))`
                // seems unintuitive and may be surprising in practice.
                if let OptVal(ref inner) = *value {
                    match *inner {
                        Some(ref inner) => value = inner,
                        None => return Ok(()),
                    }
                }

                match *value {
                    StrVal(ref value) => {
                        try!(self.write_tracking_newlines(wr, value));
                    }

                    // etags and utags use the default delimiter.
                    Fun(ref fcell) => {
                        let f = &mut *fcell.borrow_mut();
                        let tokens = try!(self.render_fun("", "{{", "}}", f));
                        try!(self.render(wr, stack, &tokens));
                    }

                    ref value => {
                        bug!("render_utag: unexpected value {:?}", value);
                    }
                }
            }
        };

        Ok(())
    }

    fn render_inverted_section<W: Write>(&mut self,
                                         wr: &mut W,
                                         stack: &mut Vec<&Data>,
                                         path: &[String],
                                         children: &[Token]) -> Result {
        match self.find(path, stack) {
            None => {}
            Some(&Bool(false)) => {}
            Some(&VecVal(ref xs)) if xs.is_empty() => {}
            Some(&OptVal(ref val)) if val.is_none() => {}
            Some(_) => {
                return Ok(());
            }
        }

        self.render(wr, stack, children)
    }

    fn render_section<W: Write>(&mut self,
                                wr: &mut W,
                                stack: &mut Vec<&Data>,
                                path: &[String],
                                children: &[Token],
                                src: &str,
                                otag: &str,
                                ctag: &str) -> Result {
        match self.find(path, stack) {
            None => {}
            Some(value) => {
                match *value {
                    Bool(true) => {
                        try!(self.render(wr, stack, children));
                    }
                    Bool(false) => {}
                    VecVal(ref vs) => {
                        for v in vs.iter() {
                            stack.push(v);
                            try!(self.render(wr, stack, children));
                            stack.pop();
                        }
                    }
                    Map(_) => {
                        stack.push(value);
                        try!(self.render(wr, stack, children));
                        stack.pop();
                    }
                    Fun(ref fcell) => {
                        let f = &mut *fcell.borrow_mut();
                        let tokens = try!(self.render_fun(src, otag, ctag, f));
                        try!(self.render(wr, stack, &tokens));
                    }
                    OptVal(ref val) => {
                        if let Some(ref val) = *val {
                            stack.push(val);
                            try!(self.render(wr, stack, children));
                            stack.pop();
                        }
                    }
                    StrVal(ref val) => {
                        if val != "" {
                            stack.push(value);
                            try!(self.render(wr, stack, children));
                            stack.pop();
                        }
                    }
                }
            }
        };

        Ok(())
    }

    fn render_partial<W: Write>(&mut self,
                                wr: &mut W,
                                stack: &mut Vec<&Data>,
                                name: &str,
                                indent: &str) -> Result {
        match self.template.partials.get(name) {
            None => {}
            Some(ref tokens) => {
                let mut indent = self.indent.clone() + indent;

                mem::swap(&mut self.indent, &mut indent);
                try!(self.render(wr, stack, &tokens));
                mem::swap(&mut self.indent, &mut indent);
            }
        };

        Ok(())
    }

    fn render_fun(&self,
                  src: &str,
                  otag: &str,
                  ctag: &str,
                  f: &mut Box<FnMut(String) -> String + Send + 'static>)
                  -> Result<Vec<Token>> {
        let src = f(src.to_string());

        let compiler = Compiler::new_with(self.template.ctx.clone(),
                                          src.chars(),
                                          self.template.partials.clone(),
                                          otag.to_string(),
                                          ctag.to_string());

        let (tokens, _) = try!(compiler.compile());
        Ok(tokens)
    }

    fn find<'c>(&self, path: &[String], stack: &mut Vec<&'c Data>) -> Option<&'c Data> {
        // If we have an empty path, we just want the top value in our stack.
        if path.is_empty() {
            match stack.last() {
                None => {
                    return None;
                }
                Some(data) => {
                    return Some(*data);
                }
            }
        }

        // Otherwise, find the stack that has the first part of our path.
        let mut value = None;

        for data in stack.iter().rev() {
            match **data {
                Map(ref m) => {
                    if let Some(v) = m.get(&path[0]) {
                        value = Some(v);
                        break;
                    }
                }
                _ => { /* continue searching the stack */ },
            }
        }

        // Walk the rest of the path to find our final value.
        let mut value = match value {
            Some(value) => value,
            None => {
                return None;
            }
        };

        for part in path[1..].iter() {
            match *value {
                Map(ref m) => {
                    match m.get(part) {
                        Some(v) => {
                            value = v;
                        }
                        None => {
                            return None;
                        }
                    }
                }
                _ => {
                    return None;
                }
            }
        }

        Some(value)
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use tempdir::TempDir;
    use std::fmt::Debug;
    use std::fs::File;
    use std::io::{Read, Write};
    use std::path::{PathBuf, Path};
    use std::collections::HashMap;
    use rustc_serialize::{json, Encodable};
    use rustc_serialize::json::Json;

    use encoder;
    use Error;

    use super::super::{Data, StrVal, VecVal, Map, Fun};
    use super::super::{Context, Template};

    #[derive(RustcEncodable, Debug)]
    struct Planet {
        name: String,
        info: Option<PlanetInfo>,
    }

    #[derive(RustcEncodable, Debug)]
    struct PlanetInfo {
        moons: Vec<String>,
        population: u64,
        description: String,
    }

    #[derive(RustcEncodable, Debug)]
    struct Person {
        name: String,
        age: Option<u32>,
    }

    fn compile_str(s: &str) -> Template {
        ::compile_str(s).expect(&format!("Failed to compile: {}", s))
    }

    fn assert_render<T: Encodable + Debug>(template: &str, data: &T) -> String {
        render(template, data).expect(&format!("Failed to render: template: {:?}\ndata: {:?}",
                                                template,
                                                data))
    }

    fn render<T: Encodable>(template: &str, data: &T) -> Result<String, Error> {
        let template = compile_str(template);

        let mut bytes = vec![];
        try!(template.render(&mut bytes, data));

        Ok(String::from_utf8(bytes).expect("Failed to encode String"))
    }

    #[test]
    fn test_render_texts() {
        let ctx = Planet {
            name: "world".to_string(),
            info: None,
        };

        assert_eq!(assert_render("hello world", &ctx), "hello world");
        assert_eq!(assert_render("hello {world", &ctx), "hello {world");
        assert_eq!(assert_render("hello world}", &ctx), "hello world}");
        assert_eq!(assert_render("hello {world}", &ctx), "hello {world}");
        assert_eq!(assert_render("hello world}}", &ctx), "hello world}}");
    }

    #[test]
    fn test_render_etags() {
        let ctx = Planet {
            name: "world".to_string(),
            info: None,
        };

        assert_eq!(assert_render("hello {{name}}", &ctx), "hello world");
    }

    #[test]
    fn test_render_utags() {
        let ctx = Planet {
            name: "world".to_string(),
            info: None,
        };

        assert_eq!(assert_render("hello {{{name}}}", &ctx), "hello world");

        assert_eq!(assert_render("hello {{{name}}}", &ctx), "hello world");
    }

    #[test]
    fn test_render_option() {
        let template = "{{name}}, {{age}}";
        let ctx = Person {
            name: "Dennis".to_string(),
            age: None,
        };

        assert_eq!(assert_render(template, &ctx), "Dennis, ");

        let ctx = Person {
            name: "Dennis".to_string(),
            age: Some(42),
        };
        assert_eq!(assert_render(template, &ctx), "Dennis, 42");
    }

    #[test]
    fn test_render_option_sections_implicit() {
        let template = "{{name}}, {{#age}}{{.}}{{/age}}{{^age}}No age{{/age}}";
        let ctx = Person {
            name: "Dennis".to_string(),
            age: None,
        };

        assert_eq!(assert_render(template, &ctx), "Dennis, No age");

        let ctx = Person {
            name: "Dennis".to_string(),
            age: Some(42),
        };
        assert_eq!(assert_render(template, &ctx), "Dennis, 42");
    }

    #[test]
    fn test_implicit_section() {
        let template = compile_str("{{#.}}{{.}}{{/.}}");
        let data = vec!["val1", "val2"];
        let mut r = Vec::new();
        template.render(&mut r, &data).unwrap();
        assert_eq!(r, b"val1val2");
    }

    #[test]
    fn test_implicit_inverted_section() {
        let template = compile_str("{{^.}}No values{{/.}}");
        let data = Vec::<&str>::new();
        let mut r = Vec::new();
        template.render(&mut r, &data).unwrap();
        assert_eq!(r, b"No values");
    }

    #[test]
    fn test_nested_implicit_render() {
        let template = compile_str("{{#.}}[{{#.}}{{.}}{{/.}}]{{/.}}");
        let data = vec![vec!["val1", "val2"], vec!["val3"]];
        let mut r = Vec::new();
        template.render(&mut r, &data).unwrap();
        assert_eq!(r, b"[val1val2][val3]");
    }

    fn test_implicit_render(tags: &str, expect_escaped: bool) {
        let template = format!("{}{}{}", "{{#list}} (", tags, ") {{/list}}");
        let mut ctx = HashMap::new();
        ctx.insert("list", vec!["&", "\"", "<", ">"]);

        let expected = if expect_escaped {
            " (&amp;)  (&quot;)  (&lt;)  (&gt;) "
        } else {
            r#" (&)  (")  (<)  (>) "#
        };

        assert_eq!(&*render(&template, &ctx).unwrap(), expected);
    }

    #[test]
    fn test_render_option_sections_implicit_escaped() {
        test_implicit_render("{{.}}", true);
    }

    #[test]
    fn test_render_option_sections_implicit_escaped_alternative_delimeters() {
        test_implicit_render("{{=<% %>=}}<%.%><%={{ }}=%>", true);
    }

    #[test]
    fn test_render_option_sections_implicit_unescaped() {
        test_implicit_render("{{{.}}}", false);
    }

    #[test]
    fn test_render_option_sections_implicit_ampersand() {
        test_implicit_render("{{&.}}", false);
    }

    #[test]
    fn test_render_option_sections_implicit_unescaped_alternative_delimeters() {
        test_implicit_render("{{=<% %>=}}<%{.}%><%={{ }}=%>", false);
    }

    #[test]
    fn test_render_option_sections_implicit_ampersand_alternative_delimeters() {
        test_implicit_render("{{=<% %>=}}<%&.%><%={{ }}=%>", false);
    }

    mod context_search {
        use MapBuilder;
        use super::{render_data, compile_str};

        fn assert_render(template: &str, expected: &str) {
            let ctx = MapBuilder::new().insert_map("payload", |map| {
                map.insert_vec("list", |vec| {
                    vec.push_str("<p>hello</p>")
                       .push_str("<p>world</p>")
                }).insert_str("test", "hello world")
            }).build();

            let template = compile_str(template);
            let rendered = render_data(&template, &ctx);
            println!("{}\n----\n{}", rendered, expected);
            assert_eq!(rendered, expected);
        }

        #[test]
        fn from_base() {
            let template = "\
{{#payload.list}}
{{payload.test}}
{{/payload.list}}";

            assert_render(template, "hello world\nhello world\n")
        }

        #[test]
        fn from_mid_stack() {
            let template = "\
{{#payload}}
{{#list}}
{{test}}
{{/list}}
{{/payload}}";

            assert_render(template, "hello world\nhello world\n")
        }

        #[test]
        fn render_nothing_when_not_found() {
            let template = "\
{{#payload.list}}
{{test}}
{{/payload.list}}";

            assert_render(template, "\n\n")
        }
    }

    #[test]
    #[should_panic]
    fn test_render_option_nested() {
        #[derive(RustcEncodable, Debug)]
        struct Nested {
            opt: Option<Option<u32>>,
        }

        let template = "-{{opt}}+";
        let ctx = Nested { opt: None };
        assert_eq!(assert_render(template, &ctx), "-+");

        let ctx = Nested { opt: Some(None) };
        assert_eq!(assert_render(template, &ctx), "-+");

        let ctx = Nested { opt: Some(Some(42)) };
        assert_eq!(assert_render(template, &ctx), "-42+");
    }

    #[test]
    fn test_render_option_complex() {
        let template = "{{name}} - {{#info}}{{description}}; It's moons are \
                        [{{#moons}}{{.}}{{/moons}}] and has a population of \
                        {{population}}{{/info}}{{^info}}No additional info{{/info}}";
        let ctx = Planet {
            name: "Jupiter".to_string(),
            info: None,
        };
        assert_eq!(assert_render(template, &ctx), "Jupiter - No additional info");

        let address = PlanetInfo {
            moons: vec!["Luna".to_string()],
            population: 7300000000,
            description: "Birthplace of rust-lang".to_string(),
        };
        let ctx = Planet {
            name: "Earth".to_string(),
            info: Some(address),
        };
        assert_eq!(assert_render(template, &ctx), "Earth - Birthplace of rust-lang; \
                                                   It's moons are [Luna] and has a \
                                                   population of 7300000000");
    }

    fn render_data(template: &Template, data: &Data) -> String {
        let mut bytes = vec![];
        template.render_data(&mut bytes, data).expect("Failed to render data");
        String::from_utf8(bytes).expect("Failed ot encode as String")
    }

    #[test]
    fn test_write_failure() {
        use std::error::Error;

        let mut ctx = HashMap::new();
        ctx.insert("name", "foobar");

        let template = compile_str("{{name}}");
        let mut buffer = [0u8; 6];
        {
            let mut writer: &mut [u8] = &mut buffer;
            template.render(&mut writer, &ctx).expect("Failed to render");
        }

        assert_eq!(&buffer, b"foobar");

        ctx.insert("name", "longerthansix");

        let mut writer: &mut [u8] = &mut buffer;
        assert_let!(Err(e) = template.render(&mut writer, &ctx) => {
            assert_eq!(e.description(), "failed to write whole buffer")
        })
    }

    #[test]
    fn test_render_sections() {
        let ctx = HashMap::new();
        let template = compile_str("0{{#a}}1 {{n}} 3{{/a}}5");

        assert_eq!(render_data(&template, &Map(ctx)), "05".to_string());

        let mut ctx = HashMap::new();
        ctx.insert("a".to_string(), VecVal(Vec::new()));

        assert_eq!(render_data(&template, &Map(ctx)), "05".to_string());

        let mut ctx = HashMap::new();
        ctx.insert("a".to_string(), VecVal(Vec::new()));
        assert_eq!(render_data(&template, &Map(ctx)), "05".to_string());

        let mut ctx0 = HashMap::new();
        let ctx1 = HashMap::new();
        ctx0.insert("a".to_string(), VecVal(vec![Map(ctx1)]));

        assert_eq!(render_data(&template, &Map(ctx0)), "01  35".to_string());

        let mut ctx0 = HashMap::new();
        let mut ctx1 = HashMap::new();
        ctx1.insert("n".to_string(), StrVal("a".to_string()));
        ctx0.insert("a".to_string(), VecVal(vec![Map(ctx1)]));
        assert_eq!(render_data(&template, &Map(ctx0)), "01 a 35".to_string());

        let mut ctx = HashMap::new();
        ctx.insert("a".to_string(),
                   Fun(RefCell::new(Box::new(|_text| "foo".to_string()))));
        assert_eq!(render_data(&template, &Map(ctx)), "0foo5".to_string());
    }

    #[test]
    fn test_render_inverted_sections() {
        let template = compile_str("0{{^a}}1 3{{/a}}5");

        let ctx = HashMap::new();
        assert_eq!(render_data(&template, &Map(ctx)), "01 35".to_string());

        let mut ctx = HashMap::new();
        ctx.insert("a".to_string(), VecVal(vec![]));
        assert_eq!(render_data(&template, &Map(ctx)), "01 35".to_string());

        let mut ctx0 = HashMap::new();
        let ctx1 = HashMap::new();
        ctx0.insert("a".to_string(), VecVal(vec![Map(ctx1)]));
        assert_eq!(render_data(&template, &Map(ctx0)), "05".to_string());

        let mut ctx0 = HashMap::new();
        let mut ctx1 = HashMap::new();
        ctx1.insert("n".to_string(), StrVal("a".to_string()));
        ctx0.insert("a".to_string(), VecVal(vec![Map(ctx1)]));
        assert_eq!(render_data(&template, &Map(ctx0)), "05".to_string());
    }

    fn assert_partials_data(template: Template) {
        let ctx = HashMap::new();
        assert_eq!(render_data(&template, &Map(ctx)),
                   "<h2>Names</h2>\n".to_string());

        let mut ctx = HashMap::new();
        ctx.insert("names".to_string(), VecVal(vec![]));
        assert_eq!(render_data(&template, &Map(ctx)),
                   "<h2>Names</h2>\n".to_string());

        let mut ctx0 = HashMap::new();
        let ctx1 = HashMap::new();
        ctx0.insert("names".to_string(), VecVal(vec![Map(ctx1)]));
        assert_eq!(render_data(&template, &Map(ctx0)),
                   "<h2>Names</h2>\n  <strong></strong>\n\n".to_string());

        let mut ctx0 = HashMap::new();
        let mut ctx1 = HashMap::new();
        ctx1.insert("name".to_string(), StrVal("a".to_string()));
        ctx0.insert("names".to_string(), VecVal(vec![Map(ctx1)]));
        assert_eq!(render_data(&template, &Map(ctx0)),
                   "<h2>Names</h2>\n  <strong>a</strong>\n\n".to_string());

        let mut ctx0 = HashMap::new();
        let mut ctx1 = HashMap::new();
        ctx1.insert("name".to_string(), StrVal("a".to_string()));
        let mut ctx2 = HashMap::new();
        ctx2.insert("name".to_string(), StrVal("<b>".to_string()));
        ctx0.insert("names".to_string(), VecVal(vec![Map(ctx1), Map(ctx2)]));
        assert_eq!(render_data(&template, &Map(ctx0)),
                   "<h2>Names</h2>\n  <strong>a</strong>\n\n  <strong>&lt;b&gt;</strong>\n\n"
                       .to_string());
    }

    #[test]
    fn test_render_partial_dot_filename() {
        let template = ::compile_path("src/test-data/base.foo.mustache").expect("Failed to compile");
        assert_partials_data(template);
    }

    #[test]
    fn test_render_partial() {
        let template = ::compile_path("src/test-data/base").expect("Failed to compile");
        assert_partials_data(template);
    }

    fn parse_spec_tests(src: &str) -> Vec<json::Json> {
        let path = PathBuf::from(src);
        let mut file_contents = vec![];
        File::open(&path)
            .and_then(|mut f| f.read_to_end(&mut file_contents))
            .expect(&format!("Could not read file {}", path.display()));

        let s = String::from_utf8(file_contents.to_vec()).expect("File was not UTF8 encoded");

        assert_let!(Ok(Json::Object(mut d)) = Json::from_str(&s) => {
            assert_let!(Some(Json::Array(tests)) = d.remove("tests") => {
                tests.into_iter().collect()
            })
        })
    }

    fn write_partials(tmpdir: &Path, value: &json::Json) {
        assert_let!(Json::Object(ref d) = *value => {
            for (key, value) in d {
                assert_let!(Json::String(ref s) = *value => {
                    let path = tmpdir.join(&(key.clone() + ".mustache"));
                    File::create(&path)
                        .and_then(|mut f| f.write_all(s.as_bytes()))
                        .expect("Failed to generate partial");
                })
            }
        })
    }

    fn run_test(test: json::Object, data: Data) {
        let template = assert_let!(Some(&Json::String(ref s)) = test.get("template") => {
            s.clone()
        });

        let expected = assert_let!(Some(&Json::String(ref s)) = test.get("expected") => {
            s.clone()
        });

        // Make a temporary dir where we'll store our partials. This is to
        // avoid a race on filenames.
        let tmpdir = TempDir::new("").expect("Failed to make tempdir");

        if let Some(value) = test.get("partials") {
            write_partials(tmpdir.path(), value)
        }

        let ctx = Context::new(tmpdir.path().to_path_buf());
        let template = ctx.compile(template.chars())
                          .expect(&format!("Failed to compile: {}", template));
        let result = render_data(&template, &data);

        if result != expected {
            println!("desc:     {:?}", test.get("desc"));
            println!("context:  {:?}", test.get("data"));
            println!("=>");
            println!("template: {:?}", template);
            println!("expected: {}", expected);
            println!("actual:   {}", result);
            println!("");
        }
        assert_eq!(result, expected);
    }

    fn run_tests(spec: &str) {
        for json in parse_spec_tests(spec).into_iter() {
            let test = assert_let!(Json::Object(m) = json => m);

            let data = test.get("data").expect("No test data").clone();
            let data = encoder::encode(&data).expect("Failed to encode");

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
        for json in parse_spec_tests("spec/specs/~lambdas.json").into_iter() {
            let mut test = assert_let!(Json::Object(m) = json => m);

            let s = assert_let!(Some(Json::String(s)) = test.remove("name") => s);

            // Replace the lambda with rust code.
            let data = test.remove("data").expect("No test data");

            let data = encoder::encode(&data).expect("Failed to encode");

            let mut ctx = assert_let!(Map(ctx) = data => ctx);

            // needed for the closure test.
            let mut calls = 0usize;

            match &*s {
                "Interpolation" => {
                    let f = |_text| "world".to_string();
                    ctx.insert("lambda".to_string(), Fun(RefCell::new(Box::new(f))));
                }
                "Interpolation - Expansion" => {
                    let f = |_text| "{{planet}}".to_string();
                    ctx.insert("lambda".to_string(), Fun(RefCell::new(Box::new(f))));
                }
                "Interpolation - Alternate Delimiters" => {
                    let f = |_text| "|planet| => {{planet}}".to_string();
                    ctx.insert("lambda".to_string(), Fun(RefCell::new(Box::new(f))));
                }
                "Interpolation - Multiple Calls" => {
                    let f = move |_text: String| {
                        calls += 1;
                        calls.to_string()
                    };
                    ctx.insert("lambda".to_string(), Fun(RefCell::new(Box::new(f))));
                }
                "Escaping" => {
                    let f = |_text| ">".to_string();
                    ctx.insert("lambda".to_string(), Fun(RefCell::new(Box::new(f))));
                }
                "Section" => {
                    let f = |text: String| {
                        if text == "{{x}}" {
                            "yes".to_string()
                        } else {
                            "no".to_string()
                        }
                    };
                    ctx.insert("lambda".to_string(), Fun(RefCell::new(Box::new(f))));
                }
                "Section - Expansion" => {
                    let f = |text: String| text.clone() + "{{planet}}" + &text;
                    ctx.insert("lambda".to_string(), Fun(RefCell::new(Box::new(f))));
                }
                "Section - Alternate Delimiters" => {
                    let f = |text: String| text.clone() + "{{planet}} => |planet|" + &text;
                    ctx.insert("lambda".to_string(), Fun(RefCell::new(Box::new(f))));
                }
                "Section - Multiple Calls" => {
                    let f = |text: String| "__".to_string() + &text + "__";
                    ctx.insert("lambda".to_string(), Fun(RefCell::new(Box::new(f))));
                }
                "Inverted Section" => {
                    let f = |_text| "".to_string();
                    ctx.insert("lambda".to_string(), Fun(RefCell::new(Box::new(f))));
                }
                spec_name => bug!("unimplemented lambda spec test: {}", spec_name),
            };

            run_test(test, Map(ctx));
        }
    }
}
