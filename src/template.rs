use std::cell::RefCell;
use std::collections::HashMap;
use std::io::MemWriter;
use std::mem;
use std::str;
use serialize::Encodable;

use compiler::Compiler;
use parser::{Token, Text, ETag, UTag, Section, Partial};
use encoder;
use encoder::{Encoder, Error};

use super::{Context, Data, Bool, Str, Vec, Map, Fun};

/// `Template` represents a compiled mustache file.
#[deriving(Show, Clone)]
pub struct Template {
    ctx: Context,
    tokens: Vec<Token>,
    partials: HashMap<String, Vec<Token>>
}

/// Construct a `Template`. This is not part of the impl of Template so it is
/// not exported outside of mustache.
pub fn new(ctx: Context, tokens: Vec<Token>, partials: HashMap<String,
Vec<Token>>) -> Template {
    Template {
        ctx: ctx,
        tokens: tokens,
        partials: partials,
    }
}

impl Template {
    /// Renders the template with the `Encodable` data.
    pub fn render<'a, W: Writer, T: Encodable<Encoder<'a>, Error>>(
        &self,
        wr: &mut W,
        data: &T
    ) -> Result<(), Error> {
        let data = try!(encoder::encode(data));
        Ok(self.render_data(wr, &data))
    }

    /// Renders the template with the `Data`.
    pub fn render_data<'a, W: Writer>(&self, wr: &mut W, data: &Data<'a>) {
        let mut render_ctx = RenderContext::new(self);
        let mut stack = vec!(data);

        render_ctx.render(
            wr,
            &mut stack,
            self.tokens.as_slice());
    }
}

struct RenderContext<'a> {
    template: &'a Template,
    indent: String,
}

impl<'a> RenderContext<'a> {
    fn new(template: &'a Template) -> RenderContext<'a> {
        RenderContext {
            template: template,
            indent: "".to_string(),
        }
    }

    fn render<'b, W: Writer>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data<'b>>,
        tokens: &[Token]
    ) {
        for token in tokens.iter() {
            self.render_token(wr, stack, token);
        }
    }

    fn render_token<'b, W: Writer>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data<'b>>,
        token: &Token
    ) {
        match *token {
            Text(ref value) => {
                self.render_text(wr, value.as_slice());
            },
            ETag(ref path, _) => {
                self.render_etag(wr, stack, path.as_slice());
            }
            UTag(ref path, _) => {
                self.render_utag(wr, stack, path.as_slice());
            }
            Section(ref path, true, ref children, _, _, _, _, _) => {
                self.render_inverted_section(wr, stack, path.as_slice(), children.as_slice());
            }
            Section(ref path, false, ref children, ref otag, _, ref src, _, ref ctag) => {
                self.render_section(
                    wr,
                    stack,
                    path.as_slice(),
                    children.as_slice(),
                    src.as_slice(),
                    otag.as_slice(),
                    ctag.as_slice())
            }
            Partial(ref name, ref indent, _) => {
                self.render_partial(wr, stack, name.as_slice(), indent.as_slice());
            }
            _ => { fail!() }
        }
    }

    fn render_text<W: Writer>(
        &mut self,
        wr: &mut W,
        value: &str
    ) {
        // Indent the lines.
        if self.indent.equiv(&("")) {
            wr.write_str(value).unwrap();
        } else {
            let mut pos = 0;
            let len = value.len();

            while pos < len {
                let v = value.slice_from(pos);
                let line = match v.find('\n') {
                    None => {
                        let line = v;
                        pos = len;
                        line
                    }
                    Some(i) => {
                        let line = v.slice_to(i + 1);
                        pos += i + 1;
                        line
                    }
                };

                if line.char_at(0) != '\n' {
                    wr.write_str(self.indent.as_slice()).unwrap();
                }

                wr.write_str(line).unwrap();
            }
        }
    }

    fn render_etag<'b, W: Writer>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data<'b>>,
        path: &[String]
    ) {
        let mut mem_wr = MemWriter::new();

        self.render_utag(&mut mem_wr, stack, path);

        let bytes = mem_wr.unwrap();
        let s = str::from_utf8(bytes.as_slice()).unwrap().to_string();

        for c in s.as_slice().chars() {
            match c {
                '<'  => { wr.write_str("&lt;").unwrap(); }
                '>'  => { wr.write_str("&gt;").unwrap(); }
                '&'  => { wr.write_str("&amp;").unwrap(); }
                '"'  => { wr.write_str("&quot;").unwrap(); }
                '\'' => { wr.write_str("&#39;").unwrap(); }
                _    => { wr.write_char(c).unwrap(); }
            }
        }
    }

    fn render_utag<'b, W: Writer>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data<'b>>,
        path: &[String]
    ) {
        match self.find(path, stack) {
            None => { }
            Some(value) => {
                wr.write_str(self.indent.as_slice()).unwrap();

                match *value {
                    Str(ref value) => {
                        wr.write_str(value.as_slice()).unwrap();
                    }

                    // etags and utags use the default delimiter.
                    Fun(ref f) => {
                        let tokens = self.render_fun("", "{{", "}}", f);
                        self.render(wr, stack, tokens.as_slice());
                    }

                    ref value => { fail!("unexpected value {}", value); }
                }
            }
        };
    }

    fn render_inverted_section<'b, W: Writer>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data<'b>>,
        path: &[String],
        children: &[Token]
    ) {
        match self.find(path, stack) {
            None => { }
            Some(&Bool(false)) => { }
            Some(&Vec(ref xs)) if xs.is_empty() => { }
            Some(_) => { return; }
        }

        self.render(wr, stack, children);
    }

    fn render_section<'b, W: Writer>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data<'b>>,
        path: &[String],
        children: &[Token],
        src: &str,
        otag: &str,
        ctag: &str
    ) {
        match self.find(path, stack) {
            None => { }
            Some(value) => {
                match *value {
                    Bool(true) => {
                        self.render(wr, stack, children);
                    }
                    Bool(false) => { }
                    Vec(ref vs) => {
                        for v in vs.iter() {
                            stack.push(v);
                            self.render(wr, stack, children);
                            stack.pop();
                        }
                    }
                    Map(_) => {
                        stack.push(value);
                        self.render(wr, stack, children);
                        stack.pop();
                    }
                    Fun(ref f) => {
                        let tokens = self.render_fun(src, otag, ctag, f);
                        self.render(wr, stack, tokens.as_slice())
                    }
                    _ => { fail!("unexpected value {}", value) }
                }
            }
        }
    }

    fn render_partial<'b, W: Writer>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data<'b>>,
        name: &str,
        indent: &str
    ) {
        match self.template.partials.find_equiv(&name) {
            None => { }
            Some(ref tokens) => {
                let mut indent = self.indent + indent;

                mem::swap(&mut self.indent, &mut indent);
                self.render(wr, stack, tokens.as_slice());
                mem::swap(&mut self.indent, &mut indent);
            }
        }
    }

    fn render_fun(
        &self,
        src: &str,
        otag: &str,
        ctag: &str,
        f: &RefCell<|String| -> String>
    ) -> Vec<Token> {
        let f = &mut *f.borrow_mut();
        let src = (*f)(src.to_string());

        let compiler = Compiler::new_with(
            self.template.ctx.clone(),
            src.as_slice().chars(),
            self.template.partials.clone(),
            otag.to_string(),
            ctag.to_string());

        let (tokens, _) = compiler.compile();
        tokens
    }

    fn find<'b, 'c>(&self, path: &[String], stack: &mut Vec<&'c Data<'b>>) -> Option<&'c Data<'b>> {
        // If we have an empty path, we just want the top value in our stack.
        if path.is_empty() {
            match stack.last() {
                None => { return None; }
                Some(data) => { return Some(*data); }
            }
        }

        // Otherwise, find the stack that has the first part of our path.
        let mut value = None;

        for data in stack.iter().rev() {
            match **data {
                Map(ref m) => {
                    match m.find_equiv(&path[0]) {
                        Some(v) => {
                            value = Some(v);
                            break;
                        }
                        None => { }
                    }
                }
                _ => { fail!("expect map: {}", path) }
            }
        }

        // Walk the rest of the path to find our final value.
        let mut value = match value {
            Some(value) => value,
            None => { return None; }
        };

        for part in path.slice_from(1).iter() {
            match *value {
                Map(ref m) => {
                    match m.find_equiv(part) {
                        Some(v) => { value = v; }
                        None => { return None; }
                    }
                }
                _ => { return None; }
            }
        }

        Some(value)
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::str;
    use std::io::{File, MemWriter, TempDir};
    use std::collections::HashMap;
    use serialize::json;
    use serialize::Encodable;

    use encoder::{Encoder, Error};

    use super::super::compile_str;
    use super::super::{Data, Str, Vec, Map, Fun};
    use super::super::{Context, Template};

    #[deriving(Encodable)]
    struct Name { name: String }

    fn render<'a, 'b, T: Encodable<Encoder<'b>, Error>>(
        template: &str,
        data: &T,
    ) -> Result<String, Error> {
        let template = compile_str(template);

        let mut wr = MemWriter::new();
        try!(template.render(&mut wr, data));

        Ok(str::from_utf8(wr.unwrap().as_slice()).unwrap().to_string())
    }

    #[test]
    fn test_render_texts() {
        let ctx = Name { name: "world".to_string() };

        assert_eq!(render("hello world", &ctx), Ok("hello world".to_string()));
        assert_eq!(render("hello {world", &ctx), Ok("hello {world".to_string()));
        assert_eq!(render("hello world}", &ctx), Ok("hello world}".to_string()));
        assert_eq!(render("hello {world}", &ctx), Ok("hello {world}".to_string()));
        assert_eq!(render("hello world}}", &ctx), Ok("hello world}}".to_string()));
    }

    #[test]
    fn test_render_etags() {
        let ctx = Name { name: "world".to_string() };

        assert_eq!(render("hello {{name}}", &ctx), Ok("hello world".to_string()));
    }

    #[test]
    fn test_render_utags() {
        let ctx = Name { name: "world".to_string() };

        assert_eq!(render("hello {{{name}}}", &ctx), Ok("hello world".to_string()));
    }

    fn render_data<'a>(template: &Template, data: &Data<'a>) -> String {
        let mut wr = MemWriter::new();
        template.render_data(&mut wr, data);
        str::from_utf8(wr.unwrap().as_slice()).unwrap().to_string()
    }

    #[test]
    fn test_render_sections() {
        let ctx = HashMap::new();
        let template = compile_str("0{{#a}}1 {{n}} 3{{/a}}5");

        assert_eq!(render_data(&template, &Map(ctx)), "05".to_string());

        let mut ctx = HashMap::new();
        ctx.insert("a".to_string(), Vec(Vec::new()));

        assert_eq!(render_data(&template, &Map(ctx)), "05".to_string());

        let mut ctx = HashMap::new();
        ctx.insert("a".to_string(), Vec(Vec::new()));
        assert_eq!(render_data(&template, &Map(ctx)), "05".to_string());

        let mut ctx0 = HashMap::new();
        let ctx1 = HashMap::new();
        ctx0.insert("a".to_string(), Vec(vec!(Map(ctx1))));

        assert_eq!(render_data(&template, &Map(ctx0)), "01  35".to_string());

        let mut ctx0 = HashMap::new();
        let mut ctx1 = HashMap::new();
        ctx1.insert("n".to_string(), Str("a".to_string()));
        ctx0.insert("a".to_string(), Vec(vec!(Map(ctx1))));
        assert_eq!(render_data(&template, &Map(ctx0)), "01 a 35".to_string());

        let mut ctx = HashMap::new();
        ctx.insert("a".to_string(), Fun(RefCell::new(|_text| "foo".to_string())));
        assert_eq!(render_data(&template, &Map(ctx)), "0foo5".to_string());
    }

    #[test]
    fn test_render_inverted_sections() {
        let template = compile_str("0{{^a}}1 3{{/a}}5");

        let ctx = HashMap::new();
        assert_eq!(render_data(&template, &Map(ctx)), "01 35".to_string());

        let mut ctx = HashMap::new();
        ctx.insert("a".to_string(), Vec(vec!()));
        assert_eq!(render_data(&template, &Map(ctx)), "01 35".to_string());

        let mut ctx0 = HashMap::new();
        let ctx1 = HashMap::new();
        ctx0.insert("a".to_string(), Vec(vec!(Map(ctx1))));
        assert_eq!(render_data(&template, &Map(ctx0)), "05".to_string());

        let mut ctx0 = HashMap::new();
        let mut ctx1 = HashMap::new();
        ctx1.insert("n".to_string(), Str("a".to_string()));
        ctx0.insert("a".to_string(), Vec(vec!(Map(ctx1))));
        assert_eq!(render_data(&template, &Map(ctx0)), "05".to_string());
    }

    #[test]
    fn test_render_partial() {
        let template = Context::new(Path::new("src/test-data"))
            .compile_path(Path::new("base"))
            .unwrap();

        let ctx = HashMap::new();
        assert_eq!(render_data(&template, &Map(ctx)), "<h2>Names</h2>\n".to_string());

        let mut ctx = HashMap::new();
        ctx.insert("names".to_string(), Vec(vec!()));
        assert_eq!(render_data(&template, &Map(ctx)), "<h2>Names</h2>\n".to_string());

        let mut ctx0 = HashMap::new();
        let ctx1 = HashMap::new();
        ctx0.insert("names".to_string(), Vec(vec!(Map(ctx1))));
        assert_eq!(
            render_data(&template, &Map(ctx0)),
            "<h2>Names</h2>\n  <strong></strong>\n\n".to_string());

        let mut ctx0 = HashMap::new();
        let mut ctx1 = HashMap::new();
        ctx1.insert("name".to_string(), Str("a".to_string()));
        ctx0.insert("names".to_string(), Vec(vec!(Map(ctx1))));
        assert_eq!(
            render_data(&template, &Map(ctx0)),
            "<h2>Names</h2>\n  <strong>a</strong>\n\n".to_string());

        let mut ctx0 = HashMap::new();
        let mut ctx1 = HashMap::new();
        ctx1.insert("name".to_string(), Str("a".to_string()));
        let mut ctx2 = HashMap::new();
        ctx2.insert("name".to_string(), Str("<b>".to_string()));
        ctx0.insert("names".to_string(), Vec(vec!(Map(ctx1), Map(ctx2))));
        assert_eq!(
            render_data(&template, &Map(ctx0)),
            "<h2>Names</h2>\n  <strong>a</strong>\n\n  <strong>&lt;b&gt;</strong>\n\n".to_string());
    }

    fn parse_spec_tests(src: &str) -> Vec<json::Json> {
        let path = Path::new(src);

        let file_contents = match File::open(&path).read_to_end() {
            Ok(reader) => reader,
            Err(e) => fail!("Could not read file {}", e),
        };

        let s = match str::from_utf8(file_contents.as_slice()){
            Some(str) => str.to_string(),
            None => {fail!("File was not UTF8 encoded");}
        };

        match json::from_str(s.as_slice()) {
            Err(e) => fail!(e.to_string()),
            Ok(json) => {
                match json {
                    json::Object(d) => {
                        let mut d = d;
                        match d.pop(&"tests".to_string()) {
                            Some(json::List(tests)) => tests.move_iter().collect(),
                            _ => fail!("{}: tests key not a list", src),
                        }
                    }
                    _ => fail!("{}: JSON value not a map", src),
                }
            }
        }
    }

    fn write_partials(tmpdir: &Path, value: &json::Json) {
        match value {
            &json::Object(ref d) => {
                for (key, value) in d.iter() {
                    match value {
                        &json::String(ref s) => {
                            let mut path = tmpdir.clone();
                            path.push(*key + ".mustache");
                            File::create(&path).write(s.as_bytes()).unwrap();
                        }
                        _ => fail!(),
                    }
                }
            },
            _ => fail!(),
        }
    }

    fn run_test(test: json::Object, data: Data) {
        let template = match test.find(&"template".to_string()) {
            Some(&json::String(ref s)) => s.clone(),
            _ => fail!(),
        };

        let expected = match test.find(&"expected".to_string()) {
            Some(&json::String(ref s)) => s.clone(),
            _ => fail!(),
        };

        // Make a temporary dir where we'll store our partials. This is to
        // avoid a race on filenames.
        let tmpdir = match TempDir::new("") {
            Ok(tmpdir) => tmpdir,
            Err(_) => fail!(),
        };

        match test.find(&"partials".to_string()) {
            Some(value) => write_partials(tmpdir.path(), value),
            None => {},
        }

        let ctx = Context::new(tmpdir.path().clone());
        let template = ctx.compile(template.as_slice().chars());
        let result = render_data(&template, &data);

        if result != expected {
            println!("desc:     {}", test.find(&"desc".to_string()).unwrap().to_string());
            println!("context:  {}", test.find(&"data".to_string()).unwrap().to_string());
            println!("=>");
            println!("template: {}", template);
            println!("expected: {}", expected);
            println!("actual:   {}", result);
            println!("");
        }
        assert_eq!(result, expected);
    }

    fn run_tests(spec: &str) {
        for json in parse_spec_tests(spec).move_iter() {
            let test = match json {
                json::Object(m) => m,
                _ => fail!(),
            };

            let data = match test.find(&"data".to_string()) {
                Some(data) => data.clone(),
                None => fail!(),
            };

            let mut encoder = Encoder::new();
            data.encode(&mut encoder).unwrap();
            assert_eq!(encoder.data.len(), 1);

            run_test(test, encoder.data.pop().unwrap());
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
        for json in parse_spec_tests("spec/specs/~lambdas.json").move_iter() {
            let mut test = match json {
                json::Object(m) => m,
                value => { fail!("{}", value) }
            };

            let s = match test.pop(&"name".to_string()) {
                Some(json::String(s)) => s,
                value => { fail!("{}", value) }
            };

            // Replace the lambda with rust code.
            let data = match test.pop(&"data".to_string()) {
                Some(data) => data,
                None => fail!(),
            };

            let mut encoder = Encoder::new();
            data.encode(&mut encoder).unwrap();

            let mut ctx = match encoder.data.pop().unwrap() {
                Map(ctx) => ctx,
                _ => fail!(),
            };

            // needed for the closure test.
            let mut calls = 0u;

            let f = match s.as_slice() {
                "Interpolation" => {
                    |_text| { "world".to_string() }
                }
                "Interpolation - Expansion" => {
                    |_text| { "{{planet}}".to_string() }
                }
                "Interpolation - Alternate Delimiters" => {
                    |_text| { "|planet| => {{planet}}".to_string() }
                }
                "Interpolation - Multiple Calls" => {
                    |_text| {
                        calls += 1;
                        calls.to_string()
                    }
                }
                "Escaping" => {
                    |_text| { ">".to_string() }
                }
                "Section" => {
                    |text: String| {
                        if text.as_slice() == "{{x}}" {
                            "yes".to_string()
                        } else {
                            "no".to_string()
                        }
                    }
                }
                "Section - Expansion" => {
                    |text: String| { text + "{{planet}}" + text }
                }
                "Section - Alternate Delimiters" => {
                    |text: String| { text + "{{planet}} => |planet|" + text }
                }
                "Section - Multiple Calls" => {
                    |text: String| { "__".to_string() + text + "__" }
                }
                "Inverted Section" => {
                    |_text| { "".to_string() }
                }

                value => { fail!("{}", value) }
            };

            ctx.insert("lambda".to_string(), Fun(RefCell::new(f)));

            run_test(test, Map(ctx));
        }
    }
}
