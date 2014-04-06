use std::io::MemWriter;
use std::mem;
use std::str;
use serialize::Encodable;
use collections::HashMap;

use compiler::Compiler;
use parser::{Token, Text, ETag, UTag, Section, Partial};

use encoder;
use encoder::{Encoder, Error, Data, Bool, Str, Vec, Map, Fun};

use super::Context;

pub struct Template {
    ctx: Context,
    tokens: Vec<Token>,
    partials: HashMap<~str, Vec<Token>>
}

impl Template {
    pub fn new(ctx: Context, tokens: Vec<Token>, partials: HashMap<~str, Vec<Token>>) -> Template {
        Template {
            ctx: ctx,
            tokens: tokens,
            partials: partials,
        }
    }

    pub fn render<
        'a,
        W: Writer,
        T: Encodable<Encoder<'a>, Error>
    >(&self, wr: &mut W, data: &T) -> Result<(), Error> {
        let data = try!(encoder::encode(data));
        Ok(self.render_data(wr, &data))
    }

    pub fn render_str<
        'a,
        T: Encodable<Encoder<'a>, Error>
    >(&self, data: &T) -> Result<~str, Error> {
        let mut wr = MemWriter::new();

        match self.render(&mut wr, data) {
            Ok(()) => Ok(str::from_utf8_owned(wr.unwrap()).unwrap()),
            Err(err) => Err(err),
        }
    }

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
    indent: ~str,
}

impl<'a> RenderContext<'a> {
    fn new(template: &'a Template) -> RenderContext<'a> {
        RenderContext {
            template: template,
            indent: ~"",
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
                self.render_text(wr, *value);
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
                    *src,
                    *otag,
                    *ctag)
            }
            Partial(ref name, ref indent, _) => {
                self.render_partial(wr, stack, *name, *indent);
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
        if self.indent.equiv(& &"") {
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
                    wr.write_str(self.indent).unwrap();
                }

                wr.write_str(line).unwrap();
            }
        }
    }

    fn render_etag<'b, W: Writer>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data<'b>>,
        path: &[~str]
    ) {
        let mut mem_wr = MemWriter::new();

        self.render_utag(&mut mem_wr, stack, path);

        let bytes = mem_wr.unwrap();
        let s = str::from_utf8_owned(bytes).unwrap();

        for c in s.chars() {
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
        path: &[~str]
    ) {
        match self.find(path, stack) {
            None => { }
            Some(value) => {
                wr.write_str(self.indent).unwrap();

                match *value {
                    Str(ref value) => {
                        wr.write_str(*value).unwrap();
                    }

                    // etags and utags use the default delimiter.
                    Fun(ref f) => {
                        let tokens = self.render_fun("", "{{", "}}", f);
                        self.render(wr, stack, tokens.as_slice());
                    }

                    ref value => { fail!("unexpected value {:?}", value); }
                }
            }
        };
    }

    fn render_inverted_section<'b, W: Writer>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data<'b>>,
        path: &[~str],
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
        path: &[~str],
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
                    _ => { fail!("unexpected value {:?}", value) }
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

    fn render_fun<'b>(
        &self,
        src: &str,
        otag: &str,
        ctag: &str,
        f: & 'b |~str| -> ~str
    ) -> Vec<Token> {
        let src = (*f)(src.to_owned());

        let compiler = Compiler::new_with(
            self.template.ctx.clone(),
            src.chars(),
            self.template.partials.clone(),
            otag.to_owned(),
            ctag.to_owned());

        let (tokens, _) = compiler.compile();
        tokens
    }

    fn find<'b, 'c>(&self, path: &[~str], stack: &mut Vec<&'c Data<'b>>) -> Option<&'c Data<'b>> {
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
                _ => { fail!("expect map: {:?}", path) }
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
    use std::str;
    use std::io::{File, MemWriter, TempDir};
    use collections::HashMap;
    use serialize::json;
    use serialize::Encodable;

    use encoder::{Encoder, Error, Data, Str, Vec, Map, Fun};

    use super::super::compile_str;
    use super::super::{Context, Template};

    #[deriving(Encodable)]
    struct Name { name: ~str }

    fn render<'a, 'b, T: Encodable<Encoder<'b>, Error>>(
        template: &str,
        data: &T,
    ) -> Result<~str, Error> {
        let template = compile_str(template);
        template.render_str(data)
    }

    #[test]
    fn test_render_texts() {
        let ctx = Name { name: ~"world" };

        assert_eq!(render("hello world", &ctx), Ok(~"hello world"));
        assert_eq!(render("hello {world", &ctx), Ok(~"hello {world"));
        assert_eq!(render("hello world}", &ctx), Ok(~"hello world}"));
        assert_eq!(render("hello {world}", &ctx), Ok(~"hello {world}"));
        assert_eq!(render("hello world}}", &ctx), Ok(~"hello world}}"));
    }

    #[test]
    fn test_render_etags() {
        let ctx = Name { name: ~"world" };

        assert_eq!(render("hello {{name}}", &ctx), Ok(~"hello world"));
    }

    #[test]
    fn test_render_utags() {
        let ctx = Name { name: ~"world" };

        assert_eq!(render("hello {{{name}}}", &ctx), Ok(~"hello world"));
    }

    fn render_data<'a>(template: &Template, data: &Data<'a>) -> ~str {
        let mut wr = MemWriter::new();
        template.render_data(&mut wr, data);
        str::from_utf8_owned(wr.unwrap()).unwrap()
    }

    #[test]
    fn test_render_sections() {
        let ctx = HashMap::new();
        let template = compile_str("0{{#a}}1 {{n}} 3{{/a}}5");

        assert_eq!(render_data(&template, &Map(ctx)), ~"05");

        let mut ctx = HashMap::new();
        ctx.insert(~"a", Vec(Vec::new()));

        assert_eq!(render_data(&template, &Map(ctx)), ~"05");

        let mut ctx = HashMap::new();
        ctx.insert(~"a", Vec(Vec::new()));
        assert_eq!(render_data(&template, &Map(ctx)), ~"05");

        let mut ctx0 = HashMap::new();
        let ctx1 = HashMap::new();
        ctx0.insert(~"a", Vec(vec!(Map(ctx1))));

        assert_eq!(render_data(&template, &Map(ctx0)), ~"01  35");

        let mut ctx0 = HashMap::new();
        let mut ctx1 = HashMap::new();
        ctx1.insert(~"n", Str(~"a"));
        ctx0.insert(~"a", Vec(vec!(Map(ctx1))));
        assert_eq!(render_data(&template, &Map(ctx0)), ~"01 a 35");

        let mut ctx = HashMap::new();
        ctx.insert(~"a", Fun(|_text| ~"foo"));
        assert_eq!(render_data(&template, &Map(ctx)), ~"0foo5");
    }

    #[test]
    fn test_render_inverted_sections() {
        let template = compile_str("0{{^a}}1 3{{/a}}5");

        let ctx = HashMap::new();
        assert_eq!(render_data(&template, &Map(ctx)), ~"01 35");

        let mut ctx = HashMap::new();
        ctx.insert(~"a", Vec(vec!()));
        assert_eq!(render_data(&template, &Map(ctx)), ~"01 35");

        let mut ctx0 = HashMap::new();
        let ctx1 = HashMap::new();
        ctx0.insert(~"a", Vec(vec!(Map(ctx1))));
        assert_eq!(render_data(&template, &Map(ctx0)), ~"05");

        let mut ctx0 = HashMap::new();
        let mut ctx1 = HashMap::new();
        ctx1.insert(~"n", Str(~"a"));
        ctx0.insert(~"a", Vec(vec!(Map(ctx1))));
        assert_eq!(render_data(&template, &Map(ctx0)), ~"05");
    }

    #[test]
    fn test_render_partial() {
        let template = Context::new(Path::new("src/test-data"))
            .compile_path(Path::new("base"))
            .unwrap();

        let ctx = HashMap::new();
        assert_eq!(render_data(&template, &Map(ctx)), ~"<h2>Names</h2>\n");

        let mut ctx = HashMap::new();
        ctx.insert(~"names", Vec(vec!()));
        assert_eq!(render_data(&template, &Map(ctx)), ~"<h2>Names</h2>\n");

        let mut ctx0 = HashMap::new();
        let ctx1 = HashMap::new();
        ctx0.insert(~"names", Vec(vec!(Map(ctx1))));
        assert_eq!(
            render_data(&template, &Map(ctx0)),
            ~"<h2>Names</h2>\n  <strong></strong>\n\n");

        let mut ctx0 = HashMap::new();
        let mut ctx1 = HashMap::new();
        ctx1.insert(~"name", Str(~"a"));
        ctx0.insert(~"names", Vec(vec!(Map(ctx1))));
        assert_eq!(
            render_data(&template, &Map(ctx0)),
            ~"<h2>Names</h2>\n  <strong>a</strong>\n\n");

        let mut ctx0 = HashMap::new();
        let mut ctx1 = HashMap::new();
        ctx1.insert(~"name", Str(~"a"));
        let mut ctx2 = HashMap::new();
        ctx2.insert(~"name", Str(~"<b>"));
        ctx0.insert(~"names", Vec(vec!(Map(ctx1), Map(ctx2))));
        assert_eq!(
            render_data(&template, &Map(ctx0)),
            ~"<h2>Names</h2>\n  <strong>a</strong>\n\n  <strong>&lt;b&gt;</strong>\n\n");
    }

    fn parse_spec_tests(src: &str) -> Vec<json::Json> {
        let path = Path::new(src);

        let file_contents = match File::open(&path).read_to_end() {
            Ok(reader) => reader,
            Err(e) => fail!("Could not read file {}", e),
        };

        let s = match str::from_utf8_owned(file_contents){
            Some(str) => str,
            None => {fail!("File was not UTF8 encoded");}
        };

        match json::from_str(s) {
            Err(e) => fail!(e.to_str()),
            Ok(json) => {
                match json {
                    json::Object(d) => {
                        let mut d = d;
                        match d.pop(&~"tests") {
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

    fn run_test(test: ~json::Object, data: Data) {
        let template = match test.find(&~"template") {
            Some(&json::String(ref s)) => s.clone(),
            _ => fail!(),
        };

        let expected = match test.find(&~"expected") {
            Some(&json::String(ref s)) => s.clone(),
            _ => fail!(),
        };

        // Make a temporary dir where we'll store our partials. This is to
        // avoid a race on filenames.
        let tmpdir = match TempDir::new("") {
            Some(tmpdir) => tmpdir,
            None => fail!(),
        };

        match test.find(&~"partials") {
            Some(value) => write_partials(tmpdir.path(), value),
            None => {},
        }

        let ctx = Context::new(tmpdir.path().clone());
        let template = ctx.compile(template.chars());
        let result = render_data(&template, &data);

        if result != expected {
            println!("desc:     {}", test.find(&~"desc").unwrap().to_str());
            println!("context:  {}", test.find(&~"data").unwrap().to_str());
            println!("=>");
            println!("template: {:?}", template);
            println!("expected: {:?}", expected);
            println!("actual:   {:?}", result);
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

            let data = match test.find(&~"data") {
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
                value => { fail!("{:?}", value) }
            };

            let s = match test.pop(&~"name") {
                Some(json::String(s)) => s,
                value => { fail!("{:?}", value) }
            };

            // Replace the lambda with rust code.
            let data = match test.pop(&~"data") {
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
            let mut calls = 0;

            let f = match s.as_slice() {
                "Interpolation" => {
                    |_text| { ~"world" }
                }
                "Interpolation - Expansion" => {
                    |_text| { ~"{{planet}}" }
                }
                "Interpolation - Alternate Delimiters" => {
                    |_text| { ~"|planet| => {{planet}}" }
                }
                "Interpolation - Multiple Calls" => {
                    |_text| {
                        calls += 1;
                        calls.to_str()
                    }
                }
                "Escaping" => {
                    |_text| { ~">" }
                }
                "Section" => {
                    |text: ~str| {
                        if text == ~"{{x}}" {
                            ~"yes"
                        } else {
                            ~"no"
                        }
                    }
                }
                "Section - Expansion" => {
                    |text: ~str| { text + "{{planet}}" + text }
                }
                "Section - Alternate Delimiters" => {
                    |text: ~str| { text + "{{planet}} => |planet|" + text }
                }
                "Section - Multiple Calls" => {
                    |text: ~str| { ~"__" + text + "__" }
                }
                "Inverted Section" => {
                    |_text| { ~"" }
                }

                value => { fail!("{:?}", value) }
            };

            ctx.insert(~"lambda", Fun(f));

            run_test(test, Map(ctx));
        }
    }
}
