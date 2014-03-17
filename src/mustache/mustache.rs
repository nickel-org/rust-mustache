#[crate_id = "github.com/erickt/rust-mustache#mustache:0.1.0"];

extern crate std;
extern crate serialize;
extern crate collections;

use std::io::File;
use std::str;
use std::vec_ng::Vec;
use collections::hashmap::HashMap;

pub use parser::{Token, Parser};
pub use encoder::{Encoder, Data, Map, Vec, Bool, Str};

pub mod parser;
pub mod encoder;


/// Represents the shared metadata needed to compile and render a mustache
/// template.
#[deriving(Clone)]
pub struct Context {
    template_path: Path,
    template_extension: ~str,
}

pub struct Template {
    ctx: Context,
    tokens: Vec<Token>,
    partials: HashMap<~str, Vec<Token>>
}

impl Context {
    /// Configures a mustache context the specified path to the templates.
    pub fn new(path: Path) -> Context {
        Context {
            template_path: path,
            template_extension: ~"mustache",
        }
    }

    /// Compiles a template from a string
    // not sure why this needs to be generic as of right now...
    //fn compile(&self, reader: &Iterator<char>) -> Template {
    pub fn compile<IT: Iterator<char>>(&self, reader: IT) -> Template {
        let mut reader = reader;
        let mut ctx = CompileContext {
            reader: &mut reader,
            partials: HashMap::new(),
            otag: ~"{{",
            ctag: ~"}}",
            template_path: self.template_path.clone(),
            template_extension: self.template_extension.to_owned(),
        };

        let tokens = ctx.compile();

        Template {
            ctx: self.clone(),
            tokens: tokens,
            partials: ctx.partials,
        }
    }

    pub fn compile_path(&self, path: Path) -> Option<Template> {
        // FIXME(#6164): This should use the file decoding tools when they are
        // written. For now we'll just read the file and treat it as UTF-8file.
        let mut path = self.template_path.join(path);
        path.set_extension(self.template_extension.clone());

        //let s = match File::open(&path) {
        //    Some(mut reader) => str::from_utf8_owned(reader.read_to_end()),
        //    None => { return None; }
        //};
        let s = match File::open(&path).read_to_end() {
            Ok(str) => str,
            Err(e) => {println!("failed to read file: {}", e); return None;}
        };
        // TODO: maybe allow UTF-16 as well?
        let template = match str::from_utf8(s) {
            Some(string) => string,
            None() => {println!("Error: File is not UTF-8 encoded"); return None;}
        };
        Some(self.compile(template.chars()))
    }

    /// Renders a template from a string.
    pub fn render<T: serialize::Encodable<Encoder>>(&self, reader: &str, data: &T) -> ~str {
        self.compile(reader.chars()).render(data)
    }
}

/// Compiles a template from an `Iterator<char>`.
pub fn compile_iter<T: Iterator<char>>(iter: T) -> Template {
    Context::new(Path::new(".")).compile(iter)
}

/// Compiles a template from a path.
// returns None if the file cannot be read OR the file is not UTF-8 encoded
pub fn compile_path(path: Path) -> Option<Template> {
    Context::new(Path::new(".")).compile_path(path)
}

/// Compiles a template from a string.
pub fn compile_str(template: &str) -> Template {
    Context::new(Path::new(".")).compile(template.chars())
}

/// Renders a template from an `Iterator<char>`.
pub fn render_iter<IT: Iterator<char>, T: serialize::Encodable<Encoder>>(reader: IT, data: &T) -> ~str {
    compile_iter(reader).render(data)
}

/// Renders a template from a file.
pub fn render_path<T: serialize::Encodable<Encoder>>(path: Path, data: &T) -> Option<~str> {
    compile_path(path).and_then(|template| {
        Some(template.render(data))
    })
}

/// Renders a template from a string.
pub fn render_str<T: serialize::Encodable<Encoder>>(template: &str, data: &T) -> ~str {
    compile_str(template).render(data)
}

impl Template {
    pub fn render<T: serialize::Encodable<Encoder> >(&self, data: &T) -> ~str {
        let mut encoder = Encoder::new();
        data.encode(&mut encoder);
        assert_eq!(encoder.data.len(), 1);
        let popped = match encoder.data.pop() {
            Some(p) => p,
            None => fail!("Error: Nothing to pop!"),
        };
        self.render_data(popped)
    }

    pub fn render_data(&self, data: Data) -> ~str {
        render_helper(&RenderContext {
            ctx: self.ctx.clone(),
            // FIXME: #rust/9382
            // This should be `tokens: self.tokens,` but that's broken
            tokens: self.tokens.as_slice(),
            partials: self.partials.clone(),
            stack: vec!(data),
            indent: ~""
        })
    }
}


struct CompileContext<'a, T> {
    reader: &'a mut T,
    partials: HashMap<~str, Vec<Token>>,
    otag: ~str,
    ctag: ~str,
    template_path: Path,
    template_extension: ~str,
}

impl<'a, T: Iterator<char>> CompileContext<'a, T> {
    pub fn compile(&mut self) -> Vec<Token> {
        let mut parser = Parser {
            reader: self.reader,
            ch: None,
            lookahead: None,
            line: 1,
            col: 1,
            content: ~"",
            state: parser::TEXT,
            otag: self.otag.to_owned(),
            ctag: self.ctag.to_owned(),
            otag_chars: self.otag.chars().collect(),
            ctag_chars: self.ctag.chars().collect(),
            tag_position: 0,
            tokens: Vec::new(),
            partials: Vec::new(),
        };

        parser.bump();
        parser.parse();

        // Compile the partials if we haven't done so already.
        for name in parser.partials.iter() {
            let path = self.template_path.join(*name + "." + self.template_extension);

            if !self.partials.contains_key(name) {
                // Insert a placeholder so we don't recurse off to infinity.
                self.partials.insert(name.to_owned(), Vec::new());
                match File::open(&path).read_to_end() {
                    Ok(contents) => {

                        let iter = match str::from_utf8_owned(contents) {
                            Some(string) => string, //.chars().clone(),
                            None => {fail!("Failed to parse file as UTF-8");}
                        };

                        let mut inner_ctx = CompileContext {
                            reader: &mut iter.chars(),
                            partials: self.partials.clone(),
                            otag: ~"{{",
                            ctag: ~"}}",
                            template_path: self.template_path.clone(),
                            template_extension: self.template_extension.to_owned(),
                        };
                        let tokens = inner_ctx.compile();

                        self.partials.insert(name.to_owned(), tokens);
                    },
                    Err(e) => {println!("failed to read file {}", e);}
                };
            }
        }

        // Destructure the parser so we get get at the tokens without a copy.
        let Parser { tokens: tokens, .. } = parser;

        tokens
    }
}

#[deriving(Clone)]
struct RenderContext<'a> {
    ctx: Context,
    tokens: &'a [Token],
    partials: HashMap<~str, Vec<Token>>,
    stack: Vec<Data>,
    indent: ~str,
}

fn render_helper(ctx: &RenderContext) -> ~str {
    fn find(stack: &[Data], path: &[~str]) -> Option<Data> {
        // If we have an empty path, we just want the top value in our stack.
        if path.is_empty() {
            return match stack.last() {
                None => None,
                Some(value) => Some(value.clone()),
            };
        }

        // Otherwise, find the stack that has the first part of our path.
        let mut value: Option<Data> = None;

        let mut i = stack.len();
        while i > 0 {
            match stack[i - 1] {
                Map(ref ctx) => {
                    match ctx.find_equiv(&path[0]) {
                        Some(v) => { value = Some(v.clone()); break; }
                        None => {}
                    }
                    i -= 1;
                }
                _ => {
                    fail!("{:?} {:?}", stack, path)
                }
            }
        }

        // Walk the rest of the path to find our final value.
        let mut value = value;

        let mut i = 1;
        let len = path.len();

        while i < len {
            value = match value {
                Some(Map(v)) => {
                    match v.find_equiv(&path[i]) {
                        Some(value) => Some(value.clone()),
                        None => None,
                    }
                }
                _ => break,
            };
            i = i + 1;
        }

        value
    }

    let mut output = ~"";

    for token in ctx.tokens.iter() {
        match *token {
            parser::Text(ref value) => {
                // Indent the lines.
                if ctx.indent.equiv(& &"") {
                    output = output + *value;
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
                            output.push_str(ctx.indent);
                        }

                        output.push_str(line);
                    }
                }
            },
            parser::ETag(ref path, _) => {
                match find(ctx.stack.as_slice(), path.as_slice()) {
                    None => { }
                    Some(value) => {
                        output = output + ctx.indent + render_etag(value, ctx);
                    }
                }
            }
            parser::UTag(ref path, _) => {
                match find(ctx.stack.as_slice(), path.as_slice()) {
                    None => { }
                    Some(value) => {
                        output = output + ctx.indent + render_utag(value, ctx);
                    }
                }
            }
            parser::Section(ref path, true, ref children, _, _, _, _, _) => {
                let ctx = RenderContext {
                    // FIXME: #rust/9382
                    // This should be `tokens: *children,` but that's broken
                    tokens: children.as_slice(),
                    .. ctx.clone()
                };

                output = output + match find(ctx.stack.as_slice(), path.as_slice()) {
                    None => { render_helper(&ctx) }
                    Some(value) => { render_inverted_section(value, &ctx) }
                };
            }
            parser::Section(ref path, false, ref children, ref otag, _, ref src, _, ref ctag) => {
                match find(ctx.stack.as_slice(), path.as_slice()) {
                    None => { }
                    Some(value) => {
                        output = output + render_section(
                            value,
                            *src,
                            *otag,
                            *ctag,
                            &RenderContext {
                                // FIXME: #rust/9382
                                // This should be `tokens: *children,` but that's broken
                                tokens: children.as_slice(),
                                .. ctx.clone()
                            }
                        );
                    }
                }
            }
            parser::Partial(ref name, ref ind, _) => {
                match ctx.partials.find(name) {
                    None => { }
                    Some(tokens) => {
                        output = output + render_helper(&RenderContext {
                            // FIXME: #rust/9382
                            // This should be `tokens: *tokens,` but that's broken
                            tokens: tokens.as_slice(),
                            indent: ctx.indent + *ind,
                            .. ctx.clone()
                        });
                    }
                }
            }
            _ => { fail!() }
        };
    };

    output
}

fn render_etag(value: Data, ctx: &RenderContext) -> ~str {
    let mut escaped = ~"";
    let utag = render_utag(value, ctx);
    for c in utag.chars() {
        match c {
            '<' => { escaped.push_str("&lt;"); }
            '>' => { escaped.push_str("&gt;"); }
            '&' => { escaped.push_str("&amp;"); }
            '"' => { escaped.push_str("&quot;"); }
            '\'' => { escaped.push_str("&#39;"); }
            _ => { escaped.push_char(c); }
        }
    }
    escaped
}

fn render_utag(value: Data, _ctx: &RenderContext) -> ~str {
    match value {
        Str(ref s) => s.clone(),

        // etags and utags use the default delimiter.
        //Fun(f) => render_fun(ctx, ~"", ~"{{", ~"}}", f),

        _ => fail!(),
    }
}

fn render_inverted_section(value: Data, ctx: &RenderContext) -> ~str {
    match value {
        Bool(false) => render_helper(ctx),
        Vec(ref xs) if xs.len() == 0 => render_helper(ctx),
        _ => ~"",
    }
}

fn render_section(value: Data,
                  _src: &str,
                  _otag: &str,
                  _ctag: &str,
                  ctx: &RenderContext) -> ~str {
    match value {
        Bool(true) => render_helper(ctx),
        Bool(false) => ~"",
        Vec(vs) => {
            vs.move_iter().map(|v| {
                let mut stack = ctx.stack.clone();
                stack.push(v.clone());

                render_helper(&RenderContext { stack: stack, .. (*ctx).clone() })
            }).collect::<Vec<~str>>().concat()
        }
        Map(_) => {
            let mut stack = ctx.stack.clone();
            stack.push(value);

            render_helper(&RenderContext { stack: stack, .. (*ctx).clone() })
        }
        //Fun(f) => render_fun(ctx, src, otag, ctag, f),
        _ => fail!(),
    }
}

/*
fn render_fun(ctx: &RenderContext,
              src: &str,
              otag: &str,
              ctag: &str,
              f: |&str| -> ~str) -> ~str {
    let src = f(src);
    let mut iter = src.chars();

    let mut inner_ctx = CompileContext {
        reader: &mut iter,
        partials: ctx.partials.clone(),
        otag: otag.to_owned(),
        ctag: ctag.to_owned(),
        template_path: ctx.ctx.template_path.clone(),
        template_extension: ctx.ctx.template_extension.to_owned(),
    };
    let tokens = inner_ctx.compile();

    render_helper(&RenderContext {
        // FIXME: #rust/9382
        // This should be `tokens: tokens,` but that's broken
        tokens: tokens.as_slice(),
        .. ctx.clone()
    })
}
*/
