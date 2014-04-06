#![crate_id = "github.com/erickt/rust-mustache#mustache:0.1.0"]

#![license = "MIT/ASL2"]
#![crate_type = "dylib"]
#![crate_type = "rlib"]

extern crate std;
extern crate serialize;
extern crate collections;

use std::char;
use std::io::{File, MemWriter};
use std::mem;
use std::str;
use collections::hashmap::HashMap;

pub use encoder::{Encoder, EncoderResult, Data, Map, Vec, Bool, Str, Fun};
pub use encoder::{Error, InvalidStr, IoError};

pub mod encoder;

/// Represents the shared metadata needed to compile and render a mustache
/// template.
#[deriving(Clone)]
pub struct Context {
    template_path: Path,
    template_extension: ~str,
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

    /// Compiles a template from a path.
    pub fn compile_path(&self, path: Path) -> Result<Template, Error> {
        // FIXME(#6164): This should use the file decoding tools when they are
        // written. For now we'll just read the file and treat it as UTF-8file.
        let mut path = self.template_path.join(path);
        path.set_extension(self.template_extension.clone());

        let s = match File::open(&path).read_to_end() {
            Ok(s) => s,
            Err(err) => { return Err(IoError(err)); }
        };

        // TODO: maybe allow UTF-16 as well?
        let template = match str::from_utf8(s) {
            Some(string) => string,
            None => { return Err(InvalidStr); }
        };

        Ok(self.compile(template.chars()))
    }

    /// Renders a template from a string.
    pub fn render<
        'a,
        T: serialize::Encodable<Encoder<'a>, Error>
    >(&self, reader: &str, data: &T) -> Result<~str, Error> {
        let template = self.compile(reader.chars());

        let mut wr = MemWriter::new();

        match template.render(&mut wr, data) {
            Ok(()) => Ok(str::from_utf8_owned(wr.unwrap()).unwrap()),
            Err(err) => Err(err),
        }
    }
}

/// Compiles a template from an `Iterator<char>`.
pub fn compile_iter<T: Iterator<char>>(iter: T) -> Template {
    Context::new(Path::new(".")).compile(iter)
}

/// Compiles a template from a path.
/// returns None if the file cannot be read OR the file is not UTF-8 encoded
pub fn compile_path(path: Path) -> Result<Template, Error> {
    Context::new(Path::new(".")).compile_path(path)
}

/// Compiles a template from a string.
pub fn compile_str(template: &str) -> Template {
    Context::new(Path::new(".")).compile(template.chars())
}

/// Renders a template from an `Iterator<char>`.
pub fn render_iter<
    'a,
    IT: Iterator<char>,
    T: serialize::Encodable<Encoder<'a>, Error>,
    W: Writer
>(reader: IT, wr: &mut W, data: &T) -> Result<(), Error> {
    let template = compile_iter(reader);
    template.render(wr, data)
}

/// Renders a template from a file.
pub fn render_path<
    'a,
    T: serialize::Encodable<Encoder<'a>, Error>
>(path: Path, data: &T) -> Result<~str, Error> {
    let template = try!(compile_path(path));
    
    let mut wr = MemWriter::new();
    try!(template.render(&mut wr, data));
    Ok(str::from_utf8_owned(wr.unwrap()).unwrap())
}

/// Renders a template from a string.
pub fn render_str<
    'a,
    T: serialize::Encodable<Encoder<'a>, Error>
>(template: &str, data: &T) -> Result<~str, Error> {
    let template = compile_str(template);

    let mut wr = MemWriter::new();
    try!(template.render(&mut wr, data));
    Ok(str::from_utf8_owned(wr.unwrap()).unwrap())
}

#[deriving(Clone)]
pub enum Token {
    Text(~str),
    ETag(Vec<~str>, ~str),
    UTag(Vec<~str>, ~str),
    Section(Vec<~str>, bool, Vec<Token>, ~str, ~str, ~str, ~str, ~str),
    IncompleteSection(Vec<~str>, bool, ~str, bool),
    Partial(~str, ~str, ~str),
}

#[deriving(Clone)]
pub enum TokenClass {
    Normal,
    StandAlone,
    WhiteSpace(~str, uint),
    NewLineWhiteSpace(~str, uint),
}

pub struct Parser<'a, T> {
    reader: &'a mut T,
    ch: Option<char>,
    lookahead: Option<char>,
    line: uint,
    col: uint,
    content: ~str,
    state: ParserState,
    otag: ~str,
    ctag: ~str,
    otag_chars: Vec<char>,
    ctag_chars: Vec<char>,
    tag_position: uint,
    tokens: Vec<Token>,
    partials: Vec<~str>,
}

pub enum ParserState { TEXT, OTAG, TAG, CTAG }

impl<'a, T: Iterator<char>> Parser<'a, T> {
    pub fn bump(&mut self) {
        match self.lookahead.take() {
            None => { self.ch = self.reader.next(); }
            Some(ch) => { self.ch = Some(ch); }
        }

        match self.ch {
            Some(ch) => {
                if ch == '\n' {
                    self.line += 1;
                    self.col = 1;
                } else {
                    self.col += 1;
                }
            }
            None => { }
        }
    }

    fn peek(&mut self) -> Option<char> {
        match self.lookahead {
            None => {
                self.lookahead = self.reader.next();
                self.lookahead
            }
            Some(ch) => Some(ch),
        }
    }

    fn ch_is(&self, ch: char) -> bool {
        match self.ch {
            Some(c) => c == ch,
            None => false,
        }
    }

    pub fn parse(&mut self) {
        let mut curly_brace_tag = false;

        loop {
            let ch = match self.ch {
                Some(ch) => ch,
                None => { break; }
            };

            match self.state {
                TEXT => {
                    if ch == *self.otag_chars.get(0) {
                        if self.otag_chars.len() > 1 {
                            self.tag_position = 1;
                            self.state = OTAG;
                        } else {
                            self.add_text();
                            self.state = TAG;
                        }
                    } else {
                        self.content.push_char(ch);
                    }
                    self.bump();
                }
                OTAG => {
                    if ch == *self.otag_chars.get(self.tag_position) {
                        if self.tag_position == self.otag_chars.len() - 1 {
                            self.add_text();
                            curly_brace_tag = false;
                            self.state = TAG;
                        } else {
                            self.tag_position = self.tag_position + 1;
                        }
                    } else {
                        // We don't have a tag, so add all the tag parts we've seen
                        // so far to the string.
                        self.state = TEXT;
                        self.not_otag();
                        self.content.push_char(ch);
                    }
                    self.bump();
                }
                TAG => {
                    if self.content == ~"" && ch == '{' {
                        curly_brace_tag = true;
                        self.content.push_char(ch);
                        self.bump();
                    } else if curly_brace_tag && ch == '}' {
                        curly_brace_tag = false;
                        self.content.push_char(ch);
                        self.bump();
                    } else if ch == *self.ctag_chars.get(0) {
                        if self.ctag_chars.len() > 1 {
                            self.tag_position = 1;
                            self.state = CTAG;
                            self.bump();
                        } else {
                            self.add_tag();
                            self.state = TEXT;
                        }
                    } else {
                        self.content.push_char(ch);
                        self.bump();
                    }
                }
                CTAG => {
                    if ch == *self.ctag_chars.get(self.tag_position) {
                        if self.tag_position == self.ctag_chars.len() - 1 {
                            self.add_tag();
                            self.state = TEXT;
                        } else {
                            self.state = TAG;
                            self.not_ctag();
                            self.content.push_char(ch);
                            self.bump();
                        }
                    } else {
                        fail!("character {} is not part of CTAG: {}",
                              ch,
                              *self.ctag_chars.get(self.tag_position));
                    }
                }
            }
        }

        match self.state {
            TEXT => { self.add_text(); }
            OTAG => { self.not_otag(); self.add_text(); }
            TAG => { fail!(~"unclosed tag"); }
            CTAG => { self.not_ctag(); self.add_text(); }
        }

        // Check that we don't have any incomplete sections.
        for token in self.tokens.iter() {
            match *token {
                IncompleteSection(ref path, _, _, _) => {
                    fail!("Unclosed mustache section {}", path.connect("."));
              }
              _ => {}
            }
        };
    }

    fn add_text(&mut self) {
        if self.content != ~"" {
            let mut content = ~"";
            mem::swap(&mut content, &mut self.content);

            self.tokens.push(Text(content));
        }
    }

    // This function classifies whether or not a token is standalone, or if it
    // has trailing whitespace. It's looking for this pattern:
    //
    //   ("\n" | "\r\n") whitespace* token ("\n" | "\r\n")
    //
    fn classify_token(&mut self) -> TokenClass {
        // Exit early if the next character is not '\n' or '\r\n'.
        match self.ch {
            None => { }
            Some(ch) => {
                if !(ch == '\n' || (ch == '\r' && self.peek() == Some('\n'))) {
                    return Normal;
                }
            }
        }

        match self.tokens.last() {
            // If the last token ends with a newline (or there is no previous
            // token), then this token is standalone.
            None => { StandAlone }

            Some(&IncompleteSection(_, _, _, true)) => { StandAlone }

            Some(&Text(ref s)) if !s.is_empty() => {
                // Look for the last newline character that may have whitespace
                // following it.
                match s.rfind(|c:char| c == '\n' || !char::is_whitespace(c)) {
                    // It's all whitespace.
                    None => {
                        if self.tokens.len() == 1 {
                            WhiteSpace(s.to_owned(), 0)
                        } else {
                            Normal
                        }
                    }
                    Some(pos) => {
                        if s.char_at(pos) == '\n' {
                            if pos == s.len() - 1 {
                                StandAlone
                            } else {
                                WhiteSpace(s.to_owned(), pos + 1)
                            }
                        } else { Normal }
                    }
                }
            }
            Some(_) => Normal,
        }
    }

    fn eat_whitespace(&mut self) -> bool {
        // If the next character is a newline, and the last token ends with a
        // newline and whitespace, clear out the whitespace.

        match self.classify_token() {
            Normal => { false }
            StandAlone => {
                if self.ch_is('\r') { self.bump(); }
                self.bump();
                true
            }
            WhiteSpace(s, pos) | NewLineWhiteSpace(s, pos) => {
                if self.ch_is('\r') { self.bump(); }
                self.bump();

                // Trim the whitespace from the last token.
                self.tokens.pop();
                self.tokens.push(Text(s.slice(0, pos).to_str()));

                true
            }
        }
    }

    fn add_tag(&mut self) {
        self.bump();

        let tag = self.otag + self.content + self.ctag;

        // Move the content to avoid a copy.
        let mut content = ~"";
        mem::swap(&mut content, &mut self.content);
        let len = content.len();

        match content[0] as char {
            '!' => {
                // ignore comments
                self.eat_whitespace();
            }
            '&' => {
                let name = content.slice(1, len);
                let name = self.check_content(name);
                let name = name.split_terminator('.')
                    .map(|x| x.to_owned())
                    .collect();
                self.tokens.push(UTag(name, tag));
            }
            '{' => {
                if content.ends_with("}") {
                    let name = content.slice(1, len - 1);
                    let name = self.check_content(name);
                    let name = name.split_terminator('.')
                        .map(|x| x.to_owned())
                        .collect();
                    self.tokens.push(UTag(name, tag));
                } else { fail!(~"unbalanced \"{\" in tag"); }
            }
            '#' => {
                let newlined = self.eat_whitespace();

                let name = self.check_content(content.slice(1, len));
                let name = name.split_terminator('.')
                    .map(|x| x.to_owned())
                    .collect();
                self.tokens.push(IncompleteSection(name, false, tag, newlined));
            }
            '^' => {
                let newlined = self.eat_whitespace();

                let name = self.check_content(content.slice(1, len));
                let name = name.split_terminator('.')
                    .map(|x| x.to_owned())
                    .collect();
                self.tokens.push(IncompleteSection(name, true, tag, newlined));
            }
            '/' => {
                self.eat_whitespace();

                let name = self.check_content(content.slice(1, len));
                let name = name.split_terminator('.')
                    .map(|x| x.to_owned())
                    .collect();
                let mut children: Vec<Token> = Vec::new();

                loop {
                    if self.tokens.len() == 0 {
                        fail!(~"closing unopened section");
                    }

                    let last = self.tokens.pop();

                    match last {
                        Some(IncompleteSection(section_name, inverted, osection, _)) => {
                            children.reverse();

                            // Collect all the children's sources.
                            let mut srcs = Vec::new();
                            for child in children.iter() {
                                match *child {
                                    Text(ref s)
                                    | ETag(_, ref s)
                                    | UTag(_, ref s)
                                    | Partial(_, _, ref s) => {
                                        srcs.push(s.clone())
                                    }
                                    Section(_, _, _, _, ref osection, ref src, ref csection, _) => {
                                        srcs.push(osection.clone());
                                        srcs.push(src.clone());
                                        srcs.push(csection.clone());
                                    }
                                    _ => fail!(),
                                }
                            }

                            if section_name == name {
                                // Cache the combination of all the sources in the
                                // section. It's unfortunate, but we need to do this in
                                // case the user uses a function to instantiate the
                                // tag.
                                let mut src = ~"";
                                for s in srcs.iter() { src.push_str(*s); }

                                self.tokens.push(
                                    Section(
                                        name,
                                        inverted,
                                        children,
                                        self.otag.to_owned(),
                                        osection,
                                        src,
                                        tag,
                                        self.ctag.to_owned()));
                                break;
                            } else {
                                fail!(~"Unclosed section");
                            }
                        }
                        _ => { match last {
                            Some(last_token) => {children.push(last_token); }
                            None => ()
                            }
                        }
                    }
                }
            }
            '>' => { self.add_partial(content, tag); }
            '=' => {
                self.eat_whitespace();

                if len > 2u && content.ends_with("=") {
                    let s = self.check_content(content.slice(1, len - 1));

                    let pos = s.find(char::is_whitespace);
                    let pos = match pos {
                      None => { fail!("invalid change delimiter tag content"); }
                      Some(pos) => { pos }
                    };

                    self.otag = s.slice(0, pos).to_str();
                    self.otag_chars = self.otag.chars().collect();

                    let s2 = s.slice_from(pos);
                    let pos = s2.find(|c| !char::is_whitespace(c));
                    let pos = match pos {
                      None => { fail!("invalid change delimiter tag content"); }
                      Some(pos) => { pos }
                    };

                    self.ctag = s2.slice_from(pos).to_str();
                    self.ctag_chars = self.ctag.chars().collect();
                } else {
                    fail!("invalid change delimiter tag content");
                }
            }
            _ => {
                // If the name is "." then we want the top element, which we represent with
                // an empty name.
                let name = self.check_content(content);
                let name = if name == ~"." {
                    Vec::new()
                } else {
                    name.split_terminator('.')
                        .map(|x| x.to_owned())
                        .collect()
                };

                self.tokens.push(ETag(name, tag));
            }
        }
    }

    fn add_partial(&mut self, content: &str, tag: ~str) {
        let indent = match self.classify_token() {
            Normal => ~"",
            StandAlone => {
                if self.ch_is('\r') { self.bump(); }
                self.bump();
                ~""
            }
            WhiteSpace(s, pos) | NewLineWhiteSpace(s, pos) => {
                if self.ch_is('\r') { self.bump(); }
                self.bump();

                let ws = s.slice(pos, s.len());

                // Trim the whitespace from the last token.
                self.tokens.pop();
                self.tokens.push(Text(s.slice(0, pos).to_str()));

                ws.to_owned()
            }
        };

        // We can't inline the tokens directly as we may have a recursive
        // partial. So instead, we'll cache the partials we used and look them
        // up later.
        let name = content.slice(1, content.len());
        let name = self.check_content(name);

        self.tokens.push(Partial(name.to_owned(), indent, tag));
        self.partials.push(name);
    }

    fn not_otag(&mut self) {
        for (i, ch) in self.otag_chars.iter().enumerate() {
            if !(i < self.tag_position) {
                break
            }
            self.content.push_char(*ch);
        }
    }

    fn not_ctag(&mut self) {
        for (i, ch) in self.ctag_chars.iter().enumerate() {
            if !(i < self.tag_position) {
                break
            }
            self.content.push_char(*ch);
        }
    }

    fn check_content(&self, content: &str) -> ~str {
        let trimmed = content.trim();
        if trimmed.len() == 0 {
            fail!(~"empty tag");
        }
        trimmed.to_owned()
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
            state: TEXT,
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

pub struct Template {
    ctx: Context,
    tokens: Vec<Token>,
    partials: HashMap<~str, Vec<Token>>
}

impl Template {
    pub fn render<
        'a,
        'b,
        W: Writer,
        T: serialize::Encodable<Encoder<'b>, Error>
    >(&self, wr: &mut W, data: &T) -> Result<(), Error> {
        let data = try!(encoder::encode(data));
        Ok(self.render_data(wr, &data))
    }

    pub fn render_str<
        'a,
        'b,
        T: serialize::Encodable<Encoder<'b>, Error>
    >(&self, data: &T) -> Result<~str, Error> {
        let mut wr = MemWriter::new();

        match self.render(&mut wr, data) {
            Ok(()) => Ok(str::from_utf8_owned(wr.unwrap()).unwrap()),
            Err(err) => Err(err),
        }
    }

    pub fn render_data<'a, 'b, W: Writer>(&'a self, wr: &mut W, data: &Data<'b>) {
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
        let mut iter = src.chars();

        let mut inner_ctx = CompileContext {
            reader: &mut iter,
            partials: self.template.partials.clone(),
            otag: otag.to_owned(),
            ctag: ctag.to_owned(),
            template_path: self.template.ctx.template_path.clone(),
            template_extension: self.template.ctx.template_extension.to_owned(),
        };

        inner_ctx.compile()
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
    use collections::hashmap::HashMap;
    use serialize::json;
    use serialize::Encodable;

    use super::compile_str;
    //use super::{compile_str, render_str};
    use super::{Context, Template};
    use super::encoder::{Encoder, Error, Data, Str, Vec, Map, Fun};
    use super::{Token, Text, ETag, UTag, Section, IncompleteSection, Partial};

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

    fn check_tokens(actual: Vec<Token>, expected: &[Token]) -> bool {
        // TODO: equality is currently broken for enums
        let actual: Vec<~str> = actual.iter().map(token_to_str).collect();
        let expected = expected.iter().map(token_to_str).collect();

        if actual != expected {
            println!("Found {:?}, but expected {:?}", actual.as_slice(), expected.as_slice());
            return false;
        }

        return true;
    }

    #[test]
    fn test_compile_texts() {
        assert!(check_tokens(compile_str("hello world").tokens, [
            Text(~"hello world")
        ]));
        assert!(check_tokens(compile_str("hello {world").tokens, [
            Text(~"hello {world")
        ]));
        assert!(check_tokens(compile_str("hello world}").tokens, [
            Text(~"hello world}")
        ]));
        assert!(check_tokens(compile_str("hello world}}").tokens, [
            Text(~"hello world}}")
        ]));
    }

    #[test]
    fn test_compile_etags() {
        assert!(check_tokens(compile_str("{{ name }}").tokens, [
            ETag(vec!(~"name"), ~"{{ name }}")
        ]));

        assert!(check_tokens(compile_str("before {{name}} after").tokens, [
            Text(~"before "),
            ETag(vec!(~"name"), ~"{{name}}"),
            Text(~" after")
        ]));

        assert!(check_tokens(compile_str("before {{name}}").tokens, [
            Text(~"before "),
            ETag(vec!(~"name"), ~"{{name}}")
        ]));

        assert!(check_tokens(compile_str("{{name}} after").tokens, [
            ETag(vec!(~"name"), ~"{{name}}"),
            Text(~" after")
        ]));
    }

    #[test]
    fn test_compile_utags() {
        assert!(check_tokens(compile_str("{{{name}}}").tokens, [
            UTag(vec!(~"name"), ~"{{{name}}}")
        ]));

        assert!(check_tokens(compile_str("before {{{name}}} after").tokens, [
            Text(~"before "),
            UTag(vec!(~"name"), ~"{{{name}}}"),
            Text(~" after")
        ]));

        assert!(check_tokens(compile_str("before {{{name}}}").tokens, [
            Text(~"before "),
            UTag(vec!(~"name"), ~"{{{name}}}")
        ]));

        assert!(check_tokens(compile_str("{{{name}}} after").tokens, [
            UTag(vec!(~"name"), ~"{{{name}}}"),
            Text(~" after")
        ]));
    }

    #[test]
    fn test_compile_sections() {
        assert!(check_tokens(compile_str("{{# name}}{{/name}}").tokens, [
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
        ]));

        assert!(check_tokens(compile_str("before {{^name}}{{/name}} after").tokens, [
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
        ]));

        assert!(check_tokens(compile_str("before {{#name}}{{/name}}").tokens, [
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
        ]));

        assert!(check_tokens(compile_str("{{#name}}{{/name}} after").tokens, [
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
        ]));

        assert!(check_tokens(compile_str(
                "before {{#a}} 1 {{^b}} 2 {{/b}} {{/a}} after").tokens, [
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
        ]));
    }

    #[test]
    fn test_compile_partials() {
        assert!(check_tokens(compile_str("{{> test}}").tokens, [
            Partial(~"test", ~"", ~"{{> test}}")
        ]));

        assert!(check_tokens(compile_str("before {{>test}} after").tokens, [
            Text(~"before "),
            Partial(~"test", ~"", ~"{{>test}}"),
            Text(~" after")
        ]));

        assert!(check_tokens(compile_str("before {{> test}}").tokens, [
            Text(~"before "),
            Partial(~"test", ~"", ~"{{> test}}")
        ]));

        assert!(check_tokens(compile_str("{{>test}} after").tokens, [
            Partial(~"test", ~"", ~"{{>test}}"),
            Text(~" after")
        ]));
    }

    #[test]
    fn test_compile_delimiters() {
        assert!(check_tokens(compile_str("before {{=<% %>=}}<%name%> after").tokens, [
            Text(~"before "),
            ETag(vec!(~"name"), ~"<%name%>"),
            Text(~" after")
        ]));
    }

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
