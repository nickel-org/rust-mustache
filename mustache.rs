#[link(name = "mustache",
       vers = "0.4pre",
       uuid = "afecaa07-75c5-466c-b3a3-fae5d32e04aa")];
#[crate_type = "lib"];

extern mod std;
extern mod extra;

use extra::serialize;
use std::io;
use std::str;
use std::char;
use std::util;
use std::hashmap::HashMap;

/// Represents template data.
#[deriving(Clone)]
pub enum Data {
    Str(@~str),
    Bool(bool),
    Vec(@mut ~[Data]),
    Map(HashMap<@~str, Data>),
    //Fun(@fn(@~str) -> ~str),
}

/**
 * Represents the shared metadata needed to compile and render a mustache
 * template.
 */
#[deriving(Clone)]
pub struct Context {
    template_path: @~str,
    template_extension: @~str,
}

pub struct Template {
    ctx: Context,
    tokens: @~[Token],
    partials: @mut HashMap<@~str, @~[Token]>
}

/**
 * Configures a mustache context with the partial template path and extension.
 */
pub fn Context(template_path: ~str, template_extension: ~str) -> Context {
    Context {
        template_path: @template_path,
        template_extension: @template_extension,
    }
}

/**
 * Configures a mustache context looking for partial files in the current
 * directory.
 */
pub fn default_context() -> Context { Context(~".", ~".mustache") }

impl Context {
    /// Compiles a template from an `io::Reader`.
    fn compile_reader(&self, rdr: @Reader) -> Template {
        let partials = @mut HashMap::new();

        let mut ctx = CompileContext {
            rdr: rdr,
            partials: partials,
            otag: @~"{{",
            ctag: @~"}}",
            template_path: self.template_path,
            template_extension: self.template_extension,
        };

        let tokens = compile_helper(&mut ctx);

        Template {
            ctx: *self,
            tokens: tokens,
            partials: partials
        }
    }

    /// Compiles a template from a file.
    fn compile_file(&self, file: &str) -> Template {
        let mut path = Path::new(self.template_path.as_slice());
        path.push(file.to_owned() + *self.template_extension);

        match io::file_reader(&path) {
          Ok(rdr) => self.compile_reader(rdr),
          Err(e) => fail!(e),
        }
    }

    /// Compiles a template from a string.
    fn compile_str(&self, src: &str) -> Template {
        io::with_str_reader(src, |rdr| self.compile_reader(rdr))
    }

    /// Renders a template from an Reader.
    fn render_reader<
        T: serialize::Encodable<Encoder>
    >(&self, rdr: @Reader, data: &T) -> ~str {
        self.compile_reader(rdr).render(data)
    }

    /// Renders a template from a file.
    fn render_file<
        T: serialize::Encodable<Encoder>
    >(&self, file: &str, data: &T) -> ~str {
        self.compile_file(file).render(data)
    }

    /// Renders a template from a string.
    fn render_str<
        T: serialize::Encodable<Encoder>
    >(&self, template: &str, data: &T) -> ~str {
        self.compile_str(template).render(data)
    }
}

/// Compiles a template from an `Reader`.
pub fn compile_reader(rdr: @Reader) -> Template {
    default_context().compile_reader(rdr)
}

/// Compiles a template from a file.
pub fn compile_file(file: &str) -> Template {
    default_context().compile_file(file)
}

/// Compiles a template from a string.
pub fn compile_str(template: &str) -> Template {
    default_context().compile_str(template)
}

/// Renders a template from an `Reader`.
pub fn render_reader<
    T: serialize::Encodable<Encoder>
>(rdr: @Reader, data: &T) -> ~str {
    default_context().compile_reader(rdr).render(data)
}

/// Renders a template from a file.
pub fn render_file<
    T: serialize::Encodable<Encoder>
>(file: &str, data: &T) -> ~str {
    default_context().compile_file(file).render(data)
}

/// Renders a template from a string.
pub fn render_str<
    T: serialize::Encodable<Encoder>
>(template: &str, data: &T) -> ~str {
    default_context().compile_str(template).render(data)
}

pub struct Encoder {
    data: ~[Data],
}

impl Encoder {
    fn new() -> Encoder {
        Encoder { data: ~[] }
    }
}

impl serialize::Encoder for Encoder {
    fn emit_nil(&mut self) { fail!() }

    fn emit_uint(&mut self, v: uint) { self.emit_str(v.to_str()); }
    fn emit_u64(&mut self, v: u64)   { self.emit_str(v.to_str()); }
    fn emit_u32(&mut self, v: u32)   { self.emit_str(v.to_str()); }
    fn emit_u16(&mut self, v: u16)   { self.emit_str(v.to_str()); }
    fn emit_u8(&mut self, v: u8)     { self.emit_str(v.to_str()); }

    fn emit_int(&mut self, v: int) { self.emit_str(v.to_str()); }
    fn emit_i64(&mut self, v: i64) { self.emit_str(v.to_str()); }
    fn emit_i32(&mut self, v: i32) { self.emit_str(v.to_str()); }
    fn emit_i16(&mut self, v: i16) { self.emit_str(v.to_str()); }
    fn emit_i8(&mut self, v: i8)   { self.emit_str(v.to_str()); }

    fn emit_bool(&mut self, v: bool) { self.data.push(Bool(v)); }

    fn emit_f64(&mut self, v: f64) {
        self.emit_str(v.to_str());
    }

    fn emit_f32(&mut self, v: f32) {
        self.emit_str(v.to_str());
    }

    fn emit_char(&mut self, v: char) {
        self.emit_str(str::from_char(v));
    }

    fn emit_str(&mut self, v: &str) {
        // copying emit_owned_str
        self.data.push(Str(@v.to_owned()));
    }

    fn emit_enum(&mut self, _name: &str, _f: &fn(&mut Encoder)) {
        fail!()
    }

    fn emit_enum_variant(&mut self, _name: &str, _id: uint, _len: uint, _f: &fn(&mut Encoder)) {
        fail!()
    }

    fn emit_enum_variant_arg(&mut self, _a_idx: uint, _f: &fn(&mut Encoder)) {
        fail!()
    }

    fn emit_enum_struct_variant(&mut self,
                                _v_name: &str,
                                _v_id: uint,
                                _len: uint,
                                _f: &fn(&mut Encoder)) {
        fail!()
    }

    fn emit_enum_struct_variant_field(&mut self,
                                      _f_name: &str,
                                      _f_idx: uint,
                                      _f: &fn(&mut Encoder)) {
        fail!()
    }

    fn emit_struct(&mut self, _name: &str, _len: uint, f: &fn(&mut Encoder)) {
        self.data.push(Map(HashMap::new()));
        f(self);
    }

    fn emit_struct_field(&mut self, name: &str, _idx: uint, f: &fn(&mut Encoder)) {
        let mut m = match self.data.pop() {
            Map(m) => m,
            _ => fail!(),
        };
        f(self);
        m.insert(@name.to_owned(), self.data.pop());
        self.data.push(Map(m));
    }

    fn emit_tuple(&mut self, len: uint, f: &fn(&mut Encoder)) {
        self.emit_seq(len, f)
    }

    fn emit_tuple_arg(&mut self, idx: uint, f: &fn(&mut Encoder)) {
        self.emit_seq_elt(idx, f)
    }

    fn emit_tuple_struct(&mut self, _name: &str, len: uint, f: &fn(&mut Encoder)) {
        self.emit_seq(len, f)
    }

    fn emit_tuple_struct_arg(&mut self, idx: uint, f: &fn(&mut Encoder)) {
        self.emit_seq_elt(idx, f)
    }

    // Specialized types:
    fn emit_option(&mut self, _f: &fn(&mut Encoder)) {
        fail!()
    }

    fn emit_option_none(&mut self) {
        fail!()
    }

    fn emit_option_some(&mut self, _f: &fn(&mut Encoder)) {
        fail!()
    }

    fn emit_seq(&mut self, _len: uint, f: &fn(&mut Encoder)) {
        self.data.push(Vec(@mut ~[]));
        f(self);
    }

    fn emit_seq_elt(&mut self, _idx: uint, f: &fn(&mut Encoder)) {
        let v = match self.data.pop() {
            Vec(v) => v,
            _ => fail!(),
        };
        f(self);
        v.push(self.data.pop());
        self.data.push(Vec(v));
    }

    fn emit_map(&mut self, _len: uint, f: &fn(&mut Encoder)) {
        self.data.push(Map(HashMap::new()));
        f(self);
    }

    fn emit_map_elt_key(&mut self, _idx: uint, f: &fn(&mut Encoder)) {
        f(self);
        match *self.data.last() {
            Str(_) => {}
            _ => fail!("error: key is not a string"),
        }
    }

    fn emit_map_elt_val(&mut self, _idx: uint, f: &fn(&mut Encoder)) {
        let k = match self.data.pop() {
            Str(s) => s,
            _ => fail!(),
        };
        let mut m = match self.data.pop() {
            Map(m) => m,
            _ => fail!(),
        };
        f(self);
        m.insert(k, self.data.pop());
        self.data.push(Map(m));
    }
}

impl Template {
    fn render< T: serialize::Encodable<Encoder> >(&self, data: &T) -> ~str {
        let mut encoder = Encoder::new();
        data.encode(&mut encoder);
        assert_eq!(encoder.data.len(), 1);
        self.render_data(encoder.data.pop())
    }

    fn render_data(&self, data: Data) -> ~str {
        render_helper(&RenderContext {
            ctx: self.ctx,
            tokens: self.tokens,
            partials: self.partials,
            stack: @~[data],
            indent: @~""
        })
    }
}

#[deriving(Clone)]
pub enum Token {
    Text(@~str),
    ETag(@~[~str], @~str),
    UTag(@~[~str], @~str),
    Section(@~[~str], bool, @~[Token], @~str, @~str, @~str, @~str, @~str),
    IncompleteSection(@~[~str], bool, @~str, bool),
    Partial(@~str, @~str, @~str),
}

#[deriving(Clone)]
pub enum TokenClass {
    Normal,
    StandAlone,
    WhiteSpace(@~str, uint),
    NewLineWhiteSpace(@~str, uint),
}

pub struct Parser {
    rdr: @Reader,
    ch: char,
    lookahead: Option<char>,
    line: uint,
    col: uint,
    content: ~str,
    state: ParserState,
    otag: @~str,
    ctag: @~str,
    otag_chars: @~[char],
    ctag_chars: @~[char],
    tag_position: uint,
    tokens: ~[Token],
    partials: ~[@~str],
}

enum ParserState { TEXT, OTAG, TAG, CTAG }

impl Parser {
    fn eof(&self) -> bool {
        self.ch == unsafe { ::std::cast::transmute(-1u32) } // FIXME: #rust/8971: unsound
    }

    fn bump(&mut self) {
        match self.lookahead.take() {
            None => { self.ch = self.rdr.read_char(); }
            Some(ch) => { self.ch = ch; }
        }

        if self.ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
    }

    fn peek(&mut self) -> char {
        match self.lookahead {
            None => {
                let ch = self.rdr.read_char();
                self.lookahead = Some(ch);
                ch
            }
            Some(ch) => ch,
        }
    }

    fn parse(&mut self) {
        let mut curly_brace_tag = false;

        while !self.eof() {
            match self.state {
                TEXT => {
                    if self.ch == self.otag_chars[0] {
                        if self.otag_chars.len() > 1u {
                            self.tag_position = 1u;
                            self.state = OTAG;
                        } else {
                            self.add_text();
                            self.state = TAG;
                        }
                    } else {
                        self.content.push_char(self.ch);
                    }
                    self.bump();
                }
                OTAG => {
                    if self.ch == self.otag_chars[self.tag_position] {
                        if self.tag_position == self.otag_chars.len() - 1u {
                            self.add_text();
                            curly_brace_tag = false;
                            self.state = TAG;
                        } else {
                            self.tag_position = self.tag_position + 1u;
                        }
                    } else {
                        // We don't have a tag, so add all the tag parts we've seen
                        // so far to the string.
                        self.state = TEXT;
                        self.not_otag();
                        self.content.push_char(self.ch);
                    }
                    self.bump();
                }
                TAG => {
                    if self.content == ~"" && self.ch == '{' {
                        curly_brace_tag = true;
                        self.content.push_char(self.ch);
                        self.bump();
                    } else if curly_brace_tag && self.ch == '}' {
                        curly_brace_tag = false;
                        self.content.push_char(self.ch);
                        self.bump();
                    } else if self.ch == self.ctag_chars[0u] {
                        if self.ctag_chars.len() > 1u {
                            self.tag_position = 1u;
                            self.state = CTAG;
                            self.bump();
                        } else {
                            self.add_tag();
                            self.state = TEXT;
                        }
                    } else {
                        self.content.push_char(self.ch);
                        self.bump();
                    }
                }
                CTAG => {
                    if self.ch == self.ctag_chars[self.tag_position] {
                        if self.tag_position == self.ctag_chars.len() - 1u {
                            self.add_tag();
                            self.state = TEXT;
                        } else {
                            self.state = TAG;
                            self.not_ctag();
                            self.content.push_char(self.ch);
                            self.bump();
                        }
                    } else {
                        fail!("character %? is not part of CTAG: %?",
                            self.ch,
                            self.ctag_chars[self.tag_position]);
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
                IncompleteSection(name, _, _, _) => {
                    fail!("Unclosed mustache section %s",
                        (*name).connect("."));
              }
              _ => {}
            }
        };
    }

    fn add_text(&mut self) {
        if self.content != ~"" {
            let mut content = ~"";
            util::swap(&mut content, &mut self.content);

            self.tokens.push(Text(@content));
        }
    }

    // This function classifies whether or not a token is standalone, or if it
    // has trailing whitespace. It's looking for this pattern:
    //
    //   ("\n" | "\r\n") whitespace* token ("\n" | "\r\n")
    //
    fn classify_token(&mut self) -> TokenClass {
        // Exit early if the next character is not '\n' or '\r\n'.
        if self.eof() ||
           self.ch == '\n' ||
           (self.ch == '\r' || self.peek() == '\n') {

            // If the last token ends with a newline (or there is no previous
            // token), then this token is standalone.
            if self.tokens.len() == 0u { return StandAlone; }

            match self.tokens[self.tokens.len() - 1u] {
                IncompleteSection(_, _, _, true) => { StandAlone }

                Text(s) if *s != ~"" => {
                    // Look for the last newline character that may have whitespace
                    // following it.
                    match s.rfind(|c:char| c == '\n' || !char::is_whitespace(c)) {
                        // It's all whitespace.
                        None => {
                            if self.tokens.len() == 1u {
                                WhiteSpace(s, 0u)
                            } else {
                                Normal
                            }
                        }
                        Some(pos) => {
                            if s.char_at(pos) == '\n' {
                                if pos == s.len() - 1u {
                                    StandAlone
                                } else {
                                    WhiteSpace(s, pos + 1u)
                                }
                            } else { Normal }
                        }
                    }
                }
                _ => Normal,
            }
        } else { Normal }
    }

    fn eat_whitespace(&mut self) -> bool {
        // If the next character is a newline, and the last token ends with a
        // newline and whitespace, clear out the whitespace.

        match self.classify_token() {
            Normal => { false }
            StandAlone => {
                if self.ch == '\r' { self.bump(); }
                self.bump();
                true
            }
            WhiteSpace(s, pos) | NewLineWhiteSpace(s, pos) => {
                if self.ch == '\r' { self.bump(); }
                self.bump();

                // Trim the whitespace from the last token.
                self.tokens.pop();
                self.tokens.push(Text(@s.slice(0u, pos).to_str()));

                true
            }
        }
    }

    fn add_tag(&mut self) {
        self.bump();

        let tag = @(*self.otag + self.content + *self.ctag);

        // Move the content to avoid a copy.
        let mut content = ~"";
        util::swap(&mut content, &mut self.content);
        let len = content.len();

        match content[0] as char {
            '!' => {
                // ignore comments
                self.eat_whitespace();
            }
            '&' => {
                let name = content.slice(1u, len);
                let name = self.check_content(name);
                let name = @name.split_terminator_iter('.')
                    .map(|x| x.to_owned())
                    .collect();
                self.tokens.push(UTag(name, tag));
            }
            '{' => {
                if content.ends_with("}") {
                    let name = content.slice(1u, len - 1u);
                    let name = self.check_content(name);
                    let name = @name.split_terminator_iter('.')
                        .map(|x| x.to_owned())
                        .collect();
                    self.tokens.push(UTag(name, tag));
                } else { fail!(~"unbalanced \"{\" in tag"); }
            }
            '#' => {
                let newlined = self.eat_whitespace();

                let name = self.check_content(content.slice(1u, len));
                let name = @name.split_terminator_iter('.')
                    .map(|x| x.to_owned())
                    .collect();
                self.tokens.push(IncompleteSection(name, false, tag, newlined));
            }
            '^' => {
                let newlined = self.eat_whitespace();

                let name = self.check_content(content.slice(1u, len));
                let name = @name.split_terminator_iter('.')
                    .map(|x| x.to_owned())
                    .collect();
                self.tokens.push(IncompleteSection(name, true, tag, newlined));
            }
            '/' => {
                self.eat_whitespace();

                let name = self.check_content(content.slice(1u, len));
                let name = @name.split_terminator_iter('.')
                    .map(|x| x.to_owned())
                    .collect();
                let mut children: ~[Token] = ~[];

                loop {
                    if self.tokens.len() == 0u {
                        fail!(~"closing unopened section");
                    }

                    let last = self.tokens.pop();

                    match last {
                        IncompleteSection(section_name, inverted, osection, _) => {
                            children.reverse();

                            // Collect all the children's sources.
                            let mut srcs = ~[];
                            for child in children.iter() {
                                match *child {
                                    Text(s)
                                    | ETag(_, s)
                                    | UTag(_, s)
                                    | Partial(_, _, s) => {
                                        srcs.push(s.clone())
                                    }
                                    Section(_, _, _, _, osection, src, csection, _) => {
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
                                for s in srcs.iter() { src.push_str(**s); }

                                self.tokens.push(
                                    Section(
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
                                fail!(~"Unclosed section");
                            }
                        }
                        _ => { children.push(last); }
                    }
                }
            }
            '>' => { self.add_partial(content, tag); }
            '=' => {
                self.eat_whitespace();

                if (len > 2u && content.ends_with("=")) {
                    let s = self.check_content(content.slice(1u, len - 1u));

                    let pos = s.find(char::is_whitespace);
                    let pos = match pos {
                      None => { fail!("invalid change delimiter tag content"); }
                      Some(pos) => { pos }
                    };

                    self.otag = @s.slice(0u, pos).to_str();
                    self.otag_chars = @(*self.otag).iter().collect();

                    let s2 = s.slice_from(pos);
                    let pos = s2.find(|c| !char::is_whitespace(c));
                    let pos = match pos {
                      None => { fail!("invalid change delimiter tag content"); }
                      Some(pos) => { pos }
                    };

                    self.ctag = @s2.slice_from(pos).to_str();
                    self.ctag_chars = @(*self.ctag).iter().collect();
                } else {
                    fail!("invalid change delimiter tag content");
                }
            }
            _ => {
                let name = self.check_content(content);
                let name = @name.split_terminator_iter('.')
                    .map(|x| x.to_owned())
                    .collect();
                self.tokens.push(ETag(name, tag));
            }
        }
    }

    fn add_partial(&mut self, content: &str, tag: @~str) {
        let token_class = self.classify_token();
        let indent = match token_class.clone() {
            Normal => ~"",
            StandAlone => {
                if self.ch == '\r' { self.bump(); }
                self.bump();
                ~""
            }
            WhiteSpace(s, pos) | NewLineWhiteSpace(s, pos) => {
                if self.ch == '\r' { self.bump(); }
                self.bump();

                let ws = s.slice(pos, s.len());

                // Trim the whitespace from the last token.
                self.tokens.pop();
                self.tokens.push(Text(@s.slice(0u, pos).to_str()));

                ws.to_owned()
            }
        };

        // We can't inline the tokens directly as we may have a recursive
        // partial. So instead, we'll cache the partials we used and look them
        // up later.
        let name = content.slice(1u, content.len());
        let name = @self.check_content(name);

        self.tokens.push(Partial(name, @indent, tag));
        self.partials.push(name);
    }

    fn not_otag(&mut self) {
        let mut i = 0u;
        while i < self.tag_position {
            self.content.push_char(self.otag_chars[i]);
            i += 1u;
        }
    }

    fn not_ctag(&mut self) {
        let mut i = 0u;
        while i < self.tag_position {
            self.content.push_char(self.ctag_chars[i]);
            i += 1u;
        }
    }

    fn check_content(&self, content: &str) -> ~str {
        let trimmed = content.trim();
        if trimmed.len() == 0u {
            fail!(~"empty tag");
        }
        trimmed.to_owned()
    }
}

struct CompileContext {
    rdr: @Reader,
    partials: @mut HashMap<@~str, @~[Token]>,
    otag: @~str,
    ctag: @~str,
    template_path: @~str,
    template_extension: @~str,
}

fn compile_helper(ctx: &mut CompileContext) -> @~[Token] {
    let mut parser = Parser {
        rdr: ctx.rdr,
        ch: '\0',
        lookahead: None,
        line: 1u,
        col: 1u,
        content: ~"",
        state: TEXT,
        otag: ctx.otag,
        ctag: ctx.ctag,
        otag_chars: @(*ctx.otag).iter().collect::<~[char]>(),
        ctag_chars: @(*ctx.ctag).iter().collect::<~[char]>(),
        tag_position: 0u,
        tokens: ~[],
        partials: ~[],
    };

    parser.bump();
    parser.parse();

    // Compile the partials if we haven't done so already.
    for name in parser.partials.iter() {
        let mut path = Path::new(ctx.template_path.as_slice());
        path.push(**name + *ctx.template_extension);

        if !ctx.partials.contains_key(name) {
            // Insert a placeholder so we don't recurse off to infinity.
            ctx.partials.insert(*name, @~[]);

            match io::file_reader(&path) {
                Err(_e) => {}
                Ok(rdr) => {
                    let mut inner_ctx = CompileContext {
                        rdr: rdr,
                        partials: ctx.partials.clone(),
                        otag: @~"{{",
                        ctag: @~"}}",
                        template_path: ctx.template_path,
                        template_extension: ctx.template_extension
                    };
                    let tokens = compile_helper(&mut inner_ctx);

                    ctx.partials.insert(*name, tokens);
              }
            }
        }
    }

    // Destructure the parser so we get get at the tokens without a copy.
    let Parser { tokens: tokens, _ } = parser;

    @tokens
}

#[deriving(Clone)]
struct RenderContext {
    ctx: Context,
    tokens: @~[Token],
    partials: @mut HashMap<@~str, @~[Token]>,
    stack: @~[Data],
    indent: @~str,
}

fn render_helper(ctx: &RenderContext) -> ~str {
    fn find(stack: &[Data], path: &[~str]) -> Option<Data> {
        // If we have an empty path, we just want the top value in our stack.
        if path.is_empty() {
            return match stack.last_opt() {
                None => None,
                Some(value) => Some(value.clone()),
            };
        }

        // Otherwise, find the stack that has the first part of our path.
        let mut value: Option<Data> = None;

        let mut i = stack.len();
        while i > 0u {
            match stack[i - 1u] {
                Map(ref ctx) => {
                    match ctx.find(&@path[0u].clone()) {
                        Some(v) => { value = Some(v.clone()); break; }
                        None => {}
                    }
                    i -= 1u;
                }
                _ => {
                    println!("hey: {:?}", stack.last());
                    fail!("%? %?", stack, path)
                }
            }
        }

        // Walk the rest of the path to find our final value.
        let mut value = value;

        let mut i = 1u;
        let len = path.len();

        while i < len {
            value = match value {
                Some(Map(v)) => {
                    match v.find(&@path[i].clone()) {
                        Some(value) => Some(value.clone()),
                        None => None,
                    }
                }
                _ => break,
            };
            i = i + 1u;
        }

        value
    }

    let mut output = ~"";
    
    for token in ctx.tokens.iter() {
        match token {
            &Text(value) => {
                // Indent the lines.
                if *ctx.indent == ~"" {
                    output = output + *value;
                } else {
                    let mut pos = 0u;
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

                        if line.char_at(0u) != '\n' {
                            output.push_str(*ctx.indent);
                        }

                        output.push_str(line);
                    }
                }
            },
            &ETag(path, _) => {
                match find(*ctx.stack, *path) {
                    None => { }
                    Some(value) => {
                        output = output + *ctx.indent + render_etag(value, ctx);
                    }
                }
            }
            &UTag(path, _) => {
                match find(*ctx.stack, *path) {
                    None => { }
                    Some(value) => {
                        output = output + *ctx.indent + render_utag(value, ctx);
                    }
                }
            }
            &Section(path, true, children, _, _, _, _, _) => {
                let ctx = RenderContext { tokens: children, .. ctx.clone() };

                output = output + match find(*ctx.stack, *path) {
                    None => { render_helper(&ctx) }
                    Some(value) => { render_inverted_section(value, &ctx) }
                };
            }
            &Section(path, false, children, otag, _, src, _, ctag) => {
                match find(*ctx.stack, *path) {
                    None => { }
                    Some(value) => {
                        output = output + render_section(
                            value,
                            src,
                            otag,
                            ctag,
                            &RenderContext { tokens: children ,.. ctx.clone() }
                        );
                    }
                }
            }
            &Partial(ref name, ind, _) => {
                match ctx.partials.find(name) {
                    None => { }
                    Some(tokens) => {
                        output = output + render_helper(&RenderContext {
                            tokens: *tokens,
                            indent: @(*ctx.indent + *ind),
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
    for c in utag.iter() {
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
        Str(s) => (*s).clone(),

        // etags and utags use the default delimiter.
        //Fun(f) => render_fun(ctx, @~"", @~"{{", @~"}}", f),

        _ => fail!(),
    }
}

fn render_inverted_section(value: Data, ctx: &RenderContext) -> ~str {
    match value {
        Bool(false) => render_helper(ctx),
        Vec(xs) if xs.len() == 0 => render_helper(ctx),
        _ => ~"",
    }
}

fn render_section(value: Data,
                  _src: @~str,
                  _otag: @~str,
                  _ctag: @~str,
                  ctx: &RenderContext) -> ~str {
    match value {
        Bool(true) => render_helper(ctx),
        Bool(false) => ~"",
        Vec(vs) => {
            do vs.map |v| {
                let mut stack = ctx.stack.to_owned();
                stack.push(v.clone());

                render_helper(&RenderContext { stack: @stack, .. (*ctx).clone() })
            }.concat()
        }
        Map(_) => {
            let mut stack = ctx.stack.to_owned();
            stack.push(value);

            render_helper(&RenderContext { stack: @stack, .. (*ctx).clone() })
        }
        //Fun(f) => render_fun(ctx, src, otag, ctag, f),
        _ => fail!(),
    }
}

fn render_fun(ctx: &RenderContext,
              src: @~str,
              otag: @~str,
              ctag: @~str,
              f: &fn(@~str) -> ~str) -> ~str {
    let tokens = do io::with_str_reader(f(src)) |rdr| {
        let mut inner_ctx = CompileContext {
            rdr: rdr,
            partials: ctx.partials.clone(),
            otag: otag,
            ctag: ctag,
            template_path: ctx.ctx.template_path,
            template_extension: ctx.ctx.template_extension
        };
        compile_helper(&mut inner_ctx)
    };

    render_helper(&RenderContext { tokens: tokens ,.. ctx.clone() })
}



#[cfg(test)]
mod tests {
    use std::hashmap::HashMap;
    use std::io;
    use std::os;
    use extra::json;
    use extra::serialize::Encodable;
    use extra::serialize;
    use extra::tempfile;
    use super::{compile_str, render_str};
    use super::{compile_file};
    use super::{Context, Encoder};
    use super::{Data, Str, Bool, Vec, Map};
    use super::{Token, Text, ETag, UTag, Section, Partial};

    fn token_to_str(token: &Token) -> ~str {
        match *token {
            // recursive enums crash %?
            Section(name, inverted, children, otag, osection, src, tag, ctag) => {
                let children = children.map(|x| token_to_str(x));
                fmt!("Section(%?, %?, %?, %?, %?, %?, %?, %?)",
                    name,
                    inverted,
                    children,
                    otag,
                    osection,
                    src,
                    tag,
                    ctag)
            }
            _ => {
                fmt!("%?", token)
            }
        }
    }

    fn check_tokens(actual: &[Token], expected: &[Token]) -> bool {
        // TODO: equality is currently broken for enums
        let actual = do actual.map |x| {token_to_str(x)};
        let expected = do expected.map |x| {token_to_str(x)};
        if actual !=  expected {
            io::stderr().write_line(fmt!("Found %?, but expected %?", actual, expected));
            return false;
        }
        return true;
    }

    #[test]
    fn test_compile_texts() {
        assert!(check_tokens(*compile_str("hello world").tokens, [Text(@~"hello world")]));
        assert!(check_tokens(*compile_str("hello {world").tokens, [Text(@~"hello {world")]));
        assert!(check_tokens(*compile_str("hello world}").tokens, [Text(@~"hello world}")]));
        assert!(check_tokens(*compile_str("hello world}}").tokens, [Text(@~"hello world}}")]));
    }

    #[test]
    fn test_compile_etags() {
        assert!(check_tokens(*compile_str("{{ name }}").tokens, [
            ETag(@~[~"name"], @~"{{ name }}")
        ]));

        assert!(check_tokens(*compile_str("before {{name}} after").tokens, [
            Text(@~"before "),
            ETag(@~[~"name"], @~"{{name}}"),
            Text(@~" after")
        ]));

        assert!(check_tokens(*compile_str("before {{name}}").tokens, [
            Text(@~"before "),
            ETag(@~[~"name"], @~"{{name}}")
        ]));

        assert!(check_tokens(*compile_str("{{name}} after").tokens, [
            ETag(@~[~"name"], @~"{{name}}"),
            Text(@~" after")
        ]));
    }

    #[test]
    fn test_compile_utags() {
        assert!(check_tokens(*compile_str("{{{name}}}").tokens, [
            UTag(@~[~"name"], @~"{{{name}}}")
        ]));

        assert!(check_tokens(*compile_str("before {{{name}}} after").tokens, [
            Text(@~"before "),
            UTag(@~[~"name"], @~"{{{name}}}"),
            Text(@~" after")
        ]));

        assert!(check_tokens(*compile_str("before {{{name}}}").tokens, [
            Text(@~"before "),
            UTag(@~[~"name"], @~"{{{name}}}")
        ]));

        assert!(check_tokens(*compile_str("{{{name}}} after").tokens, [
            UTag(@~[~"name"], @~"{{{name}}}"),
            Text(@~" after")
        ]));
    }

    #[test]
    fn test_compile_sections() {
        assert!(check_tokens(*compile_str("{{# name}}{{/name}}").tokens, [
            Section(
                @~[~"name"],
                false,
                @~[],
                @~"{{",
                @~"{{# name}}",
                @~"",
                @~"{{/name}}",
                @~"}}"
            )
        ]));

        assert!(check_tokens(*compile_str("before {{^name}}{{/name}} after").tokens, [
            Text(@~"before "),
            Section(
                @~[~"name"],
                true,
                @~[],
                @~"{{",
                @~"{{^name}}",
                @~"",
                @~"{{/name}}",
                @~"}}"
            ),
            Text(@~" after")
        ]));

        assert!(check_tokens(*compile_str("before {{#name}}{{/name}}").tokens, [
            Text(@~"before "),
            Section(
                @~[~"name"],
                false,
                @~[],
                @~"{{",
                @~"{{#name}}",
                @~"",
                @~"{{/name}}",
                @~"}}"
            )
        ]));

        assert!(check_tokens(*compile_str("{{#name}}{{/name}} after").tokens, [
            Section(
                @~[~"name"],
                false,
                @~[],
                @~"{{",
                @~"{{#name}}",
                @~"",
                @~"{{/name}}",
                @~"}}"
            ),
            Text(@~" after")
        ]));

        assert!(check_tokens(*compile_str(
                "before {{#a}} 1 {{^b}} 2 {{/b}} {{/a}} after").tokens, [
            Text(@~"before "),
            Section(
                @~[~"a"],
                false,
                @~[
                    Text(@~" 1 "),
                    Section(
                        @~[~"b"],
                        true,
                        @~[Text(@~" 2 ")],
                        @~"{{",
                        @~"{{^b}}",
                        @~" 2 ",
                        @~"{{/b}}",
                        @~"}}"
                    ),
                    Text(@~" ")
                ],
                @~"{{",
                @~"{{#a}}",
                @~" 1 {{^b}} 2 {{/b}} ",
                @~"{{/a}}",
                @~"}}"
            ),
            Text(@~" after")
        ]));
    }

    #[test]
    fn test_compile_partials() {
        assert!(check_tokens(*compile_str("{{> test}}").tokens, [
            Partial(@~"test", @~"", @~"{{> test}}")
        ]));

        assert!(check_tokens(*compile_str("before {{>test}} after").tokens, [
            Text(@~"before "),
            Partial(@~"test", @~"", @~"{{>test}}"),
            Text(@~" after")
        ]));

        assert!(check_tokens(*compile_str("before {{> test}}").tokens, [
            Text(@~"before "),
            Partial(@~"test", @~"", @~"{{> test}}")
        ]));

        assert!(check_tokens(*compile_str("{{>test}} after").tokens, [
            Partial(@~"test", @~"", @~"{{>test}}"),
            Text(@~" after")
        ]));
    }

    #[test]
    fn test_compile_delimiters() {
        assert!(check_tokens(*compile_str("before {{=<% %>=}}<%name%> after").tokens, [
            Text(@~"before "),
            ETag(@~[~"name"], @~"<%name%>"),
            Text(@~" after")
        ]));
    }

    #[deriving(Encodable)]
    struct Name { name: ~str }

    #[test]
    fn test_render_texts() {
        let ctx = &Name { name: ~"world" };

        assert_eq!(render_str("hello world", ctx), ~"hello world");
        assert_eq!(render_str("hello {world", ctx), ~"hello {world");
        assert_eq!(render_str("hello world}", ctx), ~"hello world}");
        assert_eq!(render_str("hello {world}", ctx), ~"hello {world}");
        assert_eq!(render_str("hello world}}", ctx), ~"hello world}}");
    }

    #[test]
    fn test_render_etags() {
        let ctx = &Name { name: ~"world" };

        assert!(render_str("hello {{name}}", ctx) == ~"hello world");
    }

    #[test]
    fn test_render_utags() {
        let ctx = &Name { name: ~"world" };

        assert!(render_str("hello {{{name}}}", ctx) == ~"hello world");
    }

    struct StrHash<V>(HashMap<@~str, V>);

    impl<E: serialize::Encoder, V> serialize::Encodable<E> for StrHash<V> {
        fn encode(&self, e: &mut E) {
            (*self).encode(e)
        }
    }

    #[test]
    fn test_render_sections() {
        let mut ctx0 = HashMap::new();
        let template = compile_str("0{{#a}}1 {{n}} 3{{/a}}5");

        assert!(template.render_data(Map(ctx0.clone())) == ~"05");

        ctx0.insert(@~"a", Vec(@mut ~[]));
        assert!(template.render_data(Map(ctx0.clone())) == ~"05");

        let ctx1: HashMap<@~str, Data> = HashMap::new();
        ctx0.insert(@~"a", Vec(@mut ~[Map(ctx1.clone())]));

        assert!(template.render_data(Map(ctx0.clone())) == ~"01  35");

        let mut ctx1 = HashMap::new();
        ctx1.insert(@~"n", Str(@~"a"));
        ctx0.insert(@~"a", Vec(@mut ~[Map(ctx1.clone())]));
        assert!(template.render_data(Map(ctx0.clone())) == ~"01 a 35");

        //ctx0.insert(@~"a", Fun(|_text| {~"foo"}));
        //assert!(template.render_data(Map(ctx0)) == ~"0foo5");
    }

    #[test]
    fn test_render_inverted_sections() {
        let template = compile_str("0{{^a}}1 3{{/a}}5");

        let mut ctx0 = HashMap::new();
        assert!(template.render_data(Map(ctx0.clone())) == ~"01 35");

        ctx0.insert(@~"a", Vec(@mut ~[]));
        assert!(template.render_data(Map(ctx0.clone())) == ~"01 35");

        let mut ctx1 = HashMap::new();
        ctx0.insert(@~"a", Vec(@mut ~[Map(ctx1.clone())]));
        assert!(template.render_data(Map(ctx0.clone())) == ~"05");

        ctx1.insert(@~"n", Str(@~"a"));
        assert!(template.render_data(Map(ctx0.clone())) == ~"05");
    }

    #[test]
    fn test_render_partial() {
        let path = ~"base";
        let template = compile_file(path);

        let mut ctx0 = HashMap::new();
        assert_eq!(template.render_data(Map(ctx0.clone())), ~"<h2>Names</h2>\n");

        ctx0.insert(@~"names", Vec(@mut ~[]));
        assert_eq!(template.render_data(Map(ctx0.clone())), ~"<h2>Names</h2>\n");

        let mut ctx1 = HashMap::new();
        ctx0.insert(@~"names", Vec(@mut ~[Map(ctx1.clone())]));
        assert_eq!(
            template.render_data(Map(ctx0.clone())),
            ~"<h2>Names</h2>\n  <strong></strong>\n\n");

        ctx1.insert(@~"name", Str(@~"a"));
        ctx0.insert(@~"names", Vec(@mut ~[Map(ctx1.clone())]));
        assert_eq!(
            template.render_data(Map(ctx0.clone())),
            ~"<h2>Names</h2>\n  <strong>a</strong>\n\n");

        let mut ctx2 = HashMap::new();
        ctx2.insert(@~"name", Str(@~"<b>"));
        ctx0.insert(@~"names", Vec(@mut ~[Map(ctx1), Map(ctx2)]));
        assert_eq!(
            template.render_data(Map(ctx0)),
            ~"<h2>Names</h2>\n  <strong>a</strong>\n\n  <strong>&lt;b&gt;</strong>\n\n");
    }

    fn parse_spec_tests(src: &str) -> ~[json::Json] {
        let path = Path::new(src);
        match io::read_whole_file_str(&path) {
            Err(e) => fail!(e),
            Ok(s) => {
                match json::from_str(s) {
                    Err(e) => fail!(e.to_str()),
                    Ok(json) => {
                        match json {
                            json::Object(d) => {
                                let mut d = d;
                                match d.pop(&~"tests") {
                                    Some(json::List(tests)) => tests,
                                    _ => fail!("%s: tests key not a list", src),
                                }
                            }
                            _ => fail!("%s: JSON value not a map", src),
                        }
                    }
                }
            }
        }
    }

/*
    fn convert_json_map(map: json::Object) -> HashMap<@~str, Data> {
        let mut d = HashMap::new();
        for (key, value) in map.move_iter() {
            d.insert(@key.to_owned(), convert_json(value));
        }
        d
    }

    fn convert_json(value: json::Json) -> Data {
        match value {
          json::Number(n) => {
            // We have to cheat and use %? because %f doesn't convert 3.3 to
            // 3.3.
            Str(@fmt!("%?", n))
          }
          json::String(s) => { Str(@s.to_owned()) }
          json::Boolean(b) => { Bool(b) }
          json::List(v) => { Vec(@mut v.map(convert_json)) }
          json::Object(d) => { Map(convert_json_map(d)) }
          _ => { fail!("%?", value) }
        }
    }
    */

    fn write_partials(tmpdir: &Path, value: &json::Json) {
        match value {
            &json::Object(ref d) => {
                for (key, value) in d.iter() {
                    match value {
                        &json::String(ref s) => {
                            let mut path = tmpdir.clone();
                            path.push(*key + ".mustache");

                            match io::file_writer(&path, [io::Create, io::Truncate]) {
                                Ok(wr) => wr.write_str(*s),
                                Err(e) => fail!(e),
                            }
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
        let tmpdir = match tempfile::TempDir::new("") {
            Some(tmpdir) => tmpdir,
            None => fail!(),
        };

        match test.find(&~"partials") {
            Some(value) => write_partials(tmpdir.path(), value),
            None => {},
        }

        let ctx = Context(tmpdir.path().as_str().unwrap().to_owned(), ~".mustache");
        let template = ctx.compile_str(template);
        let result = template.render_data(data);

        if result != expected {
            fn to_list(x: &json::Json) -> json::Json {
                match x {
                    &json::Object(ref d) => {
                        let mut xs = ~[];
                        for (k, v) in d.iter() {
                            let k = json::String(k.clone());
                            let v = to_list(v);
                            xs.push(json::List(~[k, v]));
                        }
                        json::List(xs)
                    },
                    &json::List(ref xs) => {
                        json::List(xs.map(|x| to_list(x)))
                    },
                    _ => { x.clone() }
                }
            }

            println(fmt!("desc:     %s", test.find(&~"desc").unwrap().to_str()));
            println(fmt!("context:  %s", test.find(&~"data").unwrap().to_str()));
            println("=>");
            println(fmt!("template: %?", template));
            println(fmt!("expected: %?", expected));
            println(fmt!("actual:   %?", result));
            println("");
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
            data.encode(&mut encoder);
            assert_eq!(encoder.data.len(), 1);

            run_test(test, encoder.data.pop());
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

    /*
    #[test]
    fn test_spec_sections() {
        run_tests("spec/specs/sections.json");
    }
    */

/*
    #[test]
    fn test_spec_lambdas() {
        for json in parse_spec_tests(~"spec/specs/~lambdas.json").iter() {
            let test = match json {
                &json::Object(m) => m,
                _ => fail!(),
            };

            // Replace the lambda with rust code.
            let data = match test.find(&~"data") {
                Some(data) => (*data).clone(),
                None => fail!(),
            };

            let encoder = Encoder::new();
            data.encode(&encoder);
            let ctx = match encoder.data {
                [Map(ctx)] => ctx,
                _ => fail!(),
            };

            let f = match *test.find(&~"name").unwrap() {
                json::String(~"Interpolation") => {
                    |_text| {~"world" }
                }
                json::String(~"Interpolation - Expansion") => {
                    |_text| {~"{{planet}}" }
                }
                json::String(~"Interpolation - Alternate Delimiters") => {
                    |_text| {~"|planet| => {{planet}}" }
                }
                json::String(~"Interpolation - Multiple Calls") => {
                    let calls = @mut 0i;
                    |_text| {*calls = *calls + 1; calls.to_str() }
                }
                json::String(~"Escaping") => {
                    |_text| {~">" }
                }
                json::String(~"Section") => {
                    |text: @~str| {if *text == ~"{{x}}" { ~"yes" } else { ~"no" } }
                }
                json::String(~"Section - Expansion") => {
                    |text: @~str| {*text + "{{planet}}" + *text }
                }
                json::String(~"Section - Alternate Delimiters") => {
                    |text: @~str| {*text + "{{planet}} => |planet|" + *text }
                }
                json::String(~"Section - Multiple Calls") => {
                    |text: @~str| {~"__" + *text + ~"__" }
                }
                json::String(~"Inverted Section") => {
                    |_text| {~"" }
                }
                value => { fail!("%?", value) }
            };
            //ctx.insert(@~"lambda", Fun(f));

            run_test(test, Map(ctx));
        }
    }
*/
}
