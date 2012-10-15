#[allow(structural_records)];  // TODO: enable more of these
#[forbid(deprecated_mode)];
#[forbid(deprecated_pattern)];
#[forbid(non_implicitly_copyable_typarams)];
#[forbid(unused_imports)];

/// Represents template data.
pub enum Data {
    Str(@~str),
    Bool(bool),
    Vec(@DVec<Data>),
    Map(HashMap<@~str, Data>),
    Fun(fn@(@~str) -> ~str),
}

/**
 * Represents the shared metadata needed to compile and render a mustache
 * template.
 */
pub struct Context {
    template_path: @~str,
    template_extension: @~str,
}

pub struct Template {
    ctx: Context,
    tokens: @~[Token],
    partials: HashMap<@~str, @~[Token]>
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
    /// Compiles a template from an io::Reader.
    fn compile_reader(rdr: io::Reader) -> Template {
        let partials = HashMap();

        let mut ctx = CompileContext {
            rdr: rdr,
            partials: partials,
            otag: @~"{{",
            ctag: @~"}}",
            template_path: self.template_path,
            template_extension: self.template_extension
        };

        let tokens = compile_helper(&ctx);

        Template {
            ctx: self,
            tokens: tokens,
            partials: partials
        }
    }

    /// Compiles a template from a file.
    fn compile_file(file: &str) -> Template {
    	let path: Path = path::from_str(*self.template_path);
    	let path = path.push(file.to_unique() + *self.template_extension);

        match io::file_reader(&path) {
          Ok(move rdr) => self.compile_reader(rdr),
          Err(move e) => fail e,
        }
    }

    /// Compiles a template from a string.
    fn compile_str(src: &str) -> Template {
        io::with_str_reader(src, |rdr| self.compile_reader(rdr))
    }

    /// Renders a template from an io::Reader.
    fn render_reader<S: Serializable>(rdr: io::Reader, data: &S) -> ~str {
        self.compile_reader(rdr).render(data)
    }

    /// Renders a template from a file.
    fn render_file<S: Serializable>(file: &str, data: &S) -> ~str {
        self.compile_file(file).render(data)
    }

    /// Renders a template from a string.
    fn render_str<S: Serializable>(template: &str, data: &S) -> ~str {
        self.compile_str(template).render(data)
    }
}

/// Compiles a template from an io::Reader.
pub fn compile_reader(rdr: io::Reader) -> Template {
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

/// Renders a template from an io::Reader.
pub fn render_reader<S: Serializable>(rdr: io::Reader, data: &S) -> ~str {
    default_context().compile_reader(rdr).render(data)
}

/// Renders a template from a file.
pub fn render_file<S: Serializable>(file: &str, data: &S) -> ~str {
    default_context().compile_file(file).render(data)
}

/// Renders a template from a string.
pub fn render_str<S: Serializable>(template: &str, data: &S) -> ~str {
    default_context().compile_str(template).render(data)
}

struct DataSerializer {
    data: std::cell::Cell<Data>,
}

fn DataSerializer() -> DataSerializer {
    DataSerializer { data: std::cell::empty_cell() }
}

impl DataSerializer: std::serialization::Serializer {
    fn emit_nil(&self) { fail }

    fn emit_uint(&self, v: uint) { self.emit_owned_str(v.to_str()); }
    fn emit_u64(&self, v: u64)   { self.emit_owned_str(v.to_str()); }
    fn emit_u32(&self, v: u32)   { self.emit_owned_str(v.to_str()); }
    fn emit_u16(&self, v: u16)   { self.emit_owned_str(v.to_str()); }
    fn emit_u8(&self, v: u8)     { self.emit_owned_str(v.to_str()); }

    fn emit_int(&self, v: int) { self.emit_owned_str(v.to_str()); }
    fn emit_i64(&self, v: i64) { self.emit_owned_str(v.to_str()); }
    fn emit_i32(&self, v: i32) { self.emit_owned_str(v.to_str()); }
    fn emit_i16(&self, v: i16) { self.emit_owned_str(v.to_str()); }
    fn emit_i8(&self, v: i8)   { self.emit_owned_str(v.to_str()); }

    fn emit_bool(&self, v: bool) { self.data.put_back(Bool(v)); }

    fn emit_f64(&self, v: f64)     { self.emit_owned_str(v.to_str()); }
    fn emit_f32(&self, v: f32)     { self.emit_owned_str(v.to_str()); }
    fn emit_float(&self, v: float) {
        // We want to strip trailing zeros.
        let s = str::trim_right_chars(v.to_str(), ~['0', '.']);
        self.emit_owned_str(s);
    }

    fn emit_char(&self, v: char) { self.emit_owned_str(str::from_char(v)); }

    fn emit_borrowed_str(&self, v: &str) {
        self.data.put_back(Str(@v.to_unique()));
    }
    fn emit_owned_str(&self, v: &str) {
        self.data.put_back(Str(@v.to_unique()));
    }
    fn emit_managed_str(&self, v: &str) {
        self.data.put_back(Str(@v.to_unique()));
    }

    fn emit_borrowed(&self, f: fn()) { f() }
    fn emit_owned(&self, f: fn()) { f() }
    fn emit_managed(&self, f: fn()) { f() }

    fn emit_enum(&self, _name: &str, _f: fn()) {
        fail
    }
    fn emit_enum_variant(&self, _name: &str, _id: uint, _cnt: uint, _f: fn()) {
        fail
    }
    fn emit_enum_variant_arg(&self, _idx: uint, _f: fn()) {
        fail
    }

    fn emit_borrowed_vec(&self, _len: uint, f: fn()) {
        self.data.put_back(Vec(@DVec()));
        f()
    }
    fn emit_owned_vec(&self, _len: uint, f: fn()) {
        self.data.put_back(Vec(@DVec()));
        f()
    }
    fn emit_managed_vec(&self, _len: uint, f: fn()) {
        self.data.put_back(Vec(@DVec()));
        f()
    }
    fn emit_vec_elt(&self, _idx: uint, f: fn()) {
        let v = self.data.take();
        f();
        match move v {
            Vec(move v) => {
                let mut v = move v;
                v.push(self.data.take());
                self.data.put_back(Vec(v));
            }
            _ => fail
        }
    }

    fn emit_rec(&self, f: fn()) {
        self.data.put_back(Map(HashMap()));
        f()
    }
    fn emit_struct(&self, _name: &str, f: fn()) {
        self.data.put_back(Map(HashMap()));
        f()
    }
    fn emit_field(&self, name: &str, _idx: uint, f: fn()) {
        let m = self.data.take();
        f();
        match move m {
            Map(move m) => {
                m.insert(@name.to_unique(), self.data.take());
                self.data.put_back(Map(m))
            }
            _ => fail
        }
    }

    fn emit_tup(&self, len: uint, f: fn()) {
        self.emit_owned_vec(len, f)
    }
    fn emit_tup_elt(&self, idx: uint, f: fn()) {
        self.emit_vec_elt(idx, f)
    }
}

impl Template {
    fn render<S: Serializable>(data: &S) -> ~str {
        let serializer = DataSerializer();
        data.serialize(&serializer);
        self.render_data(serializer.data.take())
    }

    fn render_data(data: Data) -> ~str {
        render_helper(&RenderContext {
            ctx: self.ctx,
            tokens: self.tokens,
            partials: self.partials,
            stack: @~[data],
            indent: @~""
        })
    }
}

enum Token {
    Text(@~str),
    ETag(@~[~str], @~str),
    UTag(@~[~str], @~str),
    Section(@~[~str], bool, @~[Token], @~str, @~str, @~str, @~str, @~str),
    IncompleteSection(@~[~str], bool, @~str, bool),
    Partial(@~str, @~str, @~str),
}

enum TokenClass {
    Normal,
    StandAlone,
    WhiteSpace(@~str, uint),
    NewLineWhiteSpace(@~str, uint),
}

pub struct Parser {
    rdr: io::Reader,
    mut ch: char,
    mut lookahead: Option<char>,
    mut line: uint,
    mut col: uint,
    mut content: ~str,
    mut state: ParserState,
    mut otag: @~str,
    mut ctag: @~str,
    mut otag_chars: @~[char],
    mut ctag_chars: @~[char],
    mut tag_position: uint,
    tokens: DVec<Token>,
    partials: DVec<@~str>,
}

enum ParserState { TEXT, OTAG, TAG, CTAG }

impl Parser {
    fn eof(&self) -> bool { self.ch == -1 as char }

    fn bump(&self) {
        let mut lookahead = None;
        lookahead <-> self.lookahead;

        match lookahead {
            None => { self.ch = self.rdr.read_char(); }
            Some(ch) => { self.ch = ch; }
        }

        if self.ch == '\n' {
            self.line += 1u;
            self.col = 1u;
        } else {
            self.col += 1u;
        }
    }

    fn peek(&self) -> char {
        match self.lookahead {
          None => {
            let ch = self.rdr.read_char();
            self.lookahead = Some(ch);
            ch
          }
          Some(ch) => { ch }
        }
    }

    fn parse(&self) {
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
                    unsafe { str::push_char(&mut self.content, self.ch) };
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
                        self.tag_position += 1u;
                    }
                } else {
                    // We don't have a tag, so add all the tag parts we've seen
                    // so far to the string.
                    self.state = TEXT;
                    self.not_otag();
                    unsafe { str::push_char(&mut self.content, self.ch) };
                }
                self.bump();
              }
              TAG => {
                if self.content == ~"" && self.ch == '{' {
                    curly_brace_tag = true;
                    unsafe { str::push_char(&mut self.content, self.ch) };
                    self.bump();
                } else if curly_brace_tag && self.ch == '}' {
                    curly_brace_tag = false;
                    unsafe { str::push_char(&mut self.content, self.ch) };
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
                    unsafe { str::push_char(&mut self.content, self.ch) };
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
                        unsafe { str::push_char(&mut self.content, self.ch) };
                        self.bump();
                    }
                }
              }
            }
        }

        match self.state {
          TEXT => { self.add_text() }
          OTAG => { self.not_otag(); self.add_text() }
          TAG => { fail ~"unclosed tag" }
          CTAG => { self.not_ctag(); self.add_text() }
        }

        // Check that we don't have any incomplete sections.
        for self.tokens.each |token| {
            match *token {
                IncompleteSection(name, _, _, _) => {
                    fail fmt!("Unclosed mustache section %s",
                        str::connect(*name, ~"."));
              }
              _ => {}
            }
        };
    }

    fn add_text(&self) {
        if self.content != ~"" {
            let mut content = ~"";
            content <-> self.content;

            self.tokens.push(Text(@content));
        }
    }

    // This function classifies whether or not a token is standalone, or if it
    // has trailing whitespace. It's looking for this pattern:
    //
    //   ("\n" | "\r\n") whitespace* token ("\n" | "\r\n")
    //
    fn classify_token(&self) -> TokenClass {
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
                match str::rfind(*s, |c| c == '\n' || !char::is_whitespace(c)) {
                  // It's all whitespace.
                  None => {
                    if self.tokens.len() == 1u {
                        WhiteSpace(s, 0u)
                    } else {
                        Normal
                    }
                  }
                  Some(pos) => {
                    if str::char_at(*s, pos) == '\n' {
                        if pos == s.len() - 1u {
                            StandAlone
                        } else {
                            WhiteSpace(s, pos + 1u)
                        }
                    } else { Normal }
                  }
                }
              }
              _ => { Normal }
            }
        } else { Normal }
    }

    fn eat_whitespace(&self) -> bool {
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
              self.tokens.push(Text(@s.slice(0u, pos)));

              true
          }
        }
    }

    fn add_tag(&self) {
        self.bump();

        let tag = @(*self.otag + self.content + *self.ctag);

        // Move the content to avoid a copy.
        let mut content = ~"";
        content <-> self.content;
        let len = content.len();

        match content[0] as char {
          '!' => {
            // ignore comments
            self.eat_whitespace();
          }
          '&' => {
            let name = content.slice(1u, len);
            let name = self.check_content(name);
            let name = @str::split_char_nonempty(name, '.');
            self.tokens.push(UTag(name, tag));
          }
          '{' => {
            if str::ends_with(content, "}") {
                let name = content.slice(1u, len - 1u);
                let name = self.check_content(name);
                let name = @str::split_char_nonempty(name, '.');
                self.tokens.push(UTag(name, tag));
            } else { fail ~"unbalanced \"{\" in tag"; }
          }
          '#' => {
            let newlined = self.eat_whitespace();

            let name = self.check_content(content.slice(1u, len));
            let name = @str::split_char_nonempty(name, '.');
            self.tokens.push(IncompleteSection(name, false, tag, newlined));
          }
          '^' => {
            let newlined = self.eat_whitespace();

            let name = self.check_content(content.slice(1u, len));
            let name = @str::split_char_nonempty(name, '.');
            self.tokens.push(IncompleteSection(name, true, tag, newlined));
          }
          '/' => {
            self.eat_whitespace();

            let name = self.check_content(content.slice(1u, len));
            let name = @str::split_char_nonempty(name, '.');
            let mut children = ~[];

            loop {
                if self.tokens.len() == 0u {
                    fail ~"closing unopened section";
                }

                let last = self.tokens.pop();

                match last {
                  IncompleteSection(section_name, inverted, osection, _) => {
                    let children = vec::reversed(children);

                    // Collect all the children's sources.
                    let srcs = DVec();
                    for children.each |child: &Token| {
                        match child {
                            &Text(s)
                            | &ETag(_, s)
                            | &UTag(_, s)
                            | &Partial(_, _, s) =>
                                srcs.push(copy s),
                            &Section(_, _, _, _, osection, src, csection, _) => {
                                srcs.push(copy osection);
                                srcs.push(copy src);
                                srcs.push(copy csection);
                            }
                            _ => fail,
                        }
                    }

                    if section_name == name {
                        // Cache the combination of all the sources in the
                        // section. It's unfortunate, but we need to do this in
                        // case the user uses a function to instantiate the
                        // tag.
                        let mut src = ~"";
                        for srcs.each |s| { src += **s; }

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
                        fail ~"Unclosed section";
                    }
                  }
                  _ => { children.push(last); }
                }
            }
          }
          '>' => { self.add_partial(content, tag); }
          '=' => {
            self.eat_whitespace();

            if (len > 2u && str::ends_with(content, "=")) {
                let s = self.check_content(content.slice(1u, len - 1u));

                let pos = str::find_from(s, 0u, char::is_whitespace);
                let pos = match pos {
                  None => { fail ~"invalid change delimiter tag content"; }
                  Some(pos) => { pos }
                };

                self.otag = @s.slice(0u, pos);
                self.otag_chars = @str::chars(*self.otag);

                let pos = str::find_from(s, pos, |c| !char::is_whitespace(c));
                let pos = match pos {
                  None => { fail ~"invalid change delimiter tag content"; }
                  Some(pos) => { pos }
                };

                self.ctag = @s.slice(pos, s.len());
                self.ctag_chars = @str::chars(*self.ctag);
            } else {
                fail ~"invalid change delimiter tag content";
            }
          }
          _ => {
            let name = self.check_content(content);
            let name = @str::split_char_nonempty(name, '.');
            self.tokens.push(ETag(name, tag));
          }
        }
    }

    fn add_partial(&self, content: &str, tag: @~str) {
        let token_class = self.classify_token();
        let indent = match token_class {
          Normal => { ~"" }
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
            self.tokens.push(Text(@s.slice(0u, pos)));

            ws
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

    fn not_otag(&self) {
        let mut i = 0u;
        while i < self.tag_position {
            unsafe { str::push_char(&mut self.content, self.otag_chars[i]) };
            i += 1u;
        }
    }

    fn not_ctag(&self) {
        let mut i = 0u;
        while i < self.tag_position {
            unsafe { str::push_char(&mut self.content, self.ctag_chars[i]) };
            i += 1u;
        }
    }

    fn check_content(&self, content: &str) -> ~str {
        let trimmed = content.trim();
        if trimmed.len() == 0u {
            fail ~"empty tag";
        }
        trimmed
    }
}

struct CompileContext {
    rdr: io::Reader,
    partials: HashMap<@~str, @~[Token]>,
    otag: @~str,
    ctag: @~str,
    template_path: @~str,
    template_extension: @~str,
}

fn compile_helper(ctx: &CompileContext) -> @~[Token] {
    let parser = Parser {
        rdr: ctx.rdr,
        ch: 0 as char,
        lookahead: None,
        line: 1u,
        col: 1u,
        content: ~"",
        state: TEXT,
        otag: ctx.otag,
        ctag: ctx.ctag,
        otag_chars: @str::chars(*ctx.otag),
        ctag_chars: @str::chars(*ctx.ctag),
        tag_position: 0u,
        tokens: DVec(),
        partials: DVec(),
    };

    parser.bump();
    parser.parse();

    // Compile the partials if we haven't done so already.
    for parser.partials.each |name| {
    	let path: Path = path::from_str(*ctx.template_path);
    	let path = path.push(*name + *ctx.template_extension);

        if !ctx.partials.contains_key(*name) {
            // Insert a placeholder so we don't recurse off to infinity.
            ctx.partials.insert(*name, @~[]);

            match io::file_reader(&path) {
                Err(move _e) => {}
                Ok(move rdr) => {
                    let mut inner_ctx = CompileContext {
                        rdr: rdr,
                        partials: ctx.partials,
                        otag: @~"{{",
                        ctag: @~"}}",
                        template_path: ctx.template_path,
                        template_extension: ctx.template_extension
                    };
                    let tokens = compile_helper(&inner_ctx);

                    ctx.partials.insert(*name, tokens);
              }
            }
        }
    }

    // Destructure the parser so we get get at the tokens without a copy.
    let Parser { tokens: move tokens, _ } = move parser;

    @dvec::unwrap(tokens)
}

struct RenderContext {
    ctx: Context,
    tokens: @~[Token],
    partials: HashMap<@~str, @~[Token]>,
    stack: @~[Data],
    indent: @~str,
}

fn render_helper(ctx: &RenderContext) -> ~str {
    fn find(stack: &[Data], path: &[~str]) -> Option<Data> {
        // If we have an empty path, we just want the top value in our stack.
        if path.is_empty() {
            return match vec::last_opt(stack) {
              None => { None }
              Some(value) => { Some(value) }
            };
        }

        // Otherwise, find the stack that has the first part of our path.
        let mut value = None;

        let mut i = stack.len();
        while i > 0u {
            match stack[i - 1u] {
              Map(ctx) => {
                match ctx.find(@copy path[0u]) {
                  Some(v) => { value = Some(v); break; }
                  None => {}
                }
                i -= 1u;
              }
              _ => { fail fmt!("%? %?", stack, path) }
            }
        }

        // Walk the rest of the path to find our final value.
        let mut value = value;

        let mut i = 1u;
        let len = path.len();

        while i < len {
            match copy value {
                Some(Map(v)) => { value = v.find(@copy path[i]); }
                _ => break,
            }
            i += 1u;
        }

        value
    }

    let mut output = ~"";
    
    for ctx.tokens.each |token| {
        match token {
            &Text(value) => {
                // Indent the lines.
                if *ctx.indent == ~"" {
                    output += *value;
                } else {
                    let mut pos = 0u;
                    let len = value.len();

                    while pos < len {
                        let line = match str::find_char_from(*value, '\n', pos) {
                          None => {
                            let line = value.slice(pos, len);
                            pos = len;
                            line
                          }
                          Some(i) => {
                            let line = value.slice(pos, i + 1u);
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
            },
            &ETag(path, _) => {
                match find(*ctx.stack, *path) {
                    None => { }
                    Some(value) => {
                        output += ctx.indent + render_etag(value, ctx);
                    }
                }
            }
            &UTag(path, _) => {
                match find(*ctx.stack, *path) {
                    None => { }
                    Some(value) => {
                        output += ctx.indent + render_utag(value, ctx);
                    }
                }
            }
            &Section(path, true, children, _, _, _, _, _) => {
                let ctx = RenderContext { tokens: children, .. *ctx };

                output += match find(*ctx.stack, *path) {
                    None => { render_helper(&ctx) }
                    Some(value) => { render_inverted_section(value, &ctx) }
                };
            }
            &Section(path, false, children, otag, _, src, _, ctag) => {
                match find(*ctx.stack, *path) {
                    None => { }
                    Some(value) => {
                        output += render_section(
                            value,
                            src,
                            otag,
                            ctag,
                            &RenderContext { tokens: children ,.. *ctx }
                        );
                    }
                }
            }
            &Partial(ref name, ind, _) => {
                match ctx.partials.find(*name) {
                    None => { }
                    Some(tokens) => {
                        output += render_helper(&RenderContext {
                            tokens: tokens,
                            indent: @(ctx.indent + *ind),
                            .. *ctx
                        });
                    }
                }
            }
            _ => { fail }
        };
    };

    output
}

fn render_etag(value: Data, ctx: &RenderContext) -> ~str {
    let mut escaped = ~"";
    for str::each_char(render_utag(value, ctx)) |c| {
        match c {
          '<' => { escaped += "&lt;" }
          '>' => { escaped += "&gt;" }
          '&' => { escaped += "&amp;" }
          '"' => { escaped += "&quot;" }
          '\'' => { escaped += "&#39;" }
          _ => { str::push_char(&mut escaped, c); }
        }
    }
    escaped
}

fn render_utag(value: Data, ctx: &RenderContext) -> ~str {
    match value {
        Str(s) => copy *s,

        // etags and utags use the default delimiter.
        Fun(f) => render_fun(ctx, @~"", @~"{{", @~"}}", f),

        _ => fail,
    }
}

fn render_inverted_section(value: Data, ctx: &RenderContext) -> ~str {
    match value {
      Bool(false) => { render_helper(ctx) }
      Vec(xs) if xs.len() == 0 => { render_helper(ctx) }
      _ => { ~"" }
    }
}

fn render_section(value: Data,
                  src: @~str,
                  otag: @~str,
                  ctag: @~str,
                  ctx: &RenderContext) -> ~str {
    match value {
        Bool(true) => render_helper(ctx),
        Bool(false) => ~"",
        Vec(vs) => {
            do vs.borrow |vs| {
                str::concat(do vs.map |v| {
                    render_helper(&RenderContext {
                        stack: @(ctx.stack + ~[*v]),
                        .. *ctx
                    })
                })
            }
        }
        Map(_) =>
            render_helper(&RenderContext {
                stack: @(ctx.stack + ~[value]),
                .. *ctx
            }),
        Fun(f) => render_fun(ctx, src, otag, ctag, f),
        _ => fail,
    }
}

fn render_fun(ctx: &RenderContext,
              src: @~str,
              otag: @~str,
              ctag: @~str,
              f: fn(@~str) -> ~str) -> ~str {
    let tokens = do io::with_str_reader(f(src)) |rdr| {
        let mut inner_ctx = CompileContext {
            rdr: rdr,
            partials: ctx.partials,
            otag: otag,
            ctag: ctag,
            template_path: ctx.ctx.template_path,
            template_extension: ctx.ctx.template_extension
        };
        compile_helper(&inner_ctx)
    };

    render_helper(&RenderContext { tokens: tokens ,.. *ctx })
}

pure fn token_to_str(token: &Token) -> ~str
{
	match *token {
		// recursive enums crash %?
		Section(name, inverted, children, otag, osection, src, tag, ctag) =>
		{
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
		_ =>
		{
			fmt!("%?", token)
		}
	}
}

#[cfg(test)]
fn check_tokens(actual: &[Token], expected: &[Token]) -> bool
{
	// TODO: equality is currently broken for enums
	let actual = do actual.map |x| {token_to_str(x)};
	let expected = do expected.map |x| {token_to_str(x)};
	if actual !=  expected
	{
		io::stderr().write_line(fmt!("Found %?, but expected %?", actual, expected));
		return false;
	}
	return true;
}

#[cfg(test)]
mod tests {
    use send_map::linear::LinearMap;
    use std::json;

    #[test]
    fn test_compile_texts() {
        assert check_tokens(*compile_str(~"hello world").tokens, ~[Text(@~"hello world")]);
        assert check_tokens(*compile_str(~"hello {world").tokens, ~[Text(@~"hello {world")]);
        assert check_tokens(*compile_str(~"hello world}").tokens, ~[Text(@~"hello world}")]);
        assert check_tokens(*compile_str(~"hello world}}").tokens, ~[Text(@~"hello world}}")]);
    }

    #[test]
    fn test_compile_etags() {
        assert check_tokens(*compile_str(~"{{ name }}").tokens, ~[
            ETag(@~[~"name"], @~"{{ name }}")
        ]);

        assert check_tokens(*compile_str(~"before {{name}} after").tokens, ~[
            Text(@~"before "),
            ETag(@~[~"name"], @~"{{name}}"),
            Text(@~" after")
        ]);

        assert check_tokens(*compile_str(~"before {{name}}").tokens, ~[
            Text(@~"before "),
            ETag(@~[~"name"], @~"{{name}}")
        ]);

        assert check_tokens(*compile_str(~"{{name}} after").tokens, ~[
            ETag(@~[~"name"], @~"{{name}}"),
            Text(@~" after")
        ]);
    }

    #[test]
    fn test_compile_utags() {
        assert check_tokens(*compile_str(~"{{{name}}}").tokens, ~[
            UTag(@~[~"name"], @~"{{{name}}}")
        ]);

        assert check_tokens(*compile_str(~"before {{{name}}} after").tokens, ~[
            Text(@~"before "),
            UTag(@~[~"name"], @~"{{{name}}}"),
            Text(@~" after")
        ]);

        assert check_tokens(*compile_str(~"before {{{name}}}").tokens, ~[
            Text(@~"before "),
            UTag(@~[~"name"], @~"{{{name}}}")
        ]);

        assert check_tokens(*compile_str(~"{{{name}}} after").tokens, ~[
            UTag(@~[~"name"], @~"{{{name}}}"),
            Text(@~" after")
        ]);
    }

    #[test]
    fn test_compile_sections() {
        assert check_tokens(*compile_str(~"{{# name}}{{/name}}").tokens, ~[
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
        ]);

        assert check_tokens(*compile_str(~"before {{^name}}{{/name}} after").tokens, ~[
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
        ]);

        assert check_tokens(*compile_str(~"before {{#name}}{{/name}}").tokens, ~[
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
        ]);

        assert check_tokens(*compile_str(~"{{#name}}{{/name}} after").tokens, ~[
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
        ]);

        assert check_tokens(*compile_str(
                ~"before {{#a}} 1 {{^b}} 2 {{/b}} {{/a}} after").tokens, ~[
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
        ]);
    }

    #[test]
    fn test_compile_partials() {
        assert check_tokens(*compile_str(~"{{> test}}").tokens, ~[
            Partial(@~"test", @~"", @~"{{> test}}")
        ]);

        assert check_tokens(*compile_str(~"before {{>test}} after").tokens, ~[
            Text(@~"before "),
            Partial(@~"test", @~"", @~"{{>test}}"),
            Text(@~" after")
        ]);

        assert check_tokens(*compile_str(~"before {{> test}}").tokens, ~[
            Text(@~"before "),
            Partial(@~"test", @~"", @~"{{> test}}")
        ]);

        assert check_tokens(*compile_str(~"{{>test}} after").tokens, ~[
            Partial(@~"test", @~"", @~"{{>test}}"),
            Text(@~" after")
        ]);
    }

    #[test]
    fn test_compile_delimiters() {
        assert check_tokens(*compile_str(~"before {{=<% %>=}}<%name%> after").tokens, ~[
            Text(@~"before "),
            ETag(@~[~"name"], @~"<%name%>"),
            Text(@~" after")
        ]);
    }

    #[auto_serialize]
    struct Name { name: ~str }

    #[test]
    fn test_render_texts() {
        let ctx = &Name { name: ~"world" };

        assert render_str(~"hello world", ctx) == ~"hello world";
        assert render_str(~"hello {world", ctx) == ~"hello {world";
        assert render_str(~"hello world}", ctx) == ~"hello world}";
        assert render_str(~"hello {world}", ctx) == ~"hello {world}";
        assert render_str(~"hello world}}", ctx) == ~"hello world}}";
    }

    #[test]
    fn test_render_etags() {
        let ctx = &Name { name: ~"world" };

        assert render_str(~"hello {{name}}", ctx) == ~"hello world";
    }

    #[test]
    fn test_render_utags() {
        let ctx = &Name { name: ~"world" };

        assert render_str(~"hello {{{name}}}", ctx) == ~"hello world";
    }

/*
    enum StrHash<V> = HashMap<@~str, V>;

    impl<V: Serializable> StrHash<V>: Serializable {
        fn serialize<S: Serializer>(&self, s: &S) {
            do s.emit_rec || {
                let mut idx = 0;
                for self.each_ref |key, value| {
                    do s.emit_field(**key, idx) || {
                        (*value).serialize(s);
                    }
                    idx += 1;
                }
            }
        }
    }
    */

    #[test]
    fn test_render_sections() {
        let ctx0 = HashMap();
        let template = compile_str(~"0{{#a}}1 {{n}} 3{{/a}}5");

        assert template.render_data(Map(ctx0)) == ~"05";

        ctx0.insert(@~"a", Vec(@DVec()));
        assert template.render_data(Map(ctx0)) == ~"05";

        let ctx1: HashMap<@~str, Data> = HashMap();
        ctx0.insert(@~"a", Vec(@dvec::from_elem(Map(ctx1))));

        assert template.render_data(Map(ctx0)) == ~"01  35";

        let ctx1 = HashMap();
        ctx1.insert(@~"n", Str(@~"a"));
        ctx0.insert(@~"a", Vec(@dvec::from_elem(Map(ctx1))));
        assert template.render_data(Map(ctx0)) == ~"01 a 35";

        ctx0.insert(@~"a", Fun(|_text| {~"foo"}));
        assert template.render_data(Map(ctx0)) == ~"0foo5";
    }

    #[test]
    fn test_render_inverted_sections() {
        let template = compile_str(~"0{{^a}}1 3{{/a}}5");

        let ctx0 = HashMap();
        assert template.render_data(Map(ctx0)) == ~"01 35";

        ctx0.insert(@~"a", Vec(@DVec()));
        assert template.render_data(Map(ctx0)) == ~"01 35";

        let ctx1 = HashMap();
        ctx0.insert(@~"a", Vec(@dvec::from_elem(Map(ctx1))));
        assert template.render_data(Map(ctx0)) == ~"05";

        ctx1.insert(@~"n", Str(@~"a"));
        assert template.render_data(Map(ctx0)) == ~"05";
    }

    #[test]
    fn test_render_partial() {
        let path = ~"base";
        let template = compile_file(path);

        let ctx0 = HashMap();
        assert template.render_data(Map(ctx0)) == ~"<h2>Names</h2>\n";

        ctx0.insert(@~"names", Vec(@DVec()));
        assert template.render_data(Map(ctx0)) == ~"<h2>Names</h2>\n";

        let ctx1 = HashMap();
        ctx0.insert(@~"names", Vec(@dvec::from_elem(Map(ctx1))));
        assert template.render_data(Map(ctx0)) ==
           ~ "<h2>Names</h2>\n" +
            ~"  <strong></strong>\n\n";

        ctx1.insert(@~"name", Str(@~"a"));
        assert template.render_data(Map(ctx0)) ==
            ~"<h2>Names</h2>\n" +
            ~"  <strong>a</strong>\n\n";

        let ctx2 = HashMap();
        ctx2.insert(@~"name", Str(@~"<b>"));
        ctx0.insert(@~"names", Vec(@dvec::from_vec(~[Map(ctx1), Map(ctx2)])));
        assert template.render_data(Map(ctx0)) ==
            ~"<h2>Names</h2>\n" +
            ~"  <strong>a</strong>\n\n" +
            ~"  <strong>&lt;b&gt;</strong>\n\n";
    }

    fn parse_spec_tests(src: &str) -> ~[json::Json] {
    	let path: Path = path::from_str(src);
        match io::read_whole_file_str(&path) {
            Err(move e) => fail e,
            Ok(move s) => {
                match json::from_str(s) {
                    Err(e) => fail e.to_str(),
                    Ok(move json) => {
                        match json {
                            json::Object(ref d) => {
                                match d.find_ref(&~"tests") {
                                    Some(&json::List(move tests)) => tests,
                                    _ => fail fmt!("%s: tests key not a list", src),
                                }
                            }
                            _ => fail fmt!("%s: JSON value not a map", src),
                        }
                    }
                }
            }
        }
    }

/*
    fn convert_json_map(map: &json::Object) -> HashMap<@~str, Data> {
        let d = HashMap();
        for map.each |key, value| { d.insert(@copy *key, convert_json(value)); }
        move d
    }

    fn convert_json(value: &json::Json) -> Data {
        match *value {
          json::Number(n) => {
            // We have to cheat and use %? because %f doesn't convert 3.3 to
            // 3.3.
            Str(@fmt!("%?", n))
          }
          json::String(s) => { Str(@copy s) }
          json::Boolean(b) => { Bool(b) }
          json::List(v) => { Vec(@v.map(convert_json)) }
          json::Object(d) => { Map(convert_json_map(d)) }
          _ => { fail fmt!("%?", value) }
        }
    }
*/

    struct Tmpdir {
        path: path::Path,
        drop {
            os::walk_dir(&self.path, os::remove_file);
            os::remove_dir(&self.path);
        }
    }

    fn write_partials(tmpdir: &path::Path, value: &json::Json) {
        match value {
            &json::Object(ref d) => {
                for d.each |key, value| {
                    match value {
                        &json::String(ref s) => {
                            let path = tmpdir.push(key + ".mustache");
                            match io::file_writer(&path, ~[io::Create, io::Truncate]) {
                                Ok(move wr) => wr.write_str(*s),
                                Err(move e) => fail e,
                            }
                        }
                        _ => fail,
                    }
                }
            },
            _ => fail,
        }
    }

    fn run_test(test: ~json::Object, data: Data) {
        let mut test = test;

        let template = match test.find_ref(&~"template") {
            Some(&json::String(copy s)) => s,
            _ => fail,
        };

        let expected = match test.find_ref(&~"expected") {
            Some(&json::String(copy s)) => s,
            _ => fail,
        };

        // Make a temporary dir where we'll store our partials. This is to
        // avoid a race on filenames.
        let tmpdir = match std::tempfile::mkdtemp(&path::Path("."), "") {
            Some(move path) => Tmpdir { path: path },
            None => fail,
        };

        match test.find_ref(&~"partials") {
            Some(value) => write_partials(&tmpdir.path, value),
            None => {},
        }

        let ctx = Context(tmpdir.path.to_str(), ~".mustache");
        let template = ctx.compile_str(template);
        let result = template.render_data(data);

        if result != expected {
            fn to_list(x: &json::Json) -> json::Json {
                match x {
                    &json::Object(ref d) => {
                        let mut xs = ~[];
                        for d.each |k, v| {
                            let k = json::String(copy *k);
                            let v = to_list(v);
                            xs.push(json::List(~[k, v]));
                        }
                        json::List(xs)
                    },
                    &json::List(ref xs) => {
                        json::List(xs.map(|x| to_list(x)))
                    },
                    _ => { copy *x }
                }
            }

            io::println(fmt!("desc:     %s", test.get_ref(&~"desc").to_str()));
            io::println(fmt!("context:  %s", test.get_ref(&~"data").to_str()));
            io::println(~"=>");
            io::println(fmt!("template: %?", template));
            io::println(fmt!("expected: %?", expected));
            io::println(fmt!("actual:   %?", result));
            io::println(~"");
        }
        assert result == expected;
    }

    fn run_tests(spec: &str) {
        do vec::consume(parse_spec_tests(spec)) |_i, json| {
            let test = match move json {
                json::Object(move m) => m,
                _ => fail,
            };

            let data = match test.find_ref(&~"data") {
                Some(data) => copy *data,
                None => fail,
            };

            let serializer = DataSerializer();
            data.serialize(&serializer);

            run_test(move test, serializer.data.take());
        }
    }

    #[test]
    fn test_spec_comments() {
        run_tests(~"spec/specs/comments.json");
    }

    #[test]
    fn test_spec_delimiters() {
        run_tests(~"spec/specs/delimiters.json");
    }

    #[test]
    fn test_spec_interpolation() {
        run_tests(~"spec/specs/interpolation.json");
    }

    #[test]
    fn test_spec_inverted() {
        run_tests(~"spec/specs/inverted.json");
    }

    #[test]
    fn test_spec_partials() {
        run_tests(~"spec/specs/partials.json");
    }

    #[test]
    fn test_spec_sections() {
        run_tests(~"spec/specs/sections.json");
    }

    #[test]
    fn test_spec_lambdas() {
        for parse_spec_tests(~"spec/specs/~lambdas.json").each |json| {
            let test = match json {
                &json::Object(copy m) => m,
                _ => fail,
            };

            // Replace the lambda with rust code.
            let data = match test.find_ref(&~"data") {
                Some(data) => copy *data,
                None => fail,
            };

            let serializer = DataSerializer();
            data.serialize(&serializer);
            let ctx = match serializer.data.take() {
                Map(move ctx) => ctx,
                _ => fail,
            };

            let f = match test.get_ref(&~"name") {
              &json::String(~"Interpolation") => {
                  |_text| {~"world" }
              }
              &json::String(~"Interpolation - Expansion") => {
                  |_text| {~"{{planet}}" }
              }
              &json::String(~"Interpolation - Alternate Delimiters") => {
                  |_text| {~"|planet| => {{planet}}" }
              }
              &json::String(~"Interpolation - Multiple Calls") => {
                  let calls = @mut 0;
                  |_text| {*calls += 1; int::str(*calls) }
              }
              &json::String(~"Escaping") => {
                  |_text| {~">" }
              }
              &json::String(~"Section") => {
                  |text: @~str| {if *text == ~"{{x}}" { ~"yes" } else { ~"no" } }
              }
              &json::String(~"Section - Expansion") => {
                  |text: @~str| {*text + "{{planet}}" + *text }
              }
              &json::String(~"Section - Alternate Delimiters") => {
                  |text: @~str| {*text + "{{planet}} => |planet|" + *text }
              }
              &json::String(~"Section - Multiple Calls") => {
                  |text: @~str| {~"__" + *text + ~"__" }
              }
              &json::String(~"Inverted Section") => {
                  |_text| {~"" }
              }
              value => { fail fmt!("%?", value) }
            };
            ctx.insert(@~"lambda", Fun(f));

            run_test(test, Map(ctx));
        }
    }
}
