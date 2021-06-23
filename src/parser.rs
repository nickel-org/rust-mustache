use std::error::Error as StdError;
use std::mem;
use std::fmt;

// for bug!
use log::error;

/// `Token` is a section of a compiled mustache string.
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Text(String),
    EscapedTag(Vec<String>, String),
    UnescapedTag(Vec<String>, String),
    Section(Vec<String>, bool, Vec<Token>, String, String, String, String, String),
    IncompleteSection(Vec<String>, bool, String, bool),
    Partial(String, String, String),
}

/// Error type to represent parsing failure.
///
/// This type is not intended to be matched exhaustively as new variants
/// may be added in future without a version bump.
#[derive(Debug, PartialEq)]
#[non_exhaustive]
pub enum Error {
    BadClosingTag(char, char),
    UnclosedTag,
    UnclosedSection(String),
    UnbalancedUnescapeTag,
    EmptyTag,
    EarlySectionClose(String),
    MissingSetDelimeterClosingTag,
    InvalidSetDelimeterSyntax,
}

impl StdError for Error { }

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Provide more information where possible
        match *self {
            Error::BadClosingTag(actual, expected) => write!(f, "character {:?} was unexpected in the closing tag, expected {:?}", actual, expected),
            Error::UnclosedSection(ref name) => write!(f, "found an unclosed section: {:?}", name),
            Error::EarlySectionClose(ref name) => write!(f, "found a closing tag for an unopened section {:?}", name),
            Error::UnclosedTag => write!(f, "found an unclosed tag"),
            Error::UnbalancedUnescapeTag => write!(f, "found an unbalanced unescape tag"),
            Error::EmptyTag => write!(f, "found an empty tag",),
            Error::MissingSetDelimeterClosingTag => write!(f, "missing the new closing tag in set delimeter tag"),
            Error::InvalidSetDelimeterSyntax => write!(f, "invalid set delimeter tag syntax"),
        }
    }
}

enum TokenClass {
    Normal,
    StandAlone,
    WhiteSpace(String, usize),
}

/// `Parser` parses a string into a series of `Token`s.
pub struct Parser<'a, T: 'a> {
    reader: &'a mut T,
    ch: Option<char>,
    lookahead: Option<char>,
    line: usize,
    col: usize,
    content: String,
    state: ParserState,
    opening_tag: String,
    closing_tag: String,
    opening_tag_chars: Vec<char>,
    closing_tag_chars: Vec<char>,
    tag_position: usize,
    tokens: Vec<Token>,
    partials: Vec<String>,
}

enum ParserState {
    Text,
    OpeningTag,
    Tag,
    ClosingTag,
}

impl<'a, T: Iterator<Item = char>> Parser<'a, T> {
    pub fn new(reader: &'a mut T, opening_tag: &str, closing_tag: &str) -> Parser<'a, T> {
        let mut parser = Parser {
            reader,
            ch: None,
            lookahead: None,
            line: 1,
            col: 1,
            content: String::new(),
            state: ParserState::Text,
            opening_tag: opening_tag.to_string(),
            closing_tag: closing_tag.to_string(),
            opening_tag_chars: opening_tag.chars().collect(),
            closing_tag_chars: closing_tag.chars().collect(),
            tag_position: 0,
            tokens: Vec::new(),
            partials: Vec::new(),
        };

        parser.bump();
        parser
    }

    fn bump(&mut self) {
        match self.lookahead.take() {
            None => {
                self.ch = self.reader.next();
            }
            Some(ch) => {
                self.ch = Some(ch);
            }
        }

        if let Some(ch) = self.ch {
            if ch == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
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

    /// Parse the template into tokens and a list of partial files.
    pub fn parse(mut self) -> Result<(Vec<Token>, Vec<String>), Error> {
        let mut curly_brace_tag = false;

        while let Some(ch) = self.ch {
            match self.state {
                ParserState::Text => {
                    if ch == self.opening_tag_chars[0] {
                        if self.opening_tag_chars.len() > 1 {
                            self.tag_position = 1;
                            self.state = ParserState::OpeningTag;
                        } else {
                            self.add_text();
                            self.state = ParserState::Tag;
                        }
                    } else {
                        self.content.push(ch);
                    }
                    self.bump();
                }
                ParserState::OpeningTag => {
                    if ch == self.opening_tag_chars[self.tag_position] {
                        if self.tag_position == self.opening_tag_chars.len() - 1 {
                            self.add_text();
                            curly_brace_tag = false;
                            self.state = ParserState::Tag;
                        } else {
                            self.tag_position += 1;
                        }
                    } else {
                        // We don't have a tag, so add all the tag parts we've seen
                        // so far to the string.
                        self.state = ParserState::Text;
                        self.not_otag();
                        self.content.push(ch);
                    }
                    self.bump();
                }
                ParserState::Tag => {
                    if self.content.is_empty() && ch == '{' {
                        curly_brace_tag = true;
                        self.content.push(ch);
                        self.bump();
                    } else if curly_brace_tag && ch == '}' {
                        curly_brace_tag = false;
                        self.content.push(ch);
                        self.bump();
                    } else if ch == self.closing_tag_chars[0] {
                        if self.closing_tag_chars.len() > 1 {
                            self.tag_position = 1;
                            self.state = ParserState::ClosingTag;
                            self.bump();
                        } else {
                            self.add_tag()?;
                            self.state = ParserState::Text;
                        }
                    } else {
                        self.content.push(ch);
                        self.bump();
                    }
                }
                ParserState::ClosingTag => {
                    if ch == self.closing_tag_chars[self.tag_position] {
                        if self.tag_position == self.closing_tag_chars.len() - 1 {
                            self.add_tag()?;
                            self.state = ParserState::Text;
                        } else {
                            self.state = ParserState::Tag;
                            self.not_ctag();
                            self.content.push(ch);
                            self.bump();
                        }
                    } else {
                        let expected = self.closing_tag_chars[self.tag_position];
                        return Err(Error::BadClosingTag(ch, expected));
                    }
                }
            }
        }

        match self.state {
            ParserState::Text => {
                self.add_text();
            }
            ParserState::OpeningTag => {
                self.not_otag();
                self.add_text();
            }
            ParserState::ClosingTag => {
                self.not_ctag();
                self.add_text();
            }
            ParserState::Tag => return Err(Error::UnclosedTag),
        }

        // Check that we don't have any incomplete sections.
        for token in self.tokens.iter().rev() {
            if let Token::IncompleteSection(ref path, _, _, _) = *token {
                return Err(Error::UnclosedSection(path.join(".")))
            }
        }

        let Parser { tokens, partials, .. } = self;

        Ok((tokens, partials))
    }

    fn add_text(&mut self) {
        if !self.content.is_empty() {
            let mut content = String::new();
            mem::swap(&mut content, &mut self.content);

            self.tokens.push(Token::Text(content));
        }
    }

    // This function classifies whether or not a token is standalone, or if it
    // has trailing whitespace. It's looking for this pattern:
    //
    //   ("\n" | "\r\n") whitespace* token ("\n" | "\r\n")
    //
    fn classify_token(&mut self) -> TokenClass {
        // Exit early if the next character is not '\n' or '\r\n'.
        if let Some(ch) = self.ch {
            if !(ch == '\n' || (ch == '\r' && self.peek() == Some('\n'))) {
                return TokenClass::Normal;
            }
        }

        match self.tokens.last() {
            // If the last token ends with a newline (or there is no previous
            // token), then this token is standalone.
            None => TokenClass::StandAlone,

            Some(&Token::IncompleteSection(_, _, _, true)) => TokenClass::StandAlone,

            Some(&Token::Text(ref s)) if !s.is_empty() => {
                // Look for the last newline character that may have whitespace
                // following it.
                match s.rfind(|c: char| c == '\n' || !c.is_whitespace()) {
                    // It's all whitespace.
                    None => {
                        if self.tokens.len() == 1 {
                            TokenClass::WhiteSpace(s.clone(), 0)
                        } else {
                            TokenClass::Normal
                        }
                    }
                    Some(pos) => {
                        if s.as_bytes()[pos] == b'\n' {
                            if pos == s.len() - 1 {
                                TokenClass::StandAlone
                            } else {
                                TokenClass::WhiteSpace(s.clone(), pos + 1)
                            }
                        } else {
                            TokenClass::Normal
                        }
                    }
                }
            }
            Some(_) => TokenClass::Normal,
        }
    }

    fn eat_whitespace(&mut self) -> bool {
        // If the next character is a newline, and the last token ends with a
        // newline and whitespace, clear out the whitespace.

        match self.classify_token() {
            TokenClass::Normal => false,
            TokenClass::StandAlone => {
                if self.ch_is('\r') {
                    self.bump();
                }
                self.bump();
                true
            }
            TokenClass::WhiteSpace(s, pos) => {
                if self.ch_is('\r') {
                    self.bump();
                }
                self.bump();

                // Trim the whitespace from the last token.
                self.tokens.pop();
                self.tokens.push(Token::Text(s[0..pos].to_string()));

                true
            }
        }
    }

    fn add_tag(&mut self) -> Result<(), Error> {
        self.bump();

        let tag = self.opening_tag.clone() + &self.content + &self.closing_tag;

        // Move the content to avoid a copy.
        let mut content = String::new();
        mem::swap(&mut content, &mut self.content);
        let len = content.len();
        deny_blank(&content)?;
        let content = content;

        match content.as_bytes()[0] as char {
            '!' => {
                // ignore comments
                self.eat_whitespace();
            }
            '&' => {
                let name = &content[1..len];
                let name = get_name_or_implicit(name)?;
                self.tokens.push(Token::UnescapedTag(name, tag));
            }
            '{' => {
                if content.ends_with('}') {
                    let name = &content[1..len - 1];
                    let name = get_name_or_implicit(name)?;
                    self.tokens.push(Token::UnescapedTag(name, tag));
                } else {
                    return Err(Error::UnbalancedUnescapeTag)
                }
            }
            '#' => {
                let newlined = self.eat_whitespace();

                let name = get_name_or_implicit(&content[1..len])?;
                self.tokens.push(Token::IncompleteSection(name, false, tag, newlined));
            }
            '^' => {
                let newlined = self.eat_whitespace();

                let name = get_name_or_implicit(&content[1..len])?;
                self.tokens.push(Token::IncompleteSection(name, true, tag, newlined));
            }
            '/' => {
                self.eat_whitespace();

                let name = get_name_or_implicit(&content[1..len])?;
                let mut children: Vec<Token> = Vec::new();

                loop {
                    if self.tokens.is_empty() {
                        return Err(Error::EarlySectionClose(name.join(".")))
                    }

                    let last = self.tokens.pop();

                    match last {
                        Some(Token::IncompleteSection(section_name, inverted, osection, _)) => {
                            children.reverse();

                            // Collect all the children's sources.
                            let mut srcs = Vec::new();
                            for child in children.iter() {
                                match *child {
                                    Token::Text(ref s) |
                                    Token::EscapedTag(_, ref s) |
                                    Token::UnescapedTag(_, ref s) |
                                    Token::Partial(_, _, ref s) => srcs.push(s.clone()),
                                    Token::Section(_, _, _, _, ref osection, ref src, ref csection, _) => {
                                        srcs.push(osection.clone());
                                        srcs.push(src.clone());
                                        srcs.push(csection.clone());
                                    }
                                    _ => bug!("Incomplete sections should not be nested"),
                                }
                            }

                            if section_name == name {
                                // Cache the combination of all the sources in the
                                // section. It's unfortunate, but we need to do this in
                                // case the user uses a function to instantiate the
                                // tag.
                                let mut src = String::new();
                                for s in srcs.iter() {
                                    src.push_str(s);
                                }

                                self.tokens.push(Token::Section(name,
                                                         inverted,
                                                         children,
                                                         self.opening_tag.clone(),
                                                         osection,
                                                         src,
                                                         tag,
                                                         self.closing_tag.clone()));
                                break;
                            } else {
                                return Err(Error::UnclosedSection(section_name.join(".")))
                            }
                        }
                        Some(last_token) => children.push(last_token),
                        None => (),
                    }
                }
            }
            '>' => self.add_partial(&content, tag)?,
            '=' => {
                self.eat_whitespace();

                if len > 2usize && content.ends_with('=') {
                    let s = deny_blank(&content[1..len - 1])?;

                    let pos = s.find(char::is_whitespace);
                    let pos = match pos {
                        None => return Err(Error::MissingSetDelimeterClosingTag),
                        Some(pos) => pos,
                    };

                    self.opening_tag = s[0..pos].to_string();
                    self.opening_tag_chars = self.opening_tag.chars().collect();

                    let s2 = &s[pos..];
                    let pos = s2.find(|c: char| !c.is_whitespace());
                    let pos = match pos {
                        None => return Err(Error::MissingSetDelimeterClosingTag),
                        Some(pos) => pos,
                    };

                    self.closing_tag = s2[pos..].to_string();
                    self.closing_tag_chars = self.closing_tag.chars().collect();
                } else {
                    return Err(Error::InvalidSetDelimeterSyntax)
                }
            }
            _ => {
                // If the name is "." then we want the top element, which we represent with
                // an empty name.
                let name = get_name_or_implicit(&content)?;
                self.tokens.push(Token::EscapedTag(name, tag));
            }
        };

        Ok(())
    }

    fn add_partial(&mut self, content: &str, tag: String) -> Result<(), Error> {
        let indent = match self.classify_token() {
            TokenClass::Normal => "".to_string(),
            TokenClass::StandAlone => {
                if self.ch_is('\r') {
                    self.bump();
                }
                self.bump();
                "".to_string()
            }
            TokenClass::WhiteSpace(s, pos) => {
                if self.ch_is('\r') {
                    self.bump();
                }
                self.bump();

                let ws = &s[pos..];

                // Trim the whitespace from the last token.
                self.tokens.pop();
                self.tokens.push(Token::Text(s[0..pos].to_string()));

                ws.to_string()
            }
        };

        // We can't inline the tokens directly as we may have a recursive
        // partial. So instead, we'll cache the partials we used and look them
        // up later.
        let name = &content[1..content.len()];
        let name = deny_blank(name)?;

        self.tokens.push(Token::Partial(name.into(), indent, tag));
        self.partials.push(name.into());

        Ok(())
    }

    fn not_otag(&mut self) {
        for (i, ch) in self.opening_tag_chars.iter().enumerate() {
            if i >= self.tag_position {
                break;
            }
            self.content.push(*ch);
        }
    }

    fn not_ctag(&mut self) {
        for (i, ch) in self.closing_tag_chars.iter().enumerate() {
            if i >= self.tag_position {
                break;
            }
            self.content.push(*ch);
        }
    }
}

fn get_name_or_implicit(name: &str) -> Result<Vec<String>, Error> {
    // If the name is "." then we want the top element, which we represent with
    // an empty name.
    let name = deny_blank(&name)?;
    Ok(if name == "." {
        Vec::new()
    } else {
        name.split_terminator('.')
            .map(|x| x.to_string())
            .collect()
    })
}

fn deny_blank(content: &str) -> Result<&str, Error> {
    let trimmed = content.trim();
    if trimmed.is_empty() {
        Err(Error::EmptyTag)
    } else {
        Ok(trimmed)
    }
}

//FIXME: These tests are mainly to guide the removal of panics from the parser (turning them
// into `Error`s instead). It would be good to make them more robust and make assertions on
// the tokens & partials returned.
#[cfg(test)]
mod tests {
    use super::*;

    pub fn parse(input: &str) -> Result<(Vec<Token>, Vec<String>), Error> {
        let input = &mut input.chars();
        let parser = Parser::new(input, "{{", "}}");
        parser.parse()
    }

    pub fn assert_parse(input: &str) -> (Vec<Token>, Vec<String>) {
        parse(input).expect(&format!("Failed to parse: {}", input))
    }

    #[test]
    fn empty_input() {
        assert_parse("");
    }

    #[test]
    fn empty_tag() {
        assert_eq!(parse("{{}}"), Err(Error::EmptyTag));
    }

    #[test]
    fn whitespace_only_tag() {
        assert_eq!(parse("{{ }}"), Err(Error::EmptyTag));
    }

    #[test]
    fn bad_closing_tag() {
        assert_eq!(parse("{{hello}?"), Err(Error::BadClosingTag('?', '}')))
    }

    #[test]
    fn unclosed_tag() {
        assert_eq!(parse("{{hi"), Err(Error::UnclosedTag))
    }

    mod sections {
        use super::*;

        #[test]
        fn sanity() {
            assert_parse("{{#people}} Hi {{name}}! {{/people}}");
        }

        #[test]
        fn unclosed() {
            assert_eq!(parse("{{#world}}hi"), Err(Error::UnclosedSection("world".into())))
        }

        #[test]
        fn unclosed_nested_with_wrong_closing_tag() {
            assert_eq!(
                parse("{{#universe}} {{#world}} {{/universe}}"),
                Err(Error::UnclosedSection("world".into()))
            )
        }

        #[test]
        fn unclosed_nested() {
            assert_eq!(
                parse("{{#universe}} {{#world}}"),
                Err(Error::UnclosedSection("world".into()))
            )
        }

        #[test]
        fn unclosed_with_path() {
            assert_eq!(
                parse("{{#universe}} {{#world.and.stuff}} {{/universe}}"),
                Err(Error::UnclosedSection("world.and.stuff".into()))
            )
        }

        #[test]
        fn early_close() {
            assert_eq!(parse("{{/world}}"), Err(Error::EarlySectionClose("world".into())))
        }
    }

    mod inverted {
        use super::*;

        #[test]
        fn sanity() {
            assert_parse("{{^people}} No people! {{/people}}");
        }

        #[test]
        fn unclosed() {
            assert_eq!(parse("{{^world}}hi"), Err(Error::UnclosedSection("world".into())))
        }

        #[test]
        fn unclosed_nested_with_wrong_closing_tag() {
            assert_eq!(
                parse("{{#universe}} {{^world}} {{/universe}}"),
                Err(Error::UnclosedSection("world".into()))
            );

            assert_eq!(
                parse("{{^universe}} {{^world}} {{/universe}}"),
                Err(Error::UnclosedSection("world".into()))
            )
        }

        #[test]
        fn unclosed_nested() {
            assert_eq!(
                parse("{{#universe}} {{^world}}"),
                Err(Error::UnclosedSection("world".into()))
            )
        }

        #[test]
        fn unclosed_with_path() {
            assert_eq!(
                parse("{{#universe}} {{^world.and.stuff}} {{/universe}}"),
                Err(Error::UnclosedSection("world.and.stuff".into()))
            )
        }
    }

    mod set_delimeter {
        use super::*;

        #[test]
        fn sanity() {
            assert_parse("{{=<% %>=}}");
        }

        #[test]
        fn closing_tag_is_whitespace() {
            assert_eq!(parse("{{=<% =}}"), Err(Error::MissingSetDelimeterClosingTag))
        }

        #[test]
        fn missing_closing_tag() {
            assert_eq!(parse("{{=<%=}}"), Err(Error::MissingSetDelimeterClosingTag))
        }

        #[test]
        fn missing_closing_equals() {
            assert_eq!(parse("{{=<% %>}}"), Err(Error::InvalidSetDelimeterSyntax))
        }
    }

    #[test]
    fn unbalanced_unescape() {
        // use the set delimiter tag to change the brace type. Currently this error will
        // not trigger with "{{{ }}"
        let input = "{{=<% %>=}} <%{ %>";
        assert_eq!(parse(input), Err(Error::UnbalancedUnescapeTag))
    }
}
