// SPDX-License-Identifier: LGPL-3.0-or-later

use std::borrow::Cow;
use std::fmt::Write;

use lazy_regex::{regex, regex_is_match};
use logos::{Logos, Span, SpannedIter};

use crate::DiagnosticKind;

pub type IntegerValue = i64;
pub type FloatValue = f64;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ArrayKind {
    Array,
    Command,
    Property,
}

impl ArrayKind {
    pub fn delimiter(&self, open: bool) -> char {
        let delimiters = self.delimiters();
        match open {
            true => delimiters.0,
            false => delimiters.1,
        }
    }

    pub fn delimiter_tokens(&self) -> (TokenValue<'static>, TokenValue<'static>) {
        match self {
            ArrayKind::Array => (TokenValue::ArrayOpen, TokenValue::ArrayClose),
            ArrayKind::Command => (TokenValue::CommandOpen, TokenValue::CommandClose),
            ArrayKind::Property => (TokenValue::PropertyOpen, TokenValue::PropertyClose),
        }
    }

    pub fn delimiters(&self) -> (char, char) {
        match self {
            ArrayKind::Array => ('(', ')'),
            ArrayKind::Command => ('{', '}'),
            ArrayKind::Property => ('[', ']'),
        }
    }
}

impl std::fmt::Display for ArrayKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArrayKind::Array => write!(f, "array"),
            ArrayKind::Command => write!(f, "command"),
            ArrayKind::Property => write!(f, "property"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TextToken<'src> {
    pub text: Cow<'src, str>,
    pub location: Span,
}

impl<'src> TextToken<'src> {
    pub const fn new(text: &'src str, location: Span) -> Self {
        Self { text: Cow::Borrowed(text), location }
    }

    pub const fn from_cow(text: Cow<'src, str>, location: Span) -> Self {
        Self { text, location }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockCommentToken<'src> {
    pub open: TextToken<'src>,
    pub body: TextToken<'src>,
    pub close: TextToken<'src>,
}

impl<'src> BlockCommentToken<'src> {
    pub const fn new(start_pos: usize, open: &'src str, body: &'src str, close: &'src str) -> Self {
        let open = TextToken::new(open, start_pos..start_pos + open.len());
        let body = TextToken::new(body, open.location.end..open.location.end + body.len());
        let close = TextToken::new(close, body.location.end..body.location.end + close.len());
        Self { open, body, close }
    }

    pub fn contains(&self, pat: &str) -> bool {
        self.open.text.contains(pat) || self.body.text.contains(pat) || self.close.text.contains(pat)
    }
}

impl std::fmt::Display for BlockCommentToken<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.open.text)?;
        f.write_str(&self.body.text)?;
        f.write_str(&self.close.text)
    }
}

#[derive(Debug, PartialEq)]
pub struct Token<'src> {
    pub value: TokenValue<'src>,
    pub location: Span,
}

impl<'src> Token<'src> {
    pub fn new(value: TokenValue<'src>, location: Span) -> Self {
        Self { value, location }
    }

    pub fn get_kind(&self) -> TokenKind {
        self.value.get_kind()
    }
}

/// wizardry
///
/// Type(value_type) => ("display string", "{value_format}")
macro_rules! make_tokens {
    (
        $(
            $(#[$attr:meta])*
            $variant:ident$(($(&$life:lifetime)? $value_type:ident$(<$($value_generics:tt),*>)?))? {
                kind_display: $kind_display:literal,
                value_display: |$display_f:ident $(, $display_value:ident)?| $display_expr:expr,
            }
            $(,)?
        )+
    ) => {
        #[derive(Logos, Debug, PartialEq)]
        #[logos(error = DiagnosticKind)]
        #[logos(skip r#"[ \v\t\r\n\f]+"#)]
        pub enum TokenValue<'src> {
            $($(#[$attr])* $variant$(($(&$life)? $value_type$(<$($value_generics),*>)?))?,)+
        }

        impl<'src> TokenValue<'src> {
            pub fn get_kind(&self) -> TokenKind {
                match self {
                    $(TokenValue::$variant$((meta_morph!(|$value_type| _)))? => TokenKind::$variant,)+
                }
            }
        }

        impl<'src> std::fmt::Display for TokenValue<'src> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(TokenValue::$variant$(($display_value))? => {
                        let $display_f = f;
                        $display_expr
                    },)+
                }
            }
        }

        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
        pub enum TokenKind {
            $($variant,)+
        }

        impl<'src> std::fmt::Display for TokenKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(TokenKind::$variant => write!(f, $kind_display),)+
                }
            }
        }
    }
}

// Token regexes based on the RB3 decomp.
// Behavior is replicated as closely as possible, all files should tokenize
// identically to the original Flex tokenizer based on current knowledge for it.

make_tokens! {
    #[regex(r#"[+-]?[0-9]+"#, |lex| lex.slice().parse::<IntegerValue>(), priority = 2)]
    #[regex(r#"0x[A-Fa-f0-9]+"#, parse_hex, priority = 2)]
    Integer(IntegerValue) {
        kind_display: "integer",
        value_display: |f, value| value.fmt(f),
    },
    // This one allows some weird things, such as ".", "+.", and "-.E1"
    #[regex(r#"[+-]?[0-9]*\.[0-9]*([Ee][+-]?[0-9])?"#, parse_float, priority = 2)]
    Float(FloatValue) {
        kind_display: "float",
        value_display: |f, value| {
            // Printed as f32, the additional storage precision isn't necessary for text
            // Debug display used since it acts as a general format specifier
            write!(f, "{:?}", (*value as f32))
        },
    },
    #[regex(r#""[^"]*""#, |lex| trim_delimiters(lex.slice(), 1, 1))]
    String(Cow<'src, str>) {
        kind_display: "string",
        value_display: |f, value| write!(f, "\"{}\"", value),
    },

    // Symbol consumes almost all input which doesn't match any other token,
    // including technically malformed versions of integers/floats
    #[regex(r#"[^ \v\t\r\n\f\(\)\[\]\{\}]+"#, |lex| parse_symbol(lex, 0, 0), priority = 0)]
    #[regex(r#"'[^']*'"#, |lex| parse_symbol(lex, 1, 1))]
    Symbol(Cow<'src, str>) {
        kind_display: "symbol",
        value_display: |f, value| {
            // Write without quotes where possible
            if regex_is_match!(r#"[^ \v\t\r\n\f\(\)\[\]\{\}\']+"#, value) {
                f.write_str(value)
            } else {
                write!(f, "'{}'", value)
            }
        },
    },
    #[regex(r#"\$[^ \v\t\r\n\f\(\)\[\]\{\}]+"#, |lex| parse_symbol(lex, 1, 0))]
    Variable(Cow<'src, str>) {
        kind_display: "variable",
        value_display: |f, value| write!(f, "${}", value),
    },
    #[token("kDataUnhandled")]
    Unhandled {
        kind_display: "kDataUnhandled",
        value_display: |f| f.write_str("kDataUnhandled"),
    },

    #[token("(")]
    ArrayOpen {
        kind_display: "array open",
        value_display: |f| f.write_char('('),
    },
    #[token(")")]
    ArrayClose {
        kind_display: "array close",
        value_display: |f| f.write_char(')'),
    },
    #[token("{")]
    CommandOpen {
        kind_display: "command open",
        value_display: |f| f.write_char('{'),
    },
    #[token("}")]
    CommandClose {
        kind_display: "command close",
        value_display: |f| f.write_char('}'),
    },
    #[token("[")]
    PropertyOpen {
        kind_display: "property open",
        value_display: |f| f.write_char('['),
    },
    #[token("]")]
    PropertyClose {
        kind_display: "property close",
        value_display: |f| f.write_char(']'),
    },

    #[token("#define")]
    Define {
        kind_display: "#define directive",
        value_display: |f| f.write_str("#define"),
    },
    #[token("#undef")]
    Undefine {
        kind_display: "#undef directive",
        value_display: |f| f.write_str("#undef"),
    },
    #[token("#include")]
    Include {
        kind_display: "#include directive",
        value_display: |f| f.write_str("#include"),
    },
    #[token("#include_opt")]
    IncludeOptional {
        kind_display: "#include_opt directive",
        value_display: |f| f.write_str("#include_opt"),
    },
    #[token("#merge")]
    Merge {
        kind_display: "#merge directive",
        value_display: |f| f.write_str("#merge"),
    },
    #[token("#autorun")]
    Autorun {
        kind_display: "#autorun directive",
        value_display: |f| f.write_str("#autorun"),
    },

    #[token("#ifdef")]
    Ifdef {
        kind_display: "#ifdef directive",
        value_display: |f| f.write_str("#ifdef"),
    },
    #[token("#ifndef")]
    Ifndef {
        kind_display: "#ifndef directive",
        value_display: |f| f.write_str("#ifndef"),
    },
    #[token("#else")]
    Else {
        kind_display: "#else directive",
        value_display: |f| f.write_str("#else"),
    },
    #[token("#endif")]
    Endif {
        kind_display: "#endif directive",
        value_display: |f| f.write_str("#endif"),
    },

    // this regex is stupidly complex because the skip case
    // overrides this regex if we're not overly specific
    #[regex(r#"[ \v\t\r\f]*\r?\n[ \v\t\r\n\f]*\r?\n[ \v\t\r\f]*"#)]
    BlankLine {
        kind_display: "newline",
        value_display: |f| f.write_char('\n'),
    },
    #[regex(r#";[^\n]*"#, |lex| trim_delimiters(lex.slice(), 1, 0), priority = 1)]
    Comment(Cow<'src, str>) {
        kind_display: "comment",
        value_display: |f, value| write!(f, ";{}", value),
    },
    // These block comment regexes are very particular, for compatibility reasons
    #[regex(r#"(\/\*)+[^\n*]*"#, parse_block_comment)]
    BlockComment(BlockCommentToken<'src>) {
        kind_display: "block comment",
        value_display: |f, value| value.fmt(f),
    },
    // Handled in parse_block_comment
    // #[regex(r#"\*+\/"#)]
    // BlockCommentEnd(Cow<'src, str>) => ("block comment end"),

    #[regex(r#"#[^ \v\t\r\n\f\(\)\[\]\{\}]+"#, |_lex| DiagnosticKind::BadDirective)]
    Error(DiagnosticKind) {
        kind_display: "token error",
        value_display: |f, value| value.fmt(f),
    },
}

impl<'src> TokenValue<'src> {
    pub const fn make_string(text: &'src str) -> Self {
        TokenValue::String(Cow::Borrowed(text))
    }

    pub const fn make_symbol(text: &'src str) -> Self {
        TokenValue::Symbol(Cow::Borrowed(text))
    }

    pub const fn make_variable(text: &'src str) -> Self {
        TokenValue::Variable(Cow::Borrowed(text))
    }

    pub const fn make_comment(text: &'src str) -> Self {
        TokenValue::Comment(Cow::Borrowed(text))
    }
}

type Lexer<'src> = logos::Lexer<'src, TokenValue<'src>>;

fn trim_delimiters(text: &str, before: usize, after: usize) -> Result<Cow<'_, str>, DiagnosticKind> {
    let trim_range = before..text.len() - after;
    text.get(trim_range.clone())
        .map(Cow::Borrowed)
        .ok_or(DiagnosticKind::TrimDelimiterError { trim_range, actual_length: text.len() })
}

fn parse_hex(lex: &mut Lexer<'_>) -> Result<IntegerValue, DiagnosticKind> {
    let trimmed = trim_delimiters(lex.slice(), 2, 0)?;
    u64::from_str_radix(&trimmed, 16)
        .map(|v| v as IntegerValue)
        .map_err(DiagnosticKind::IntegerParseError)
}

fn parse_float(lex: &mut Lexer<'_>) -> Result<FloatValue, std::num::ParseFloatError> {
    let text = lex.slice();
    match text.parse::<FloatValue>() {
        Ok(value) => Ok(value),
        Err(err) => match text {
            // Some weird cases that the original lexer accepts as valid
            text if text.starts_with(".") => Ok(0.0),
            text if text.starts_with("+.") => Ok(0.0),
            text if text.starts_with("-.") => Ok(-0.0),
            _ => Err(err),
        },
    }
}

fn parse_symbol<'src>(
    lex: &mut Lexer<'src>,
    before: usize,
    after: usize,
) -> Result<Cow<'src, str>, DiagnosticKind> {
    let trimmed = trim_delimiters(lex.slice(), before, after)?;
    if trimmed.len() > crate::MAX_SYMBOL_LENGTH {
        return Err(DiagnosticKind::SymbolTooLong);
    }

    Ok(trimmed)
}

fn parse_block_comment<'src>(lex: &mut Lexer<'src>) -> Result<BlockCommentToken<'src>, DiagnosticKind> {
    let matched_range = lex.span();
    let close_offset = matched_range.end - matched_range.start;

    let open_range = regex!(r#"(\/\*)+"#)
        .find(lex.slice())
        .map(|m| m.range())
        .expect("token can't match without matching this pattern");

    let Some(close_range) = regex!(r#"\*+\/"#).find(lex.remainder()).map(|m| m.range()) else {
        lex.bump(lex.remainder().len());
        return Err(DiagnosticKind::UnclosedBlockComment);
    };
    lex.bump(close_range.end);

    let full_text = lex.slice();
    let range_start = lex.span().start;

    let close_range = (close_range.start + close_offset)..(close_range.end + close_offset);
    let body_range = open_range.end..close_range.start;

    Ok(BlockCommentToken::new(
        range_start,
        &full_text[open_range.clone()],
        &full_text[body_range.clone()],
        &full_text[close_range.clone()],
    ))
}

pub struct Tokenizer<'src> {
    text: &'src str,
    lexer: SpannedIter<'src, TokenValue<'src>>,
    current: Option<Token<'src>>,
}

impl<'src> Tokenizer<'src> {
    pub fn new(text: &'src str) -> Self {
        let mut lexer = TokenValue::lexer(text).spanned();
        let current = lexer.next().map(Self::map_token);
        Self { text, lexer, current }
    }

    pub fn source_text(&self) -> &'src str {
        self.text
    }

    pub fn peek(&self) -> Option<&Token<'src>> {
        self.current.as_ref()
    }

    fn map_token((value, location): (Result<TokenValue<'src>, DiagnosticKind>, Span)) -> Token<'src> {
        match value {
            Ok(value) => Token { value, location },
            Err(error) => Token { value: TokenValue::Error(error), location },
        }
    }
}

impl<'src> Iterator for Tokenizer<'src> {
    type Item = Token<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.lexer.next().map(Self::map_token);
        std::mem::replace(&mut self.current, next)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_token(text: &str, kind: TokenValue<'_>, location: Span) {
        let expected = Token { value: kind, location };
        let actual = Tokenizer::new(text).collect::<Vec<_>>();
        assert_eq!(actual, vec![expected], "Unexpected token result for '{text}'");
    }

    fn assert_tokens(text: &str, expected: Vec<(TokenValue<'_>, Span)>) {
        let expected = Vec::from_iter(expected.into_iter().map(|t| Token { value: t.0, location: t.1 }));
        let actual = Tokenizer::new(text).collect::<Vec<_>>();
        assert_eq!(actual, expected);
    }

    #[test]
    fn integer() {
        assert_token("64", TokenValue::Integer(64), 0..2);
        assert_token("1234567890", TokenValue::Integer(1234567890), 0..10);
        assert_token("0xABCD", TokenValue::Integer(0xABCD), 0..6);

        assert_token(
            "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
            TokenValue::Error(DiagnosticKind::IntegerParseError(
                IntegerValue::from_str_radix("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16).unwrap_err(),
            )),
            0..34,
        );
    }

    #[test]
    fn float() {
        assert_token("12.0", TokenValue::Float(12.0), 0..4);
        assert_token("12.", TokenValue::Float(12.0), 0..3);
        assert_token(".12", TokenValue::Float(0.12), 0..3);

        assert_token(".", TokenValue::Float(0.0), 0..1);
        assert_token("+.", TokenValue::Float(0.0), 0..2);
        assert_token("-.", TokenValue::Float(-0.0), 0..2);
    }

    #[test]
    fn string() {
        assert_token("\"text\"", TokenValue::make_string("text"), 0..6);

        assert_token("\"64\"", TokenValue::make_string("64"), 0..4);
        assert_token("\"12.0\"", TokenValue::make_string("12.0"), 0..6);

        assert_token("\"'text'\"", TokenValue::make_string("'text'"), 0..8);
        assert_token("\"$text\"", TokenValue::make_string("$text"), 0..7);
        assert_token("\"kDataUnhandled\"", TokenValue::make_string("kDataUnhandled"), 0..16);

        assert_token("\"(\"", TokenValue::make_string("("), 0..3);
        assert_token("\")\"", TokenValue::make_string(")"), 0..3);
        assert_token("\"{\"", TokenValue::make_string("{"), 0..3);
        assert_token("\"}\"", TokenValue::make_string("}"), 0..3);
        assert_token("\"[\"", TokenValue::make_string("["), 0..3);
        assert_token("\"]\"", TokenValue::make_string("]"), 0..3);

        assert_token("\"#define\"", TokenValue::make_string("#define"), 0..9);
        assert_token("\"#undef\"", TokenValue::make_string("#undef"), 0..8);
        assert_token("\"#include\"", TokenValue::make_string("#include"), 0..10);
        assert_token("\"#include_opt\"", TokenValue::make_string("#include_opt"), 0..14);
        assert_token("\"#merge\"", TokenValue::make_string("#merge"), 0..8);
        assert_token("\"#autorun\"", TokenValue::make_string("#autorun"), 0..10);
        assert_token("\"#ifdef\"", TokenValue::make_string("#ifdef"), 0..8);
        assert_token("\"#ifndef\"", TokenValue::make_string("#ifndef"), 0..9);
        assert_token("\"#else\"", TokenValue::make_string("#else"), 0..7);
        assert_token("\"#endif\"", TokenValue::make_string("#endif"), 0..8);
        assert_token("\"#bad\"", TokenValue::make_string("#bad"), 0..6);

        assert_token("\"\n\"", TokenValue::make_string("\n"), 0..3);
        assert_token("\"; a comment\"", TokenValue::make_string("; a comment"), 0..13);
        assert_token("\"/* a comment */\"", TokenValue::make_string("/* a comment */"), 0..17);
    }

    #[test]
    fn symbol() {
        assert_token("text", TokenValue::make_symbol("text"), 0..4);

        assert_token("+", TokenValue::make_symbol("+"), 0..1);
        assert_token("-", TokenValue::make_symbol("-"), 0..1);
        assert_token("*", TokenValue::make_symbol("*"), 0..1);
        assert_token("/", TokenValue::make_symbol("/"), 0..1);
        assert_token("%", TokenValue::make_symbol("%"), 0..1);
        assert_token("_", TokenValue::make_symbol("_"), 0..1);

        for char in 'a'..'z' {
            let str = char.to_string();
            assert_token(&str, TokenValue::make_symbol(&str), 0..1);

            for char2 in 'a'..'z' {
                let str = char.to_string() + &char2.to_string();
                assert_token(&str, TokenValue::make_symbol(&str), 0..2);
            }
        }
    }

    #[test]
    fn variable() {
        assert_token("$text", TokenValue::make_variable("text"), 0..5);

        assert_token("$+", TokenValue::make_variable("+"), 0..2);
        assert_token("$-", TokenValue::make_variable("-"), 0..2);
        assert_token("$*", TokenValue::make_variable("*"), 0..2);
        assert_token("$/", TokenValue::make_variable("/"), 0..2);
        assert_token("$%", TokenValue::make_variable("%"), 0..2);
        assert_token("$_", TokenValue::make_variable("_"), 0..2);

        for char in 'a'..'z' {
            let str = char.to_string();
            assert_token(&("$".to_owned() + &str), TokenValue::make_variable(&str), 0..2);

            for char2 in 'a'..'z' {
                let str = char.to_string() + &char2.to_string();
                assert_token(&("$".to_owned() + &str), TokenValue::make_variable(&str), 0..3);
            }
        }
    }

    #[test]
    fn unhandled() {
        assert_token("kDataUnhandled", TokenValue::Unhandled, 0..14);
    }

    #[test]
    fn arrays() {
        assert_token("(", TokenValue::ArrayOpen, 0..1);
        assert_token(")", TokenValue::ArrayClose, 0..1);
        assert_token("{", TokenValue::CommandOpen, 0..1);
        assert_token("}", TokenValue::CommandClose, 0..1);
        assert_token("[", TokenValue::PropertyOpen, 0..1);
        assert_token("]", TokenValue::PropertyClose, 0..1);
    }

    #[test]
    fn directives() {
        assert_token("#define", TokenValue::Define, 0..7);
        assert_token("#undef", TokenValue::Undefine, 0..6);
        assert_token("#include", TokenValue::Include, 0..8);
        assert_token("#include_opt", TokenValue::IncludeOptional, 0..12);
        assert_token("#merge", TokenValue::Merge, 0..6);
        assert_token("#autorun", TokenValue::Autorun, 0..8);
        assert_token("#ifdef", TokenValue::Ifdef, 0..6);
        assert_token("#ifndef", TokenValue::Ifndef, 0..7);
        assert_token("#else", TokenValue::Else, 0..5);
        assert_token("#endif", TokenValue::Endif, 0..6);
        assert_token("#bad", TokenValue::Error(DiagnosticKind::BadDirective), 0..4);
    }

    #[test]
    fn comments() {
        fn new_block_comment<'src>(
            start: usize,
            open: &'src str,
            body: &'src str,
            close: &'src str,
        ) -> TokenValue<'src> {
            TokenValue::BlockComment(BlockCommentToken::new(start, open, body, close))
        }

        assert_token("; comment", TokenValue::make_comment(" comment"), 0..9);
        assert_token(";comment", TokenValue::make_comment("comment"), 0..8);
        assert_token("/* comment */", new_block_comment(0, "/*", " comment ", "*/"), 0..13);
        assert_token("/*comment */", new_block_comment(0, "/*", "comment ", "*/"), 0..12);
        assert_token("/* comment*/", new_block_comment(0, "/*", " comment", "*/"), 0..12);
        assert_token(
            "/*\
           \nmulti\
           \nline\
           \ncomment\
           \n*/",
            new_block_comment(
                0,
                "/*",
                "\
               \nmulti\
               \nline\
               \ncomment\
               \n",
                "*/",
            ),
            0..24,
        );

        // These get parsed as symbols in the original lexer
        assert_token("a;symbol", TokenValue::make_symbol("a;symbol"), 0..8);
        assert_token("/**", TokenValue::make_symbol("/**"), 0..3);
        assert_token("/**/", TokenValue::make_symbol("/**/"), 0..4);
        assert_token("/*****", TokenValue::make_symbol("/*****"), 0..6);
        assert_token("/*****/", TokenValue::make_symbol("/*****/"), 0..7);
        assert_token("/*comment*/", TokenValue::make_symbol("/*comment*/"), 0..11);

        // Block comment requirements are weird
        assert_token(
            "/*/*/* comment */",
            new_block_comment(0, "/*/*/*", " comment ", "*/"),
            0..17,
        );
        assert_tokens("/**/** comment */", vec![
            (TokenValue::make_symbol("/**/**"), 0..6),
            (TokenValue::make_symbol("comment"), 7..14),
            (TokenValue::make_symbol("*/"), 15..17),
        ]);
        assert_tokens("/***** comment */", vec![
            (TokenValue::make_symbol("/*****"), 0..6),
            (TokenValue::make_symbol("comment"), 7..14),
            (TokenValue::make_symbol("*/"), 15..17),
        ]);
        assert_token(
            "/* comment *****/",
            new_block_comment(0, "/*", " comment ", "*****/"),
            0..17,
        );
        assert_tokens("/* comment **/**/", vec![
            (new_block_comment(0, "/*", " comment ", "**/"), 0..14),
            (TokenValue::make_symbol("**/"), 14..17),
        ]);
        assert_tokens("/* comment */*/*/", vec![
            (new_block_comment(0, "/*", " comment ", "*/"), 0..13),
            (TokenValue::make_symbol("*/*/"), 13..17),
        ]);

        // Ensure block comment ends aren't swallowed up by delimiters for other tokens
        fn assert_block_comment_with_delimiter(delimiter: char) {
            assert_token(
                &format!("/* {delimiter}*/"),
                new_block_comment(0, "/*", &format!(" {delimiter}"), "*/"),
                0..6,
            );
        }

        assert_block_comment_with_delimiter('$');
        assert_block_comment_with_delimiter('\"');
        assert_block_comment_with_delimiter('\'');
        assert_block_comment_with_delimiter(';');
        assert_block_comment_with_delimiter('#');

        assert_token("/*", TokenValue::Error(DiagnosticKind::UnclosedBlockComment), 0..2);
        assert_token("/*/", TokenValue::Error(DiagnosticKind::UnclosedBlockComment), 0..3);
        assert_token("/*/*/*", TokenValue::Error(DiagnosticKind::UnclosedBlockComment), 0..6);
        assert_token(
            "/*a bunch of\ntext",
            TokenValue::Error(DiagnosticKind::UnclosedBlockComment),
            0..17,
        );
    }

    #[test]
    fn whitespace() {
        assert_tokens("\n", vec![]);
        assert_tokens("\r\n", vec![]);
        assert_tokens("\n\t", vec![]);
        assert_tokens(" \x0b\t\r\n\x0c", vec![]); // " \v\t\r\n\f"

        assert_token("\n\n", TokenValue::BlankLine, 0..2);
        assert_token("\n\n\n\n\n\n", TokenValue::BlankLine, 0..6);
        assert_token("\n     \n", TokenValue::BlankLine, 0..7);
        assert_token("\n\t\n", TokenValue::BlankLine, 0..3);
        assert_token("\r\n\r\r\n", TokenValue::BlankLine, 0..5);

        assert_tokens("(symbol\n\n)", vec![
            (TokenValue::ArrayOpen, 0..1),
            (TokenValue::make_symbol("symbol"), 1..7),
            (TokenValue::BlankLine, 7..9),
            (TokenValue::ArrayClose, 9..10),
        ]);
        assert_tokens(
            "(foo\
           \n\
           \n   (bar 50)\
           \n\
           \n   (quz 100) \
           \n\
           \n)",
            vec![
                (TokenValue::ArrayOpen, 0..1),
                (TokenValue::make_symbol("foo"), 1..4),
                (TokenValue::BlankLine, 4..9),
                (TokenValue::ArrayOpen, 9..10),
                (TokenValue::make_symbol("bar"), 10..13),
                (TokenValue::Integer(50), 14..16),
                (TokenValue::ArrayClose, 16..17),
                (TokenValue::BlankLine, 17..22),
                (TokenValue::ArrayOpen, 22..23),
                (TokenValue::make_symbol("quz"), 23..26),
                (TokenValue::Integer(100), 27..30),
                (TokenValue::ArrayClose, 30..31),
                (TokenValue::BlankLine, 31..34),
                (TokenValue::ArrayClose, 34..35),
            ],
        );
        assert_tokens(
            "#ifdef kDefine\
           \n\
           \n(foo 50)\
           \n\
           \n(bar 100)\
           \n\
           \n#endif",
            vec![
                (TokenValue::Ifdef, 0..6),
                (TokenValue::make_symbol("kDefine"), 7..14),
                (TokenValue::BlankLine, 14..16),
                (TokenValue::ArrayOpen, 16..17),
                (TokenValue::make_symbol("foo"), 17..20),
                (TokenValue::Integer(50), 21..23),
                (TokenValue::ArrayClose, 23..24),
                (TokenValue::BlankLine, 24..26),
                (TokenValue::ArrayOpen, 26..27),
                (TokenValue::make_symbol("bar"), 27..30),
                (TokenValue::Integer(100), 31..34),
                (TokenValue::ArrayClose, 34..35),
                (TokenValue::BlankLine, 35..37),
                (TokenValue::Endif, 37..43),
            ],
        );
    }

    mod display {}
}
