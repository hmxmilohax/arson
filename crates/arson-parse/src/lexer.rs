// SPDX-License-Identifier: LGPL-3.0-or-later

use std::num::{ParseFloatError, ParseIntError};

use arson_core::{FloatValue, IntegerValue};
use logos::{Logos, Span};

type Lexer<'src> = logos::Lexer<'src, TokenValue<'src>>;

#[derive(Debug, PartialEq)]
pub struct Token<'src> {
    pub kind: TokenValue<'src>,
    pub location: Span,
}

impl Token<'_> {
    pub fn to_owned(&self) -> OwnedToken {
        OwnedToken {
            kind: self.kind.to_owned(),
            location: self.location.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct OwnedToken {
    pub kind: OwnedTokenValue,
    pub location: Span,
}

impl OwnedToken {
    pub const fn new(kind: OwnedTokenValue, location: Span) -> OwnedToken {
        OwnedToken { kind, location }
    }
}

macro_rules! to_owned_type {
    (str) => {
        String
    };
    ($type:tt) => {
        $type
    };
}

/// wizardry
///
/// Type(value_type) => ("display string", "{value_format}")
macro_rules! make_tokens {
    (
        $(
            $(#[$attr:meta])*
            $type:ident$(($value:ident $(: $life:lifetime)?))?
                => ($display:literal $(, $format:literal)?)
            $(,)?
        )+
    ) => {
        #[derive(Logos, Debug, PartialEq)]
        #[logos(error = LexError)]
        #[logos(skip r#"[ \v\t\r\n\f]+"#)]
        pub enum TokenValue<'src> {
            $($(#[$attr])* $type$(($(& $life)? $value))?,)+
        }

        impl<'src> TokenValue<'src> {
            pub fn get_type(&self) -> TokenKind {
                match self {
                    $(TokenValue::$type$((meta_morph!($value => _)))? => TokenKind::$type,)+
                }
            }

            pub fn to_owned(&self) -> OwnedTokenValue {
                match self {
                    $(TokenValue::$type$((meta_morph!($value => ref value)))?
                        => OwnedTokenValue::$type$(
                            (meta_morph!($value => ((*value).to_owned())))
                        )?,
                    )+
                }
            }
        }

        impl<'src> std::fmt::Display for TokenValue<'src> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(TokenValue::$type$((meta_morph!($value => _value)))?
                        => write!(f, concat!($display, $($format)?) $(,meta_morph!($format => _value))?),
                    )+
                }
            }
        }

        #[derive(Debug, Clone, PartialEq)]
        pub enum OwnedTokenValue {
            $($type$((to_owned_type!($value)))?,)+
        }

        impl OwnedTokenValue {
            pub fn get_type(&self) -> TokenKind {
                match self {
                    $(OwnedTokenValue::$type$((meta_morph!($value => _)))? => TokenKind::$type,)+
                }
            }
        }

        impl std::fmt::Display for OwnedTokenValue {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(OwnedTokenValue::$type$((meta_morph!($value => _value)))?
                        => write!(f, concat!($display, $($format)?) $(,meta_morph!($format => _value))?),
                    )+
                }
            }
        }

        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
        pub enum TokenKind {
            $($type,)+
        }

        impl<'src> std::fmt::Display for TokenKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(TokenKind::$type => write!(f, $display),)+
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
    Integer(IntegerValue) => ("integer", "{}"),
    // This one allows some weird things, such as ".", "+.", and "-.E1"
    #[regex(r#"[+-]?[0-9]*\.[0-9]*([Ee][+-]?[0-9])?"#, parse_float, priority = 2)]
    Float(FloatValue) => ("float", "{}"),
    #[regex(r#""[^"]*""#, |lex| trim_delimiters(lex.slice(), 1, 1))]
    String(str: 'src) => ("string", "\"{}\""),

    // Symbol consumes almost all input which doesn't match any other token,
    // including technically malformed versions of integers/floats
    #[regex(r#"[^ \v\t\r\n\f\(\)\[\]\{\}]+"#, priority = 0)]
    #[regex(r#"'[^']*'"#, |lex| trim_delimiters(lex.slice(), 1, 1))]
    Symbol(str: 'src) => ("symbol", "'{}'"),
    #[regex(r#"\$[^ \v\t\r\n\f\(\)\[\]\{\}]+"#, |lex| trim_delimiters(lex.slice(), 1, 0))]
    Variable(str: 'src) => ("variable", "${}"),
    #[token("kDataUnhandled")]
    Unhandled => ("kDataUnhandled"),

    #[token("(")]
    ArrayOpen => ("array open"),
    #[token(")")]
    ArrayClose => ("array close"),
    #[token("{")]
    CommandOpen => ("command open"),
    #[token("}")]
    CommandClose => ("command close"),
    #[token("[")]
    PropertyOpen => ("property open"),
    #[token("]")]
    PropertyClose => ("property close"),

    #[token("#define")]
    Define => ("#define directive"),
    #[token("#undef")]
    Undefine => ("#undef directive"),
    #[token("#include")]
    Include => ("#include directive"),
    #[token("#include_opt")]
    IncludeOptional => ("#include_opt directive"),
    #[token("#merge")]
    Merge => ("#merge directive"),
    #[token("#autorun")]
    Autorun => ("#autorun directive"),

    #[token("#ifdef")]
    Ifdef => ("#ifdef directive"),
    #[token("#ifndef")]
    Ifndef => ("#ifndef directive"),
    #[token("#else")]
    Else => ("#else directive"),
    #[token("#endif")]
    Endif => ("#endif directive"),

    #[regex(r#"#[^ \v\t\r\n\f\(\)\[\]\{\}]+"#, |lex| trim_delimiters(lex.slice(), 1, 0))]
    BadDirective(str: 'src) => ("invalid directive", "#{}"),

    #[regex(r#";[^\n]*"#, priority = 1)]
    Comment => ("comment"),
    // These block comment regexes are very particular, for compatibility reasons
    #[regex(r#"(\/\*)+[^\n*]*"#)]
    BlockCommentStart(str: 'src) => ("block comment start"),
    #[regex(r#"\*+\/"#)]
    BlockCommentEnd(str: 'src) => ("block comment end"),

    Error(LexError) => ("token error", ": {}"),
}

#[derive(thiserror::Error, Debug, Clone, Default, PartialEq)]
pub enum LexError {
    #[default]
    #[error("Invalid token")]
    InvalidToken,

    #[error("Integer parse error: {0}")]
    IntegerError(#[from] ParseIntError),
    #[error("Float parse error: {0}")]
    FloatError(#[from] ParseFloatError),
    #[error("Failed to remove token delimiters")]
    DelimiterError,
}

fn trim_delimiters(text: &str, before: usize, after: usize) -> Result<&str, LexError> {
    text.get(before..text.len() - after).ok_or(LexError::DelimiterError)
}

fn parse_hex(lex: &mut Lexer<'_>) -> Result<IntegerValue, LexError> {
    let trimmed = trim_delimiters(lex.slice(), 2, 0)?;
    u64::from_str_radix(trimmed, 16)
        .map(|v| v as IntegerValue)
        .map_err(LexError::IntegerError)
}

fn parse_float(lex: &mut Lexer<'_>) -> Result<FloatValue, ParseFloatError> {
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

pub fn lex_text(text: &str) -> impl Iterator<Item = Token<'_>> {
    TokenValue::lexer(text).spanned().map(|t| match t {
        (Ok(kind), location) => Token { kind, location },
        (Err(error), location) => Token { kind: TokenValue::Error(error), location },
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_token(text: &str, kind: TokenValue<'_>, location: Span) {
        let expected = Token { kind, location };
        let tokens = lex_text(text).collect::<Vec<_>>();
        assert_eq!(tokens, vec![expected], "Unexpected token result for '{text}'");
    }

    fn assert_tokens(text: &str, expected: Vec<(TokenValue<'_>, Span)>) {
        let expected = Vec::from_iter(expected.into_iter().map(|t| (Ok(t.0), t.1)));
        let actual = Vec::from_iter(TokenValue::lexer(text).spanned());
        assert_eq!(actual, expected);
    }

    #[test]
    fn integer() {
        assert_token("64", TokenValue::Integer(64), 0..2);
        assert_token("1234567890", TokenValue::Integer(1234567890), 0..10);
        assert_token("0xABCD", TokenValue::Integer(0xABCD), 0..6);
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
        assert_token("\"text\"", TokenValue::String("text"), 0..6);

        assert_token("\"64\"", TokenValue::String("64"), 0..4);
        assert_token("\"12.0\"", TokenValue::String("12.0"), 0..6);

        assert_token("\"'text'\"", TokenValue::String("'text'"), 0..8);
        assert_token("\"$text\"", TokenValue::String("$text"), 0..7);
        assert_token("\"kDataUnhandled\"", TokenValue::String("kDataUnhandled"), 0..16);

        assert_token("\"(\"", TokenValue::String("("), 0..3);
        assert_token("\")\"", TokenValue::String(")"), 0..3);
        assert_token("\"{\"", TokenValue::String("{"), 0..3);
        assert_token("\"}\"", TokenValue::String("}"), 0..3);
        assert_token("\"[\"", TokenValue::String("["), 0..3);
        assert_token("\"]\"", TokenValue::String("]"), 0..3);

        assert_token("\"#define\"", TokenValue::String("#define"), 0..9);
        assert_token("\"#undef\"", TokenValue::String("#undef"), 0..8);
        assert_token("\"#include\"", TokenValue::String("#include"), 0..10);
        assert_token("\"#include_opt\"", TokenValue::String("#include_opt"), 0..14);
        assert_token("\"#merge\"", TokenValue::String("#merge"), 0..8);
        assert_token("\"#autorun\"", TokenValue::String("#autorun"), 0..10);
        assert_token("\"#ifdef\"", TokenValue::String("#ifdef"), 0..8);
        assert_token("\"#ifndef\"", TokenValue::String("#ifndef"), 0..9);
        assert_token("\"#else\"", TokenValue::String("#else"), 0..7);
        assert_token("\"#endif\"", TokenValue::String("#endif"), 0..8);
        assert_token("\"#bad\"", TokenValue::String("#bad"), 0..6);

        assert_token("\"\n\"", TokenValue::String("\n"), 0..3);
        assert_token("\"; a comment\"", TokenValue::String("; a comment"), 0..13);
        assert_token("\"/* a comment */\"", TokenValue::String("/* a comment */"), 0..17);
    }

    #[test]
    fn symbol() {
        assert_token("text", TokenValue::Symbol("text"), 0..4);

        assert_token("+", TokenValue::Symbol("+"), 0..1);
        assert_token("-", TokenValue::Symbol("-"), 0..1);
        assert_token("*", TokenValue::Symbol("*"), 0..1);
        assert_token("/", TokenValue::Symbol("/"), 0..1);
        assert_token("%", TokenValue::Symbol("%"), 0..1);
        assert_token("_", TokenValue::Symbol("_"), 0..1);

        for char in 'a'..'z' {
            let str = char.to_string();
            assert_token(&str, TokenValue::Symbol(&str), 0..1);

            for char2 in 'a'..'z' {
                let str = char.to_string() + &char2.to_string();
                assert_token(&str, TokenValue::Symbol(&str), 0..2);
            }
        }
    }

    #[test]
    fn variable() {
        assert_token("$text", TokenValue::Variable("text"), 0..5);

        assert_token("$+", TokenValue::Variable("+"), 0..2);
        assert_token("$-", TokenValue::Variable("-"), 0..2);
        assert_token("$*", TokenValue::Variable("*"), 0..2);
        assert_token("$/", TokenValue::Variable("/"), 0..2);
        assert_token("$%", TokenValue::Variable("%"), 0..2);
        assert_token("$_", TokenValue::Variable("_"), 0..2);

        for char in 'a'..'z' {
            let str = char.to_string();
            assert_token(&("$".to_owned() + &str), TokenValue::Variable(&str), 0..2);

            for char2 in 'a'..'z' {
                let str = char.to_string() + &char2.to_string();
                assert_token(&("$".to_owned() + &str), TokenValue::Variable(&str), 0..3);
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
        assert_token("#bad", TokenValue::BadDirective("bad"), 0..4);
    }

    #[test]
    fn comments() {
        assert_token("; comment", TokenValue::Comment, 0..9);
        assert_token(";comment", TokenValue::Comment, 0..8);
        assert_tokens("/* comment */", vec![
            (TokenValue::BlockCommentStart("/* comment "), 0..11),
            (TokenValue::BlockCommentEnd("*/"), 11..13),
        ]);

        // These get parsed as symbols in the original lexer
        assert_token("a;symbol", TokenValue::Symbol("a;symbol"), 0..8);
        assert_token("/**/", TokenValue::Symbol("/**/"), 0..4);
        assert_token("/*****/", TokenValue::Symbol("/*****/"), 0..7);
        assert_token("/*comment*/", TokenValue::Symbol("/*comment*/"), 0..11);
    }

    #[test]
    fn whitespace() {
        assert_tokens("\n", vec![]);
        assert_tokens("\r\n", vec![]);
        assert_tokens("\n\t", vec![]);
        assert_tokens(" \x0b\t\r\n\x0c", vec![]); // " \v\t\r\n\f"
    }

    #[test]
    fn thorough() {
        use TokenValue::*;

        let text = include_str!("./test_files/thorough.dta").replace("\r\n", "\n");

        // Format for this behemoth:
        // - each line is its own inner vec
        // - each token is stored as the value + location
        // - token location is relative to the current line, not the whole file
        #[rustfmt::skip] // structure/formatting matches the test file
        let tokens = vec![
            // integers
            (1, vec![(Comment, 0..10)]),
            (2, vec![(Integer(1), 0..1), (Integer(2), 2..4), (Integer(-3), 5..7)]),
            (3, vec![(Integer(1), 0..2), (Integer(2), 3..6), (Integer(-3), 7..10)]),

            // hex numbers
            (5, vec![(Comment, 0..13)]),
            (6, vec![(Integer(0x1), 0..3), (Integer(0xA), 4..7), (Integer(0xa), 8..11)]),
            (7, vec![(Integer(0xFFFFFFFF), 0..10)]),
            (8, vec![(Integer(0xFFFFFFFFFFFFFFFFu64 as IntegerValue), 0..18)]),
            // invalid (lexed as symbols)
            (9, vec![(Comment, 0..28)]),
            (10, vec![(Symbol("0x"), 0..2), (Symbol("x1"), 5..7)]),
            (11, vec![(Symbol("+0x2"), 0..4), (Symbol("-0x3"), 5..9)]),
            (12, vec![(Symbol("+0xB"), 0..4), (Symbol("-0xC"), 5..9)]),
            (13, vec![(Symbol("+0xb"), 0..4), (Symbol("-0xc"), 5..9)]),

            // floats
            (15, vec![(Comment, 0..8)]),
            (16, vec![(Float(1.0), 0..3), (Float(2.0), 7..11), (Float(-3.0), 15..19)]),
            (17, vec![(Float(1.0), 0..2), (Float(2.0), 7..10), (Float(-3.0), 15..18)]),
            (18, vec![(Float(0.1), 0..2), (Float(0.2), 7..10), (Float(-0.3), 15..18)]),
            // these are valid
            (19, vec![(Comment, 0..17)]),
            (20, vec![(Float(0.0), 0..1), (Float(0.0), 7..9), (Float(-0.0), 15..17)]),

            // floats with exponents
            (22, vec![(Comment, 0..23)]),
            // valid                                          -  invalid
            (23, vec![(Comment, 0..34)]),
            (24, vec![(Float(1.0E1), 0..5),  (Float(2.0E1), 7..13),  (Float(-3.0E1), 15..21),      (Symbol("1.0-E1"), 27..33),  (Symbol("+2.0-E1"), 35..42),  (Symbol("-3.0-E1"), 44..51)]),
            (25, vec![(Float(1.0E+1), 0..6), (Float(2.0E+1), 7..14), (Float(-3.0E+1), 15..22),     (Symbol("1.0-E+1"), 27..34), (Symbol("+2.0-E+1"), 35..43), (Symbol("-3.0-E+1"), 44..52)]),
            (26, vec![(Float(1.0E-1), 0..6), (Float(2.0E-1), 7..14), (Float(-3.0E-1), 15..22),     (Symbol("1.0-E-1"), 27..34), (Symbol("+2.0-E-1"), 35..43), (Symbol("-3.0-E-1"), 44..52)]),

            (28, vec![(Float(1.0E1), 0..4),  (Float(2.0E1), 7..12),  (Float(-3.0E1), 15..20),      (Symbol("1.-E1"), 27..32),   (Symbol("+2.-E1"), 35..41),   (Symbol("-3.-E1"), 44..50)]),
            (29, vec![(Float(1.0E+1), 0..5), (Float(2.0E+1), 7..13), (Float(-3.0E+1), 15..21),     (Symbol("1.-E+1"), 27..33),  (Symbol("+2.-E+1"), 35..42),  (Symbol("-3.-E+1"), 44..51)]),
            (30, vec![(Float(1.0E-1), 0..5), (Float(2.0E-1), 7..13), (Float(-3.0E-1), 15..21),     (Symbol("1.-E-1"), 27..33),  (Symbol("+2.-E-1"), 35..42),  (Symbol("-3.-E-1"), 44..51)]),

            (32, vec![(Float(0.1E1), 0..4),  (Float(0.2E1), 7..12),  (Float(-0.3E1), 15..20),      (Symbol(".1-E1"), 27..32),   (Symbol("+.2-E1"), 35..41),   (Symbol("-.3-E1"), 44..50)]),
            (33, vec![(Float(0.1E+1), 0..5), (Float(0.2E+1), 7..13), (Float(-0.3E+1), 15..21),     (Symbol(".1-E+1"), 27..33),  (Symbol("+.2-E+1"), 35..42),  (Symbol("-.3-E+1"), 44..51)]),
            (34, vec![(Float(0.1E-1), 0..5), (Float(0.2E-1), 7..13), (Float(-0.3E-1), 15..21),     (Symbol(".1-E-1"), 27..33),  (Symbol("+.2-E-1"), 35..42),  (Symbol("-.3-E-1"), 44..51)]),

            (36, vec![(Float(0.0E1), 0..3),  (Float(0.0E1), 7..11),  (Float(-0.0E1), 15..19),      (Symbol(".-E1"), 27..31),    (Symbol("+.-E1"), 35..40),    (Symbol("-.-E1"), 44..49)]),
            (37, vec![(Float(0.0E+1), 0..4), (Float(0.0E+1), 7..12), (Float(-0.0E+1), 15..20),     (Symbol(".-E+1"), 27..32),   (Symbol("+.-E+1"), 35..41),   (Symbol("-.-E+1"), 44..50)]),
            (38, vec![(Float(0.0E-1), 0..4), (Float(0.0E-1), 7..12), (Float(-0.0E-1), 15..20),     (Symbol(".-E-1"), 27..32),   (Symbol("+.-E-1"), 35..41),   (Symbol("-.-E-1"), 44..50)]),

            // strings
            (40, vec![(Comment, 0..9)]),
            (41, vec![(String("asdf"), 0..6)]),
            (42, vec![(String(""), 0..2), (String(""), 3..5)]),

            (44, vec![(String(
                "\n\
                asdf\n\
                jkl\n\
                qwerty\
                \n"
            ), 0..19)]),


            // symbols
            (51, vec![(Comment, 0..9)]),
            (52, vec![(Symbol("asdf"), 0..4)]),
            (53, vec![(Symbol("jkl"), 0..3)]),
            (54, vec![(Symbol("qwerty"), 0..6)]),

            // quoted symbols
            (56, vec![(Comment, 0..16)]),
            (57, vec![(Symbol("asdf"), 0..6)]),
            (58, vec![(Symbol(""), 0..2), (Symbol(""), 3..5)]),

            (60, vec![(Symbol(
                "\n\
                asdf\n\
                jkl\n\
                qwerty\
                \n"
            ), 0..19)]),

            // variables
            (66, vec![(Comment, 0..11)]),
            (67, vec![(Variable("asdf"), 0..5)]),
            (68, vec![(Variable("jkl"), 0..4)]),
            (69, vec![(Variable("qwerty"), 0..7)]),

            // kDataUnhandled is its own token
            (71, vec![(Comment, 0..33)]),
            (72, vec![(Unhandled, 0..14)]),

            // arrays
            (75, vec![(Comment, 0..8)]),
            (76, vec![(ArrayOpen, 0..1), (Symbol("array"), 1..6), (Integer(1), 7..8), (Integer(2), 9..10), (ArrayClose, 10..11), (Comment, 13..20)]),
            (77, vec![(CommandOpen, 0..1), (Symbol("+"), 1..2), (Integer(1), 3..4), (Integer(2), 5..6), (CommandClose, 6..7), (Comment, 13..22)]),
            (78, vec![(PropertyOpen, 0..1), (Symbol("property"), 1..9), (PropertyClose, 9..10), (Comment, 13..23)]),

            // directives
            (81, vec![(Comment, 0..12)]),
            (82, vec![(IncludeOptional, 0..12), (Symbol("../file.dta"), 13..24)]),
            (83, vec![(Include, 0..8), (Symbol("../file.dta"), 9..20)]),
            (84, vec![(Merge, 0..6), (Symbol("../file.dta"), 7..18)]),
            (85, vec![(Ifdef, 0..6), (Symbol("kDefine"), 7..14)]),
            (86, vec![(Undefine, 0..6), (Symbol("kDefine"), 7..14)]),
            (87, vec![(Endif, 0..6)]),
            (88, vec![(Ifndef, 0..7), (Symbol("kDefine"), 8..15)]),
            (89, vec![(Define, 0..7), (Symbol("kDefine"), 8..15)]),
            (90, vec![(Else, 0..5)]),
            (91, vec![(Autorun, 0..8), (CommandOpen, 9..10), (Symbol("action"), 10..16), (CommandClose, 16..17)]),
            (92, vec![(Endif, 0..6)]),
            // invalid
            (93, vec![(Comment, 0..9)]),
            (94, vec![(BadDirective("bad"), 0..4)]),
            (95, vec![(BadDirective("#"), 0..2)]),

            // *not* directives, these are lexed as symbols
            (97, vec![(Comment, 0..46)]),
            (98, vec![(Symbol("#"), 0..1)]),
            (99, vec![(Symbol("#"), 0..1), (Symbol("#"), 2..3), (Comment, 4..21)]),
            (100, vec![(Symbol("#"), 0..1), (Symbol("#"), 2..3), (Comment, 4..19)]),
            // lexed as symbols and arrays
            (101, vec![(Comment, 0..29)]),
            (102, vec![(Symbol("#"), 0..1), (ArrayOpen, 1..2), (Symbol("#"), 2..3), (ArrayClose, 3..4), (Comment, 5..25)]),
            (103, vec![(Symbol("#"), 0..1), (CommandOpen, 1..2), (Symbol("#"), 2..3), (CommandClose, 3..4), (Comment, 5..25)]),
            (104, vec![(Symbol("#"), 0..1), (PropertyOpen, 1..2), (Symbol("#"), 2..3), (PropertyClose, 3..4), (Comment, 5..25)]),

            // line comment
            (107, vec![(Comment, 0..14)]),
            (108, vec![(Comment, 0..2)]), // ;;
            (109, vec![(Comment, 0..3)]), // ; ;
            (110, vec![(Comment, 0..3)]), // ;	;
            (111, vec![(Comment, 0..8)]), // ;;;;;;;;
            (112, vec![(Comment, 0..8)]), // ;nospace
            (113, vec![(Symbol("asdf;jkl"), 0..8), (Comment, 9..47)]),

            // block comment
            (115, vec![(BlockCommentStart("/*"), 0..2)]),
            (116, vec![(Symbol("block"), 0..5), (Symbol("comment"), 6..13)]),
            (117, vec![(BlockCommentEnd("*/"), 0..2)]),

            (119, vec![(Symbol("/*asdf*/"), 0..8), (Comment, 9..37)]),
            (120, vec![(BlockCommentStart("/*jkl "), 0..6), (BlockCommentEnd("*/"), 6..8)]),

            (122, vec![(Symbol("/**/"), 0..4), (Comment, 5..33)]),
            (123, vec![(BlockCommentStart("/* "), 0..3), (BlockCommentEnd("*/"), 3..5)]),
            (124, vec![(BlockCommentStart("/*\t"), 0..3), (BlockCommentEnd("*/"), 3..5)]),

            // stray block-comment close, lexed as a symbol
            // note: handled by the parser
            (126, vec![(Comment, 0..46)]),
            (127, vec![(BlockCommentEnd("*/"), 0..2)]),

            (129, vec![(Symbol("/*****/"), 0..7), (Comment, 8..36)]),

            (131, vec![(Symbol("/*****"), 0..6), (Comment, 7..35)]),
            (133, vec![(BlockCommentStart("/*"), 4..6)]),
            (135, vec![(Symbol("*****"), 4..9)]),
            (137, vec![(BlockCommentEnd("***/"), 0..4)]),
        ];

        // Apply proper locations to tokens
        let tokens = {
            // Collect lines by index and their locations in the original text
            let line_locations = {
                let mut lines = vec![];
                let mut location = 0;
                for line in text.lines() {
                    lines.push(location);
                    location += line.len() + 1; // + 1 for \n
                }

                lines
            };

            let mut adjusted_tokens = vec![];
            for (line_number, token_line) in tokens {
                let location = line_locations[line_number - 1];
                for token in token_line {
                    let new_start = location + token.1.start;
                    let new_end = location + token.1.end;
                    adjusted_tokens.push((token.0, new_start..new_end))
                }
            }

            adjusted_tokens
        };

        assert_tokens(&text, tokens);
    }
}
