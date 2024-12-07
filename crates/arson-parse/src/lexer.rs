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
    text.get(before..text.len() - after)
        .ok_or(LexError::DelimiterError)
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

    fn assert_tokens(text: &str, expected: Vec<TokenValue<'_>>) {
        let expected = Vec::from_iter(expected.into_iter().map(|t| Ok(t)));
        let actual = Vec::from_iter(TokenValue::lexer(text));
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
        assert_tokens(
            "/* comment */",
            vec![
                TokenValue::BlockCommentStart("/* comment "),
                TokenValue::BlockCommentEnd("*/"),
            ],
        );

        // These get parsed as symbols in the original lexer
        assert_token("a;symbol", TokenValue::Symbol("a;symbol"), 0..8);
        assert_token("/**/", TokenValue::Symbol("/**/"), 0..4);
        assert_token("/*****/", TokenValue::Symbol("/*****/"), 0..7);
        assert_token("/*comment*/", TokenValue::Symbol("/*comment*/"), 0..11);
    }

    #[test]
    fn whitespace() {
        assert_eq!(Vec::from_iter(TokenValue::lexer("\n")), []);
        assert_eq!(Vec::from_iter(TokenValue::lexer("\r\n")), []);
        assert_eq!(Vec::from_iter(TokenValue::lexer("\n\t")), []);
        assert_eq!(Vec::from_iter(TokenValue::lexer(" \x0b\t\r\n\x0c")), []); // " \v\t\r\n\f"
    }

    #[test]
    fn thorough() {
        use TokenValue::*;

        let text = include_str!("./test_files/thorough.dta").replace("\r\n", "\n");

        #[rustfmt::skip]
        let tokens = vec![
            // integers
            Comment,
            Integer(1), Integer(2), Integer(-3),
            Integer(1), Integer(2), Integer(-3),

            // hex numbers
            Comment,
            Integer(0x1), Integer(0xA), Integer(0xa),
            Integer(0xFFFFFFFF),
            Integer(0xFFFFFFFFFFFFFFFFu64 as IntegerValue),
            // invalid (lexed as symbols)
            Comment,
            Symbol("0x"), Symbol("x1"),
            Symbol("+0x2"), Symbol("-0x3"),
            Symbol("+0xB"), Symbol("-0xC"),
            Symbol("+0xb"), Symbol("-0xc"),

            // floats
            Comment,
            Float(1.0), Float(2.0), Float(-3.0),
            Float(1.0), Float(2.0), Float(-3.0),
            Float(0.1), Float(0.2), Float(-0.3),
            // these are valid
            Comment,
            Float(0.0), Float(0.0), Float(-0.0),

            // floats with exponents
            Comment,
            // valid                                          -  invalid
            Comment,
            Float(1.0E1),  Float(2.0E1),  Float(-3.0E1),      Symbol("1.0-E1"),  Symbol("+2.0-E1"),  Symbol("-3.0-E1"), 
            Float(1.0E+1), Float(2.0E+1), Float(-3.0E+1),     Symbol("1.0-E+1"), Symbol("+2.0-E+1"), Symbol("-3.0-E+1"),
            Float(1.0E-1), Float(2.0E-1), Float(-3.0E-1),     Symbol("1.0-E-1"), Symbol("+2.0-E-1"), Symbol("-3.0-E-1"),
           
            Float(1.0E1),  Float(2.0E1),  Float(-3.0E1),      Symbol("1.-E1"),   Symbol("+2.-E1"),   Symbol("-3.-E1"),  
            Float(1.0E+1), Float(2.0E+1), Float(-3.0E+1),     Symbol("1.-E+1"),  Symbol("+2.-E+1"),  Symbol("-3.-E+1"), 
            Float(1.0E-1), Float(2.0E-1), Float(-3.0E-1),     Symbol("1.-E-1"),  Symbol("+2.-E-1"),  Symbol("-3.-E-1"), 
           
            Float(0.1E1),  Float(0.2E1),  Float(-0.3E1),      Symbol(".1-E1"),   Symbol("+.2-E1"),   Symbol("-.3-E1"),  
            Float(0.1E+1), Float(0.2E+1), Float(-0.3E+1),     Symbol(".1-E+1"),  Symbol("+.2-E+1"),  Symbol("-.3-E+1"), 
            Float(0.1E-1), Float(0.2E-1), Float(-0.3E-1),     Symbol(".1-E-1"),  Symbol("+.2-E-1"),  Symbol("-.3-E-1"), 
           
            Float(0.0E1),  Float(0.0E1),  Float(-0.0E1),      Symbol(".-E1"),    Symbol("+.-E1"),    Symbol("-.-E1"),   
            Float(0.0E+1), Float(0.0E+1), Float(-0.0E+1),     Symbol(".-E+1"),   Symbol("+.-E+1"),   Symbol("-.-E+1"),  
            Float(0.0E-1), Float(0.0E-1), Float(-0.0E-1),     Symbol(".-E-1"),   Symbol("+.-E-1"),   Symbol("-.-E-1"),  

            // strings
            Comment,
            String("asdf"),
            String(""), String(""),
           
            String(
                "\n\
                asdf\n\
                jkl\n\
                qwerty\
                \n"
            ),

            // symbols
            Comment,
            Symbol("asdf"),
            Symbol("jkl"),
            Symbol("qwerty"),

            // quoted symbols
            Comment,
            Symbol("asdf"),
            Symbol(""), Symbol(""),
           
            Symbol(
                "\n\
                asdf\n\
                jkl\n\
                qwerty\
                \n"
            ),

            // variables
            Comment,
            Variable("asdf"),
            Variable("jkl"),
            Variable("qwerty"),

            // kDataUnhandled is its own token
            Comment,
            Unhandled,

            // arrays
            Comment,
            ArrayOpen, Symbol("array"), Integer(1), Integer(2), ArrayClose, Comment,
            CommandOpen, Symbol("+"), Integer(1), Integer(2), CommandClose, Comment,
            PropertyOpen, Symbol("property"), PropertyClose, Comment,

            // directives
            Comment,
            IncludeOptional, Symbol("../file.dta"),
            Include, Symbol("../file.dta"),
            Merge, Symbol("../file.dta"),
            Ifdef, Symbol("kDefine"),
            Undefine, Symbol("kDefine"),
            Endif,
            Ifndef, Symbol("kDefine"),
            Define, Symbol("kDefine"),
            Else,
            Autorun, CommandOpen, Symbol("action"), CommandClose,
            Endif,
            // invalid
            Comment,
            BadDirective("bad"),
            BadDirective("#"),

            // *not* directives, these are lexed as symbols
            Comment,
            Symbol("#"),
            Symbol("#"), Symbol("#"), Comment,
            Symbol("#"), Symbol("#"), Comment,
            // lexed as symbols and arrays
            Comment,
            Symbol("#"), ArrayOpen, Symbol("#"), ArrayClose, Comment,
            Symbol("#"), CommandOpen, Symbol("#"), CommandClose, Comment,
            Symbol("#"), PropertyOpen, Symbol("#"), PropertyClose, Comment,

            // line comment
            Comment,
            Comment, // ;;
            Comment, // ; ;
            Comment, // ;	;
            Comment, // ;;;;;;;;
            Comment, // ;nospace
            Symbol("asdf;jkl"), Comment,

            // block comment
            BlockCommentStart("/*"),
            Symbol("block"), Symbol("comment"),
            BlockCommentEnd("*/"),

            Symbol("/*asdf*/"), Comment,
            BlockCommentStart("/*jkl "), BlockCommentEnd("*/"),
           
            Symbol("/**/"), Comment,
            BlockCommentStart("/* "), BlockCommentEnd("*/"),
            BlockCommentStart("/*\t"), BlockCommentEnd("*/"),

            // stray block-comment close, lexed as a symbol
            // note: handled by the parser
            Comment,
            BlockCommentEnd("*/"),
            Symbol("/*****/"), Comment,
            Symbol("/*****"), Comment,
            BlockCommentStart("/*"),
            Symbol("*****"),
            BlockCommentEnd("***/"),
        ];

        assert_tokens(&text, tokens);
    }
}
