// SPDX-License-Identifier: LGPL-3.0-or-later

use std::num::{ParseFloatError, ParseIntError};

use logos::{Logos, Span};

type Lexer<'src> = logos::Lexer<'src, TokenKind<'src>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'src> {
    pub kind: TokenKind<'src>,
    pub location: Span,
}

// Token regexes based on the RB3 decomp.
// Behavior is replicated as closely as possible, all files should tokenize
// identically to the original Flex tokenizer based on current knowledge for it.

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(error = LexError)]
#[logos(skip r#"[ \v\t\r\n\f]+"#)]
pub enum TokenKind<'src> {
    #[regex(r#"[+-]?[0-9]+"#, |lex| lex.slice().parse::<i64>(), priority = 2)]
    #[regex(r#"0x[A-Fa-f0-9]+"#, parse_hex, priority = 2)]
    Integer(i64),
    // This one allows some weird things, such as ".", "+.", and "-.E1"
    #[regex(r#"[+-]?[0-9]*\.[0-9]*([Ee][+-]?[0-9])?"#, parse_float, priority = 2)]
    Float(f64),
    #[regex(r#""[^"]*""#, |lex| trim_delimiters(lex.slice(), 1, 1))]
    String(&'src str),

    // Symbol consumes almost all input which doesn't match any other token,
    // including technically malformed versions of integers/floats
    #[regex(r#"[^ \v\t\r\n\f\(\)\[\]\{\}]+"#, priority = 0)]
    #[regex(r#"'[^']*'"#, |lex| trim_delimiters(lex.slice(), 1, 1))]
    Symbol(&'src str),
    #[regex(r#"\$[^ \v\t\r\n\f\(\)\[\]\{\}]+"#, |lex| trim_delimiters(lex.slice(), 1, 0))]
    Variable(&'src str),
    #[token("kDataUnhandled")]
    Unhandled,

    #[token("(")]
    ArrayOpen,
    #[token(")")]
    ArrayClose,
    #[token("{")]
    CommandOpen,
    #[token("}")]
    CommandClose,
    #[token("[")]
    PropertyOpen,
    #[token("]")]
    PropertyClose,

    #[token("#define")]
    Define,
    #[token("#undef")]
    Undefine,
    #[token("#include")]
    Include,
    #[token("#include_opt")]
    IncludeOptional,
    #[token("#merge")]
    Merge,
    #[token("#autorun")]
    Autorun,

    #[token("#ifdef")]
    Ifdef,
    #[token("#ifndef")]
    Ifndef,
    #[token("#else")]
    Else,
    #[token("#endif")]
    Endif,

    #[regex(r#"#[^ \v\t\r\n\f\(\)\[\]\{\}]+"#, |lex| trim_delimiters(lex.slice(), 1, 0))]
    BadDirective(&'src str),

    #[regex(r#";[^\n]*"#, priority = 1)]
    Comment,
    // These block comment regexes are very particular, for compatibility reasons
    #[regex(r#"(\/\*)+[^\n*]*"#)]
    BlockCommentStart(&'src str),
    #[regex(r#"\*+\/"#)]
    BlockCommentEnd(&'src str),

    Error(LexError),
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

fn parse_hex(lex: &mut Lexer<'_>) -> Result<i64, LexError> {
    let trimmed = trim_delimiters(lex.slice(), 2, 0)?;
    u64::from_str_radix(trimmed, 16)
        .map(|v| v as i64)
        .map_err(LexError::IntegerError)
}

fn parse_float(lex: &mut Lexer<'_>) -> Result<f64, ParseFloatError> {
    let text = lex.slice();
    match text.parse::<f64>() {
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

pub fn lex(text: &str) -> impl Iterator<Item = Token<'_>> {
    TokenKind::lexer(text)
        .spanned()
        .map(|t| match t {
            (Ok(kind), location) => Token { kind, location },
            (Err(error), location) => Token { kind: TokenKind::Error(error), location },
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_token(text: &str, kind: TokenKind<'_>, location: Span) {
        let expected = Token { kind, location };
        let tokens = lex(text).collect::<Vec<_>>();
        assert_eq!(tokens, vec![expected], "Unexpected token result for '{text}'");
    }

    fn assert_tokens(text: &str, expected: Vec<TokenKind<'_>>) {
        let expected = Vec::from_iter(expected.into_iter().map(|t| Ok(t)));
        let actual = Vec::from_iter(TokenKind::lexer(text));
        assert_eq!(actual, expected);
    }

    #[test]
    fn integer() {
        assert_token("64", TokenKind::Integer(64), 0..2);
        assert_token("1234567890", TokenKind::Integer(1234567890), 0..10);
        assert_token("0xABCD", TokenKind::Integer(0xABCD), 0..6);
    }

    #[test]
    fn float() {
        assert_token("12.0", TokenKind::Float(12.0), 0..4);
        assert_token("12.", TokenKind::Float(12.0), 0..3);
        assert_token(".12", TokenKind::Float(0.12), 0..3);

        assert_token(".", TokenKind::Float(0.0), 0..1);
        assert_token("+.", TokenKind::Float(0.0), 0..2);
        assert_token("-.", TokenKind::Float(-0.0), 0..2);
    }

    #[test]
    fn string() {
        assert_token("\"text\"", TokenKind::String("text"), 0..6);

        assert_token("\"64\"", TokenKind::String("64"), 0..4);
        assert_token("\"12.0\"", TokenKind::String("12.0"), 0..6);

        assert_token("\"'text'\"", TokenKind::String("'text'"), 0..8);
        assert_token("\"$text\"", TokenKind::String("$text"), 0..7);
        assert_token("\"kDataUnhandled\"", TokenKind::String("kDataUnhandled"), 0..16);

        assert_token("\"(\"", TokenKind::String("("), 0..3);
        assert_token("\")\"", TokenKind::String(")"), 0..3);
        assert_token("\"{\"", TokenKind::String("{"), 0..3);
        assert_token("\"}\"", TokenKind::String("}"), 0..3);
        assert_token("\"[\"", TokenKind::String("["), 0..3);
        assert_token("\"]\"", TokenKind::String("]"), 0..3);

        assert_token("\"#define\"", TokenKind::String("#define"), 0..9);
        assert_token("\"#undef\"", TokenKind::String("#undef"), 0..8);
        assert_token("\"#include\"", TokenKind::String("#include"), 0..10);
        assert_token("\"#include_opt\"", TokenKind::String("#include_opt"), 0..14);
        assert_token("\"#merge\"", TokenKind::String("#merge"), 0..8);
        assert_token("\"#autorun\"", TokenKind::String("#autorun"), 0..10);
        assert_token("\"#ifdef\"", TokenKind::String("#ifdef"), 0..8);
        assert_token("\"#ifndef\"", TokenKind::String("#ifndef"), 0..9);
        assert_token("\"#else\"", TokenKind::String("#else"), 0..7);
        assert_token("\"#endif\"", TokenKind::String("#endif"), 0..8);
        assert_token("\"#bad\"", TokenKind::String("#bad"), 0..6);

        assert_token("\"\n\"", TokenKind::String("\n"), 0..3);
        assert_token("\"; a comment\"", TokenKind::String("; a comment"), 0..13);
        assert_token("\"/* a comment */\"", TokenKind::String("/* a comment */"), 0..17);
    }

    #[test]
    fn symbol() {
        assert_token("text", TokenKind::Symbol("text"), 0..4);

        assert_token("+", TokenKind::Symbol("+"), 0..1);
        assert_token("-", TokenKind::Symbol("-"), 0..1);
        assert_token("*", TokenKind::Symbol("*"), 0..1);
        assert_token("/", TokenKind::Symbol("/"), 0..1);
        assert_token("%", TokenKind::Symbol("%"), 0..1);
        assert_token("_", TokenKind::Symbol("_"), 0..1);

        for char in 'a'..'z' {
            let str = char.to_string();
            assert_token(&str, TokenKind::Symbol(&str), 0..1);

            for char2 in 'a'..'z' {
                let str = char.to_string() + &char2.to_string();
                assert_token(&str, TokenKind::Symbol(&str), 0..2);
            }
        }
    }

    #[test]
    fn variable() {
        assert_token("$text", TokenKind::Variable("text"), 0..5);

        assert_token("$+", TokenKind::Variable("+"), 0..2);
        assert_token("$-", TokenKind::Variable("-"), 0..2);
        assert_token("$*", TokenKind::Variable("*"), 0..2);
        assert_token("$/", TokenKind::Variable("/"), 0..2);
        assert_token("$%", TokenKind::Variable("%"), 0..2);
        assert_token("$_", TokenKind::Variable("_"), 0..2);

        for char in 'a'..'z' {
            let str = char.to_string();
            assert_token(&("$".to_owned() + &str), TokenKind::Variable(&str), 0..2);

            for char2 in 'a'..'z' {
                let str = char.to_string() + &char2.to_string();
                assert_token(&("$".to_owned() + &str), TokenKind::Variable(&str), 0..3);
            }
        }
    }

    #[test]
    fn unhandled() {
        assert_token("kDataUnhandled", TokenKind::Unhandled, 0..14);
    }

    #[test]
    fn arrays() {
        assert_token("(", TokenKind::ArrayOpen, 0..1);
        assert_token(")", TokenKind::ArrayClose, 0..1);
        assert_token("{", TokenKind::CommandOpen, 0..1);
        assert_token("}", TokenKind::CommandClose, 0..1);
        assert_token("[", TokenKind::PropertyOpen, 0..1);
        assert_token("]", TokenKind::PropertyClose, 0..1);
    }

    #[test]
    fn directives() {
        assert_token("#define", TokenKind::Define, 0..7);
        assert_token("#undef", TokenKind::Undefine, 0..6);
        assert_token("#include", TokenKind::Include, 0..8);
        assert_token("#include_opt", TokenKind::IncludeOptional, 0..12);
        assert_token("#merge", TokenKind::Merge, 0..6);
        assert_token("#autorun", TokenKind::Autorun, 0..8);
        assert_token("#ifdef", TokenKind::Ifdef, 0..6);
        assert_token("#ifndef", TokenKind::Ifndef, 0..7);
        assert_token("#else", TokenKind::Else, 0..5);
        assert_token("#endif", TokenKind::Endif, 0..6);
        assert_token("#bad", TokenKind::BadDirective("bad"), 0..4);
    }

    #[test]
    fn comments() {
        assert_token("; comment", TokenKind::Comment, 0..9);
        assert_token(";comment", TokenKind::Comment, 0..8);
        assert_tokens(
            "/* comment */",
            vec![
                TokenKind::BlockCommentStart("/* comment "),
                TokenKind::BlockCommentEnd("*/"),
            ],
        );

        // These get parsed as symbols in the original lexer
        assert_token("a;symbol", TokenKind::Symbol("a;symbol"), 0..8);
        assert_token("/**/", TokenKind::Symbol("/**/"), 0..4);
        assert_token("/*****/", TokenKind::Symbol("/*****/"), 0..7);
        assert_token("/*comment*/", TokenKind::Symbol("/*comment*/"), 0..11);
    }

    #[test]
    fn whitespace() {
        assert_eq!(Vec::from_iter(TokenKind::lexer("\n")), []);
        assert_eq!(Vec::from_iter(TokenKind::lexer("\r\n")), []);
        assert_eq!(Vec::from_iter(TokenKind::lexer("\n\t")), []);
        assert_eq!(Vec::from_iter(TokenKind::lexer(" \x0b\t\r\n\x0c")), []); // " \v\t\r\n\f"
    }

    #[test]
    fn thorough() {
        use TokenKind::*;

        let text = include_str!("../../tests/test_files/thorough.dta").replace("\r\n", "\n");

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
            Integer(0xFFFFFFFFFFFFFFFFu64 as i64),
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
