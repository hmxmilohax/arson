// SPDX-License-Identifier: LGPL-3.0-or-later

use std::num::{ParseFloatError, ParseIntError};

use logos::{Logos, Span};

type Lexer<'src> = logos::Lexer<'src, TokenKind<'src>>;

#[derive(Debug, Clone)]
pub struct Token<'src> {
    pub kind: TokenKind<'src>,
    pub location: Span,
}

// Token regexes based on the RB3 decomp.
// Behavior is replicated as closely as possible, all files should tokenize
// identically to the original Flex tokenizer based on current knowledge for it.

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(error = LexErrorKind)]
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
}

#[derive(Debug, Clone)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub location: Span,
}

#[derive(thiserror::Error, Debug, Clone, Default, PartialEq)]
pub enum LexErrorKind {
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

fn trim_delimiters(text: &str, before: usize, after: usize) -> Result<&str, LexErrorKind> {
    text.get(before..text.len() - after)
        .ok_or(LexErrorKind::DelimiterError)
}

fn parse_hex(lex: &mut Lexer<'_>) -> Result<i64, LexErrorKind> {
    let trimmed = trim_delimiters(lex.slice(), 2, 0)?;
    u64::from_str_radix(trimmed, 16)
        .map(|v| v as i64)
        .map_err(LexErrorKind::IntegerError)
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

pub fn lex(text: &str) -> Vec<Result<Token<'_>, LexError>> {
    TokenKind::lexer(text)
        .spanned()
        .map(|t| match t {
            (Ok(kind), location) => Ok(Token { kind, location }),
            (Err(kind), location) => Err(LexError { kind, location }),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_token(text: &str, expected: TokenKind<'_>) {
        let tokens = Vec::from_iter(TokenKind::lexer(text));
        assert_eq!(tokens, vec![Ok(expected)], "Unexpected token result for '{text}'");
    }

    fn assert_tokens(text: &str, expected: Vec<TokenKind<'_>>) {
        let expected = Vec::from_iter(expected.into_iter().map(|t| Ok(t)));
        let actual = Vec::from_iter(TokenKind::lexer(text));
        assert_eq!(actual, expected);
    }

    #[test]
    fn integer() {
        assert_token("64", TokenKind::Integer(64));
        assert_token("1234567890", TokenKind::Integer(1234567890));
        assert_token("0xABCD", TokenKind::Integer(0xABCD));
    }

    #[test]
    fn float() {
        assert_token("12.0", TokenKind::Float(12.0));
        assert_token("12.", TokenKind::Float(12.0));
        assert_token(".12", TokenKind::Float(0.12));

        assert_token(".", TokenKind::Float(0.0));
        assert_token("+.", TokenKind::Float(0.0));
        assert_token("-.", TokenKind::Float(-0.0));
    }

    #[test]
    fn string() {
        assert_token("\"text\"", TokenKind::String("text"));

        assert_token("\"64\"", TokenKind::String("64"));
        assert_token("\"12.0\"", TokenKind::String("12.0"));

        assert_token("\"'text'\"", TokenKind::String("'text'"));
        assert_token("\"$text\"", TokenKind::String("$text"));
        assert_token("\"kDataUnhandled\"", TokenKind::String("kDataUnhandled"));

        assert_token("\"(\"", TokenKind::String("("));
        assert_token("\")\"", TokenKind::String(")"));
        assert_token("\"{\"", TokenKind::String("{"));
        assert_token("\"}\"", TokenKind::String("}"));
        assert_token("\"[\"", TokenKind::String("["));
        assert_token("\"]\"", TokenKind::String("]"));

        assert_token("\"#define\"", TokenKind::String("#define"));
        assert_token("\"#undef\"", TokenKind::String("#undef"));
        assert_token("\"#include\"", TokenKind::String("#include"));
        assert_token("\"#include_opt\"", TokenKind::String("#include_opt"));
        assert_token("\"#merge\"", TokenKind::String("#merge"));
        assert_token("\"#autorun\"", TokenKind::String("#autorun"));
        assert_token("\"#ifdef\"", TokenKind::String("#ifdef"));
        assert_token("\"#ifndef\"", TokenKind::String("#ifndef"));
        assert_token("\"#else\"", TokenKind::String("#else"));
        assert_token("\"#endif\"", TokenKind::String("#endif"));
        assert_token("\"#bad\"", TokenKind::String("#bad"));

        assert_token("\"\n\"", TokenKind::String("\n"));
        assert_token("\"; a comment\"", TokenKind::String("; a comment"));
        assert_token("\"/* a comment */\"", TokenKind::String("/* a comment */"));
    }

    #[test]
    fn symbol() {
        assert_token("text", TokenKind::Symbol("text"));

        assert_token("+", TokenKind::Symbol("+"));
        assert_token("-", TokenKind::Symbol("-"));
        assert_token("*", TokenKind::Symbol("*"));
        assert_token("/", TokenKind::Symbol("/"));
        assert_token("%", TokenKind::Symbol("%"));
        assert_token("_", TokenKind::Symbol("_"));

        for char in 'a'..'z' {
            let str = char.to_string();
            assert_token(&str, TokenKind::Symbol(&str));

            for char2 in 'a'..'z' {
                let str = char.to_string() + &char2.to_string();
                assert_token(&str, TokenKind::Symbol(&str));
            }
        }
    }

    #[test]
    fn variable() {
        assert_token("$text", TokenKind::Variable("text"));

        assert_token("$+", TokenKind::Variable("+"));
        assert_token("$-", TokenKind::Variable("-"));
        assert_token("$*", TokenKind::Variable("*"));
        assert_token("$/", TokenKind::Variable("/"));
        assert_token("$%", TokenKind::Variable("%"));
        assert_token("$_", TokenKind::Variable("_"));

        for char in 'a'..'z' {
            let str = char.to_string();
            assert_token(&("$".to_owned() + &str), TokenKind::Variable(&str));

            for char2 in 'a'..'z' {
                let str = char.to_string() + &char2.to_string();
                assert_token(&("$".to_owned() + &str), TokenKind::Variable(&str));
            }
        }
    }

    #[test]
    fn unhandled() {
        assert_token("kDataUnhandled", TokenKind::Unhandled);
    }

    #[test]
    fn arrays() {
        assert_token("(", TokenKind::ArrayOpen);
        assert_token(")", TokenKind::ArrayClose);
        assert_token("{", TokenKind::CommandOpen);
        assert_token("}", TokenKind::CommandClose);
        assert_token("[", TokenKind::PropertyOpen);
        assert_token("]", TokenKind::PropertyClose);
    }

    #[test]
    fn directives() {
        assert_token("#define", TokenKind::Define);
        assert_token("#undef", TokenKind::Undefine);
        assert_token("#include", TokenKind::Include);
        assert_token("#include_opt", TokenKind::IncludeOptional);
        assert_token("#merge", TokenKind::Merge);
        assert_token("#autorun", TokenKind::Autorun);
        assert_token("#ifdef", TokenKind::Ifdef);
        assert_token("#ifndef", TokenKind::Ifndef);
        assert_token("#else", TokenKind::Else);
        assert_token("#endif", TokenKind::Endif);
        assert_token("#bad", TokenKind::BadDirective("bad"));
    }

    #[test]
    fn comments() {
        assert_token("; comment", TokenKind::Comment);
        assert_token(";comment", TokenKind::Comment);
        assert_tokens(
            "/* comment */",
            vec![
                TokenKind::BlockCommentStart("/* comment "),
                TokenKind::BlockCommentEnd("*/"),
            ],
        );

        // These get parsed as symbols in the original lexer
        assert_token("a;symbol", TokenKind::Symbol("a;symbol"));
        assert_token("/**/", TokenKind::Symbol("/**/"));
        assert_token("/*****/", TokenKind::Symbol("/*****/"));
        assert_token("/*comment*/", TokenKind::Symbol("/*comment*/"));
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
