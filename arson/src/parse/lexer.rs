// SPDX-License-Identifier: LGPL-3.0-or-later

use std::num::{ParseFloatError, ParseIntError};

use logos::{Logos, Span};

type Lexer<'src> = logos::Lexer<'src, TokenValue<'src>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'src> {
    pub kind: TokenValue<'src>,
    pub location: Span,
}

// Token regexes based on the RB3 decomp.
// Behavior is replicated as closely as possible, all files should tokenize
// identically to the original Flex tokenizer based on current knowledge for it.

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(error = LexError)]
#[logos(skip r#"[ \v\t\r\n\f]+"#)]
pub enum TokenValue<'src> {
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

impl<'src> TokenValue<'src> {
    pub fn get_type(&self) -> TokenKind {
        match self {
            TokenValue::Integer(_) => TokenKind::Integer,
            TokenValue::Float(_) => TokenKind::Float,
            TokenValue::String(_) => TokenKind::String,
            TokenValue::Symbol(_) => TokenKind::Symbol,
            TokenValue::Variable(_) => TokenKind::Variable,
            TokenValue::Unhandled => TokenKind::Unhandled,

            TokenValue::ArrayOpen => TokenKind::ArrayOpen,
            TokenValue::ArrayClose => TokenKind::ArrayClose,
            TokenValue::CommandOpen => TokenKind::CommandOpen,
            TokenValue::CommandClose => TokenKind::CommandClose,
            TokenValue::PropertyOpen => TokenKind::PropertyOpen,
            TokenValue::PropertyClose => TokenKind::PropertyClose,

            TokenValue::Define => TokenKind::Define,
            TokenValue::Undefine => TokenKind::Undefine,
            TokenValue::Include => TokenKind::Include,
            TokenValue::IncludeOptional => TokenKind::IncludeOptional,
            TokenValue::Merge => TokenKind::Merge,
            TokenValue::Autorun => TokenKind::Autorun,

            TokenValue::Ifdef => TokenKind::Ifdef,
            TokenValue::Ifndef => TokenKind::Ifndef,
            TokenValue::Else => TokenKind::Else,
            TokenValue::Endif => TokenKind::Endif,

            TokenValue::BadDirective(_) => TokenKind::BadDirective,

            TokenValue::Comment => TokenKind::Comment,
            TokenValue::BlockCommentStart(_) => TokenKind::BlockCommentStart,
            TokenValue::BlockCommentEnd(_) => TokenKind::BlockCommentEnd,

            TokenValue::Error(_) => TokenKind::Error,
        }
    }
}

impl<'src> std::fmt::Display for TokenValue<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenValue::Integer(value) => write!(f, "integer {value}"),
            TokenValue::Float(value) => write!(f, "float {value}"),
            TokenValue::String(value) => write!(f, "string \"{value}\""),
            TokenValue::Symbol(value) => write!(f, "symbol '{value}'"),
            TokenValue::Variable(value) => write!(f, "variable ${value}"),
            TokenValue::Unhandled => write!(f, "kDataUnhandled"),

            TokenValue::ArrayOpen => write!(f, "array open"),
            TokenValue::ArrayClose => write!(f, "array close"),
            TokenValue::CommandOpen => write!(f, "command open"),
            TokenValue::CommandClose => write!(f, "command close"),
            TokenValue::PropertyOpen => write!(f, "property open"),
            TokenValue::PropertyClose => write!(f, "property close"),

            TokenValue::Define => write!(f, "#define directive"),
            TokenValue::Undefine => write!(f, "#undef directive"),
            TokenValue::Include => write!(f, "#include directive"),
            TokenValue::IncludeOptional => write!(f, "#include_opt directive"),
            TokenValue::Merge => write!(f, "#merge directive"),
            TokenValue::Autorun => write!(f, "#autorun directive"),

            TokenValue::Ifdef => write!(f, "#ifdef directive"),
            TokenValue::Ifndef => write!(f, "#ifndef directive"),
            TokenValue::Else => write!(f, "#else directive"),
            TokenValue::Endif => write!(f, "#endif directive"),

            TokenValue::BadDirective(name) => write!(f, "bad #{name} directive"),

            TokenValue::Comment => write!(f, "comment"),
            TokenValue::BlockCommentStart(_) => write!(f, "block comment start"),
            TokenValue::BlockCommentEnd(_) => write!(f, "block comment end"),

            TokenValue::Error(lex_error) => write!(f, "token error: {lex_error}"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum TokenKind {
    Integer,
    Float,
    String,
    Symbol,
    Variable,
    Unhandled,

    ArrayOpen,
    ArrayClose,
    CommandOpen,
    CommandClose,
    PropertyOpen,
    PropertyClose,

    Define,
    Undefine,
    Include,
    IncludeOptional,
    Merge,
    Autorun,

    Ifdef,
    Ifndef,
    Else,
    Endif,

    BadDirective,

    Comment,
    BlockCommentStart,
    BlockCommentEnd,

    Error,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Integer => write!(f, "integer"),
            TokenKind::Float => write!(f, "float"),
            TokenKind::String => write!(f, "string"),
            TokenKind::Symbol => write!(f, "symbol"),
            TokenKind::Variable => write!(f, "variable"),
            TokenKind::Unhandled => write!(f, "kDataUnhandled"),

            TokenKind::ArrayOpen => write!(f, "array open"),
            TokenKind::ArrayClose => write!(f, "array close"),
            TokenKind::CommandOpen => write!(f, "command open"),
            TokenKind::CommandClose => write!(f, "command close"),
            TokenKind::PropertyOpen => write!(f, "property open"),
            TokenKind::PropertyClose => write!(f, "property close"),

            TokenKind::Define => write!(f, "#define directive"),
            TokenKind::Undefine => write!(f, "#undef directive"),
            TokenKind::Include => write!(f, "#include directive"),
            TokenKind::IncludeOptional => write!(f, "#include_opt directive"),
            TokenKind::Merge => write!(f, "#merge directive"),
            TokenKind::Autorun => write!(f, "#autorun directive"),

            TokenKind::Ifdef => write!(f, "#ifdef directive"),
            TokenKind::Ifndef => write!(f, "#ifndef directive"),
            TokenKind::Else => write!(f, "#else directive"),
            TokenKind::Endif => write!(f, "#endif directive"),

            TokenKind::BadDirective => write!(f, "bad directive"),

            TokenKind::Comment => write!(f, "comment"),
            TokenKind::BlockCommentStart => write!(f, "block comment start"),
            TokenKind::BlockCommentEnd => write!(f, "block comment end"),

            TokenKind::Error => write!(f, "token error"),
        }
    }
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
        let tokens = lex(text).collect::<Vec<_>>();
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
