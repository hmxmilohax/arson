// SPDX-License-Identifier: LGPL-3.0-or-later

use std::fmt::Write;

use lazy_regex::{regex, regex_is_match};
use logos::{Logos, Span, SpannedIter};

use crate::{DiagnosticKind, FloatValue, IntegerValue};

type Lexer<'src> = logos::Lexer<'src, TokenValue<'src>>;

#[derive(Debug, PartialEq)]
pub struct Token<'src> {
    pub value: TokenValue<'src>,
    pub location: Span,
}

impl Token<'_> {
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
            $variant:ident$(($(&$life:lifetime)? $value_type:ident$(<$value_generics:tt>)?))? {
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
            $($(#[$attr])* $variant$(($(&$life)? $value_type$(<$value_generics>)?))?,)+
        }

        impl<'src> TokenValue<'src> {
            pub fn get_kind(&self) -> TokenKind {
                match self {
                    $(TokenValue::$variant$((meta_morph!($value_type => _)))? => TokenKind::$variant,)+
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
        value_display: |f, value| value.fmt(f),
    },
    #[regex(r#""[^"]*""#, |lex| trim_delimiters(lex.slice(), 1, 1))]
    String(&'src str) {
        kind_display: "string",
        value_display: |f, value| write!(f, "\"{}\"", value),
    },

    // Symbol consumes almost all input which doesn't match any other token,
    // including technically malformed versions of integers/floats
    #[regex(r#"[^ \v\t\r\n\f\(\)\[\]\{\}]+"#, priority = 0)]
    #[regex(r#"'[^']*'"#, |lex| trim_delimiters(lex.slice(), 1, 1))]
    Symbol(&'src str) {
        kind_display: "symbol",
        value_display: |f, value| {
            // Write without quotes where possible
            if regex_is_match!(r#"[^ \v\t\r\n\f\(\)\[\]\{\}]+"#, value) {
                f.write_str(value)
            } else {
                write!(f, "'{}'", value)
            }
        },
    },
    #[regex(r#"\$[^ \v\t\r\n\f\(\)\[\]\{\}]+"#, |lex| trim_delimiters(lex.slice(), 1, 0))]
    Variable(&'src str) {
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

    #[regex(r#";[^\n]*"#, |lex| trim_delimiters(lex.slice(), 1, 0), priority = 1)]
    Comment(&'src str) {
        kind_display: "comment",
        value_display: |f, value| write!(f, ";{}", value),
    },
    // These block comment regexes are very particular, for compatibility reasons
    #[regex(r#"(\/\*)+[^\n*]*"#, parse_block_comment)]
    BlockComment(&'src str) {
        kind_display: "block comment",
        value_display: |f, value| f.write_str(value),
    },
    // Handled in parse_block_comment
    // #[regex(r#"\*+\/"#)]
    // BlockCommentEnd(&'src str) => ("block comment end"),

    #[regex(r#"#[^ \v\t\r\n\f\(\)\[\]\{\}]+"#, |_lex| DiagnosticKind::BadDirective)]
    Error(DiagnosticKind) {
        kind_display: "token error",
        value_display: |f, value| value.fmt(f),
    },
}

fn trim_delimiters(text: &str, before: usize, after: usize) -> Result<&str, DiagnosticKind> {
    let trim_range = before..text.len() - after;
    text.get(trim_range.clone())
        .ok_or(DiagnosticKind::TrimDelimiterError { trim_range, actual_length: text.len() })
}

fn parse_hex(lex: &mut Lexer<'_>) -> Result<IntegerValue, DiagnosticKind> {
    let trimmed = trim_delimiters(lex.slice(), 2, 0)?;
    u64::from_str_radix(trimmed, 16)
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

fn parse_block_comment<'src>(lex: &mut Lexer<'src>) -> Result<&'src str, DiagnosticKind> {
    let remainder = lex.remainder();
    let Some(found) = regex!(r#"\*+\/"#).find(remainder).map(|m| m.range()) else {
        lex.bump(remainder.len());
        return Err(DiagnosticKind::UnclosedBlockComment);
    };

    lex.bump(found.end);
    Ok(lex.slice())
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

    pub fn peek(&mut self) -> Option<&Token<'src>> {
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
        assert_token("#bad", TokenValue::Error(DiagnosticKind::BadDirective), 0..4);
    }

    #[test]
    fn comments() {
        assert_token("; comment", TokenValue::Comment(" comment"), 0..9);
        assert_token(";comment", TokenValue::Comment("comment"), 0..8);
        assert_token("/* comment */", TokenValue::BlockComment("/* comment */"), 0..13);
        assert_token(
            "/*\n\
            multi\n\
            line\n\
            comment\n\
            */",
            TokenValue::BlockComment(
                "/*\n\
                multi\n\
                line\n\
                comment\n\
                */",
            ),
            0..24,
        );

        // These get parsed as symbols in the original lexer
        assert_token("a;symbol", TokenValue::Symbol("a;symbol"), 0..8);
        assert_token("/**/", TokenValue::Symbol("/**/"), 0..4);
        assert_token("/*****/", TokenValue::Symbol("/*****/"), 0..7);
        assert_token("/*comment*/", TokenValue::Symbol("/*comment*/"), 0..11);

        assert_token("/*", TokenValue::Error(DiagnosticKind::UnclosedBlockComment), 0..2);
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
    }
}
