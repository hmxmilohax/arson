// SPDX-License-Identifier: LGPL-3.0-or-later

use logos::{Lexer, Logos};

use super::{Context, Node, NodeArray, NodeCommand, NodeProperty};
use crate::{arson_assert, arson_fail};

type Tokenizer<'src> = Lexer<'src, Token<'src>>;

#[derive(thiserror::Error, Debug, Clone, PartialEq, Default)]
enum TokenError {
    #[default]
    #[error("Invalid token")]
    InvalidToken,

    #[error("Integer parse error: {0}")]
    IntegerError(#[from] std::num::ParseIntError),
    #[error("Float parse error: {0}")]
    FloatError(#[from] std::num::ParseFloatError),
    #[error("Failed to remove token delimiters")]
    DelimiterError,
}

// Token regexes based on the RB3 decomp.
// Behavior is replicated as closely as possible, all files should tokenize
// identically to the original Flex tokenizer based on current knowledge for it.

#[derive(logos::Logos, Debug, PartialEq)]
#[logos(error = TokenError)]
#[logos(skip r#"[ \v\t\r\f]+"#)]
enum Token<'src> {
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

    #[token("\n")]
    Newline,
    #[regex(r#";[^\n]*"#, priority = 1)]
    Comment,
    // These block comment regexes are very particular, for compatibility reasons
    #[regex(r#"(\/\*)+[^\n*]*"#)]
    BlockCommentStart(&'src str),
    #[regex(r#"\*+\/"#)]
    BlockCommentEnd(&'src str),
}

fn parse_hex<'src>(lex: &mut Lexer<'src, Token<'src>>) -> Result<i64, TokenError> {
    let trimmed = trim_delimiters(lex.slice(), 2, 0)?;
    u64::from_str_radix(trimmed, 16)
        .map(|v| v as i64)
        .map_err(|err| TokenError::IntegerError(err))
}

fn parse_float<'src>(lex: &mut Lexer<'src, Token<'src>>) -> Result<f64, TokenError> {
    let text = lex.slice();
    match text.parse::<f64>() {
        Ok(value) => Ok(value),
        Err(err) => match text {
            text if text.starts_with(".") => Ok(0.0),
            text if text.starts_with("+.") => Ok(0.0),
            text if text.starts_with("-.") => Ok(-0.0),
            _ => Err(TokenError::FloatError(err)),
        },
    }
}

fn trim_delimiters(text: &str, before: usize, after: usize) -> Result<&str, TokenError> {
    text.get(before..text.len() - after)
        .ok_or(TokenError::DelimiterError)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ArrayType {
    None,
    Array,
    Command,
    Property,
}

#[derive(Debug, Clone)]
enum NodeParseStatus {
    Continue,
    Break,
    Node(Node),
}

pub struct Parser {
    line_number: usize,
    array_open: ArrayType,
    conditionals: Vec<bool>,
    block_comment: bool,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            line_number: 1,
            array_open: ArrayType::None,
            conditionals: Vec::new(),
            block_comment: false,
        }
    }

    fn parse_array<'src>(
        &mut self,
        context: &mut Context,
        lexer: &mut Tokenizer<'src>,
        array_type: ArrayType,
    ) -> crate::Result<NodeArray> {
        // Save current state
        let old_open = self.array_open;
        self.array_open = array_type;

        let mut array = NodeArray::with_capacity(4);
        loop {
            let status = self.parse_node(context, lexer)?;
            match status {
                NodeParseStatus::Continue => continue,
                NodeParseStatus::Break => break,
                NodeParseStatus::Node(node) => array.push(node),
            };
        }
        array.shrink_to_fit();

        // Restore state
        self.array_open = old_open;

        Ok(array)
    }

    fn parse_node<'src>(
        &mut self,
        context: &mut Context,
        lexer: &mut Tokenizer<'src>,
    ) -> crate::Result<NodeParseStatus> {
        let token = match lexer.next() {
            None => match self.array_open {
                ArrayType::None => return Ok(NodeParseStatus::Break),
                open => arson_fail!("{open:?} not closed"),
            },
            Some(token) => match token {
                Ok(token) => token,
                Err(error) => arson_fail!("Internal parser error: {error}"),
            },
        };

        let array_close = |array_type: ArrayType| -> crate::Result<NodeParseStatus> {
            arson_assert!(
                self.array_open == array_type,
                "{:?} closed incorrectly, found {array_type:?} instead",
                self.array_open,
            );
            Ok(NodeParseStatus::Break)
        };

        let node = match token {
            Token::Newline => {
                self.line_number += 1;
                return Ok(NodeParseStatus::Continue);
            },

            Token::Comment => {
                return Ok(NodeParseStatus::Continue);
            },
            Token::BlockCommentStart(_) => {
                self.block_comment = true;
                return Ok(NodeParseStatus::Continue);
            },
            Token::BlockCommentEnd(text) => {
                if self.block_comment {
                    self.block_comment = false;
                    return Ok(NodeParseStatus::Continue);
                } else {
                    // For compatibility, stray block ends are parsed as symbols
                    Node::Symbol(context.add_symbol(text))
                }
            },

            // Skip everything else during block comments
            _ if self.block_comment => {
                return Ok(NodeParseStatus::Continue);
            },

            Token::Ifdef => {
                let name = match next_token(lexer)? {
                    Token::Symbol(name) => context.add_symbol(name),
                    token => arson_fail!("Expected symbol for #ifdef name, got {token:?} instead"),
                };
                self.conditionals.push(context.get_macro(&name).is_some());
                return Ok(NodeParseStatus::Continue);
            },
            Token::Ifndef => {
                let name = match next_token(lexer)? {
                    Token::Symbol(name) => context.add_symbol(name),
                    token => arson_fail!("Expected symbol for #ifndef name, got {token:?} instead"),
                };
                self.conditionals.push(context.get_macro(&name).is_none());
                return Ok(NodeParseStatus::Continue);
            },
            Token::Else => {
                match self.conditionals.pop() {
                    Some(value) => self.conditionals.push(!value),
                    None => arson_fail!("No #ifdef/#ifndef found for this #else directive"),
                }
                return Ok(NodeParseStatus::Continue);
            },
            Token::Endif => {
                if let None = self.conditionals.pop() {
                    arson_fail!("No #ifdef/#ifndef/#else found for this #endif directive");
                }
                return Ok(NodeParseStatus::Continue);
            },

            Token::BadDirective(name) => arson_fail!("Bad directive {name}"),

            // Skip everything else during negative conditionals
            _ if match self.conditionals.last() {
                Some(value) => !value,
                None => false,
            } =>
            {
                return Ok(NodeParseStatus::Continue);
            },

            Token::Integer(value) => Node::Integer(value),
            Token::Float(value) => Node::Float(value),
            Token::String(value) => Node::String(value.to_owned()),
            Token::Symbol(value) => Node::Symbol(context.add_symbol(value)),
            Token::Variable(_value) => todo!("variables"),
            Token::Unhandled => Node::Unhandled,

            Token::ArrayOpen => {
                let array = self.parse_array(context, lexer, ArrayType::Array)?;
                Node::Array(array)
            },
            Token::CommandOpen => {
                let array = self.parse_array(context, lexer, ArrayType::Command)?;
                Node::Command(NodeCommand::from(array))
            },
            Token::PropertyOpen => {
                let array = self.parse_array(context, lexer, ArrayType::Property)?;
                Node::Property(NodeProperty::from(array))
            },
            Token::ArrayClose => return array_close(ArrayType::Array),
            Token::CommandClose => return array_close(ArrayType::Command),
            Token::PropertyClose => return array_close(ArrayType::Property),

            token => todo!("Token {token:?} not yet handled")
        };

        Ok(NodeParseStatus::Node(node))
    }
}

fn next_token<'src>(lexer: &mut Tokenizer<'src>) -> crate::Result<Token<'src>> {
    match lexer.next() {
        Some(token) => match token {
            Ok(token) => Ok(token),
            Err(error) => arson_fail!("Internal parser error: {error}"),
        },
        None => arson_fail!("Unexpected end of file"),
    }
}

pub fn parse_dta(context: &mut Context, text: &str) -> crate::Result<NodeArray> {
    let mut parser = Parser::new();
    // The contents of the entire file are parsed into an array, not a node.
    // Since there are no open/close tokens for this file-level array,
    // ArrayType::None is used as the starting type.
    parser.parse_array(context, &mut Token::lexer(text), ArrayType::None)
}

#[cfg(test)]
mod tests {
    use super::*;

    mod tokens {
        use super::*;

        fn assert_token(text: &str, expected: Token<'_>) {
            let tokens = Vec::from_iter(Token::lexer(text));
            assert_eq!(tokens.len(), 1, "Unexpected token count");
            assert_eq!(tokens[0], Ok(expected), "Error tokenizing {text}");
        }

        fn assert_tokens(text: &str, expected: Vec<Token<'_>>) {
            let expected = Vec::from_iter(expected.into_iter().map(|t| Ok(t)));
            let actual = Vec::from_iter(Token::lexer(text));
            assert_eq!(actual, expected);
        }

        #[test]
        fn integer() {
            assert_token("64", Token::Integer(64));
            assert_token("1234567890", Token::Integer(1234567890));
            assert_token("0xABCD", Token::Integer(0xABCD));
        }

        #[test]
        fn float() {
            assert_token("12.0", Token::Float(12.0));
            assert_token("12.", Token::Float(12.0));
            assert_token(".12", Token::Float(0.12));

            assert_token(".", Token::Float(0.0));
            assert_token("+.", Token::Float(0.0));
            assert_token("-.", Token::Float(-0.0));
        }

        #[test]
        fn string() {
            assert_token("\"text\"", Token::String("text"));

            assert_token("\"64\"", Token::String("64"));
            assert_token("\"12.0\"", Token::String("12.0"));

            assert_token("\"'text'\"", Token::String("'text'"));
            assert_token("\"$text\"", Token::String("$text"));
            assert_token("\"kDataUnhandled\"", Token::String("kDataUnhandled"));

            assert_token("\"(\"", Token::String("("));
            assert_token("\")\"", Token::String(")"));
            assert_token("\"{\"", Token::String("{"));
            assert_token("\"}\"", Token::String("}"));
            assert_token("\"[\"", Token::String("["));
            assert_token("\"]\"", Token::String("]"));

            assert_token("\"#define\"", Token::String("#define"));
            assert_token("\"#undef\"", Token::String("#undef"));
            assert_token("\"#include\"", Token::String("#include"));
            assert_token("\"#include_opt\"", Token::String("#include_opt"));
            assert_token("\"#merge\"", Token::String("#merge"));
            assert_token("\"#autorun\"", Token::String("#autorun"));
            assert_token("\"#ifdef\"", Token::String("#ifdef"));
            assert_token("\"#ifndef\"", Token::String("#ifndef"));
            assert_token("\"#else\"", Token::String("#else"));
            assert_token("\"#endif\"", Token::String("#endif"));
            assert_token("\"#bad\"", Token::String("#bad"));

            assert_token("\"\n\"", Token::String("\n"));
            assert_token("\"; a comment\"", Token::String("; a comment"));
            assert_token("\"/* a comment */\"", Token::String("/* a comment */"));
        }

        #[test]
        fn symbol() {
            assert_token("text", Token::Symbol("text"));

            assert_token("+", Token::Symbol("+"));
            assert_token("-", Token::Symbol("-"));
            assert_token("*", Token::Symbol("*"));
            assert_token("/", Token::Symbol("/"));
            assert_token("%", Token::Symbol("%"));
            assert_token("_", Token::Symbol("_"));

            for char in 'a'..'z' {
                let str = char.to_string();
                assert_token(&str, Token::Symbol(&str));

                for char2 in 'a'..'z' {
                    let str = char.to_string() + &char2.to_string();
                    assert_token(&str, Token::Symbol(&str));
                }
            }
        }

        #[test]
        fn variable() {
            assert_token("$text", Token::Variable("text"));

            assert_token("$+", Token::Variable("+"));
            assert_token("$-", Token::Variable("-"));
            assert_token("$*", Token::Variable("*"));
            assert_token("$/", Token::Variable("/"));
            assert_token("$%", Token::Variable("%"));
            assert_token("$_", Token::Variable("_"));

            for char in 'a'..'z' {
                let str = char.to_string();
                assert_token(&("$".to_owned() + &str), Token::Variable(&str));

                for char2 in 'a'..'z' {
                    let str = char.to_string() + &char2.to_string();
                    assert_token(&("$".to_owned() + &str), Token::Variable(&str));
                }
            }
        }

        #[test]
        fn unhandled() {
            assert_token("kDataUnhandled", Token::Unhandled);
        }

        #[test]
        fn arrays() {
            assert_token("(", Token::ArrayOpen);
            assert_token(")", Token::ArrayClose);
            assert_token("{", Token::CommandOpen);
            assert_token("}", Token::CommandClose);
            assert_token("[", Token::PropertyOpen);
            assert_token("]", Token::PropertyClose);
        }

        #[test]
        fn directives() {
            assert_token("#define", Token::Define);
            assert_token("#undef", Token::Undefine);
            assert_token("#include", Token::Include);
            assert_token("#include_opt", Token::IncludeOptional);
            assert_token("#merge", Token::Merge);
            assert_token("#autorun", Token::Autorun);
            assert_token("#ifdef", Token::Ifdef);
            assert_token("#ifndef", Token::Ifndef);
            assert_token("#else", Token::Else);
            assert_token("#endif", Token::Endif);
            assert_token("#bad", Token::BadDirective("bad"));
        }

        #[test]
        fn comments() {
            assert_token("; comment", Token::Comment);
            assert_token(";comment", Token::Comment);
            assert_tokens(
                "/* comment */",
                vec![Token::BlockCommentStart("/* comment "), Token::BlockCommentEnd("*/")],
            );

            // These get parsed as symbols in the original lexer
            assert_token("a;symbol", Token::Symbol("a;symbol"));
            assert_token("/**/", Token::Symbol("/**/"));
            assert_token("/*****/", Token::Symbol("/*****/"));
            assert_token("/*comment*/", Token::Symbol("/*comment*/"));
        }

        #[test]
        fn whitespace() {
            assert_token("\n", Token::Newline);
            assert_token("\r\n", Token::Newline);
            assert_token("\n\t", Token::Newline);
        }

        #[test]
        fn thorough() {
            use Token::*;

            let text = include_str!("../../tests/test_files/thorough.dta").replace("\r\n", "\n");

            #[rustfmt::skip]
            let tokens = vec![
                // integers
                Comment, Newline,
                Integer(1), Integer(2), Integer(-3), Newline,
                Integer(1), Integer(2), Integer(-3), Newline,
                Newline,

                // hex numbers
                Comment, Newline,
                Integer(0x1), Integer(0xA), Integer(0xa), Newline,
                Integer(0xFFFFFFFF), Newline,
                Integer(0xFFFFFFFFFFFFFFFFu64 as i64), Newline,
                // invalid (lexed as symbols)
                Comment, Newline,
                Symbol("0x"), Symbol("x1"), Newline,
                Symbol("+0x2"), Symbol("-0x3"), Newline,
                Symbol("+0xB"), Symbol("-0xC"), Newline,
                Symbol("+0xb"), Symbol("-0xc"), Newline,
                Newline,

                // floats
                Comment, Newline,
                Float(1.0), Float(2.0), Float(-3.0), Newline,
                Float(1.0), Float(2.0), Float(-3.0), Newline,
                Float(0.1), Float(0.2), Float(-0.3), Newline,
                // these are valid
                Comment, Newline,
                Float(0.0), Float(0.0), Float(-0.0), Newline,
                Newline,

                // floats with exponents
                Comment, Newline,
                // valid                                          -  invalid
                Comment, Newline,
                Float(1.0E1),  Float(2.0E1),  Float(-3.0E1),      Symbol("1.0-E1"),  Symbol("+2.0-E1"),  Symbol("-3.0-E1"),  Newline,
                Float(1.0E+1), Float(2.0E+1), Float(-3.0E+1),     Symbol("1.0-E+1"), Symbol("+2.0-E+1"), Symbol("-3.0-E+1"), Newline,
                Float(1.0E-1), Float(2.0E-1), Float(-3.0E-1),     Symbol("1.0-E-1"), Symbol("+2.0-E-1"), Symbol("-3.0-E-1"), Newline,
                Newline,
                Float(1.0E1),  Float(2.0E1),  Float(-3.0E1),      Symbol("1.-E1"),   Symbol("+2.-E1"),   Symbol("-3.-E1"),   Newline,
                Float(1.0E+1), Float(2.0E+1), Float(-3.0E+1),     Symbol("1.-E+1"),  Symbol("+2.-E+1"),  Symbol("-3.-E+1"),  Newline,
                Float(1.0E-1), Float(2.0E-1), Float(-3.0E-1),     Symbol("1.-E-1"),  Symbol("+2.-E-1"),  Symbol("-3.-E-1"),  Newline,
                Newline,
                Float(0.1E1),  Float(0.2E1),  Float(-0.3E1),      Symbol(".1-E1"),   Symbol("+.2-E1"),   Symbol("-.3-E1"),   Newline,
                Float(0.1E+1), Float(0.2E+1), Float(-0.3E+1),     Symbol(".1-E+1"),  Symbol("+.2-E+1"),  Symbol("-.3-E+1"),  Newline,
                Float(0.1E-1), Float(0.2E-1), Float(-0.3E-1),     Symbol(".1-E-1"),  Symbol("+.2-E-1"),  Symbol("-.3-E-1"),  Newline,
                Newline,
                Float(0.0E1),  Float(0.0E1),  Float(-0.0E1),      Symbol(".-E1"),    Symbol("+.-E1"),    Symbol("-.-E1"),    Newline,
                Float(0.0E+1), Float(0.0E+1), Float(-0.0E+1),     Symbol(".-E+1"),   Symbol("+.-E+1"),   Symbol("-.-E+1"),   Newline,
                Float(0.0E-1), Float(0.0E-1), Float(-0.0E-1),     Symbol(".-E-1"),   Symbol("+.-E-1"),   Symbol("-.-E-1"),   Newline,
                Newline,

                // strings
                Comment, Newline,
                String("asdf"), Newline,
                String(""), String(""), Newline,
                Newline,
                String(
                    "\n\
                    asdf\n\
                    jkl\n\
                    qwerty\
                    \n"
                ), Newline,
                Newline,

                Newline,

                // symbols
                Comment, Newline,
                Symbol("asdf"), Newline,
                Symbol("jkl"), Newline,
                Symbol("qwerty"), Newline,
                Newline,

                // quoted symbols
                Comment, Newline,
                Symbol("asdf"), Newline,
                Symbol(""), Symbol(""), Newline,
                Newline,
                Symbol(
                    "\n\
                    asdf\n\
                    jkl\n\
                    qwerty\
                    \n"
                ), Newline,
                Newline,

                // variables
                Comment, Newline,
                Variable("asdf"), Newline,
                Variable("jkl"), Newline,
                Variable("qwerty"), Newline,
                Newline,

                // kDataUnhandled is its own token
                Comment, Newline,
                Unhandled, Newline,
                Newline,

                Newline,

                // arrays
                Comment, Newline,
                ArrayOpen, Symbol("array"), Integer(1), Integer(2), ArrayClose, Comment, Newline,
                CommandOpen, Symbol("+"), Integer(1), Integer(2), CommandClose, Comment, Newline,
                PropertyOpen, Symbol("property"), PropertyClose, Comment, Newline,
                Newline,

                Newline,

                // directives
                Comment, Newline,
                IncludeOptional, Symbol("../file.dta"), Newline,
                Include, Symbol("../file.dta"), Newline,
                Merge, Symbol("../file.dta"), Newline,
                Ifdef, Symbol("kDefine"), Newline,
                Undefine, Symbol("kDefine"), Newline,
                Endif, Newline,
                Ifndef, Symbol("kDefine"), Newline,
                Define, Symbol("kDefine"), Newline,
                Else, Newline,
                Autorun, CommandOpen, Symbol("action"), CommandClose, Newline,
                Endif, Newline,
                // invalid
                Comment, Newline,
                BadDirective("bad"), Newline,
                BadDirective("#"), Newline,
                Newline,

                // *not* directives, these are lexed as symbols
                Comment, Newline,
                Symbol("#"), Newline,
                Symbol("#"), Symbol("#"), Comment, Newline,
                Symbol("#"), Symbol("#"), Comment, Newline,
                // lexed as symbols and arrays
                Comment, Newline,
                Symbol("#"), ArrayOpen, Symbol("#"), ArrayClose, Comment, Newline,
                Symbol("#"), CommandOpen, Symbol("#"), CommandClose, Comment, Newline,
                Symbol("#"), PropertyOpen, Symbol("#"), PropertyClose, Comment, Newline,
                Newline,

                Newline,

                // line comment
                Comment, Newline,
                Comment, Newline, // ;;
                Comment, Newline, // ; ;
                Comment, Newline, // ;	;
                Comment, Newline, // ;;;;;;;;
                Comment, Newline, // ;nospace
                Symbol("asdf;jkl"), Comment, Newline,
                Newline,

                // block comment
                BlockCommentStart("/*"), Newline,
                Symbol("block"), Symbol("comment"), Newline,
                BlockCommentEnd("*/"), Newline,
                Newline,

                Symbol("/*asdf*/"), Comment, Newline,
                BlockCommentStart("/*jkl "), BlockCommentEnd("*/"), Newline,
                Newline,
                Symbol("/**/"), Comment, Newline,
                BlockCommentStart("/* "), BlockCommentEnd("*/"), Newline,
                BlockCommentStart("/*\t"), BlockCommentEnd("*/"), Newline,
                Newline,

                // stray block-comment close, lexed as a symbol
                // note: handled by the parser
                Comment, Newline,
                BlockCommentEnd("*/"), Newline,
                Newline,

                Symbol("/*****/"), Comment, Newline,
                Newline,

                Symbol("/*****"), Comment, Newline,
                Newline,
                BlockCommentStart("/*"), Newline,
                Newline,
                Symbol("*****"), Newline,
                Newline,
                BlockCommentEnd("***/"), Newline,
            ];

            assert_tokens(&text, tokens);
        }
    }
}
