// SPDX-License-Identifier: LGPL-3.0-or-later

use logos::{Lexer, Logos};

use super::{Context, Node, NodeArray, NodeCommand, NodeProperty};
use crate::arson_fail;

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
// Behavior is replicated as closely as possible, with the only exception being block comments,
// as those are rather badly handled in the original lex file.

#[derive(logos::Logos, Debug, PartialEq)]
#[logos(error = TokenError)]
#[logos(skip r#"[ \v\t\r\f]+"#)]
enum Token<'src> {
    #[regex(r#"[+-]?[0-9]+"#, |lex| lex.slice().parse::<i64>(), priority = 2)]
    #[regex(r#"0x[A-Fa-f0-9]+"#, parse_hex, priority = 2)]
    Integer(i64),
    // This one allows some weird things, such as ".", "+.", and "-.E1"
    #[regex(r#"[+-]?[0-9]*\.[0-9]*([Ee][+-]?[0-9])?"#, |lex| lex.slice().parse::<f64>(), priority = 2)]
    Float(f64),
    #[regex(r#""[^"]*""#, |lex| trim_delimiters(lex.slice(), 1, 1))]
    String(&'src str),

    // Symbol consumes almost all input which doesn't match any other token,
    // including technically malformed versions of integers/floats
    #[regex(r#"[^ \v\t\r\f\(\)\[\]\{\}]+"#, priority = 0)]
    #[regex(r#"'[^']*'"#, |lex| trim_delimiters(lex.slice(), 1, 1))]
    Symbol(&'src str),
    #[regex(r#"\$[^ \v\t\r\f\(\)\[\]\{\}]+"#, |lex| trim_delimiters(lex.slice(), 1, 0))]
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

    #[regex(r#"#[^ \v\t\r\f\(\)\[\]\{\}]+"#)]
    BadDirective(&'src str),

    #[token("\n")]
    Newline,
    #[regex(r#";[^\n]*"#, priority = 1)]
    Comment,
    #[token("/*")]
    BlockCommentStart,
    #[token("*/")]
    BlockCommentEnd,
}

fn parse_hex<'src>(lex: &mut Lexer<'src, Token<'src>>) -> Result<i64, TokenError> {
    let trimmed = trim_delimiters(lex.slice(), 2, 0)?;
    i64::from_str_radix(trimmed, 16).map_err(|err| TokenError::IntegerError(err))
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
    block_comment: bool,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            line_number: 1,
            array_open: ArrayType::None,
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
                open => arson_fail!("{open:?} closed incorrectly."),
            },
            Some(token) => match token {
                Ok(token) => token,
                Err(error) => arson_fail!("Internal parser error: {error}"),
            },
        };

        let array_close = |array_type: ArrayType| -> crate::Result<NodeParseStatus> {
            if self.array_open != array_type {
                arson_fail!(
                    "Improper array close! Expected {:?}, found {:?}.",
                    self.array_open,
                    array_type
                );
            }
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
            Token::BlockCommentStart => {
                self.block_comment = true;
                return Ok(NodeParseStatus::Continue);
            },
            Token::BlockCommentEnd => {
                self.block_comment = false;
                return Ok(NodeParseStatus::Continue);
            },

            // Skip everything else during necessary conditions
            _ if self.block_comment => {
                return Ok(NodeParseStatus::Continue);
            },

            Token::Integer(value) => Node::Integer(value),
            Token::Float(value) => Node::Float(value),
            Token::String(value) => Node::String(value.to_owned()),
            Token::Symbol(value) => Node::Symbol(context.add_symbol(value)),
            Token::Variable(value) => Node::Symbol(context.add_symbol(value)),
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
    parser.parse_array(context, &mut Token::lexer(text), ArrayType::Array)
}

#[cfg(test)]
mod tests {
    use super::*;

    mod tokens {
        use super::*;

        macro_rules! assert_token_error {
            ($text:expr, $type:ident) => {
                let tokens = Vec::from_iter(Token::lexer($text));
                assert_eq!(tokens.len(), 1);
                match &tokens[0] {
                    Ok(token) => panic!("expected error result, got token {token:?}"),
                    Err(err) => match err {
                        TokenError::$type(_) => (),
                        err => panic!(
                            "expected error {}, got error {:?}",
                            stringify!(TokenError::$type),
                            err
                        ),
                    },
                }
            };
        }

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

            assert_token_error!(".", FloatError);
            assert_token_error!("+.", FloatError);
            assert_token_error!("-.", FloatError);
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
            assert_token("#bad", Token::BadDirective("#bad"));
        }

        #[test]
        fn comments() {
            assert_token("; comment", Token::Comment);
            assert_token(";comment", Token::Comment);
            assert_tokens(
                "/* comment */",
                vec![
                    Token::BlockCommentStart,
                    Token::Symbol("comment"),
                    Token::BlockCommentEnd,
                ],
            );

            // These get parsed as symbols in the original lexer
            assert_token("a;symbol", Token::Symbol("a;symbol"));
            assert_token("/**/", Token::Symbol("/**/"));
            assert_token("/*****/", Token::Symbol("/*****/"));
            assert_token("/*comment*/", Token::Symbol("/*comment*/"));

            assert_token("/** */", Token::Symbol("/**/"));

            assert_tokens(
                r#"
                /*****

                    /*

                    *****

                ***/
                "#,
                vec![
                    Token::Symbol("/*****"),
                    Token::BlockCommentStart,
                    Token::Symbol("*****"),
                    Token::BlockCommentEnd,
                ],
            )
        }

        #[test]
        fn whitespace() {
            assert_token("\n", Token::Newline);
            assert_token("\r\n", Token::Newline);
            assert_token("\n\t", Token::Newline);
        }
    }
}
