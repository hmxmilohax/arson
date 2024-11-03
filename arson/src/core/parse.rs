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
        .map_err(TokenError::IntegerError)
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

    fn parse_array(
        &mut self,
        context: &mut Context,
        lexer: &mut Tokenizer<'_>,
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

    fn parse_node(&mut self, context: &mut Context, lexer: &mut Tokenizer<'_>) -> crate::Result<NodeParseStatus> {
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
                if self.conditionals.pop().is_none() {
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
            Token::String(value) => {
                self.line_number += value.matches('\n').count();
                Node::String(value.to_owned())
            },
            Token::Symbol(value) => {
                self.line_number += value.matches('\n').count();
                Node::Symbol(context.add_symbol(value))
            },
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

            token => todo!("Token {token:?} not yet handled"),
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
