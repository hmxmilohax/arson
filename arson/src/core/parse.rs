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

#[derive(logos::Logos, Debug)]
#[logos(error = TokenError)]
enum Token<'src> {
    #[regex(r#"[+-][0-9]+"#, |lex| lex.slice().parse::<i64>())]
    #[regex(r#"0x[A-Fa-f0-9]+"#, |lex| i64::from_str_radix(lex.slice(), 16))]
    Integer(i64),
    // This one allows some weird things, such as ".", "+.", and "-.E1"
    #[regex(r#"[+-][0-9]*\.[0-9]*([Ee][+-]?[0-9])?"#, |lex| lex.slice().parse::<f64>())]
    Float(f64),
    #[regex(r#"\"[^\"]*\""#, trim_delimiters)]
    String(&'src str),

    // Symbol consumes almost all input which doesn't match any other token,
    // including technically malformed versions of integers/floats
    #[regex(r#"[^ \t\n\r\(\)\[\]\{\}]+"#, priority = 1)] // note: symbols take priority over comments
    #[regex(r#"\'[^\']*\'"#, trim_delimiters)]
    Symbol(&'src str),
    #[regex(r#"\$[^ \t\n\r\(\)\[\]\{\}]+"#, |lex| lex.slice().get(1..).ok_or(TokenError::DelimiterError))]
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

    #[regex(r#"#[^ \t\n\r\(\)\[\]\{\}]+"#)]
    BadDirective(&'src str),

    #[token("\n")]
    Newline,
    #[regex(r#";[^\n]*"#, logos::skip, priority = 0)]
    Comment,
    #[token("/*")]
    BlockCommentStart,
    #[token("*/")]
    BlockCommentEnd,
}

fn trim_delimiters<'src>(lex: &mut Lexer<'src, Token<'src>>) -> Result<&'src str, TokenError> {
    let text = lex.slice();
    text.get(1..text.len() - 1)
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
