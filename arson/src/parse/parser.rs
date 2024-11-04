use std::iter::Peekable;

use logos::Span;

use super::lexer::{LexError, Token, TokenKind};

pub enum ExpressionKind<'src> {
    Integer(i64),
    Float(f64),
    String(&'src str),

    Symbol(&'src str),
    Variable(&'src str),
    Unhandled,

    Array(Vec<Expression<'src>>),
    Command(Vec<Expression<'src>>),
    Property(Vec<Expression<'src>>),
}

pub struct Expression<'src> {
    pub kind: ExpressionKind<'src>,
    pub location: Span,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    UnmatchedBrace(Span, ArrayKind),
    BadDirective(Span),
    InvalidToken(LexError),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ArrayKind {
    Array,
    Command,
    Property,
}

#[derive(Debug, Clone)]
struct ArrayMarker {
    kind: ArrayKind,
    location: Span,
}

#[derive(Clone)]
struct Parser {
    array_stack: Vec<ArrayMarker>,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            array_stack: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn parse<'src, I: Iterator<Item = Token<'src>>>(&mut self, tokens: &mut Peekable<I>) -> Vec<Expression<'src>> {
        let mut exprs = Vec::new();

        loop {
            match self.parse_node(tokens) {
                Some(node) => exprs.push(node),
                None => break,
            };
        }

        exprs
    }

    fn parse_array<'src, I: Iterator<Item = Token<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
        kind: ArrayKind,
        start: Span,
    ) -> Vec<Expression<'src>> {
        self.array_stack.push(ArrayMarker { kind, location: start });
        let array = self.parse(tokens);
        self.array_stack.pop();

        array
    }

    fn open_array<'src, I: Iterator<Item = Token<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
        token: &mut Token<'src>,
        kind: ArrayKind,
    ) -> Vec<Expression<'src>> {
        let array = self.parse_array(tokens, kind, token.location.clone());
        if let Some(last) = array.last() {
            token.location.end = last.location.end;
        }
        array
    }

    fn close_array<'src>(&mut self, kind: ArrayKind, end: Span) -> bool {
        match self.array_stack.last() {
            Some(last) => {
                if kind != last.kind {
                    return self.recover_mismatched_array(kind, end);
                }
            },
            None => {
                self.errors.push(ParseError::UnmatchedBrace(end, kind));
                return false;
            },
        }

        return true;
    }

    fn recover_mismatched_array<'src>(&mut self, kind: ArrayKind, end: Span) -> bool {
        todo!()
    }

    fn parse_node<'src, I: Iterator<Item = Token<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
    ) -> Option<Expression<'src>> {
        loop {
            let mut token = match tokens.next() {
                None => {
                    if let Some(unmatched) = self.array_stack.last() {
                        self.errors
                            .push(ParseError::UnmatchedBrace(unmatched.location.clone(), unmatched.kind));
                    }

                    return None;
                },
                Some(token) => token,
            };

            let expr = match token.kind {
                TokenKind::Integer(value) => ExpressionKind::Integer(value),
                TokenKind::Float(value) => ExpressionKind::Float(value),
                TokenKind::String(value) => ExpressionKind::String(value),
                TokenKind::Symbol(value) => ExpressionKind::Symbol(value),
                TokenKind::Variable(value) => ExpressionKind::Variable(value),
                TokenKind::Unhandled => ExpressionKind::Unhandled,

                TokenKind::ArrayOpen => {
                    let array = self.open_array(tokens, &mut token, ArrayKind::Array);
                    ExpressionKind::Array(array)
                },
                TokenKind::CommandOpen => {
                    let array = self.open_array(tokens, &mut token, ArrayKind::Command);
                    ExpressionKind::Command(array)
                },
                TokenKind::PropertyOpen => {
                    let array = self.open_array(tokens, &mut token, ArrayKind::Property);
                    ExpressionKind::Property(array)
                },
                TokenKind::ArrayClose => match self.close_array(ArrayKind::Array, token.location) {
                    true => return None,
                    false => continue,
                },
                TokenKind::CommandClose => match self.close_array(ArrayKind::Command, token.location) {
                    true => return None,
                    false => continue,
                },
                TokenKind::PropertyClose => match self.close_array(ArrayKind::Property, token.location) {
                    true => return None,
                    false => continue,
                },

                TokenKind::Ifdef => todo!(),
                TokenKind::Ifndef => todo!(),
                TokenKind::Else => todo!(),
                TokenKind::Endif => todo!(),

                TokenKind::Define => todo!(),
                TokenKind::Undefine => todo!(),
                TokenKind::Include => todo!(),
                TokenKind::IncludeOptional => todo!(),
                TokenKind::Merge => todo!(),
                TokenKind::Autorun => todo!(),

                TokenKind::BadDirective(_) => {
                    self.errors.push(ParseError::BadDirective(token.location));
                    continue;
                },

                TokenKind::Comment => continue,
                TokenKind::BlockCommentStart(_) => {
                    self.skip_block_comment(tokens);
                    continue;
                },
                TokenKind::BlockCommentEnd(_) => continue,
            };

            return Some(Expression { kind: expr, location: token.location });
        }
    }

    fn skip_block_comment<'src, I: Iterator<Item = Token<'src>>>(&mut self, tokens: &mut Peekable<I>) {
        while let Some(token) = tokens.peek() {
            let TokenKind::BlockCommentEnd(_) = token.kind else {
                tokens.next();
                continue;
            };
            break;
        }
    }
}

fn filter_token_errors(tokens: Vec<Result<Token<'_>, LexError>>) -> Result<Vec<Token<'_>>, Vec<ParseError>> {
    let mut errors = Vec::new();
    let tokens = tokens
        .into_iter()
        .filter_map(|t| match t {
            Ok(token) => Some(token),
            Err(error) => {
                errors.push(ParseError::InvalidToken(error));
                None
            },
        })
        .collect();

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(tokens)
}

pub fn parse(tokens: Vec<Result<Token<'_>, LexError>>) -> Result<Vec<Expression>, Vec<ParseError>> {
    let tokens = filter_token_errors(tokens)?;
    let mut parser = Parser::new();
    let exprs = parser.parse(&mut tokens.into_iter().peekable());
    match parser.errors.is_empty() {
        true => Ok(exprs),
        false => Err(parser.errors),
    }
}
