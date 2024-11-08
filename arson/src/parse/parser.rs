// SPDX-License-Identifier: LGPL-3.0-or-later

use std::iter::Peekable;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use logos::Span;

use super::lexer::{LexError, Token, TokenKind};

#[derive(Debug, Clone, PartialEq)]
pub struct StrExpression<'src> {
    pub text: &'src str,
    pub location: Span,
}

impl<'src> StrExpression<'src> {
    fn new(text: &'src str, location: Span) -> Self {
        Self { text, location }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayExpression<'src> {
    pub exprs: Vec<Expression<'src>>,
    pub location: Span,
}

impl<'src> ArrayExpression<'src> {
    fn new(exprs: Vec<Expression<'src>>, location: Span) -> Self {
        Self { exprs, location }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum PreprocessedTokenKind<'src> {
    Integer(i64),
    Float(f64),
    String(&'src str),
    Symbol(&'src str),
    Variable(&'src str),
    Unhandled,

    ArrayOpen,
    ArrayClose,
    CommandOpen,
    CommandClose,
    PropertyOpen,
    PropertyClose,

    Define(StrExpression<'src>),
    Undefine(StrExpression<'src>),
    Include(StrExpression<'src>),
    IncludeOptional(StrExpression<'src>),
    Merge(StrExpression<'src>),
    Autorun,

    Conditional {
        is_positive: bool,
        symbol: StrExpression<'src>,
        true_branch: (Vec<PreprocessedToken<'src>>, Span),
        false_branch: Option<(Vec<PreprocessedToken<'src>>, Span)>,
    },

    Error(LexError),
}

#[derive(Debug, Clone, PartialEq)]
struct PreprocessedToken<'src> {
    kind: PreprocessedTokenKind<'src>,
    location: Span,
}

#[derive(Debug, Clone, PartialEq)]
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

    Define(StrExpression<'src>, ArrayExpression<'src>),
    Undefine(StrExpression<'src>),
    Include(StrExpression<'src>),
    IncludeOptional(StrExpression<'src>),
    Merge(StrExpression<'src>),
    Autorun(ArrayExpression<'src>),

    Conditional {
        is_positive: bool,
        symbol: StrExpression<'src>,
        true_branch: ArrayExpression<'src>,
        false_branch: Option<ArrayExpression<'src>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'src> {
    pub kind: ExpressionKind<'src>,
    pub location: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError<'src> {
    UnmatchedBrace(Span, ArrayKind),

    UnexpectedConditional(Span),
    UnmatchedConditional(Span),
    UnbalancedConditional(Span),
    BadDirective(Span),

    TokenError(Span, LexError),
    IncorrectToken {
        expected: TokenKind<'static>,
        actual: Token<'src>,
    },

    UnexpectedEof,
}

impl<'src> ParseError<'src> {
    pub fn to_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        match self {
            ParseError::UnexpectedEof => Diagnostic::error()
                .with_code("DTA0000")
                .with_message("unexpected end of file"),
            ParseError::TokenError(range, lex_error) => Diagnostic::error()
                .with_code("DTA0001")
                .with_message(format!("internal tokenizer error: {lex_error}"))
                .with_labels(vec![Label::primary(file_id, range.clone())]),
            ParseError::BadDirective(range) => Diagnostic::error()
                .with_code("DTA0002")
                .with_message("unrecognized parser directive")
                .with_labels(vec![Label::primary(file_id, range.clone())]),
            ParseError::UnexpectedConditional(range) => Diagnostic::error()
                .with_code("DTA0003")
                .with_message("unexpected conditional directive")
                .with_labels(vec![Label::primary(file_id, range.clone())]),
            ParseError::UnmatchedConditional(range) => Diagnostic::error()
                .with_code("DTA0004")
                .with_message("unmatched conditional directive")
                .with_notes(vec![
                    "#else or #endif required to close the conditional block".to_owned()
                ])
                .with_labels(vec![Label::primary(file_id, range.clone())]),
            ParseError::UnbalancedConditional(range) => Diagnostic::error()
                .with_code("DTA0005")
                .with_message("unbalanced conditional block")
                .with_notes(vec!["all arrays in conditionals must be self-contained".to_owned()])
                .with_labels(vec![Label::primary(file_id, range.clone())]),
            ParseError::UnmatchedBrace(range, array_kind) => Diagnostic::error()
                .with_code("DTA0006")
                .with_message(format!("unmatched {array_kind} delimiter"))
                .with_labels(vec![Label::primary(file_id, range.clone())]),
            ParseError::IncorrectToken { expected, actual } => Diagnostic::error()
                .with_code("DTA0007")
                .with_message(format!(
                    "expected token of type {}, found {} instead",
                    expected, actual.kind
                ))
                .with_labels(vec![Label::primary(file_id, actual.location.clone())]),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ArrayKind {
    Array,
    Command,
    Property,
}

impl std::fmt::Display for ArrayKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArrayKind::Array => write!(f, "array"),
            ArrayKind::Command => write!(f, "command"),
            ArrayKind::Property => write!(f, "property"),
        }
    }
}

#[derive(Debug, Clone)]
struct ArrayMarker {
    kind: ArrayKind,
    location: Span,
}

struct ConditionalMarker<'src> {
    location: Span,
    is_positive: bool,
    symbol: StrExpression<'src>,
    false_location: Option<Span>,
    false_branch: Option<(Vec<PreprocessedToken<'src>>, Span)>,
}

enum ProcessResult<T> {
    Result(T),
    BlockEnd(Span),
    SkipToken,
    Eof,
}

struct Preprocessor<'src> {
    conditional_stack: Vec<ConditionalMarker<'src>>,
    errors: Vec<ParseError<'src>>,
    unexpected_eof: bool,
}

macro_rules! next_token {
    ($self:expr, $tokens:expr, $expected:ident) => {{
        let Some(next) = $tokens.peek() else {
            return $self.unexpected_eof();
        };
        let TokenKind::$expected = next.kind else {
            return $self.incorrect_token(TokenKind::$expected, next.clone());
        };
        let location = next.location.clone();
        $tokens.next();
        location
    }};
    ($self:expr, $tokens:expr, $expected:ident($dummy:expr)) => {{
        let Some(next) = $tokens.peek() else {
            return $self.unexpected_eof();
        };
        let TokenKind::$expected(value) = next.kind else {
            return $self.incorrect_token(TokenKind::$expected($dummy), next.clone());
        };
        let result = (value, next.location.clone());
        $tokens.next();
        result
    }};
}

impl<'src> Preprocessor<'src> {
    pub fn new() -> Self {
        Self {
            conditional_stack: Vec::new(),
            errors: Vec::new(),
            unexpected_eof: false,
        }
    }

    pub fn preprocess<I: Iterator<Item = Token<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
    ) -> (Vec<PreprocessedToken<'src>>, Span) {
        let result = self.preprocess_loop(tokens);
        if self.unexpected_eof {
            self.errors.push(ParseError::UnexpectedEof);
        }
        result
    }

    fn preprocess_loop<I: Iterator<Item = Token<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
    ) -> (Vec<PreprocessedToken<'src>>, Span) {
        let mut processed = Vec::new();

        loop {
            match self.process_token(tokens) {
                ProcessResult::Result(node) => processed.push(node),
                ProcessResult::BlockEnd(end_location) => return (processed, end_location),
                ProcessResult::SkipToken => continue,
                ProcessResult::Eof => {
                    self.verify_eof();
                    match processed.last().cloned() {
                        Some(last) => return (processed, last.location.clone()),
                        None => return (processed, 0..0),
                    }
                },
            };
        }
    }

    fn symbol_directive<I: Iterator<Item = Token<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
        location: Span,
        kind: impl Fn(StrExpression<'src>) -> PreprocessedTokenKind<'src>,
    ) -> ProcessResult<PreprocessedToken<'src>> {
        let (name, name_location) = next_token!(self, tokens, Symbol("dummy"));
        let name = StrExpression::new(name, name_location.clone());
        let location = location.start..name_location.end;
        ProcessResult::Result(PreprocessedToken { kind: kind(name), location })
    }

    fn process_token<I: Iterator<Item = Token<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
    ) -> ProcessResult<PreprocessedToken<'src>> {
        let token = match tokens.next() {
            None => return ProcessResult::Eof,
            Some(token) => token,
        };

        let kind = match token.kind {
            TokenKind::Integer(value) => PreprocessedTokenKind::Integer(value),
            TokenKind::Float(value) => PreprocessedTokenKind::Float(value),
            TokenKind::String(value) => PreprocessedTokenKind::String(value),
            TokenKind::Symbol(value) => PreprocessedTokenKind::Symbol(value),
            TokenKind::Variable(value) => PreprocessedTokenKind::Variable(value),
            TokenKind::Unhandled => PreprocessedTokenKind::Unhandled,

            TokenKind::ArrayOpen => PreprocessedTokenKind::ArrayOpen,
            TokenKind::ArrayClose => PreprocessedTokenKind::ArrayClose,
            TokenKind::CommandOpen => PreprocessedTokenKind::CommandOpen,
            TokenKind::CommandClose => PreprocessedTokenKind::CommandClose,
            TokenKind::PropertyOpen => PreprocessedTokenKind::PropertyOpen,
            TokenKind::PropertyClose => PreprocessedTokenKind::PropertyClose,

            TokenKind::Define => return self.symbol_directive(tokens, token.location, PreprocessedTokenKind::Define),
            TokenKind::Undefine => {
                return self.symbol_directive(tokens, token.location, PreprocessedTokenKind::Undefine)
            },
            TokenKind::Include => return self.symbol_directive(tokens, token.location, PreprocessedTokenKind::Include),
            TokenKind::IncludeOptional => {
                return self.symbol_directive(tokens, token.location, PreprocessedTokenKind::IncludeOptional)
            },
            TokenKind::Merge => return self.symbol_directive(tokens, token.location, PreprocessedTokenKind::Merge),
            TokenKind::Autorun => PreprocessedTokenKind::Autorun,

            TokenKind::Ifdef => {
                let (name, name_location) = next_token!(self, tokens, Symbol("dummy"));
                return self.parse_conditional(tokens, token.location, true, StrExpression::new(name, name_location));
            },
            TokenKind::Ifndef => {
                let (name, name_location) = next_token!(self, tokens, Symbol("dummy"));
                return self.parse_conditional(tokens, token.location, false, StrExpression::new(name, name_location));
            },
            TokenKind::Else => {
                match self.conditional_stack.last_mut() {
                    Some(conditional) => {
                        conditional.false_location = Some(token.location.clone());
                    },
                    None => {
                        self.errors
                            .push(ParseError::UnexpectedConditional(token.location.clone()));
                        self.conditional_stack.push(ConditionalMarker {
                            location: token.location.clone(),
                            is_positive: true,
                            symbol: StrExpression::new("<invalid>", token.location.clone()),
                            false_location: Some(token.location.clone()),
                            false_branch: None,
                        });
                    },
                }

                let (exprs, location) = self.parse_conditional_block(tokens, token.location.clone());
                if let Some(conditional) = self.conditional_stack.last_mut() {
                    conditional.false_branch = Some((exprs, location));
                }

                return ProcessResult::BlockEnd(token.location);
            },
            TokenKind::Endif => match self.conditional_stack.last() {
                Some(_) => return ProcessResult::BlockEnd(token.location),
                None => {
                    self.errors
                        .push(ParseError::UnexpectedConditional(token.location));
                    return ProcessResult::SkipToken;
                },
            },

            TokenKind::BadDirective(_) => {
                self.errors.push(ParseError::BadDirective(token.location));
                return ProcessResult::SkipToken;
            },

            TokenKind::Comment => return ProcessResult::SkipToken,
            TokenKind::BlockCommentStart(_) => {
                self.skip_block_comment(tokens);
                return ProcessResult::SkipToken;
            },
            TokenKind::BlockCommentEnd(_) => return ProcessResult::SkipToken,

            TokenKind::Error(error) => PreprocessedTokenKind::Error(error),
        };

        ProcessResult::Result(PreprocessedToken { kind, location: token.location })
    }

    fn parse_conditional<I: Iterator<Item = Token<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
        start: Span,
        positive: bool,
        symbol: StrExpression<'src>,
    ) -> ProcessResult<PreprocessedToken<'src>> {
        self.conditional_stack.push(ConditionalMarker {
            location: start.clone(),
            is_positive: positive,
            symbol,
            false_location: None,
            false_branch: None,
        });

        let (true_exprs, true_location) = self.parse_conditional_block(tokens, start);
        let conditional = self
            .conditional_stack
            .pop()
            .expect("conditional was added just above");

        let location = match &conditional.false_branch {
            Some(false_branch) => conditional.location.start..false_branch.1.end,
            None => conditional.location.start..true_location.end,
        };

        ProcessResult::Result(PreprocessedToken {
            kind: PreprocessedTokenKind::Conditional {
                is_positive: conditional.is_positive,
                symbol: conditional.symbol,
                true_branch: (true_exprs, true_location),
                false_branch: conditional.false_branch,
            },
            location,
        })
    }

    fn parse_conditional_block<I: Iterator<Item = Token<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
        start: Span,
    ) -> (Vec<PreprocessedToken<'src>>, Span) {
        let (exprs, next_directive_location) = self.preprocess_loop(tokens);
        let location = start.start..next_directive_location.end;
        (exprs, location)
    }

    fn verify_eof(&mut self) {
        for unmatched in &self.conditional_stack {
            let location = match &unmatched.false_location {
                Some(else_location) => else_location.clone(),
                None => unmatched.location.clone(),
            };
            self.errors
                .push(ParseError::UnmatchedConditional(location.clone()));
            self.unexpected_eof = true;
        }
    }

    fn skip_block_comment<I: Iterator<Item = Token<'src>>>(&mut self, tokens: &mut Peekable<I>) {
        while let Some(token) = tokens.peek() {
            let TokenKind::BlockCommentEnd(_) = token.kind else {
                tokens.next();
                continue;
            };
            break;
        }
    }

    fn incorrect_token<T>(&mut self, expected: TokenKind<'static>, actual: Token<'src>) -> ProcessResult<T> {
        self.errors
            .push(ParseError::IncorrectToken { expected, actual });
        ProcessResult::SkipToken
    }

    fn unexpected_eof<T>(&mut self) -> ProcessResult<T> {
        self.errors.push(ParseError::UnexpectedEof);
        ProcessResult::Eof
    }
}

struct Parser<'src> {
    array_stack: Vec<ArrayMarker>,
    errors: Vec<ParseError<'src>>,
    unexpected_eof: bool,
}

impl<'src> Parser<'src> {
    pub fn new() -> Self {
        Self {
            array_stack: Vec::new(),
            errors: Vec::new(),
            unexpected_eof: false,
        }
    }

    pub fn parse<I: Iterator<Item = PreprocessedToken<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
    ) -> (Vec<Expression<'src>>, Span) {
        let result = self.parse_exprs(tokens);
        if self.unexpected_eof {
            self.errors.push(ParseError::UnexpectedEof);
        }
        result
    }

    fn parse_exprs<I: Iterator<Item = PreprocessedToken<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
    ) -> (Vec<Expression<'src>>, Span) {
        let mut exprs = Vec::new();

        loop {
            match self.parse_node(tokens) {
                ProcessResult::Result(node) => exprs.push(node),
                ProcessResult::BlockEnd(end_location) => return (exprs, end_location),
                ProcessResult::SkipToken => continue,
                ProcessResult::Eof => {
                    self.verify_eof();
                    match exprs.last().cloned() {
                        Some(last) => return (exprs, last.location.clone()),
                        None => return (exprs, 0..0),
                    }
                },
            };
        }
    }

    fn parse_array<I: Iterator<Item = PreprocessedToken<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
        kind: ArrayKind,
        start: Span,
    ) -> (Vec<Expression<'src>>, Span) {
        self.array_stack.push(ArrayMarker { kind, location: start });
        let array = self.parse_exprs(tokens);
        self.array_stack.pop();

        array
    }

    fn open_array<I: Iterator<Item = PreprocessedToken<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
        start: &Span,
        kind: ArrayKind,
    ) -> (Vec<Expression<'src>>, Span) {
        let (array, end) = self.parse_array(tokens, kind, start.clone());
        (array, start.start..end.end)
    }

    fn close_array<T>(&mut self, kind: ArrayKind, end: Span) -> ProcessResult<T> {
        match self.array_stack.last() {
            Some(last) => {
                if kind != last.kind {
                    self.recover_mismatched_array(kind, end)
                } else {
                    ProcessResult::BlockEnd(end)
                }
            },
            None => {
                self.errors.push(ParseError::UnmatchedBrace(end, kind));
                ProcessResult::SkipToken
            },
        }
    }

    fn recover_mismatched_array<T>(&mut self, kind: ArrayKind, end: Span) -> ProcessResult<T> {
        if !self.array_stack.iter().any(|arr| arr.kind == kind) {
            self.errors.push(ParseError::UnmatchedBrace(end, kind));
            return ProcessResult::SkipToken;
        }

        while let Some(array) = self.array_stack.last() {
            if array.kind == kind {
                break;
            }

            self.errors
                .push(ParseError::UnmatchedBrace(array.location.clone(), array.kind));
            self.array_stack.pop();
        }

        ProcessResult::BlockEnd(end)
    }

    fn parse_node<I: Iterator<Item = PreprocessedToken<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
    ) -> ProcessResult<Expression<'src>> {
        let token = match tokens.next() {
            None => return ProcessResult::Eof,
            Some(token) => token,
        };

        let (kind, location) = match token.kind {
            PreprocessedTokenKind::Integer(value) => (ExpressionKind::Integer(value), token.location),
            PreprocessedTokenKind::Float(value) => (ExpressionKind::Float(value), token.location),
            PreprocessedTokenKind::String(value) => (ExpressionKind::String(value), token.location),
            PreprocessedTokenKind::Symbol(value) => (ExpressionKind::Symbol(value), token.location),
            PreprocessedTokenKind::Variable(value) => (ExpressionKind::Variable(value), token.location),
            PreprocessedTokenKind::Unhandled => (ExpressionKind::Unhandled, token.location),

            PreprocessedTokenKind::ArrayOpen => {
                let (array, location) = self.open_array(tokens, &token.location, ArrayKind::Array);
                (ExpressionKind::Array(array), location)
            },
            PreprocessedTokenKind::CommandOpen => {
                let (array, location) = self.open_array(tokens, &token.location, ArrayKind::Command);
                (ExpressionKind::Command(array), location)
            },
            PreprocessedTokenKind::PropertyOpen => {
                let (array, location) = self.open_array(tokens, &token.location, ArrayKind::Property);
                (ExpressionKind::Property(array), location)
            },
            PreprocessedTokenKind::ArrayClose => return self.close_array(ArrayKind::Array, token.location),
            PreprocessedTokenKind::CommandClose => return self.close_array(ArrayKind::Command, token.location),
            PreprocessedTokenKind::PropertyClose => return self.close_array(ArrayKind::Property, token.location),

            PreprocessedTokenKind::Conditional { is_positive, symbol, true_branch, false_branch } => {
                let true_branch = self.parse_conditional_block(true_branch);
                let false_branch = false_branch.map(|block| self.parse_conditional_block(block));
                let expr = ExpressionKind::Conditional { is_positive, symbol, true_branch, false_branch };
                (expr, token.location)
            },

            PreprocessedTokenKind::Define(name) => {
                let start_location = {
                    let Some(next) = tokens.peek() else {
                        return self.unexpected_eof();
                    };
                    let PreprocessedTokenKind::ArrayOpen = next.kind else {
                        return self.incorrect_token(TokenKind::ArrayOpen, next.clone());
                    };
                    let location = next.location.clone();
                    tokens.next();
                    location
                };

                let (body, body_location) = self.open_array(tokens, &start_location, ArrayKind::Array);
                let body = ArrayExpression::new(body, body_location.clone());
                let location = token.location.start..body_location.end;

                (ExpressionKind::Define(name, body), location)
            },
            PreprocessedTokenKind::Undefine(name) => (ExpressionKind::Undefine(name), token.location),
            PreprocessedTokenKind::Include(path) => (ExpressionKind::Include(path), token.location),
            PreprocessedTokenKind::IncludeOptional(path) => (ExpressionKind::IncludeOptional(path), token.location),
            PreprocessedTokenKind::Merge(name) => (ExpressionKind::Merge(name), token.location),
            PreprocessedTokenKind::Autorun => {
                let start_location = {
                    let Some(next) = tokens.peek() else {
                        return self.unexpected_eof();
                    };
                    let PreprocessedTokenKind::CommandOpen = next.kind else {
                        return self.incorrect_token(TokenKind::CommandOpen, next.clone());
                    };
                    let location = next.location.clone();
                    tokens.next();
                    location
                };
                let (body, body_location) = self.open_array(tokens, &start_location, ArrayKind::Command);
                let body = ArrayExpression::new(body, body_location.clone());
                (ExpressionKind::Autorun(body), token.location.start..body_location.end)
            },

            PreprocessedTokenKind::Error(error) => {
                self.errors
                    .push(ParseError::TokenError(token.location, error));
                return ProcessResult::SkipToken;
            },
        };

        ProcessResult::Result(Expression { kind, location })
    }

    fn parse_conditional_block(
        &mut self,
        (branch, location): (Vec<PreprocessedToken<'src>>, Span),
    ) -> ArrayExpression<'src> {
        let mut block_parser = Parser::new();
        let (branch, _) = block_parser.parse_exprs(&mut branch.into_iter().peekable());
        if block_parser
            .errors
            .iter()
            .any(|e| matches!(e, ParseError::UnmatchedBrace(_, _)))
        {
            self.errors
                .push(ParseError::UnbalancedConditional(location.clone()))
        }
        self.errors.append(&mut block_parser.errors);

        ArrayExpression::new(branch, location)
    }

    fn verify_eof(&mut self) {
        for unmatched in &self.array_stack {
            self.errors
                .push(ParseError::UnmatchedBrace(unmatched.location.clone(), unmatched.kind));
            self.unexpected_eof = true;
        }
    }

    fn incorrect_token(
        &mut self,
        expected: TokenKind<'static>,
        actual: PreprocessedToken<'src>,
    ) -> ProcessResult<Expression<'src>> {
        let actual_kind = match actual.kind {
            PreprocessedTokenKind::Integer(value) => TokenKind::Integer(value),
            PreprocessedTokenKind::Float(value) => TokenKind::Float(value),
            PreprocessedTokenKind::String(value) => TokenKind::String(value),
            PreprocessedTokenKind::Symbol(value) => TokenKind::Symbol(value),
            PreprocessedTokenKind::Variable(value) => TokenKind::Variable(value),
            PreprocessedTokenKind::Unhandled => TokenKind::Unhandled,

            PreprocessedTokenKind::ArrayOpen => TokenKind::ArrayOpen,
            PreprocessedTokenKind::ArrayClose => TokenKind::ArrayClose,
            PreprocessedTokenKind::CommandOpen => TokenKind::CommandOpen,
            PreprocessedTokenKind::CommandClose => TokenKind::CommandClose,
            PreprocessedTokenKind::PropertyOpen => TokenKind::PropertyOpen,
            PreprocessedTokenKind::PropertyClose => TokenKind::PropertyClose,

            PreprocessedTokenKind::Define(_) => TokenKind::Define,
            PreprocessedTokenKind::Undefine(_) => TokenKind::Undefine,
            PreprocessedTokenKind::Include(_) => TokenKind::Include,
            PreprocessedTokenKind::IncludeOptional(_) => TokenKind::IncludeOptional,
            PreprocessedTokenKind::Merge(_) => TokenKind::Merge,
            PreprocessedTokenKind::Autorun => TokenKind::Autorun,

            PreprocessedTokenKind::Conditional { .. } => TokenKind::Ifdef,

            PreprocessedTokenKind::Error(error) => TokenKind::Error(error),
        };
        let actual = Token { kind: actual_kind, location: actual.location };

        self.errors
            .push(ParseError::IncorrectToken { expected, actual });
        ProcessResult::SkipToken
    }

    fn unexpected_eof(&mut self) -> ProcessResult<Expression<'src>> {
        self.errors.push(ParseError::UnexpectedEof);
        ProcessResult::Eof
    }
}

fn preprocess<'src>(
    tokens: impl Iterator<Item = Token<'src>>,
) -> Result<Vec<PreprocessedToken<'src>>, Vec<ParseError<'src>>> {
    let mut preprocessor = Preprocessor::new();
    let (exprs, _) = preprocessor.preprocess(&mut tokens.peekable());
    match preprocessor.errors.is_empty() {
        true => Ok(exprs),
        false => Err(preprocessor.errors),
    }
}

pub fn parse<'src>(tokens: impl Iterator<Item = Token<'src>>) -> Result<Vec<Expression<'src>>, Vec<ParseError<'src>>> {
    let preprocessed = preprocess(tokens)?;

    let mut parser = Parser::new();
    let (exprs, _) = parser.parse(&mut preprocessed.into_iter().peekable());
    match parser.errors.is_empty() {
        true => Ok(exprs),
        false => Err(parser.errors),
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::lexer;

    use super::*;

    const fn new_token(kind: TokenKind, location: Span) -> Token {
        Token { kind, location }
    }

    mod preprocessor {
        use super::*;

        const fn new_pretoken(kind: PreprocessedTokenKind<'_>, location: Span) -> PreprocessedToken<'_> {
            PreprocessedToken { kind, location }
        }

        fn assert_preprocessed(text: &str, exprs: Vec<PreprocessedToken>) {
            let tokens = lexer::lex(text);
            let result = match preprocess(tokens) {
                Ok(exprs) => exprs,
                Err(errs) => panic!("Errors encountered while preprocessing: {errs:?}"),
            };
            assert_eq!(result, exprs, "Unexpected result for '{text}'");
        }

        fn assert_errors(text: &str, errs: Vec<ParseError<'_>>) {
            let tokens = lexer::lex(text);
            let result = match preprocess(tokens) {
                Ok(preprocessed) => panic!("Expected preprocessing errors, got success instead instead.\nText: {text}\nResult: {preprocessed:?}"),
                Err(errs) => errs,
            };
            assert_eq!(result, errs, "Unexpected result for '{text}'");
        }

        #[test]
        fn integer() {
            assert_preprocessed(
                "1 2 3",
                vec![
                    new_pretoken(PreprocessedTokenKind::Integer(1), 0..1),
                    new_pretoken(PreprocessedTokenKind::Integer(2), 2..3),
                    new_pretoken(PreprocessedTokenKind::Integer(3), 4..5),
                ],
            );
        }

        #[test]
        fn float() {
            assert_preprocessed(
                "1.0 2.0 3.0",
                vec![
                    new_pretoken(PreprocessedTokenKind::Float(1.0), 0..3),
                    new_pretoken(PreprocessedTokenKind::Float(2.0), 4..7),
                    new_pretoken(PreprocessedTokenKind::Float(3.0), 8..11),
                ],
            );
        }

        #[test]
        fn string() {
            assert_preprocessed(
                "\"a\" \"b\" \"c\"",
                vec![
                    new_pretoken(PreprocessedTokenKind::String("a"), 0..3),
                    new_pretoken(PreprocessedTokenKind::String("b"), 4..7),
                    new_pretoken(PreprocessedTokenKind::String("c"), 8..11),
                ],
            );
        }

        #[test]
        fn symbol() {
            assert_preprocessed(
                "asdf + '10'",
                vec![
                    new_pretoken(PreprocessedTokenKind::Symbol("asdf"), 0..4),
                    new_pretoken(PreprocessedTokenKind::Symbol("+"), 5..6),
                    new_pretoken(PreprocessedTokenKind::Symbol("10"), 7..11),
                ],
            );
        }

        #[test]
        fn variable() {
            assert_preprocessed(
                "$asdf $this",
                vec![
                    new_pretoken(PreprocessedTokenKind::Variable("asdf"), 0..5),
                    new_pretoken(PreprocessedTokenKind::Variable("this"), 6..11),
                ],
            );
        }

        #[test]
        fn unhandled() {
            assert_preprocessed(
                "kDataUnhandled",
                vec![new_pretoken(PreprocessedTokenKind::Unhandled, 0..14)],
            )
        }

        #[test]
        fn arrays() {
            assert_preprocessed(
                "(asdf \"text\" 1)",
                vec![
                    new_pretoken(PreprocessedTokenKind::ArrayOpen, 0..1),
                    new_pretoken(PreprocessedTokenKind::Symbol("asdf"), 1..5),
                    new_pretoken(PreprocessedTokenKind::String("text"), 6..12),
                    new_pretoken(PreprocessedTokenKind::Integer(1), 13..14),
                    new_pretoken(PreprocessedTokenKind::ArrayClose, 14..15),
                ],
            );
            assert_preprocessed(
                "{set $var \"asdf\"}",
                vec![
                    new_pretoken(PreprocessedTokenKind::CommandOpen, 0..1),
                    new_pretoken(PreprocessedTokenKind::Symbol("set"), 1..4),
                    new_pretoken(PreprocessedTokenKind::Variable("var"), 5..9),
                    new_pretoken(PreprocessedTokenKind::String("asdf"), 10..16),
                    new_pretoken(PreprocessedTokenKind::CommandClose, 16..17),
                ],
            );
            assert_preprocessed(
                "[property]",
                vec![
                    new_pretoken(PreprocessedTokenKind::PropertyOpen, 0..1),
                    new_pretoken(PreprocessedTokenKind::Symbol("property"), 1..9),
                    new_pretoken(PreprocessedTokenKind::PropertyClose, 9..10),
                ],
            );
        }

        fn assert_directive_symbol_error(name: &str) {
            let text = name.to_owned() + " 1";
            assert_errors(
                &text,
                vec![ParseError::IncorrectToken {
                    expected: TokenKind::Symbol("dummy"),
                    actual: new_token(TokenKind::Integer(1), name.len() + 1..name.len() + 2),
                }],
            );
        }

        fn assert_directive_eof_error(name: &str) {
            assert_errors(name, vec![ParseError::UnexpectedEof]);
        }

        #[test]
        fn directives() {
            assert_preprocessed(
                "#define kDefine (1)",
                vec![
                    new_pretoken(
                        PreprocessedTokenKind::Define(StrExpression::new("kDefine", 8..15)),
                        0..15,
                    ),
                    new_pretoken(PreprocessedTokenKind::ArrayOpen, 16..17),
                    new_pretoken(PreprocessedTokenKind::Integer(1), 17..18),
                    new_pretoken(PreprocessedTokenKind::ArrayClose, 18..19),
                ],
            );
            assert_preprocessed(
                "#undef kDefine",
                vec![new_pretoken(
                    PreprocessedTokenKind::Undefine(StrExpression::new("kDefine", 7..14)),
                    0..14,
                )],
            );
            assert_preprocessed(
                "#include ../file.dta",
                vec![new_pretoken(
                    PreprocessedTokenKind::Include(StrExpression::new("../file.dta", 9..20)),
                    0..20,
                )],
            );
            assert_preprocessed(
                "#include_opt ../file.dta",
                vec![new_pretoken(
                    PreprocessedTokenKind::IncludeOptional(StrExpression::new("../file.dta", 13..24)),
                    0..24,
                )],
            );
            assert_preprocessed(
                "#merge ../file.dta",
                vec![new_pretoken(
                    PreprocessedTokenKind::Merge(StrExpression::new("../file.dta", 7..18)),
                    0..18,
                )],
            );
            assert_preprocessed(
                "#autorun {print \"Auto-run action\"}",
                vec![
                    new_pretoken(PreprocessedTokenKind::Autorun, 0..8),
                    new_pretoken(PreprocessedTokenKind::CommandOpen, 9..10),
                    new_pretoken(PreprocessedTokenKind::Symbol("print"), 10..15),
                    new_pretoken(PreprocessedTokenKind::String("Auto-run action"), 16..33),
                    new_pretoken(PreprocessedTokenKind::CommandClose, 33..34),
                ],
            );

            assert_directive_symbol_error("#define");
            assert_directive_symbol_error("#undef");
            assert_directive_symbol_error("#include");
            assert_directive_symbol_error("#include_opt");
            assert_directive_symbol_error("#merge");

            assert_directive_eof_error("#define");
            assert_directive_eof_error("#undef");
            assert_directive_eof_error("#include");
            assert_directive_eof_error("#include_opt");
            assert_directive_eof_error("#merge");

            assert_errors("#bad", vec![ParseError::BadDirective(0..4)]);
        }

        #[test]
        fn conditionals() {
            assert_preprocessed(
                "#ifdef kDefine (array1 10) #else (array2 5) #endif",
                vec![new_pretoken(
                    PreprocessedTokenKind::Conditional {
                        is_positive: true,
                        symbol: StrExpression::new("kDefine", 7..14),
                        true_branch: (
                            vec![
                                new_pretoken(PreprocessedTokenKind::ArrayOpen, 15..16),
                                new_pretoken(PreprocessedTokenKind::Symbol("array1"), 16..22),
                                new_pretoken(PreprocessedTokenKind::Integer(10), 23..25),
                                new_pretoken(PreprocessedTokenKind::ArrayClose, 25..26),
                            ],
                            0..32,
                        ),
                        false_branch: Some((
                            vec![
                                new_pretoken(PreprocessedTokenKind::ArrayOpen, 33..34),
                                new_pretoken(PreprocessedTokenKind::Symbol("array2"), 34..40),
                                new_pretoken(PreprocessedTokenKind::Integer(5), 41..42),
                                new_pretoken(PreprocessedTokenKind::ArrayClose, 42..43),
                            ],
                            27..50,
                        )),
                    },
                    0..50,
                )],
            );
            assert_preprocessed(
                "#ifndef kDefine (array 10) #endif",
                vec![new_pretoken(
                    PreprocessedTokenKind::Conditional {
                        is_positive: false,
                        symbol: StrExpression::new("kDefine", 8..15),
                        true_branch: (
                            vec![
                                new_pretoken(PreprocessedTokenKind::ArrayOpen, 16..17),
                                new_pretoken(PreprocessedTokenKind::Symbol("array"), 17..22),
                                new_pretoken(PreprocessedTokenKind::Integer(10), 23..25),
                                new_pretoken(PreprocessedTokenKind::ArrayClose, 25..26),
                            ],
                            0..33,
                        ),
                        false_branch: None,
                    },
                    0..33,
                )],
            );

            assert_directive_symbol_error("#ifdef");
            assert_directive_symbol_error("#ifndef");

            assert_directive_eof_error("#ifdef");
            assert_directive_eof_error("#ifndef");

            assert_errors(
                "#ifndef kDefine (array 10)",
                vec![ParseError::UnmatchedConditional(0..7), ParseError::UnexpectedEof],
            );
            assert_errors(
                "#ifdef kDefine (array1 10) #else",
                vec![ParseError::UnmatchedConditional(27..32), ParseError::UnexpectedEof],
            );
            assert_errors("#else (array2 5) #endif", vec![ParseError::UnexpectedConditional(0..5)]);
            assert_errors("(array 10) #endif", vec![ParseError::UnexpectedConditional(11..17)]);
        }
    }

    mod parser {
        use super::*;

        const fn new_expression(kind: ExpressionKind<'_>, location: Span) -> Expression<'_> {
            Expression { kind, location }
        }

        fn assert_parsed(text: &str, exprs: Vec<Expression>) {
            let tokens = lexer::lex(text);
            let result = match parse(tokens) {
                Ok(exprs) => exprs,
                Err(errs) => panic!("Errors encountered while parsing: {errs:?}"),
            };
            assert_eq!(result, exprs, "Unexpected result for '{text}'");
        }

        fn assert_errors(text: &str, errs: Vec<ParseError<'_>>) {
            let tokens = lexer::lex(text);
            let result = match parse(tokens) {
                Ok(exprs) => panic!("Expected parsing errors, got AST instead: {exprs:?}"),
                Err(errs) => errs,
            };
            assert_eq!(result, errs, "Unexpected result for '{text}'");
        }

        #[test]
        fn integer() {
            assert_parsed(
                "1 2 3",
                vec![
                    new_expression(ExpressionKind::Integer(1), 0..1),
                    new_expression(ExpressionKind::Integer(2), 2..3),
                    new_expression(ExpressionKind::Integer(3), 4..5),
                ],
            );
        }

        #[test]
        fn float() {
            assert_parsed(
                "1.0 2.0 3.0",
                vec![
                    new_expression(ExpressionKind::Float(1.0), 0..3),
                    new_expression(ExpressionKind::Float(2.0), 4..7),
                    new_expression(ExpressionKind::Float(3.0), 8..11),
                ],
            );
        }

        #[test]
        fn string() {
            assert_parsed(
                "\"a\" \"b\" \"c\"",
                vec![
                    new_expression(ExpressionKind::String("a"), 0..3),
                    new_expression(ExpressionKind::String("b"), 4..7),
                    new_expression(ExpressionKind::String("c"), 8..11),
                ],
            );
        }

        #[test]
        fn symbol() {
            assert_parsed(
                "asdf + '10'",
                vec![
                    new_expression(ExpressionKind::Symbol("asdf"), 0..4),
                    new_expression(ExpressionKind::Symbol("+"), 5..6),
                    new_expression(ExpressionKind::Symbol("10"), 7..11),
                ],
            );
        }

        #[test]
        fn variable() {
            assert_parsed(
                "$asdf $this",
                vec![
                    new_expression(ExpressionKind::Variable("asdf"), 0..5),
                    new_expression(ExpressionKind::Variable("this"), 6..11),
                ],
            );
        }

        #[test]
        fn unhandled() {
            assert_parsed("kDataUnhandled", vec![new_expression(ExpressionKind::Unhandled, 0..14)])
        }

        #[test]
        fn arrays() {
            assert_parsed(
                "(asdf \"text\" 1)",
                vec![new_expression(
                    ExpressionKind::Array(vec![
                        new_expression(ExpressionKind::Symbol("asdf"), 1..5),
                        new_expression(ExpressionKind::String("text"), 6..12),
                        new_expression(ExpressionKind::Integer(1), 13..14),
                    ]),
                    0..15,
                )],
            );
            assert_parsed(
                "{set $var \"asdf\"}",
                vec![new_expression(
                    ExpressionKind::Command(vec![
                        new_expression(ExpressionKind::Symbol("set"), 1..4),
                        new_expression(ExpressionKind::Variable("var"), 5..9),
                        new_expression(ExpressionKind::String("asdf"), 10..16),
                    ]),
                    0..17,
                )],
            );
            assert_parsed(
                "[property]",
                vec![new_expression(
                    ExpressionKind::Property(vec![new_expression(ExpressionKind::Symbol("property"), 1..9)]),
                    0..10,
                )],
            );

            assert_errors(
                "( ( )",
                vec![
                    ParseError::UnmatchedBrace(0..1, ArrayKind::Array),
                    ParseError::UnexpectedEof,
                ],
            );
            assert_errors(") ( )", vec![ParseError::UnmatchedBrace(0..1, ArrayKind::Array)]);
            assert_errors("( ) )", vec![ParseError::UnmatchedBrace(4..5, ArrayKind::Array)]);

            assert_errors(
                "{ ( )",
                vec![
                    ParseError::UnmatchedBrace(0..1, ArrayKind::Command),
                    ParseError::UnexpectedEof,
                ],
            );
            assert_errors("} ( )", vec![ParseError::UnmatchedBrace(0..1, ArrayKind::Command)]);
            assert_errors("( { )", vec![ParseError::UnmatchedBrace(2..3, ArrayKind::Command)]);
            assert_errors("( } )", vec![ParseError::UnmatchedBrace(2..3, ArrayKind::Command)]);
            assert_errors(
                "( ) {",
                vec![
                    ParseError::UnmatchedBrace(4..5, ArrayKind::Command),
                    ParseError::UnexpectedEof,
                ],
            );
            assert_errors("( ) }", vec![ParseError::UnmatchedBrace(4..5, ArrayKind::Command)]);

            assert_errors(
                "[ ( )",
                vec![
                    ParseError::UnmatchedBrace(0..1, ArrayKind::Property),
                    ParseError::UnexpectedEof,
                ],
            );
            assert_errors("] ( )", vec![ParseError::UnmatchedBrace(0..1, ArrayKind::Property)]);
            assert_errors("( [ )", vec![ParseError::UnmatchedBrace(2..3, ArrayKind::Property)]);
            assert_errors("( ] )", vec![ParseError::UnmatchedBrace(2..3, ArrayKind::Property)]);
            assert_errors(
                "( ) [",
                vec![
                    ParseError::UnmatchedBrace(4..5, ArrayKind::Property),
                    ParseError::UnexpectedEof,
                ],
            );
            assert_errors("( ) ]", vec![ParseError::UnmatchedBrace(4..5, ArrayKind::Property)]);
        }

        fn assert_directive_symbol_error(name: &str) {
            let text = name.to_owned() + " 1";
            assert_errors(
                &text,
                vec![ParseError::IncorrectToken {
                    expected: TokenKind::Symbol("dummy"),
                    actual: new_token(TokenKind::Integer(1), name.len() + 1..name.len() + 2),
                }],
            );
        }

        fn assert_directive_eof_error(name: &str) {
            assert_errors(name, vec![ParseError::UnexpectedEof]);
        }

        #[test]
        fn directives() {
            assert_parsed(
                "#define kDefine (1)",
                vec![new_expression(
                    ExpressionKind::Define(
                        StrExpression::new("kDefine", 8..15),
                        ArrayExpression::new(vec![new_expression(ExpressionKind::Integer(1), 17..18)], 16..19),
                    ),
                    0..19,
                )],
            );
            assert_parsed(
                "#undef kDefine",
                vec![new_expression(
                    ExpressionKind::Undefine(StrExpression::new("kDefine", 7..14)),
                    0..14,
                )],
            );
            assert_parsed(
                "#include ../file.dta",
                vec![new_expression(
                    ExpressionKind::Include(StrExpression::new("../file.dta", 9..20)),
                    0..20,
                )],
            );
            assert_parsed(
                "#include_opt ../file.dta",
                vec![new_expression(
                    ExpressionKind::IncludeOptional(StrExpression::new("../file.dta", 13..24)),
                    0..24,
                )],
            );
            assert_parsed(
                "#merge ../file.dta",
                vec![new_expression(
                    ExpressionKind::Merge(StrExpression::new("../file.dta", 7..18)),
                    0..18,
                )],
            );
            assert_parsed(
                "#autorun {print \"Auto-run action\"}",
                vec![new_expression(
                    ExpressionKind::Autorun(ArrayExpression::new(
                        vec![
                            new_expression(ExpressionKind::Symbol("print"), 10..15),
                            new_expression(ExpressionKind::String("Auto-run action"), 16..33),
                        ],
                        9..34,
                    )),
                    0..34,
                )],
            );

            assert_directive_symbol_error("#define");
            assert_directive_symbol_error("#undef");
            assert_directive_symbol_error("#include");
            assert_directive_symbol_error("#include_opt");
            assert_directive_symbol_error("#merge");

            assert_directive_eof_error("#define");
            assert_directive_eof_error("#undef");
            assert_directive_eof_error("#include");
            assert_directive_eof_error("#include_opt");
            assert_directive_eof_error("#merge");
            assert_directive_eof_error("#autorun");

            assert_errors(
                "#define kDefine 1",
                vec![ParseError::IncorrectToken {
                    expected: TokenKind::ArrayOpen,
                    actual: new_token(TokenKind::Integer(1), 16..17),
                }],
            );
            assert_errors(
                "#autorun kDefine",
                vec![ParseError::IncorrectToken {
                    expected: TokenKind::CommandOpen,
                    actual: new_token(TokenKind::Symbol("kDefine"), 9..16),
                }],
            );

            assert_errors("#bad", vec![ParseError::BadDirective(0..4)]);
        }

        #[test]
        fn conditionals() {
            assert_parsed(
                "#ifdef kDefine (array1 10) #else (array2 5) #endif",
                vec![new_expression(
                    ExpressionKind::Conditional {
                        is_positive: true,
                        symbol: StrExpression::new("kDefine", 7..14),
                        true_branch: ArrayExpression::new(
                            vec![new_expression(
                                ExpressionKind::Array(vec![
                                    new_expression(ExpressionKind::Symbol("array1"), 16..22),
                                    new_expression(ExpressionKind::Integer(10), 23..25),
                                ]),
                                15..26,
                            )],
                            0..32,
                        ),
                        false_branch: Some(ArrayExpression::new(
                            vec![new_expression(
                                ExpressionKind::Array(vec![
                                    new_expression(ExpressionKind::Symbol("array2"), 34..40),
                                    new_expression(ExpressionKind::Integer(5), 41..42),
                                ]),
                                33..43,
                            )],
                            27..50,
                        )),
                    },
                    0..50,
                )],
            );
            assert_parsed(
                "#ifndef kDefine (array 10) #endif",
                vec![new_expression(
                    ExpressionKind::Conditional {
                        is_positive: false,
                        symbol: StrExpression::new("kDefine", 8..15),
                        true_branch: ArrayExpression::new(
                            vec![new_expression(
                                ExpressionKind::Array(vec![
                                    new_expression(ExpressionKind::Symbol("array"), 17..22),
                                    new_expression(ExpressionKind::Integer(10), 23..25),
                                ]),
                                16..26,
                            )],
                            0..33,
                        ),
                        false_branch: None,
                    },
                    0..33,
                )],
            );

            assert_directive_symbol_error("#ifdef");
            assert_directive_symbol_error("#ifndef");

            assert_directive_eof_error("#ifdef");
            assert_directive_eof_error("#ifndef");

            assert_errors(
                "#ifndef kDefine (array 10)",
                vec![ParseError::UnmatchedConditional(0..7), ParseError::UnexpectedEof],
            );
            assert_errors(
                "#ifdef kDefine (array1 10) #else",
                vec![ParseError::UnmatchedConditional(27..32), ParseError::UnexpectedEof],
            );
            assert_errors("#else (array2 5) #endif", vec![ParseError::UnexpectedConditional(0..5)]);
            assert_errors("(array 10) #endif", vec![ParseError::UnexpectedConditional(11..17)]);

            assert_errors(
                "(#ifdef kDefine array1 10) #else array2 5) #endif)",
                vec![
                    ParseError::UnbalancedConditional(1..32),
                    ParseError::UnmatchedBrace(25..26, ArrayKind::Array),
                    ParseError::UnbalancedConditional(27..49),
                    ParseError::UnmatchedBrace(41..42, ArrayKind::Array),
                ],
            );

            assert_errors(
                "\
                #ifdef kDefine\n\
                {do\n\
                #endif\
                \n    {+ 1 2}\n\
                #ifdef kDefine\n\
                }\n\
                #endif\
                ",
                vec![
                    ParseError::UnbalancedConditional(0..25),
                    ParseError::UnmatchedBrace(15..16, ArrayKind::Command),
                    ParseError::UnbalancedConditional(38..61),
                    ParseError::UnmatchedBrace(53..54, ArrayKind::Command),
                ],
            );
        }
    }
}
