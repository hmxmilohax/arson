// SPDX-License-Identifier: LGPL-3.0-or-later

use std::iter::Peekable;
use std::marker::PhantomData;

use arson_core::{ArrayKind, FloatValue, IntegerValue};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use logos::Span;

use super::lexer::{self, LexError, OwnedToken, OwnedTokenValue, Token, TokenKind, TokenValue};

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
    Integer(IntegerValue),
    Float(FloatValue),
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
pub enum ExpressionValue<'src> {
    Integer(IntegerValue),
    Float(FloatValue),
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

pub enum ExpressionKind {
    Integer,
    Float,
    String,

    Symbol,
    Variable,
    Unhandled,

    Array,
    Command,
    Property,

    Define,
    Undefine,
    Include,
    IncludeOptional,
    Merge,
    Autorun,

    Conditional,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'src> {
    pub value: ExpressionValue<'src>,
    pub location: Span,
}

impl ExpressionValue<'_> {
    pub fn get_kind(&self) -> ExpressionKind {
        match self {
            ExpressionValue::Integer(_) => ExpressionKind::Integer,
            ExpressionValue::Float(_) => ExpressionKind::Float,
            ExpressionValue::String(_) => ExpressionKind::String,

            ExpressionValue::Symbol(_) => ExpressionKind::Symbol,
            ExpressionValue::Variable(_) => ExpressionKind::Variable,
            ExpressionValue::Unhandled => ExpressionKind::Unhandled,

            ExpressionValue::Array(_) => ExpressionKind::Array,
            ExpressionValue::Command(_) => ExpressionKind::Command,
            ExpressionValue::Property(_) => ExpressionKind::Property,

            ExpressionValue::Define(_, _) => ExpressionKind::Define,
            ExpressionValue::Undefine(_) => ExpressionKind::Undefine,
            ExpressionValue::Include(_) => ExpressionKind::Include,
            ExpressionValue::IncludeOptional(_) => ExpressionKind::IncludeOptional,
            ExpressionValue::Merge(_) => ExpressionKind::Merge,
            ExpressionValue::Autorun(_) => ExpressionKind::Autorun,

            ExpressionValue::Conditional { .. } => ExpressionKind::Conditional,
        }
    }
}

impl Expression<'_> {
    pub fn get_kind(&self) -> ExpressionKind {
        self.value.get_kind()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnmatchedBrace(Span, ArrayKind),

    UnexpectedConditional(Span),
    UnmatchedConditional(Span),
    UnbalancedConditional(Span),
    BadDirective(Span),

    TokenError(Span, LexError),
    IncorrectToken { expected: TokenKind, actual: OwnedToken },

    UnexpectedEof,
}

impl ParseError {
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
                .with_notes(vec!["#else or #endif required to close the conditional block".to_owned()])
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
    errors: Vec<ParseError>,
    unexpected_eof: bool,
}

macro_rules! next_token {
    ($self:expr, $tokens:expr, $expected:ident) => {{
        let Some(next) = $tokens.peek() else {
            return $self.unexpected_eof();
        };
        let TokenKind::$expected = next.kind else {
            return $self.incorrect_token(TokenKind::$expected, next.to_owned());
        };
        let location = next.location.clone();
        $tokens.next();
        location
    }};
    ($self:expr, $tokens:expr, $expected:ident()) => {{
        let Some(next) = $tokens.peek() else {
            return $self.unexpected_eof();
        };
        let TokenValue::$expected(value) = next.kind else {
            return $self.incorrect_token(TokenKind::$expected, next.to_owned());
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
        let (name, name_location) = next_token!(self, tokens, Symbol());
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
            TokenValue::Integer(value) => PreprocessedTokenKind::Integer(value),
            TokenValue::Float(value) => PreprocessedTokenKind::Float(value),
            TokenValue::String(value) => PreprocessedTokenKind::String(value),
            TokenValue::Symbol(value) => PreprocessedTokenKind::Symbol(value),
            TokenValue::Variable(value) => PreprocessedTokenKind::Variable(value),
            TokenValue::Unhandled => PreprocessedTokenKind::Unhandled,

            TokenValue::ArrayOpen => PreprocessedTokenKind::ArrayOpen,
            TokenValue::ArrayClose => PreprocessedTokenKind::ArrayClose,
            TokenValue::CommandOpen => PreprocessedTokenKind::CommandOpen,
            TokenValue::CommandClose => PreprocessedTokenKind::CommandClose,
            TokenValue::PropertyOpen => PreprocessedTokenKind::PropertyOpen,
            TokenValue::PropertyClose => PreprocessedTokenKind::PropertyClose,

            TokenValue::Define => {
                return self.symbol_directive(tokens, token.location, PreprocessedTokenKind::Define)
            },
            TokenValue::Undefine => {
                return self.symbol_directive(tokens, token.location, PreprocessedTokenKind::Undefine)
            },
            TokenValue::Include => {
                return self.symbol_directive(tokens, token.location, PreprocessedTokenKind::Include)
            },
            TokenValue::IncludeOptional => {
                return self.symbol_directive(tokens, token.location, PreprocessedTokenKind::IncludeOptional)
            },
            TokenValue::Merge => {
                return self.symbol_directive(tokens, token.location, PreprocessedTokenKind::Merge)
            },
            TokenValue::Autorun => PreprocessedTokenKind::Autorun,

            TokenValue::Ifdef => {
                let (name, name_location) = next_token!(self, tokens, Symbol());
                let name = StrExpression::new(name, name_location);
                return self.parse_conditional(tokens, token.location, true, name);
            },
            TokenValue::Ifndef => {
                let (name, name_location) = next_token!(self, tokens, Symbol());
                let name = StrExpression::new(name, name_location);
                return self.parse_conditional(tokens, token.location, false, name);
            },
            TokenValue::Else => {
                match self.conditional_stack.last_mut() {
                    Some(conditional) => {
                        conditional.false_location = Some(token.location.clone());
                    },
                    None => {
                        self.errors.push(ParseError::UnexpectedConditional(token.location.clone()));
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
            TokenValue::Endif => match self.conditional_stack.last() {
                Some(_) => return ProcessResult::BlockEnd(token.location),
                None => {
                    self.errors.push(ParseError::UnexpectedConditional(token.location));
                    return ProcessResult::SkipToken;
                },
            },

            TokenValue::BadDirective(_) => {
                self.errors.push(ParseError::BadDirective(token.location));
                return ProcessResult::SkipToken;
            },

            TokenValue::Comment => return ProcessResult::SkipToken,
            TokenValue::BlockCommentStart(_) => {
                self.skip_block_comment(tokens);
                return ProcessResult::SkipToken;
            },
            TokenValue::BlockCommentEnd(_) => return ProcessResult::SkipToken,

            TokenValue::Error(error) => PreprocessedTokenKind::Error(error),
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
        let conditional = self.conditional_stack.pop().expect("conditional was added just above");

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
            self.errors.push(ParseError::UnmatchedConditional(location.clone()));
            self.unexpected_eof = true;
        }
    }

    fn skip_block_comment<I: Iterator<Item = Token<'src>>>(&mut self, tokens: &mut Peekable<I>) {
        while let Some(token) = tokens.peek() {
            let TokenValue::BlockCommentEnd(_) = token.kind else {
                tokens.next();
                continue;
            };
            break;
        }
    }

    fn incorrect_token<T>(&mut self, expected: TokenKind, actual: OwnedToken) -> ProcessResult<T> {
        self.errors.push(ParseError::IncorrectToken { expected, actual });
        ProcessResult::SkipToken
    }

    fn unexpected_eof<T>(&mut self) -> ProcessResult<T> {
        self.errors.push(ParseError::UnexpectedEof);
        ProcessResult::Eof
    }
}

struct Parser<'src> {
    array_stack: Vec<ArrayMarker>,
    errors: Vec<ParseError>,
    unexpected_eof: bool,
    phantom: PhantomData<&'src ()>,
}

impl<'src> Parser<'src> {
    pub fn new() -> Self {
        Self {
            array_stack: Vec::new(),
            errors: Vec::new(),
            unexpected_eof: false,
            phantom: PhantomData,
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
            PreprocessedTokenKind::Integer(value) => (ExpressionValue::Integer(value), token.location),
            PreprocessedTokenKind::Float(value) => (ExpressionValue::Float(value), token.location),
            PreprocessedTokenKind::String(value) => (ExpressionValue::String(value), token.location),
            PreprocessedTokenKind::Symbol(value) => (ExpressionValue::Symbol(value), token.location),
            PreprocessedTokenKind::Variable(value) => (ExpressionValue::Variable(value), token.location),
            PreprocessedTokenKind::Unhandled => (ExpressionValue::Unhandled, token.location),

            PreprocessedTokenKind::ArrayOpen => {
                let (array, location) = self.open_array(tokens, &token.location, ArrayKind::Array);
                (ExpressionValue::Array(array), location)
            },
            PreprocessedTokenKind::CommandOpen => {
                let (array, location) = self.open_array(tokens, &token.location, ArrayKind::Command);
                (ExpressionValue::Command(array), location)
            },
            PreprocessedTokenKind::PropertyOpen => {
                let (array, location) = self.open_array(tokens, &token.location, ArrayKind::Property);
                (ExpressionValue::Property(array), location)
            },
            PreprocessedTokenKind::ArrayClose => return self.close_array(ArrayKind::Array, token.location),
            PreprocessedTokenKind::CommandClose => {
                return self.close_array(ArrayKind::Command, token.location)
            },
            PreprocessedTokenKind::PropertyClose => {
                return self.close_array(ArrayKind::Property, token.location)
            },

            PreprocessedTokenKind::Conditional { is_positive, symbol, true_branch, false_branch } => {
                let true_branch = self.parse_conditional_block(true_branch);
                let false_branch = false_branch.map(|block| self.parse_conditional_block(block));
                let expr = ExpressionValue::Conditional { is_positive, symbol, true_branch, false_branch };
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

                (ExpressionValue::Define(name, body), location)
            },
            PreprocessedTokenKind::Undefine(name) => (ExpressionValue::Undefine(name), token.location),
            PreprocessedTokenKind::Include(path) => (ExpressionValue::Include(path), token.location),
            PreprocessedTokenKind::IncludeOptional(path) => {
                (ExpressionValue::IncludeOptional(path), token.location)
            },
            PreprocessedTokenKind::Merge(name) => (ExpressionValue::Merge(name), token.location),
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
                (ExpressionValue::Autorun(body), token.location.start..body_location.end)
            },

            PreprocessedTokenKind::Error(error) => {
                self.errors.push(ParseError::TokenError(token.location, error));
                return ProcessResult::SkipToken;
            },
        };

        ProcessResult::Result(Expression { value: kind, location })
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
            self.errors.push(ParseError::UnbalancedConditional(location.clone()))
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
        expected: TokenKind,
        actual: PreprocessedToken<'src>,
    ) -> ProcessResult<Expression<'src>> {
        let actual_kind = match actual.kind {
            PreprocessedTokenKind::Integer(value) => OwnedTokenValue::Integer(value),
            PreprocessedTokenKind::Float(value) => OwnedTokenValue::Float(value),
            PreprocessedTokenKind::String(value) => OwnedTokenValue::String(value.to_owned()),
            PreprocessedTokenKind::Symbol(value) => OwnedTokenValue::Symbol(value.to_owned()),
            PreprocessedTokenKind::Variable(value) => OwnedTokenValue::Variable(value.to_owned()),
            PreprocessedTokenKind::Unhandled => OwnedTokenValue::Unhandled,

            PreprocessedTokenKind::ArrayOpen => OwnedTokenValue::ArrayOpen,
            PreprocessedTokenKind::ArrayClose => OwnedTokenValue::ArrayClose,
            PreprocessedTokenKind::CommandOpen => OwnedTokenValue::CommandOpen,
            PreprocessedTokenKind::CommandClose => OwnedTokenValue::CommandClose,
            PreprocessedTokenKind::PropertyOpen => OwnedTokenValue::PropertyOpen,
            PreprocessedTokenKind::PropertyClose => OwnedTokenValue::PropertyClose,

            PreprocessedTokenKind::Define(_) => OwnedTokenValue::Define,
            PreprocessedTokenKind::Undefine(_) => OwnedTokenValue::Undefine,
            PreprocessedTokenKind::Include(_) => OwnedTokenValue::Include,
            PreprocessedTokenKind::IncludeOptional(_) => OwnedTokenValue::IncludeOptional,
            PreprocessedTokenKind::Merge(_) => OwnedTokenValue::Merge,
            PreprocessedTokenKind::Autorun => OwnedTokenValue::Autorun,

            PreprocessedTokenKind::Conditional { .. } => OwnedTokenValue::Ifdef,

            PreprocessedTokenKind::Error(error) => OwnedTokenValue::Error(error),
        };
        let actual = OwnedToken { kind: actual_kind, location: actual.location };

        self.errors.push(ParseError::IncorrectToken { expected, actual });
        ProcessResult::SkipToken
    }

    fn unexpected_eof(&mut self) -> ProcessResult<Expression<'src>> {
        self.errors.push(ParseError::UnexpectedEof);
        ProcessResult::Eof
    }
}

fn preprocess<'src>(
    tokens: impl Iterator<Item = Token<'src>>,
) -> Result<Vec<PreprocessedToken<'src>>, Vec<ParseError>> {
    let mut preprocessor = Preprocessor::new();
    let (preprocessed, _) = preprocessor.preprocess(&mut tokens.peekable());
    match preprocessor.errors.is_empty() {
        true => Ok(preprocessed),
        false => Err(preprocessor.errors),
    }
}

pub fn parse_text(text: &str) -> Result<Vec<Expression<'_>>, Vec<ParseError>> {
    parse_tokens(lexer::lex_text(text))
}

pub fn parse_tokens<'src>(
    tokens: impl Iterator<Item = Token<'src>>,
) -> Result<Vec<Expression<'src>>, Vec<ParseError>> {
    let preprocessed = preprocess(tokens)?;

    let mut parser = Parser::new();
    let (ast, _) = parser.parse(&mut preprocessed.into_iter().peekable());
    match parser.errors.is_empty() {
        true => Ok(ast),
        false => Err(parser.errors),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod preprocessor {
        use super::*;

        const fn new_pretoken(kind: PreprocessedTokenKind<'_>, location: Span) -> PreprocessedToken<'_> {
            PreprocessedToken { kind, location }
        }

        fn assert_preprocessed(text: &str, expected: Vec<PreprocessedToken>) {
            let tokens = lexer::lex_text(text);
            let preprocessed = match preprocess(tokens) {
                Ok(preprocessed) => preprocessed,
                Err(errors) => panic!("Errors encountered while preprocessing: {errors:?}"),
            };
            assert_eq!(preprocessed, expected, "Unexpected result for '{text}'");
        }

        fn assert_errors(text: &str, expected: Vec<ParseError>) {
            let tokens = lexer::lex_text(text);
            let errors = match preprocess(tokens) {
                Ok(preprocessed) => panic!("Expected preprocessing errors, got success instead instead.\nText: {text}\nResult: {preprocessed:?}"),
                Err(errors) => errors,
            };
            assert_eq!(errors, expected, "Unexpected result for '{text}'");
        }

        #[test]
        fn integer() {
            assert_preprocessed("1 2 3", vec![
                new_pretoken(PreprocessedTokenKind::Integer(1), 0..1),
                new_pretoken(PreprocessedTokenKind::Integer(2), 2..3),
                new_pretoken(PreprocessedTokenKind::Integer(3), 4..5),
            ]);
        }

        #[test]
        fn float() {
            assert_preprocessed("1.0 2.0 3.0", vec![
                new_pretoken(PreprocessedTokenKind::Float(1.0), 0..3),
                new_pretoken(PreprocessedTokenKind::Float(2.0), 4..7),
                new_pretoken(PreprocessedTokenKind::Float(3.0), 8..11),
            ]);
        }

        #[test]
        fn string() {
            assert_preprocessed("\"a\" \"b\" \"c\"", vec![
                new_pretoken(PreprocessedTokenKind::String("a"), 0..3),
                new_pretoken(PreprocessedTokenKind::String("b"), 4..7),
                new_pretoken(PreprocessedTokenKind::String("c"), 8..11),
            ]);
        }

        #[test]
        fn symbol() {
            assert_preprocessed("asdf + '10'", vec![
                new_pretoken(PreprocessedTokenKind::Symbol("asdf"), 0..4),
                new_pretoken(PreprocessedTokenKind::Symbol("+"), 5..6),
                new_pretoken(PreprocessedTokenKind::Symbol("10"), 7..11),
            ]);
        }

        #[test]
        fn variable() {
            assert_preprocessed("$asdf $this", vec![
                new_pretoken(PreprocessedTokenKind::Variable("asdf"), 0..5),
                new_pretoken(PreprocessedTokenKind::Variable("this"), 6..11),
            ]);
        }

        #[test]
        fn unhandled() {
            assert_preprocessed("kDataUnhandled", vec![new_pretoken(
                PreprocessedTokenKind::Unhandled,
                0..14,
            )])
        }

        #[test]
        fn arrays() {
            assert_preprocessed("(asdf \"text\" 1)", vec![
                new_pretoken(PreprocessedTokenKind::ArrayOpen, 0..1),
                new_pretoken(PreprocessedTokenKind::Symbol("asdf"), 1..5),
                new_pretoken(PreprocessedTokenKind::String("text"), 6..12),
                new_pretoken(PreprocessedTokenKind::Integer(1), 13..14),
                new_pretoken(PreprocessedTokenKind::ArrayClose, 14..15),
            ]);
            assert_preprocessed("{set $var \"asdf\"}", vec![
                new_pretoken(PreprocessedTokenKind::CommandOpen, 0..1),
                new_pretoken(PreprocessedTokenKind::Symbol("set"), 1..4),
                new_pretoken(PreprocessedTokenKind::Variable("var"), 5..9),
                new_pretoken(PreprocessedTokenKind::String("asdf"), 10..16),
                new_pretoken(PreprocessedTokenKind::CommandClose, 16..17),
            ]);
            assert_preprocessed("[property]", vec![
                new_pretoken(PreprocessedTokenKind::PropertyOpen, 0..1),
                new_pretoken(PreprocessedTokenKind::Symbol("property"), 1..9),
                new_pretoken(PreprocessedTokenKind::PropertyClose, 9..10),
            ]);
        }

        fn assert_directive_symbol_error(name: &str) {
            let text = name.to_owned() + " 1";
            assert_errors(&text, vec![ParseError::IncorrectToken {
                expected: TokenKind::Symbol,
                actual: OwnedToken::new(OwnedTokenValue::Integer(1), name.len() + 1..name.len() + 2),
            }]);
        }

        fn assert_directive_eof_error(name: &str) {
            assert_errors(name, vec![ParseError::UnexpectedEof]);
        }

        #[test]
        fn directives() {
            assert_preprocessed("#define kDefine (1)", vec![
                new_pretoken(PreprocessedTokenKind::Define(StrExpression::new("kDefine", 8..15)), 0..15),
                new_pretoken(PreprocessedTokenKind::ArrayOpen, 16..17),
                new_pretoken(PreprocessedTokenKind::Integer(1), 17..18),
                new_pretoken(PreprocessedTokenKind::ArrayClose, 18..19),
            ]);
            assert_preprocessed("#undef kDefine", vec![new_pretoken(
                PreprocessedTokenKind::Undefine(StrExpression::new("kDefine", 7..14)),
                0..14,
            )]);
            assert_preprocessed("#include ../file.dta", vec![new_pretoken(
                PreprocessedTokenKind::Include(StrExpression::new("../file.dta", 9..20)),
                0..20,
            )]);
            assert_preprocessed("#include_opt ../file.dta", vec![new_pretoken(
                PreprocessedTokenKind::IncludeOptional(StrExpression::new("../file.dta", 13..24)),
                0..24,
            )]);
            assert_preprocessed("#merge ../file.dta", vec![new_pretoken(
                PreprocessedTokenKind::Merge(StrExpression::new("../file.dta", 7..18)),
                0..18,
            )]);
            assert_preprocessed("#autorun {print \"Auto-run action\"}", vec![
                new_pretoken(PreprocessedTokenKind::Autorun, 0..8),
                new_pretoken(PreprocessedTokenKind::CommandOpen, 9..10),
                new_pretoken(PreprocessedTokenKind::Symbol("print"), 10..15),
                new_pretoken(PreprocessedTokenKind::String("Auto-run action"), 16..33),
                new_pretoken(PreprocessedTokenKind::CommandClose, 33..34),
            ]);

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
            assert_preprocessed("#ifdef kDefine (array1 10) #else (array2 5) #endif", vec![new_pretoken(
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
            )]);
            assert_preprocessed("#ifndef kDefine (array 10) #endif", vec![new_pretoken(
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
            )]);

            assert_directive_symbol_error("#ifdef");
            assert_directive_symbol_error("#ifndef");

            assert_directive_eof_error("#ifdef");
            assert_directive_eof_error("#ifndef");

            assert_errors("#ifndef kDefine (array 10)", vec![
                ParseError::UnmatchedConditional(0..7),
                ParseError::UnexpectedEof,
            ]);
            assert_errors("#ifdef kDefine (array1 10) #else", vec![
                ParseError::UnmatchedConditional(27..32),
                ParseError::UnexpectedEof,
            ]);
            assert_errors("#else (array2 5) #endif", vec![ParseError::UnexpectedConditional(0..5)]);
            assert_errors("(array 10) #endif", vec![ParseError::UnexpectedConditional(11..17)]);
        }
    }

    mod parser {
        use super::*;

        const fn new_expression(kind: ExpressionValue<'_>, location: Span) -> Expression<'_> {
            Expression { value: kind, location }
        }

        fn assert_parsed(text: &str, expected: Vec<Expression>) {
            let ast = match parse_text(text) {
                Ok(ast) => ast,
                Err(errs) => panic!("Errors encountered while parsing: {errs:?}"),
            };
            assert_eq!(ast, expected, "Unexpected result for '{text}'");
        }

        fn assert_errors(text: &str, expected: Vec<ParseError>) {
            let errors = match parse_text(text) {
                Ok(ast) => panic!("Expected parsing errors, got AST instead: {ast:?}"),
                Err(errors) => errors,
            };
            assert_eq!(errors, expected, "Unexpected result for '{text}'");
        }

        #[test]
        fn integer() {
            assert_parsed("1 2 3", vec![
                new_expression(ExpressionValue::Integer(1), 0..1),
                new_expression(ExpressionValue::Integer(2), 2..3),
                new_expression(ExpressionValue::Integer(3), 4..5),
            ]);
        }

        #[test]
        fn float() {
            assert_parsed("1.0 2.0 3.0", vec![
                new_expression(ExpressionValue::Float(1.0), 0..3),
                new_expression(ExpressionValue::Float(2.0), 4..7),
                new_expression(ExpressionValue::Float(3.0), 8..11),
            ]);
        }

        #[test]
        fn string() {
            assert_parsed("\"a\" \"b\" \"c\"", vec![
                new_expression(ExpressionValue::String("a"), 0..3),
                new_expression(ExpressionValue::String("b"), 4..7),
                new_expression(ExpressionValue::String("c"), 8..11),
            ]);
        }

        #[test]
        fn symbol() {
            assert_parsed("asdf + '10'", vec![
                new_expression(ExpressionValue::Symbol("asdf"), 0..4),
                new_expression(ExpressionValue::Symbol("+"), 5..6),
                new_expression(ExpressionValue::Symbol("10"), 7..11),
            ]);
        }

        #[test]
        fn variable() {
            assert_parsed("$asdf $this", vec![
                new_expression(ExpressionValue::Variable("asdf"), 0..5),
                new_expression(ExpressionValue::Variable("this"), 6..11),
            ]);
        }

        #[test]
        fn unhandled() {
            assert_parsed("kDataUnhandled", vec![new_expression(ExpressionValue::Unhandled, 0..14)])
        }

        #[test]
        fn arrays() {
            assert_parsed("(asdf \"text\" 1)", vec![new_expression(
                ExpressionValue::Array(vec![
                    new_expression(ExpressionValue::Symbol("asdf"), 1..5),
                    new_expression(ExpressionValue::String("text"), 6..12),
                    new_expression(ExpressionValue::Integer(1), 13..14),
                ]),
                0..15,
            )]);
            assert_parsed("{set $var \"asdf\"}", vec![new_expression(
                ExpressionValue::Command(vec![
                    new_expression(ExpressionValue::Symbol("set"), 1..4),
                    new_expression(ExpressionValue::Variable("var"), 5..9),
                    new_expression(ExpressionValue::String("asdf"), 10..16),
                ]),
                0..17,
            )]);
            assert_parsed("[property]", vec![new_expression(
                ExpressionValue::Property(vec![new_expression(ExpressionValue::Symbol("property"), 1..9)]),
                0..10,
            )]);

            fn assert_array_mismatch(text: &str, kind: ArrayKind, location: Span, eof_expected: bool) {
                let mut expected = vec![ParseError::UnmatchedBrace(location, kind)];
                if !eof_expected {
                    expected.push(ParseError::UnexpectedEof);
                }

                assert_errors(text, expected);
            }

            fn assert_array_mismatches(kind: ArrayKind) {
                let (l, r) = kind.delimiters();
                assert_array_mismatch(&format!("{l} {l} {r}"), kind, 0..1, false);
                assert_array_mismatch(&format!("{r} {l} {r}"), kind, 0..1, true);
                assert_array_mismatch(&format!("{l} {r} {l}"), kind, 4..5, false);
                assert_array_mismatch(&format!("{l} {r} {r}"), kind, 4..5, true);
            }

            fn assert_multi_array_mismatches(matched_kind: ArrayKind, unmatched_kind: ArrayKind) {
                let (ml, mr) = matched_kind.delimiters();
                let (ul, ur) = unmatched_kind.delimiters();

                assert_array_mismatch(&format!("{ul} {ml} {mr}"), unmatched_kind, 0..1, false);
                assert_array_mismatch(&format!("{ur} {ml} {mr}"), unmatched_kind, 0..1, true);
                assert_array_mismatch(&format!("{ml} {ul} {mr}"), unmatched_kind, 2..3, true);
                assert_array_mismatch(&format!("{ml} {ur} {mr}"), unmatched_kind, 2..3, true);
                assert_array_mismatch(&format!("{ml} {mr} {ul}"), unmatched_kind, 4..5, false);
                assert_array_mismatch(&format!("{ml} {mr} {ur}"), unmatched_kind, 4..5, true);
            }

            assert_array_mismatches(ArrayKind::Array);
            assert_array_mismatches(ArrayKind::Command);
            assert_array_mismatches(ArrayKind::Property);

            assert_multi_array_mismatches(ArrayKind::Array, ArrayKind::Command);
            assert_multi_array_mismatches(ArrayKind::Array, ArrayKind::Property);
            assert_multi_array_mismatches(ArrayKind::Command, ArrayKind::Array);
            assert_multi_array_mismatches(ArrayKind::Command, ArrayKind::Property);
            assert_multi_array_mismatches(ArrayKind::Property, ArrayKind::Array);
            assert_multi_array_mismatches(ArrayKind::Property, ArrayKind::Command);
        }

        fn assert_directive_symbol_error(name: &str) {
            let text = name.to_owned() + " 1";
            assert_errors(&text, vec![ParseError::IncorrectToken {
                expected: TokenKind::Symbol,
                actual: OwnedToken::new(OwnedTokenValue::Integer(1), name.len() + 1..name.len() + 2),
            }]);
        }

        fn assert_directive_eof_error(name: &str) {
            assert_errors(name, vec![ParseError::UnexpectedEof]);
        }

        #[test]
        fn directives() {
            assert_parsed("#define kDefine (1)", vec![new_expression(
                ExpressionValue::Define(
                    StrExpression::new("kDefine", 8..15),
                    ArrayExpression::new(vec![new_expression(ExpressionValue::Integer(1), 17..18)], 16..19),
                ),
                0..19,
            )]);
            assert_parsed("#undef kDefine", vec![new_expression(
                ExpressionValue::Undefine(StrExpression::new("kDefine", 7..14)),
                0..14,
            )]);
            assert_parsed("#include ../file.dta", vec![new_expression(
                ExpressionValue::Include(StrExpression::new("../file.dta", 9..20)),
                0..20,
            )]);
            assert_parsed("#include_opt ../file.dta", vec![new_expression(
                ExpressionValue::IncludeOptional(StrExpression::new("../file.dta", 13..24)),
                0..24,
            )]);
            assert_parsed("#merge ../file.dta", vec![new_expression(
                ExpressionValue::Merge(StrExpression::new("../file.dta", 7..18)),
                0..18,
            )]);
            assert_parsed("#autorun {print \"Auto-run action\"}", vec![new_expression(
                ExpressionValue::Autorun(ArrayExpression::new(
                    vec![
                        new_expression(ExpressionValue::Symbol("print"), 10..15),
                        new_expression(ExpressionValue::String("Auto-run action"), 16..33),
                    ],
                    9..34,
                )),
                0..34,
            )]);

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

            assert_errors("#define kDefine 1", vec![ParseError::IncorrectToken {
                expected: TokenKind::ArrayOpen,
                actual: OwnedToken::new(OwnedTokenValue::Integer(1), 16..17),
            }]);
            assert_errors("#autorun kDefine", vec![ParseError::IncorrectToken {
                expected: TokenKind::CommandOpen,
                actual: OwnedToken::new(OwnedTokenValue::Symbol("kDefine".to_owned()), 9..16),
            }]);

            assert_errors("#bad", vec![ParseError::BadDirective(0..4)]);
        }

        #[test]
        fn conditionals() {
            assert_parsed("#ifdef kDefine (array1 10) #else (array2 5) #endif", vec![new_expression(
                ExpressionValue::Conditional {
                    is_positive: true,
                    symbol: StrExpression::new("kDefine", 7..14),
                    true_branch: ArrayExpression::new(
                        vec![new_expression(
                            ExpressionValue::Array(vec![
                                new_expression(ExpressionValue::Symbol("array1"), 16..22),
                                new_expression(ExpressionValue::Integer(10), 23..25),
                            ]),
                            15..26,
                        )],
                        0..32,
                    ),
                    false_branch: Some(ArrayExpression::new(
                        vec![new_expression(
                            ExpressionValue::Array(vec![
                                new_expression(ExpressionValue::Symbol("array2"), 34..40),
                                new_expression(ExpressionValue::Integer(5), 41..42),
                            ]),
                            33..43,
                        )],
                        27..50,
                    )),
                },
                0..50,
            )]);
            assert_parsed("#ifndef kDefine (array 10) #endif", vec![new_expression(
                ExpressionValue::Conditional {
                    is_positive: false,
                    symbol: StrExpression::new("kDefine", 8..15),
                    true_branch: ArrayExpression::new(
                        vec![new_expression(
                            ExpressionValue::Array(vec![
                                new_expression(ExpressionValue::Symbol("array"), 17..22),
                                new_expression(ExpressionValue::Integer(10), 23..25),
                            ]),
                            16..26,
                        )],
                        0..33,
                    ),
                    false_branch: None,
                },
                0..33,
            )]);

            assert_directive_symbol_error("#ifdef");
            assert_directive_symbol_error("#ifndef");

            assert_directive_eof_error("#ifdef");
            assert_directive_eof_error("#ifndef");

            assert_errors("#ifndef kDefine (array 10)", vec![
                ParseError::UnmatchedConditional(0..7),
                ParseError::UnexpectedEof,
            ]);
            assert_errors("#ifdef kDefine (array1 10) #else", vec![
                ParseError::UnmatchedConditional(27..32),
                ParseError::UnexpectedEof,
            ]);
            assert_errors("#else (array2 5) #endif", vec![ParseError::UnexpectedConditional(0..5)]);
            assert_errors("(array 10) #endif", vec![ParseError::UnexpectedConditional(11..17)]);

            assert_errors("(#ifdef kDefine array1 10) #else array2 5) #endif)", vec![
                ParseError::UnbalancedConditional(1..32),
                ParseError::UnmatchedBrace(25..26, ArrayKind::Array),
                ParseError::UnbalancedConditional(27..49),
                ParseError::UnmatchedBrace(41..42, ArrayKind::Array),
            ]);

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
