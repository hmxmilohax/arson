// SPDX-License-Identifier: LGPL-3.0-or-later

use std::borrow::Cow;
use std::iter::Peekable;

use logos::Span;

use super::{Diagnostic, DiagnosticKind, TokenKind, TokenValue, Tokenizer};
use crate::{ArrayKind, BlockCommentToken, DirectiveArgumentDescription, FloatValue, IntegerValue};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ParseOptions {
    pub include_comments: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StrExpression<'src> {
    pub text: Cow<'src, str>,
    pub location: Span,
}

impl<'src> StrExpression<'src> {
    pub fn new(text: &'src str, location: Span) -> Self {
        Self { text: Cow::Borrowed(text), location }
    }

    pub fn from_cow(text: Cow<'src, str>, location: Span) -> Self {
        Self { text, location }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayExpression<'src> {
    pub exprs: Vec<Expression<'src>>,
    pub location: Span,
}

impl<'src> ArrayExpression<'src> {
    pub fn new(exprs: Vec<Expression<'src>>, location: Span) -> Self {
        Self { exprs, location }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum PreprocessedTokenValue<'src> {
    Integer(IntegerValue),
    Float(FloatValue),
    String(Cow<'src, str>),
    Symbol(Cow<'src, str>),
    Variable(Cow<'src, str>),
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

    BlankLine,
    Comment(Cow<'src, str>),
    BlockComment(BlockCommentToken<'src>),
}

impl<'src> PreprocessedTokenValue<'src> {
    #[allow(dead_code, reason = "used by tests")]
    const fn make_string(text: &'src str) -> Self {
        PreprocessedTokenValue::String(Cow::Borrowed(text))
    }

    #[allow(dead_code, reason = "used by tests")]
    const fn make_symbol(text: &'src str) -> Self {
        PreprocessedTokenValue::Symbol(Cow::Borrowed(text))
    }

    #[allow(dead_code, reason = "used by tests")]
    const fn make_variable(text: &'src str) -> Self {
        PreprocessedTokenValue::Variable(Cow::Borrowed(text))
    }

    #[allow(dead_code, reason = "used by tests")]
    const fn make_comment(text: &'src str) -> Self {
        PreprocessedTokenValue::Comment(Cow::Borrowed(text))
    }

    fn get_kind(&self) -> TokenKind {
        match self {
            Self::Integer(_) => TokenKind::Integer,
            Self::Float(_) => TokenKind::Float,
            Self::String(_) => TokenKind::String,
            Self::Symbol(_) => TokenKind::Symbol,
            Self::Variable(_) => TokenKind::Variable,
            Self::Unhandled => TokenKind::Unhandled,

            Self::ArrayOpen => TokenKind::ArrayOpen,
            Self::ArrayClose => TokenKind::ArrayClose,
            Self::CommandOpen => TokenKind::CommandOpen,
            Self::CommandClose => TokenKind::CommandClose,
            Self::PropertyOpen => TokenKind::PropertyOpen,
            Self::PropertyClose => TokenKind::PropertyClose,

            Self::Define(_) => TokenKind::Define,
            Self::Undefine(_) => TokenKind::Undefine,
            Self::Include(_) => TokenKind::Include,
            Self::IncludeOptional(_) => TokenKind::IncludeOptional,
            Self::Merge(_) => TokenKind::Merge,
            Self::Autorun => TokenKind::Autorun,

            Self::Conditional { .. } => TokenKind::Ifdef,

            Self::BlankLine => TokenKind::BlankLine,
            Self::Comment(_) => TokenKind::Comment,
            Self::BlockComment(_) => TokenKind::BlockComment,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct PreprocessedToken<'src> {
    value: PreprocessedTokenValue<'src>,
    location: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionValue<'src> {
    Integer(IntegerValue),
    Float(FloatValue),
    String(Cow<'src, str>),

    Symbol(Cow<'src, str>),
    Variable(Cow<'src, str>),
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

    BlankLine,
    Comment(Cow<'src, str>),
    BlockComment(BlockCommentToken<'src>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
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

    Newline,
    Comment,
    BlockComment,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'src> {
    pub value: ExpressionValue<'src>,
    pub location: Span,
}

impl<'src> ExpressionValue<'src> {
    pub const fn make_string(text: &'src str) -> Self {
        ExpressionValue::String(Cow::Borrowed(text))
    }

    pub const fn make_symbol(text: &'src str) -> Self {
        ExpressionValue::Symbol(Cow::Borrowed(text))
    }

    pub const fn make_variable(text: &'src str) -> Self {
        ExpressionValue::Variable(Cow::Borrowed(text))
    }

    pub const fn make_comment(text: &'src str) -> Self {
        ExpressionValue::Comment(Cow::Borrowed(text))
    }

    pub fn get_kind(&self) -> ExpressionKind {
        match self {
            Self::Integer(_) => ExpressionKind::Integer,
            Self::Float(_) => ExpressionKind::Float,
            Self::String(_) => ExpressionKind::String,

            Self::Symbol(_) => ExpressionKind::Symbol,
            Self::Variable(_) => ExpressionKind::Variable,
            Self::Unhandled => ExpressionKind::Unhandled,

            Self::Array(_) => ExpressionKind::Array,
            Self::Command(_) => ExpressionKind::Command,
            Self::Property(_) => ExpressionKind::Property,

            Self::Define(_, _) => ExpressionKind::Define,
            Self::Undefine(_) => ExpressionKind::Undefine,
            Self::Include(_) => ExpressionKind::Include,
            Self::IncludeOptional(_) => ExpressionKind::IncludeOptional,
            Self::Merge(_) => ExpressionKind::Merge,
            Self::Autorun(_) => ExpressionKind::Autorun,

            Self::Conditional { .. } => ExpressionKind::Conditional,

            Self::BlankLine => ExpressionKind::Newline,
            Self::Comment(_) => ExpressionKind::Comment,
            Self::BlockComment(_) => ExpressionKind::BlockComment,
        }
    }
}

impl<'src> Expression<'src> {
    pub fn new(value: ExpressionValue<'src>, location: Span) -> Self {
        Self { value, location }
    }

    pub fn get_kind(&self) -> ExpressionKind {
        self.value.get_kind()
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
    Skip,
    Error(DiagnosticKind, Span),
    Eof,
}

struct Preprocessor<'src> {
    expressions: Vec<PreprocessedToken<'src>>,
    conditional_stack: Vec<ConditionalMarker<'src>>,

    errors: Vec<Diagnostic>,
    eof_checked: bool,
    unexpected_eof: bool,
}

impl<'src> Preprocessor<'src> {
    pub fn new() -> Self {
        Self {
            expressions: Vec::new(),
            conditional_stack: Vec::new(),

            errors: Vec::new(),
            eof_checked: false,
            unexpected_eof: false,
        }
    }

    pub fn preprocess(&mut self, mut tokens: Tokenizer<'src>) -> Vec<PreprocessedToken<'src>> {
        self.preprocess_loop(&mut tokens).0
    }

    fn preprocess_loop(&mut self, tokens: &mut Tokenizer<'src>) -> (Vec<PreprocessedToken<'src>>, Span) {
        let prev_exprs = std::mem::take(&mut self.expressions);

        let last_location = loop {
            match self.process_token(tokens) {
                ProcessResult::Result(node) => self.expressions.push(node),
                ProcessResult::BlockEnd(end_location) => break end_location,
                ProcessResult::Skip => continue,
                ProcessResult::Error(kind, location) => {
                    self.push_error(kind, location);
                    continue;
                },
                ProcessResult::Eof => {
                    self.verify_eof();
                    match self.expressions.last().cloned() {
                        Some(last) => break last.location.clone(),
                        None => break 0..0,
                    }
                },
            };
        };

        (std::mem::replace(&mut self.expressions, prev_exprs), last_location)
    }

    fn symbol_directive(
        &mut self,
        tokens: &mut Tokenizer<'src>,
        location: Span,
        kind: impl Fn(StrExpression<'src>) -> PreprocessedTokenValue<'src>,
        description: DirectiveArgumentDescription,
    ) -> ProcessResult<PreprocessedToken<'src>> {
        // Note: comments between the directive and symbol will be emitted
        // before the directive gets emitted. Trying to handle this more sanely
        // is far more complex than I have the patience for currently lol, and
        // it should impact very few scenarios in practice.
        self.skip_comments(tokens);

        let Some(name_token) = tokens.peek() else {
            self.unexpected_eof = true;
            return ProcessResult::Error(
                DiagnosticKind::MissingDirectiveArgument { missing: TokenKind::Symbol, description },
                location,
            );
        };

        let name_location = name_token.location.clone();
        let TokenValue::Symbol(_) = name_token.value else {
            return ProcessResult::Error(
                DiagnosticKind::IncorrectDirectiveArgument {
                    expected: TokenKind::Symbol,
                    expected_description: description,
                    actual: name_token.get_kind(),
                    expecting_location: location,
                },
                name_location.clone(),
            );
        };

        let name_token = tokens.next().unwrap();
        let name = match_unwrap!(name_token.value, TokenValue::Symbol(name) => name);

        let name = StrExpression::from_cow(name, name_location.clone());
        let location = location.start..name_location.end;
        ProcessResult::Result(PreprocessedToken { value: kind(name), location })
    }

    fn process_token(&mut self, tokens: &mut Tokenizer<'src>) -> ProcessResult<PreprocessedToken<'src>> {
        let token = match tokens.next() {
            Some(token) => token,
            None => return ProcessResult::Eof,
        };

        let kind = match token.value {
            TokenValue::Integer(value) => PreprocessedTokenValue::Integer(value),
            TokenValue::Float(value) => PreprocessedTokenValue::Float(value),
            TokenValue::String(value) => PreprocessedTokenValue::String(value),
            TokenValue::Symbol(value) => PreprocessedTokenValue::Symbol(value),
            TokenValue::Variable(value) => PreprocessedTokenValue::Variable(value),
            TokenValue::Unhandled => PreprocessedTokenValue::Unhandled,

            TokenValue::ArrayOpen => PreprocessedTokenValue::ArrayOpen,
            TokenValue::ArrayClose => PreprocessedTokenValue::ArrayClose,
            TokenValue::CommandOpen => PreprocessedTokenValue::CommandOpen,
            TokenValue::CommandClose => PreprocessedTokenValue::CommandClose,
            TokenValue::PropertyOpen => PreprocessedTokenValue::PropertyOpen,
            TokenValue::PropertyClose => PreprocessedTokenValue::PropertyClose,

            TokenValue::Define => {
                return self.symbol_directive(
                    tokens,
                    token.location,
                    PreprocessedTokenValue::Define,
                    DirectiveArgumentDescription::MacroName,
                )
            },
            TokenValue::Undefine => {
                return self.symbol_directive(
                    tokens,
                    token.location,
                    PreprocessedTokenValue::Undefine,
                    DirectiveArgumentDescription::MacroName,
                )
            },
            TokenValue::Include => {
                return self.symbol_directive(
                    tokens,
                    token.location,
                    PreprocessedTokenValue::Include,
                    DirectiveArgumentDescription::FilePath,
                )
            },
            TokenValue::IncludeOptional => {
                return self.symbol_directive(
                    tokens,
                    token.location,
                    PreprocessedTokenValue::IncludeOptional,
                    DirectiveArgumentDescription::FilePath,
                )
            },
            TokenValue::Merge => {
                return self.symbol_directive(
                    tokens,
                    token.location,
                    PreprocessedTokenValue::Merge,
                    DirectiveArgumentDescription::FilePath,
                )
            },
            TokenValue::Autorun => PreprocessedTokenValue::Autorun,

            TokenValue::Ifdef => {
                return self.parse_conditional_start(tokens, token.location, true);
            },
            TokenValue::Ifndef => {
                return self.parse_conditional_start(tokens, token.location, false);
            },
            TokenValue::Else => {
                match self.conditional_stack.last_mut() {
                    Some(conditional) => {
                        conditional.false_location = Some(token.location.clone());
                    },
                    None => {
                        self.push_error(DiagnosticKind::UnexpectedConditional, token.location.clone());
                        return self.handle_conditional(tokens, token.location.clone(), ConditionalMarker {
                            location: token.location.clone(),
                            is_positive: true,
                            symbol: StrExpression::new("<invalid>", token.location.clone()),
                            false_location: Some(token.location),
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
                None => return ProcessResult::Error(DiagnosticKind::UnexpectedConditional, token.location),
            },

            TokenValue::BlankLine => PreprocessedTokenValue::BlankLine,
            TokenValue::Comment(text) => PreprocessedTokenValue::Comment(text),
            TokenValue::BlockComment(text) => PreprocessedTokenValue::BlockComment(text),

            TokenValue::Error(error) => match error {
                DiagnosticKind::UnclosedBlockComment => {
                    self.unexpected_eof = true;
                    return ProcessResult::Error(DiagnosticKind::UnclosedBlockComment, token.location);
                },
                _ => return ProcessResult::Error(error, token.location),
            },
        };

        ProcessResult::Result(PreprocessedToken { value: kind, location: token.location })
    }

    fn parse_conditional_start(
        &mut self,
        tokens: &mut Tokenizer<'src>,
        start: Span,
        positive: bool,
    ) -> ProcessResult<PreprocessedToken<'src>> {
        // Note: comments between the directive and symbol will be emitted
        // before the directive gets emitted. Trying to handle this more sanely
        // is far more complex than I have the patience for currently lol, and
        // it should impact very few scenarios in practice.
        self.skip_comments(tokens);

        let Some(define_token) = tokens.peek() else {
            self.unexpected_eof = true;
            self.push_error(DiagnosticKind::UnmatchedConditional, start.clone());
            return ProcessResult::Error(
                DiagnosticKind::MissingDirectiveArgument {
                    missing: TokenKind::Symbol,
                    description: DirectiveArgumentDescription::MacroName,
                },
                start,
            );
        };

        let define_location = define_token.location.clone();
        let define_name = match define_token.value {
            TokenValue::Symbol(_) => {
                let token = tokens.next().unwrap();
                match_unwrap!(token.value, TokenValue::Symbol(text) => text)
            },
            _ => {
                self.push_error(
                    DiagnosticKind::IncorrectDirectiveArgument {
                        expected: TokenKind::Symbol,
                        expected_description: DirectiveArgumentDescription::MacroName,
                        actual: define_token.get_kind(),
                        expecting_location: start.clone(),
                    },
                    define_location.clone(),
                );
                // Recover with a dummy name instead of bailing
                Cow::Borrowed("<invalid>")
            },
        };

        self.handle_conditional(tokens, start.clone(), ConditionalMarker {
            location: start,
            is_positive: positive,
            symbol: StrExpression::from_cow(define_name, define_location),
            false_location: None,
            false_branch: None,
        })
    }

    fn handle_conditional(
        &mut self,
        tokens: &mut Tokenizer<'src>,
        start: Span,
        marker: ConditionalMarker<'src>,
    ) -> ProcessResult<PreprocessedToken<'src>> {
        self.conditional_stack.push(marker);

        let (true_exprs, true_location) = self.parse_conditional_block(tokens, start);
        let conditional = self.conditional_stack.pop().expect("conditional was added just above");

        let location = match &conditional.false_branch {
            Some(false_branch) => conditional.location.start..false_branch.1.end,
            None => conditional.location.start..true_location.end,
        };

        ProcessResult::Result(PreprocessedToken {
            value: PreprocessedTokenValue::Conditional {
                is_positive: conditional.is_positive,
                symbol: conditional.symbol,
                true_branch: (true_exprs, true_location),
                false_branch: conditional.false_branch,
            },
            location,
        })
    }

    fn parse_conditional_block(
        &mut self,
        tokens: &mut Tokenizer<'src>,
        start: Span,
    ) -> (Vec<PreprocessedToken<'src>>, Span) {
        let (exprs, next_directive_location) = self.preprocess_loop(tokens);
        let location = start.start..next_directive_location.end;
        (exprs, location)
    }

    fn skip_comments(&mut self, tokens: &mut Tokenizer<'src>) {
        while let Some(token) = tokens.peek() {
            match &token.value {
                TokenValue::BlankLine => {
                    // Don't transfer this over, it has no meaning
                    // and formatting wants to strip it out anyways
                    tokens.next();
                },
                TokenValue::Comment(_) => {
                    let token = tokens.next().unwrap();
                    let text = match_unwrap!(token.value, TokenValue::Comment(text) => text);
                    let token = PreprocessedToken {
                        value: PreprocessedTokenValue::Comment(text),
                        location: token.location.clone(),
                    };
                    self.expressions.push(token);
                },
                TokenValue::BlockComment(_) => {
                    let token = tokens.next().unwrap();
                    let text = match_unwrap!(token.value, TokenValue::BlockComment(text) => text);
                    let token = PreprocessedToken {
                        value: PreprocessedTokenValue::BlockComment(text.clone()),
                        location: token.location.clone(),
                    };
                    self.expressions.push(token);
                },
                _ => break,
            }
        }
    }

    fn verify_eof(&mut self) {
        // Without this check, end-of-file will be checked repeatedly
        // if we end while still nested inside arrays
        if self.eof_checked {
            return;
        }

        for unmatched in &self.conditional_stack {
            let location = match &unmatched.false_location {
                Some(else_location) => else_location.clone(),
                None => unmatched.location.clone(),
            };
            let error = Diagnostic::new(DiagnosticKind::UnmatchedConditional, location.clone());
            self.errors.push(error);
            self.unexpected_eof = true;
        }

        self.eof_checked = true;
    }

    fn push_error(&mut self, kind: DiagnosticKind, location: Span) {
        self.errors.push(Diagnostic::new(kind, location));
    }
}

/// An error result from file parsing.
#[derive(thiserror::Error, Debug, PartialEq)]
pub struct ParseError {
    /// All diagnostics issued for the file.
    pub diagnostics: Vec<Diagnostic>,
    /// The number of unclosed arrays remaining when the end-of-file was reached.
    pub unclosed_array_count: usize,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("failed to parse the given text")
    }
}

/// An error result from file parsing, with recovered AST information.
#[derive(Debug, PartialEq)]
pub struct ParseRecoveryError<'src> {
    /// The recovered AST of the file.
    pub recovered: Vec<Expression<'src>>,
    /// All diagnostics issued for the file.
    pub diagnostics: Vec<Diagnostic>,
    /// The number of unclosed arrays remaining when the end-of-file was reached.
    pub unclosed_array_count: usize,
}

impl From<ParseRecoveryError<'_>> for ParseError {
    fn from(value: ParseRecoveryError<'_>) -> Self {
        Self {
            diagnostics: value.diagnostics,
            unclosed_array_count: value.unclosed_array_count,
        }
    }
}

struct Parser<'src> {
    options: ParseOptions,

    expressions: Vec<Expression<'src>>,

    array_stack: Vec<ArrayMarker>,
    eof_checked: bool,

    errors: Vec<Diagnostic>,
    unexpected_eof: bool,
    unclosed_array_count: usize,
}

impl<'src> Parser<'src> {
    pub fn new(options: ParseOptions) -> Self {
        Self {
            options,

            expressions: Vec::new(),

            array_stack: Vec::new(),
            eof_checked: false,

            errors: Vec::new(),
            unexpected_eof: false,
            unclosed_array_count: 0,
        }
    }

    pub fn parse(
        &mut self,
        tokens: Tokenizer<'src>,
    ) -> Result<Vec<Expression<'src>>, ParseRecoveryError<'src>> {
        // No items, Fox only, Final Destination.
        let final_location = {
            let text = tokens.source_text();
            text.len()..text.len()
        };

        let mut preprocessor = Preprocessor::new();
        let preprocessed = preprocessor.preprocess(tokens);

        self.errors = preprocessor.errors;
        self.unexpected_eof = preprocessor.unexpected_eof;

        let (ast, _) = self.parse_exprs(&mut preprocessed.into_iter().peekable());

        if self.unexpected_eof {
            self.push_error(DiagnosticKind::UnexpectedEof, final_location);
        }

        if self.errors.is_empty() {
            Ok(ast)
        } else {
            self.errors.sort_by(|left, right| left.sort_cmp(right));
            Err(ParseRecoveryError {
                recovered: ast,
                diagnostics: std::mem::take(&mut self.errors),
                unclosed_array_count: self.unclosed_array_count,
            })
        }
    }

    fn parse_exprs<I: Iterator<Item = PreprocessedToken<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
    ) -> (Vec<Expression<'src>>, Span) {
        let prev_exprs = std::mem::take(&mut self.expressions);

        let last_location = loop {
            match self.parse_node(tokens) {
                ProcessResult::Result(node) => self.expressions.push(node),
                ProcessResult::BlockEnd(end_location) => break end_location,
                ProcessResult::Skip => continue,
                ProcessResult::Error(kind, location) => {
                    self.push_error(kind, location);
                    continue;
                },
                ProcessResult::Eof => {
                    self.verify_eof();
                    match self.expressions.last() {
                        Some(last) => {
                            let last_location = last.location.clone();
                            break last_location;
                        },
                        None => break 0..0,
                    }
                },
            };
        };

        (std::mem::replace(&mut self.expressions, prev_exprs), last_location)
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
            None => ProcessResult::Error(DiagnosticKind::UnmatchedBrace { kind, open: false }, end),
        }
    }

    fn recover_mismatched_array<T>(&mut self, kind: ArrayKind, end: Span) -> ProcessResult<T> {
        if !self.array_stack.iter().any(|arr| arr.kind == kind) {
            return ProcessResult::Error(DiagnosticKind::UnmatchedBrace { kind, open: false }, end);
        }

        while let Some(array) = self.array_stack.last() {
            if array.kind == kind {
                break;
            }

            self.push_error(
                DiagnosticKind::UnmatchedBrace { kind: array.kind, open: true },
                array.location.clone(),
            );
            self.array_stack.pop();
        }

        ProcessResult::BlockEnd(end)
    }

    fn parse_node<I: Iterator<Item = PreprocessedToken<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
    ) -> ProcessResult<Expression<'src>> {
        let token = match tokens.next() {
            Some(token) => token,
            None => return ProcessResult::Eof,
        };

        let (kind, location) = match token.value {
            PreprocessedTokenValue::Integer(value) => (ExpressionValue::Integer(value), token.location),
            PreprocessedTokenValue::Float(value) => (ExpressionValue::Float(value), token.location),
            PreprocessedTokenValue::String(value) => (ExpressionValue::String(value), token.location),
            PreprocessedTokenValue::Symbol(value) => (ExpressionValue::Symbol(value), token.location),
            PreprocessedTokenValue::Variable(value) => (ExpressionValue::Variable(value), token.location),
            PreprocessedTokenValue::Unhandled => (ExpressionValue::Unhandled, token.location),

            PreprocessedTokenValue::ArrayOpen => {
                let (array, location) = self.open_array(tokens, &token.location, ArrayKind::Array);
                (ExpressionValue::Array(array), location)
            },
            PreprocessedTokenValue::CommandOpen => {
                let (array, location) = self.open_array(tokens, &token.location, ArrayKind::Command);
                (ExpressionValue::Command(array), location)
            },
            PreprocessedTokenValue::PropertyOpen => {
                let (array, location) = self.open_array(tokens, &token.location, ArrayKind::Property);
                (ExpressionValue::Property(array), location)
            },
            PreprocessedTokenValue::ArrayClose => return self.close_array(ArrayKind::Array, token.location),
            PreprocessedTokenValue::CommandClose => {
                return self.close_array(ArrayKind::Command, token.location)
            },
            PreprocessedTokenValue::PropertyClose => {
                return self.close_array(ArrayKind::Property, token.location)
            },

            PreprocessedTokenValue::Conditional { is_positive, symbol, true_branch, false_branch } => {
                let true_branch = self.parse_conditional_block(true_branch);
                let false_branch = false_branch.map(|block| self.parse_conditional_block(block));
                let expr = ExpressionValue::Conditional { is_positive, symbol, true_branch, false_branch };
                (expr, token.location)
            },

            PreprocessedTokenValue::Define(name) => {
                let result = self.array_directive(
                    tokens,
                    token.location.clone(),
                    ArrayKind::Array,
                    DirectiveArgumentDescription::MacroBody,
                );
                match result {
                    Ok((body, location)) => (ExpressionValue::Define(name, body), location),
                    Err((kind, location)) => return ProcessResult::Error(kind, location),
                }
            },
            PreprocessedTokenValue::Undefine(name) => (ExpressionValue::Undefine(name), token.location),
            PreprocessedTokenValue::Include(path) => (ExpressionValue::Include(path), token.location),
            PreprocessedTokenValue::IncludeOptional(path) => {
                (ExpressionValue::IncludeOptional(path), token.location)
            },
            PreprocessedTokenValue::Merge(name) => (ExpressionValue::Merge(name), token.location),
            PreprocessedTokenValue::Autorun => {
                let result = self.array_directive(
                    tokens,
                    token.location.clone(),
                    ArrayKind::Command,
                    DirectiveArgumentDescription::CommandBody,
                );
                match result {
                    Ok((body, location)) => (ExpressionValue::Autorun(body), location),
                    Err((kind, location)) => return ProcessResult::Error(kind, location),
                }
            },

            PreprocessedTokenValue::BlankLine => (ExpressionValue::BlankLine, token.location),
            PreprocessedTokenValue::Comment(text) => {
                if !self.options.include_comments {
                    return ProcessResult::Skip;
                }

                (ExpressionValue::Comment(text), token.location)
            },
            PreprocessedTokenValue::BlockComment(text) => {
                if !self.options.include_comments {
                    return ProcessResult::Skip;
                }

                (ExpressionValue::BlockComment(text), token.location)
            },
        };

        ProcessResult::Result(Expression { value: kind, location })
    }

    fn array_directive<I: Iterator<Item = PreprocessedToken<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
        location: Span,
        kind: ArrayKind,
        description: DirectiveArgumentDescription,
    ) -> Result<(ArrayExpression<'src>, Span), (DiagnosticKind, Span)> {
        let start_location = {
            let token_kind = match kind {
                ArrayKind::Array => TokenKind::ArrayOpen,
                ArrayKind::Command => TokenKind::CommandOpen,
                ArrayKind::Property => TokenKind::PropertyOpen,
            };

            // Note: comments between the directive and array will be emitted
            // before the full directive gets emitted. Trying to handle this more sanely
            // is far more complex than I have the patience for currently lol, and
            // it should impact very few scenarios in practice.
            self.skip_comments(tokens);

            let Some(next) = tokens.peek() else {
                self.unexpected_eof = true;
                return Err((
                    DiagnosticKind::MissingDirectiveArgument { missing: token_kind, description },
                    location,
                ));
            };

            if next.value.get_kind() != token_kind {
                return Err((
                    DiagnosticKind::IncorrectDirectiveArgument {
                        expected: token_kind,
                        expected_description: description,
                        actual: next.value.get_kind(),
                        expecting_location: location,
                    },
                    next.location.clone(),
                ));
            };

            let location = next.location.clone();
            tokens.next();
            location
        };

        let (body, body_location) = self.open_array(tokens, &start_location, kind);
        let full_location = location.start..body_location.end;
        Ok((ArrayExpression::new(body, body_location), full_location))
    }

    fn parse_conditional_block(
        &mut self,
        (branch, location): (Vec<PreprocessedToken<'src>>, Span),
    ) -> ArrayExpression<'src> {
        let mut block_parser = Parser::new(self.options.clone());
        let (branch, _) = block_parser.parse_exprs(&mut branch.into_iter().peekable());
        if block_parser
            .errors
            .iter()
            .any(|e| matches!(e.kind(), DiagnosticKind::UnmatchedBrace { .. }))
        {
            self.push_error(DiagnosticKind::UnbalancedConditional, location.clone())
        }
        self.errors.append(&mut block_parser.errors);

        ArrayExpression::new(branch, location)
    }

    fn skip_comments<I: Iterator<Item = PreprocessedToken<'src>>>(&mut self, tokens: &mut Peekable<I>) {
        while let Some(token) = tokens.peek() {
            match &token.value {
                PreprocessedTokenValue::BlankLine => {
                    // Don't transfer this over, it has no meaning
                    // and formatting wants to strip it out anyways
                    tokens.next();
                },
                PreprocessedTokenValue::Comment(_) => {
                    let token = tokens.next().unwrap();
                    let text = match_unwrap!(token.value, PreprocessedTokenValue::Comment(text) => text);
                    let token = Expression {
                        value: ExpressionValue::Comment(text),
                        location: token.location.clone(),
                    };
                    self.expressions.push(token);
                },
                PreprocessedTokenValue::BlockComment(_) => {
                    let token = tokens.next().unwrap();
                    let text = match_unwrap!(token.value, PreprocessedTokenValue::BlockComment(text) => text);
                    let token = Expression {
                        value: ExpressionValue::BlockComment(text.clone()),
                        location: token.location.clone(),
                    };
                    self.expressions.push(token);
                },
                _ => break,
            }
        }
    }

    fn push_error(&mut self, kind: DiagnosticKind, location: Span) {
        self.errors.push(Diagnostic::new(kind, location));
    }

    fn verify_eof(&mut self) {
        // Without this check, end-of-file will be checked repeatedly
        // if we end while still nested inside arrays
        if self.eof_checked {
            return;
        }

        for unmatched in &self.array_stack {
            let error = Diagnostic::new(
                DiagnosticKind::UnmatchedBrace { kind: unmatched.kind, open: true },
                unmatched.location.clone(),
            );
            self.errors.push(error);
            self.unexpected_eof = true;
        }

        self.eof_checked = true;
        self.unclosed_array_count = self.array_stack.len();
    }
}

pub fn parse_text(text: &str, options: ParseOptions) -> Result<Vec<Expression<'_>>, ParseRecoveryError<'_>> {
    parse_tokens(Tokenizer::new(text), options)
}

pub fn parse_tokens(
    tokens: Tokenizer<'_>,
    options: ParseOptions,
) -> Result<Vec<Expression<'_>>, ParseRecoveryError<'_>> {
    let mut parser = Parser::new(options);
    parser.parse(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TextToken;

    fn assert_directive_symbol_error(
        directive: &str,
        description: DirectiveArgumentDescription,
        assert_errors: fn(&str, Vec<(DiagnosticKind, Span)>),
    ) {
        fn assert_symbol_error(
            directive: &str,
            suffix: &str,
            description: DirectiveArgumentDescription,
            assert_errors: fn(&str, Vec<(DiagnosticKind, Span)>),
        ) {
            let text = directive.to_owned() + suffix;
            assert_errors(&text, vec![(
                DiagnosticKind::IncorrectDirectiveArgument {
                    expected: TokenKind::Symbol,
                    expected_description: description,
                    actual: TokenKind::Integer,
                    expecting_location: 0..directive.len(),
                },
                text.len() - 1..text.len(),
            )]);
        }

        assert_symbol_error(directive, " 1", description, assert_errors);
        // Ensure comments are properly ignored
        assert_symbol_error(directive, " ;asdf\n 1", description, assert_errors);
        assert_symbol_error(directive, " /* asdf */ 1", description, assert_errors);
    }

    fn assert_conditional_symbol_error(
        directive: &str,
        eof: bool,
        assert_errors: fn(&str, Vec<(DiagnosticKind, Span)>),
    ) {
        fn assert_symbol_error(
            directive: &str,
            suffix: &str,
            eof: bool,
            assert_errors: fn(&str, Vec<(DiagnosticKind, Span)>),
        ) {
            let text = directive.to_owned() + suffix;
            let mut errors = vec![
                (DiagnosticKind::UnmatchedConditional, 0..directive.len()),
                (
                    DiagnosticKind::IncorrectDirectiveArgument {
                        expected: TokenKind::Symbol,
                        expected_description: DirectiveArgumentDescription::MacroName,
                        actual: TokenKind::Integer,
                        expecting_location: 0..directive.len(),
                    },
                    text.len() - 1..text.len(),
                ),
            ];
            if eof {
                errors.push((DiagnosticKind::UnexpectedEof, text.len()..text.len()));
            }
            assert_errors(&text, errors);
        }

        assert_symbol_error(directive, " 1", eof, assert_errors);
        assert_symbol_error(directive, " ;asdf\n 1", eof, assert_errors);
        assert_symbol_error(directive, " /* asdf */ 1", eof, assert_errors);
    }

    mod preprocessor {
        use super::*;

        const fn new_pretoken(kind: PreprocessedTokenValue<'_>, location: Span) -> PreprocessedToken<'_> {
            PreprocessedToken { value: kind, location }
        }

        fn assert_tokens(text: &str, expected: Vec<PreprocessedToken>) {
            let tokens = Tokenizer::new(text);

            let mut preprocessor = Preprocessor::new();
            let preprocessed = preprocessor.preprocess(tokens);

            let errors = preprocessor.errors;
            if !errors.is_empty() {
                panic!("Errors encountered while preprocessing.\nText: {text}\nResult: {errors:?}")
            }
            assert_eq!(preprocessed, expected, "Unexpected result for '{text}'");
        }

        fn assert_errors(text: &str, expected: Vec<(DiagnosticKind, Span)>) {
            let expected = Vec::from_iter(expected.into_iter().map(|(k, l)| Diagnostic::new(k, l)));
            let tokens = Tokenizer::new(text);

            let mut preprocessor = Preprocessor::new();
            let preprocessed = preprocessor.preprocess(tokens);

            let mut errors = preprocessor.errors;
            if errors.is_empty() {
                panic!("Expected preprocessing errors, got success instead.\nText: {text}\nResult: {preprocessed:?}")
            }
            errors.sort_by(|left, right| left.sort_cmp(right));
            assert_eq!(errors, expected, "Unexpected result for '{text}'");
        }

        #[test]
        fn integer() {
            assert_tokens("1 2 3", vec![
                new_pretoken(PreprocessedTokenValue::Integer(1), 0..1),
                new_pretoken(PreprocessedTokenValue::Integer(2), 2..3),
                new_pretoken(PreprocessedTokenValue::Integer(3), 4..5),
            ]);
        }

        #[test]
        fn float() {
            assert_tokens("1.0 2.0 3.0", vec![
                new_pretoken(PreprocessedTokenValue::Float(1.0), 0..3),
                new_pretoken(PreprocessedTokenValue::Float(2.0), 4..7),
                new_pretoken(PreprocessedTokenValue::Float(3.0), 8..11),
            ]);
        }

        #[test]
        fn string() {
            assert_tokens("\"a\" \"b\" \"c\"", vec![
                new_pretoken(PreprocessedTokenValue::make_string("a"), 0..3),
                new_pretoken(PreprocessedTokenValue::make_string("b"), 4..7),
                new_pretoken(PreprocessedTokenValue::make_string("c"), 8..11),
            ]);
        }

        #[test]
        fn symbol() {
            assert_tokens("asdf + '10'", vec![
                new_pretoken(PreprocessedTokenValue::make_symbol("asdf"), 0..4),
                new_pretoken(PreprocessedTokenValue::make_symbol("+"), 5..6),
                new_pretoken(PreprocessedTokenValue::make_symbol("10"), 7..11),
            ]);
        }

        #[test]
        fn variable() {
            assert_tokens("$asdf $this", vec![
                new_pretoken(PreprocessedTokenValue::make_variable("asdf"), 0..5),
                new_pretoken(PreprocessedTokenValue::make_variable("this"), 6..11),
            ]);
        }

        #[test]
        fn unhandled() {
            assert_tokens("kDataUnhandled", vec![new_pretoken(
                PreprocessedTokenValue::Unhandled,
                0..14,
            )])
        }

        #[test]
        fn arrays() {
            assert_tokens("(asdf \"text\" 1)", vec![
                new_pretoken(PreprocessedTokenValue::ArrayOpen, 0..1),
                new_pretoken(PreprocessedTokenValue::make_symbol("asdf"), 1..5),
                new_pretoken(PreprocessedTokenValue::make_string("text"), 6..12),
                new_pretoken(PreprocessedTokenValue::Integer(1), 13..14),
                new_pretoken(PreprocessedTokenValue::ArrayClose, 14..15),
            ]);
            assert_tokens("{set $var \"asdf\"}", vec![
                new_pretoken(PreprocessedTokenValue::CommandOpen, 0..1),
                new_pretoken(PreprocessedTokenValue::make_symbol("set"), 1..4),
                new_pretoken(PreprocessedTokenValue::make_variable("var"), 5..9),
                new_pretoken(PreprocessedTokenValue::make_string("asdf"), 10..16),
                new_pretoken(PreprocessedTokenValue::CommandClose, 16..17),
            ]);
            assert_tokens("[property]", vec![
                new_pretoken(PreprocessedTokenValue::PropertyOpen, 0..1),
                new_pretoken(PreprocessedTokenValue::make_symbol("property"), 1..9),
                new_pretoken(PreprocessedTokenValue::PropertyClose, 9..10),
            ]);
        }

        #[test]
        fn comments() {
            assert_tokens("; comment", vec![new_pretoken(
                PreprocessedTokenValue::make_comment(" comment"),
                0..9,
            )]);
            assert_tokens("/* block comment */", vec![new_pretoken(
                PreprocessedTokenValue::BlockComment(BlockCommentToken {
                    open: TextToken::new("/*", 0..2),
                    body: TextToken::new(" block comment ", 2..17),
                    close: TextToken::new("*/", 17..19),
                }),
                0..19,
            )]);
            assert_tokens("/*symbol*/", vec![new_pretoken(
                PreprocessedTokenValue::make_symbol("/*symbol*/"),
                0..10,
            )]);
            assert_tokens("*/", vec![new_pretoken(PreprocessedTokenValue::make_symbol("*/"), 0..2)]);

            assert_errors("/*", vec![
                (DiagnosticKind::UnclosedBlockComment, 0..2),
                // (DiagnosticKind::UnexpectedEof, 2..2), // tested in super::parser
            ]);
            assert_errors("/*a bunch of\ntext", vec![
                (DiagnosticKind::UnclosedBlockComment, 0..17),
                // (DiagnosticKind::UnexpectedEof, 17..17), // tested in super::parser
            ]);
        }

        #[test]
        fn directives() {
            assert_tokens("#define kDefine (1)", vec![
                new_pretoken(PreprocessedTokenValue::Define(StrExpression::new("kDefine", 8..15)), 0..15),
                new_pretoken(PreprocessedTokenValue::ArrayOpen, 16..17),
                new_pretoken(PreprocessedTokenValue::Integer(1), 17..18),
                new_pretoken(PreprocessedTokenValue::ArrayClose, 18..19),
            ]);
            assert_tokens("#undef kDefine", vec![new_pretoken(
                PreprocessedTokenValue::Undefine(StrExpression::new("kDefine", 7..14)),
                0..14,
            )]);
            assert_tokens("#include ../file.dta", vec![new_pretoken(
                PreprocessedTokenValue::Include(StrExpression::new("../file.dta", 9..20)),
                0..20,
            )]);
            assert_tokens("#include_opt ../file.dta", vec![new_pretoken(
                PreprocessedTokenValue::IncludeOptional(StrExpression::new("../file.dta", 13..24)),
                0..24,
            )]);
            assert_tokens("#merge ../file.dta", vec![new_pretoken(
                PreprocessedTokenValue::Merge(StrExpression::new("../file.dta", 7..18)),
                0..18,
            )]);
            assert_tokens("#autorun {print \"Auto-run action\"}", vec![
                new_pretoken(PreprocessedTokenValue::Autorun, 0..8),
                new_pretoken(PreprocessedTokenValue::CommandOpen, 9..10),
                new_pretoken(PreprocessedTokenValue::make_symbol("print"), 10..15),
                new_pretoken(PreprocessedTokenValue::make_string("Auto-run action"), 16..33),
                new_pretoken(PreprocessedTokenValue::CommandClose, 33..34),
            ]);

            assert_directive_symbol_error("#define", DirectiveArgumentDescription::MacroName, assert_errors);
            assert_directive_symbol_error("#undef", DirectiveArgumentDescription::MacroName, assert_errors);
            assert_directive_symbol_error("#include", DirectiveArgumentDescription::FilePath, assert_errors);
            assert_directive_symbol_error(
                "#include_opt",
                DirectiveArgumentDescription::FilePath,
                assert_errors,
            );
            assert_directive_symbol_error("#merge", DirectiveArgumentDescription::FilePath, assert_errors);

            // tested in super::parser
            // assert_directive_incomplete_error("#define", assert_errors);
            // assert_directive_incomplete_error("#undef", assert_errors);
            // assert_directive_incomplete_error("#include", assert_errors);
            // assert_directive_incomplete_error("#include_opt", assert_errors);
            // assert_directive_incomplete_error("#merge", assert_errors);

            assert_errors("#bad", vec![(DiagnosticKind::BadDirective, 0..4)]);
        }

        #[test]
        fn conditionals() {
            assert_tokens("#ifdef kDefine (array1 10) #else (array2 5) #endif", vec![new_pretoken(
                PreprocessedTokenValue::Conditional {
                    is_positive: true,
                    symbol: StrExpression::new("kDefine", 7..14),
                    true_branch: (
                        vec![
                            new_pretoken(PreprocessedTokenValue::ArrayOpen, 15..16),
                            new_pretoken(PreprocessedTokenValue::make_symbol("array1"), 16..22),
                            new_pretoken(PreprocessedTokenValue::Integer(10), 23..25),
                            new_pretoken(PreprocessedTokenValue::ArrayClose, 25..26),
                        ],
                        0..32,
                    ),
                    false_branch: Some((
                        vec![
                            new_pretoken(PreprocessedTokenValue::ArrayOpen, 33..34),
                            new_pretoken(PreprocessedTokenValue::make_symbol("array2"), 34..40),
                            new_pretoken(PreprocessedTokenValue::Integer(5), 41..42),
                            new_pretoken(PreprocessedTokenValue::ArrayClose, 42..43),
                        ],
                        27..50,
                    )),
                },
                0..50,
            )]);
            assert_tokens("#ifndef kDefine (array 10) #endif", vec![new_pretoken(
                PreprocessedTokenValue::Conditional {
                    is_positive: false,
                    symbol: StrExpression::new("kDefine", 8..15),
                    true_branch: (
                        vec![
                            new_pretoken(PreprocessedTokenValue::ArrayOpen, 16..17),
                            new_pretoken(PreprocessedTokenValue::make_symbol("array"), 17..22),
                            new_pretoken(PreprocessedTokenValue::Integer(10), 23..25),
                            new_pretoken(PreprocessedTokenValue::ArrayClose, 25..26),
                        ],
                        0..33,
                    ),
                    false_branch: None,
                },
                0..33,
            )]);

            assert_conditional_symbol_error("#ifdef", false, assert_errors);
            assert_conditional_symbol_error("#ifndef", false, assert_errors);

            // tested in super::parser
            // assert_conditional_eof_error("#ifdef", assert_errors);
            // assert_conditional_eof_error("#ifndef", assert_errors);

            assert_errors("#ifndef kDefine (array 10)", vec![
                (DiagnosticKind::UnmatchedConditional, 0..7),
                // (DiagnosticKind::UnexpectedEof, 26..26), // tested in super::parser
            ]);
            assert_errors("#ifdef kDefine (array1 10) #else", vec![
                (DiagnosticKind::UnmatchedConditional, 27..32),
                // (DiagnosticKind::UnexpectedEof, 32..32), // tested in super::parser
            ]);
            assert_errors("#else (array2 5) #endif", vec![(
                DiagnosticKind::UnexpectedConditional,
                0..5,
            )]);
            assert_errors("(array 10) #endif", vec![(DiagnosticKind::UnexpectedConditional, 11..17)]);
        }
    }

    mod parser {
        use super::*;

        const fn new_expression(kind: ExpressionValue<'_>, location: Span) -> Expression<'_> {
            Expression { value: kind, location }
        }

        fn assert_parsed(text: &str, expected: Vec<Expression>) {
            assert_parsed_with_options(text, ParseOptions { include_comments: true }, expected)
        }

        fn assert_parsed_with_options(text: &str, options: ParseOptions, expected: Vec<Expression>) {
            let ast = match parse_text(text, options) {
                Ok(ast) => ast,
                Err(error) => {
                    panic!(
                        "Errors encountered while parsing.\nText: {text}\nResult: {:?}",
                        error.diagnostics
                    )
                },
            };
            assert_eq!(ast, expected, "Unexpected result for '{text}'");
        }

        fn assert_errors(text: &str, expected: Vec<(DiagnosticKind, Span)>) {
            let expected = Vec::from_iter(expected.into_iter().map(|(k, l)| Diagnostic::new(k, l)));
            let error = match parse_text(text, ParseOptions { include_comments: true }) {
                Ok(ast) => {
                    panic!("Expected parsing errors, got success instead.\nText: {text}\nResult: {ast:?}")
                },
                Err(err) => err,
            };
            assert_eq!(error.diagnostics, expected, "Unexpected result for '{text}'");
            assert_eq!(error.unclosed_array_count, 0, "Unexpected unclosed arrays for '{text}'");
        }

        fn assert_errors_and_unclosed(
            text: &str,
            diagnostics: Vec<(DiagnosticKind, Span)>,
            unclosed_count: usize,
        ) {
            let expected = Vec::from_iter(diagnostics.into_iter().map(|(k, l)| Diagnostic::new(k, l)));
            let err = match parse_text(text, ParseOptions { include_comments: true }) {
                Ok(ast) => {
                    panic!("Expected parsing errors, got success instead.\nText: {text}\nResult: {ast:?}")
                },
                Err(err) => err,
            };
            assert_eq!(err.diagnostics, expected, "Unexpected result for '{text}'");
            assert_eq!(
                err.unclosed_array_count, unclosed_count,
                "Wrong unclosed array count for '{text}'"
            );
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
                new_expression(ExpressionValue::make_string("a"), 0..3),
                new_expression(ExpressionValue::make_string("b"), 4..7),
                new_expression(ExpressionValue::make_string("c"), 8..11),
            ]);
        }

        #[test]
        fn symbol() {
            assert_parsed("asdf + '10'", vec![
                new_expression(ExpressionValue::make_symbol("asdf"), 0..4),
                new_expression(ExpressionValue::make_symbol("+"), 5..6),
                new_expression(ExpressionValue::make_symbol("10"), 7..11),
            ]);
        }

        #[test]
        fn variable() {
            assert_parsed("$asdf $this", vec![
                new_expression(ExpressionValue::make_variable("asdf"), 0..5),
                new_expression(ExpressionValue::make_variable("this"), 6..11),
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
                    new_expression(ExpressionValue::make_symbol("asdf"), 1..5),
                    new_expression(ExpressionValue::make_string("text"), 6..12),
                    new_expression(ExpressionValue::Integer(1), 13..14),
                ]),
                0..15,
            )]);
            assert_parsed("{set $var \"asdf\"}", vec![new_expression(
                ExpressionValue::Command(vec![
                    new_expression(ExpressionValue::make_symbol("set"), 1..4),
                    new_expression(ExpressionValue::make_variable("var"), 5..9),
                    new_expression(ExpressionValue::make_string("asdf"), 10..16),
                ]),
                0..17,
            )]);
            assert_parsed("[property]", vec![new_expression(
                ExpressionValue::Property(vec![new_expression(
                    ExpressionValue::make_symbol("property"),
                    1..9,
                )]),
                0..10,
            )]);

            fn assert_array_mismatch(
                text: &str,
                kind: ArrayKind,
                open: bool,
                location: Span,
                eof_expected: bool,
            ) {
                let mut expected = vec![(DiagnosticKind::UnmatchedBrace { kind, open }, location)];
                if !eof_expected {
                    expected.push((DiagnosticKind::UnexpectedEof, text.len()..text.len()));
                }
                let remaining_depth = !eof_expected as usize;

                assert_errors_and_unclosed(text, expected, remaining_depth);
            }

            fn assert_array_mismatches(kind: ArrayKind) {
                let (l, r) = kind.delimiters();

                // ( ( )
                // ) ( )
                // ( ) (
                // ( ) )
                assert_array_mismatch(&format!("{l} {l} {r}"), kind, true, 0..1, false);
                assert_array_mismatch(&format!("{r} {l} {r}"), kind, false, 0..1, true);
                assert_array_mismatch(&format!("{l} {r} {l}"), kind, true, 4..5, false);
                assert_array_mismatch(&format!("{l} {r} {r}"), kind, false, 4..5, true);
            }

            fn assert_multi_array_mismatches(matched_kind: ArrayKind, unmatched_kind: ArrayKind) {
                let (ml, mr) = matched_kind.delimiters();
                let (ul, ur) = unmatched_kind.delimiters();

                // { ( )
                // } ( )
                // ( { )
                // ( } )
                // ( ) {
                // ( ) }
                assert_array_mismatch(&format!("{ul} {ml} {mr}"), unmatched_kind, true, 0..1, false);
                assert_array_mismatch(&format!("{ur} {ml} {mr}"), unmatched_kind, false, 0..1, true);
                assert_array_mismatch(&format!("{ml} {ul} {mr}"), unmatched_kind, true, 2..3, true);
                assert_array_mismatch(&format!("{ml} {ur} {mr}"), unmatched_kind, false, 2..3, true);
                assert_array_mismatch(&format!("{ml} {mr} {ul}"), unmatched_kind, true, 4..5, false);
                assert_array_mismatch(&format!("{ml} {mr} {ur}"), unmatched_kind, false, 4..5, true);
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

            assert_errors_and_unclosed(
                "(((((",
                vec![
                    (DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Array, open: true }, 0..1),
                    (DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Array, open: true }, 1..2),
                    (DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Array, open: true }, 2..3),
                    (DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Array, open: true }, 3..4),
                    (DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Array, open: true }, 4..5),
                    (DiagnosticKind::UnexpectedEof, 5..5),
                ],
                5,
            );
        }

        #[test]
        fn comments() {
            assert_parsed("; comment", vec![new_expression(
                ExpressionValue::make_comment(" comment"),
                0..9,
            )]);
            assert_parsed("/* block comment */", vec![new_expression(
                ExpressionValue::BlockComment(BlockCommentToken {
                    open: TextToken::new("/*", 0..2),
                    body: TextToken::new(" block comment ", 2..17),
                    close: TextToken::new("*/", 17..19),
                }),
                0..19,
            )]);
            assert_parsed("/*symbol*/", vec![new_expression(
                ExpressionValue::make_symbol("/*symbol*/"),
                0..10,
            )]);
            assert_parsed("*/", vec![new_expression(ExpressionValue::make_symbol("*/"), 0..2)]);

            assert_errors("/*", vec![
                (DiagnosticKind::UnclosedBlockComment, 0..2),
                (DiagnosticKind::UnexpectedEof, 2..2),
            ]);
            assert_errors("/*a bunch of\ntext", vec![
                (DiagnosticKind::UnclosedBlockComment, 0..17),
                (DiagnosticKind::UnexpectedEof, 17..17),
            ]);
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
                        new_expression(ExpressionValue::make_symbol("print"), 10..15),
                        new_expression(ExpressionValue::make_string("Auto-run action"), 16..33),
                    ],
                    9..34,
                )),
                0..34,
            )]);

            fn assert_directive_incomplete_error(
                directive: &str,
                expected_token: TokenKind,
                description: DirectiveArgumentDescription,
            ) {
                fn assert_incomplete_error(
                    directive: &str,
                    suffix: &str,
                    expected_token: TokenKind,
                    description: DirectiveArgumentDescription,
                ) {
                    let text = directive.to_owned() + suffix;
                    assert_errors(&text, vec![
                        (
                            DiagnosticKind::MissingDirectiveArgument { missing: expected_token, description },
                            0..directive.len(),
                        ),
                        (DiagnosticKind::UnexpectedEof, text.len()..text.len()),
                    ]);
                }

                assert_incomplete_error(directive, "", expected_token, description);
                assert_incomplete_error(directive, " ; asdf", expected_token, description);
                assert_incomplete_error(directive, " ; asdf\n", expected_token, description);
                assert_incomplete_error(directive, " /* asdf */", expected_token, description);
            }

            assert_directive_symbol_error("#define", DirectiveArgumentDescription::MacroName, assert_errors);
            assert_directive_symbol_error("#undef", DirectiveArgumentDescription::MacroName, assert_errors);
            assert_directive_symbol_error("#include", DirectiveArgumentDescription::FilePath, assert_errors);
            assert_directive_symbol_error(
                "#include_opt",
                DirectiveArgumentDescription::FilePath,
                assert_errors,
            );
            assert_directive_symbol_error("#merge", DirectiveArgumentDescription::FilePath, assert_errors);

            assert_directive_incomplete_error(
                "#define",
                TokenKind::Symbol,
                DirectiveArgumentDescription::MacroName,
            );
            assert_directive_incomplete_error(
                "#undef",
                TokenKind::Symbol,
                DirectiveArgumentDescription::MacroName,
            );
            assert_directive_incomplete_error(
                "#include",
                TokenKind::Symbol,
                DirectiveArgumentDescription::FilePath,
            );
            assert_directive_incomplete_error(
                "#include_opt",
                TokenKind::Symbol,
                DirectiveArgumentDescription::FilePath,
            );
            assert_directive_incomplete_error(
                "#merge",
                TokenKind::Symbol,
                DirectiveArgumentDescription::FilePath,
            );
            assert_directive_incomplete_error(
                "#autorun",
                TokenKind::CommandOpen,
                DirectiveArgumentDescription::CommandBody,
            );

            assert_errors("#define kDefine 1", vec![(
                DiagnosticKind::IncorrectDirectiveArgument {
                    expected: TokenKind::ArrayOpen,
                    expected_description: DirectiveArgumentDescription::MacroBody,
                    actual: TokenKind::Integer,
                    expecting_location: 0..15,
                },
                16..17,
            )]);
            assert_errors("#autorun kDefine", vec![(
                DiagnosticKind::IncorrectDirectiveArgument {
                    expected: TokenKind::CommandOpen,
                    expected_description: DirectiveArgumentDescription::CommandBody,
                    actual: TokenKind::Symbol,
                    expecting_location: 0..8,
                },
                9..16,
            )]);

            assert_errors("#bad", vec![(DiagnosticKind::BadDirective, 0..4)]);
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
                                new_expression(ExpressionValue::make_symbol("array1"), 16..22),
                                new_expression(ExpressionValue::Integer(10), 23..25),
                            ]),
                            15..26,
                        )],
                        0..32,
                    ),
                    false_branch: Some(ArrayExpression::new(
                        vec![new_expression(
                            ExpressionValue::Array(vec![
                                new_expression(ExpressionValue::make_symbol("array2"), 34..40),
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
                                new_expression(ExpressionValue::make_symbol("array"), 17..22),
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

            fn assert_conditional_incomplete_error(directive: &str) {
                assert_errors(directive, vec![
                    (
                        DiagnosticKind::MissingDirectiveArgument {
                            missing: TokenKind::Symbol,
                            description: DirectiveArgumentDescription::MacroName,
                        },
                        0..directive.len(),
                    ),
                    (DiagnosticKind::UnmatchedConditional, 0..directive.len()),
                    (DiagnosticKind::UnexpectedEof, directive.len()..directive.len()),
                ]);
            }

            assert_conditional_symbol_error("#ifdef", true, assert_errors);
            assert_conditional_symbol_error("#ifndef", true, assert_errors);

            assert_conditional_incomplete_error("#ifdef");
            assert_conditional_incomplete_error("#ifndef");

            assert_errors("#ifndef kDefine (array 10)", vec![
                (DiagnosticKind::UnmatchedConditional, 0..7),
                (DiagnosticKind::UnexpectedEof, 26..26),
            ]);
            assert_errors("#ifdef kDefine (array1 10) #else", vec![
                (DiagnosticKind::UnmatchedConditional, 27..32),
                (DiagnosticKind::UnexpectedEof, 32..32),
            ]);
            assert_errors("#else (array2 5) #endif", vec![(
                DiagnosticKind::UnexpectedConditional,
                0..5,
            )]);
            assert_errors("(array 10) #endif", vec![(DiagnosticKind::UnexpectedConditional, 11..17)]);

            assert_errors("(#ifdef kDefine array1 10) #else array2 5) #endif)", vec![
                (DiagnosticKind::UnbalancedConditional, 1..32),
                (
                    DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Array, open: false },
                    25..26,
                ),
                (DiagnosticKind::UnbalancedConditional, 27..49),
                (
                    DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Array, open: false },
                    41..42,
                ),
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
                    (DiagnosticKind::UnbalancedConditional, 0..25),
                    (
                        DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Command, open: true },
                        15..16,
                    ),
                    (DiagnosticKind::UnbalancedConditional, 38..61),
                    (
                        DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Command, open: false },
                        53..54,
                    ),
                ],
            );
        }

        #[test]
        fn options() {
            let text = "\
                ; Line comment\
              \n(foo 10)\
              \n/* Block comment */\
              \n(bar \"text\")\
              \n\
            ";

            let options = ParseOptions { include_comments: true };
            assert_parsed_with_options(text, options, vec![
                new_expression(ExpressionValue::make_comment(" Line comment"), 0..14),
                new_expression(
                    ExpressionValue::Array(vec![
                        new_expression(ExpressionValue::make_symbol("foo"), 16..19),
                        new_expression(ExpressionValue::Integer(10), 20..22),
                    ]),
                    15..23,
                ),
                new_expression(
                    ExpressionValue::BlockComment(BlockCommentToken {
                        open: TextToken::new("/*", 24..26),
                        body: TextToken::new(" Block comment ", 26..41),
                        close: TextToken::new("*/", 41..43),
                    }),
                    24..43,
                ),
                new_expression(
                    ExpressionValue::Array(vec![
                        new_expression(ExpressionValue::make_symbol("bar"), 45..48),
                        new_expression(ExpressionValue::make_string("text"), 49..55),
                    ]),
                    44..56,
                ),
            ]);

            let options = ParseOptions { include_comments: false };
            assert_parsed_with_options(text, options, vec![
                new_expression(
                    ExpressionValue::Array(vec![
                        new_expression(ExpressionValue::make_symbol("foo"), 16..19),
                        new_expression(ExpressionValue::Integer(10), 20..22),
                    ]),
                    15..23,
                ),
                new_expression(
                    ExpressionValue::Array(vec![
                        new_expression(ExpressionValue::make_symbol("bar"), 45..48),
                        new_expression(ExpressionValue::make_string("text"), 49..55),
                    ]),
                    44..56,
                ),
            ]);
        }
    }
}
