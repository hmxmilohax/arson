// SPDX-License-Identifier: LGPL-3.0-or-later

use std::iter::Peekable;
use std::marker::PhantomData;

use logos::Span;

use super::{Diagnostic, DiagnosticKind, TokenKind, TokenValue, Tokenizer};
use crate::{ArrayKind, DirectiveArgumentDescription, FloatValue, IntegerValue};

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
enum PreprocessedTokenValue<'src> {
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
}

impl PreprocessedTokenValue<'_> {
    fn get_kind(&self) -> TokenKind {
        match self {
            PreprocessedTokenValue::Integer(_) => TokenKind::Integer,
            PreprocessedTokenValue::Float(_) => TokenKind::Float,
            PreprocessedTokenValue::String(_) => TokenKind::String,
            PreprocessedTokenValue::Symbol(_) => TokenKind::Symbol,
            PreprocessedTokenValue::Variable(_) => TokenKind::Variable,
            PreprocessedTokenValue::Unhandled => TokenKind::Unhandled,

            PreprocessedTokenValue::ArrayOpen => TokenKind::ArrayOpen,
            PreprocessedTokenValue::ArrayClose => TokenKind::ArrayClose,
            PreprocessedTokenValue::CommandOpen => TokenKind::CommandOpen,
            PreprocessedTokenValue::CommandClose => TokenKind::CommandClose,
            PreprocessedTokenValue::PropertyOpen => TokenKind::PropertyOpen,
            PreprocessedTokenValue::PropertyClose => TokenKind::PropertyClose,

            PreprocessedTokenValue::Define(_) => TokenKind::Define,
            PreprocessedTokenValue::Undefine(_) => TokenKind::Undefine,
            PreprocessedTokenValue::Include(_) => TokenKind::Include,
            PreprocessedTokenValue::IncludeOptional(_) => TokenKind::IncludeOptional,
            PreprocessedTokenValue::Merge(_) => TokenKind::Merge,
            PreprocessedTokenValue::Autorun => TokenKind::Autorun,

            PreprocessedTokenValue::Conditional { .. } => TokenKind::Ifdef,
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
    Error(DiagnosticKind, Span),
    Eof,
}

struct Preprocessor<'src> {
    conditional_stack: Vec<ConditionalMarker<'src>>,
    errors: Vec<Diagnostic>,
    eof_checked: bool,
    unexpected_eof: bool,
}

impl<'src> Preprocessor<'src> {
    pub fn new() -> Self {
        Self {
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
        let mut processed = Vec::new();

        loop {
            match self.process_token(tokens) {
                ProcessResult::Result(node) => processed.push(node),
                ProcessResult::BlockEnd(end_location) => return (processed, end_location),
                ProcessResult::SkipToken => continue,
                ProcessResult::Error(kind, location) => {
                    self.push_error(kind, location);
                    continue;
                },
                ProcessResult::Eof => {
                    self.verify_eof();
                    match processed.last().cloned() {
                        Some(last) => {
                            let last_location = last.location.clone();
                            return (processed, last_location);
                        },
                        None => return (processed, 0..0),
                    }
                },
            };
        }
    }

    fn symbol_directive(
        &mut self,
        tokens: &mut Tokenizer<'src>,
        location: Span,
        kind: impl Fn(StrExpression<'src>) -> PreprocessedTokenValue<'src>,
        description: DirectiveArgumentDescription,
    ) -> ProcessResult<PreprocessedToken<'src>> {
        let Some(name_token) = tokens.peek() else {
            self.unexpected_eof = true;
            return ProcessResult::Error(
                DiagnosticKind::MissingDirectiveArgument { missing: TokenKind::Symbol, description },
                location,
            );
        };

        let name_location = name_token.location.clone();
        let TokenValue::Symbol(name) = name_token.value else {
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
        tokens.next();

        let name = StrExpression::new(name, name_location.clone());
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

            TokenValue::Comment(_) => return ProcessResult::SkipToken,
            TokenValue::BlockComment(_) => return ProcessResult::SkipToken,

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
            TokenValue::Symbol(value) => {
                tokens.next();
                value
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
                "<invalid>"
            },
        };

        self.handle_conditional(tokens, start.clone(), ConditionalMarker {
            location: start,
            is_positive: positive,
            symbol: StrExpression::new(define_name, define_location),
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

struct Parser<'src> {
    array_stack: Vec<ArrayMarker>,
    errors: Vec<Diagnostic>,
    eof_checked: bool,
    unexpected_eof: bool,
    phantom: PhantomData<&'src ()>,
}

impl<'src> Parser<'src> {
    pub fn new() -> Self {
        Self {
            array_stack: Vec::new(),
            errors: Vec::new(),
            eof_checked: false,
            unexpected_eof: false,
            phantom: PhantomData,
        }
    }

    pub fn parse(&mut self, tokens: Tokenizer<'src>) -> Vec<Expression<'src>> {
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

        self.errors.sort_by(|left, right| left.sort_cmp(right));

        ast
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
                ProcessResult::Error(kind, location) => {
                    self.push_error(kind, location);
                    continue;
                },
                ProcessResult::Eof => {
                    self.verify_eof();
                    match exprs.last() {
                        Some(last) => {
                            let last_location = last.location.clone();
                            return (exprs, last_location);
                        },
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

            self.push_error(DiagnosticKind::UnmatchedBrace { kind: array.kind, open: true }, array.location.clone());
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
        let mut block_parser = Parser::new();
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
            let error =
                Diagnostic::new(DiagnosticKind::UnmatchedBrace { kind: unmatched.kind, open: true }, unmatched.location.clone());
            self.errors.push(error);
            self.unexpected_eof = true;
        }

        self.eof_checked = true;
    }
}

pub fn parse_text(text: &str) -> Result<Vec<Expression<'_>>, Vec<Diagnostic>> {
    parse_tokens(Tokenizer::new(text))
}

pub fn parse_tokens<'src>(tokens: Tokenizer<'src>) -> Result<Vec<Expression<'src>>, Vec<Diagnostic>> {
    let mut parser = Parser::new();
    let ast = parser.parse(tokens);
    match parser.errors.is_empty() {
        true => Ok(ast),
        false => Err(parser.errors),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_directive_symbol_error(
        directive: &str,
        description: DirectiveArgumentDescription,
        assert_errors: fn(&str, Vec<(DiagnosticKind, Span)>),
    ) {
        let text = directive.to_owned() + " 1";
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

    fn assert_conditional_symbol_error(
        directive: &str,
        eof: bool,
        assert_errors: fn(&str, Vec<(DiagnosticKind, Span)>),
    ) {
        let text = directive.to_owned() + " 1";
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
                new_pretoken(PreprocessedTokenValue::String("a"), 0..3),
                new_pretoken(PreprocessedTokenValue::String("b"), 4..7),
                new_pretoken(PreprocessedTokenValue::String("c"), 8..11),
            ]);
        }

        #[test]
        fn symbol() {
            assert_tokens("asdf + '10'", vec![
                new_pretoken(PreprocessedTokenValue::Symbol("asdf"), 0..4),
                new_pretoken(PreprocessedTokenValue::Symbol("+"), 5..6),
                new_pretoken(PreprocessedTokenValue::Symbol("10"), 7..11),
            ]);
        }

        #[test]
        fn variable() {
            assert_tokens("$asdf $this", vec![
                new_pretoken(PreprocessedTokenValue::Variable("asdf"), 0..5),
                new_pretoken(PreprocessedTokenValue::Variable("this"), 6..11),
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
                new_pretoken(PreprocessedTokenValue::Symbol("asdf"), 1..5),
                new_pretoken(PreprocessedTokenValue::String("text"), 6..12),
                new_pretoken(PreprocessedTokenValue::Integer(1), 13..14),
                new_pretoken(PreprocessedTokenValue::ArrayClose, 14..15),
            ]);
            assert_tokens("{set $var \"asdf\"}", vec![
                new_pretoken(PreprocessedTokenValue::CommandOpen, 0..1),
                new_pretoken(PreprocessedTokenValue::Symbol("set"), 1..4),
                new_pretoken(PreprocessedTokenValue::Variable("var"), 5..9),
                new_pretoken(PreprocessedTokenValue::String("asdf"), 10..16),
                new_pretoken(PreprocessedTokenValue::CommandClose, 16..17),
            ]);
            assert_tokens("[property]", vec![
                new_pretoken(PreprocessedTokenValue::PropertyOpen, 0..1),
                new_pretoken(PreprocessedTokenValue::Symbol("property"), 1..9),
                new_pretoken(PreprocessedTokenValue::PropertyClose, 9..10),
            ]);
        }

        #[test]
        fn comments() {
            assert_tokens("; comment", vec![]);
            assert_tokens("/* block comment */", vec![]);
            assert_tokens("/*symbol*/", vec![new_pretoken(
                PreprocessedTokenValue::Symbol("/*symbol*/"),
                0..10,
            )]);
            assert_tokens("*/", vec![new_pretoken(PreprocessedTokenValue::Symbol("*/"), 0..2)]);

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
                new_pretoken(PreprocessedTokenValue::Symbol("print"), 10..15),
                new_pretoken(PreprocessedTokenValue::String("Auto-run action"), 16..33),
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
            // assert_directive_eof_error("#define", assert_errors);
            // assert_directive_eof_error("#undef", assert_errors);
            // assert_directive_eof_error("#include", assert_errors);
            // assert_directive_eof_error("#include_opt", assert_errors);
            // assert_directive_eof_error("#merge", assert_errors);

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
                            new_pretoken(PreprocessedTokenValue::Symbol("array1"), 16..22),
                            new_pretoken(PreprocessedTokenValue::Integer(10), 23..25),
                            new_pretoken(PreprocessedTokenValue::ArrayClose, 25..26),
                        ],
                        0..32,
                    ),
                    false_branch: Some((
                        vec![
                            new_pretoken(PreprocessedTokenValue::ArrayOpen, 33..34),
                            new_pretoken(PreprocessedTokenValue::Symbol("array2"), 34..40),
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
                            new_pretoken(PreprocessedTokenValue::Symbol("array"), 17..22),
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
            let ast = match parse_text(text) {
                Ok(ast) => ast,
                Err(errs) => panic!("Errors encountered while parsing.\nText: {text}\nResult: {errs:?}"),
            };
            assert_eq!(ast, expected, "Unexpected result for '{text}'");
        }

        fn assert_errors(text: &str, expected: Vec<(DiagnosticKind, Span)>) {
            let expected = Vec::from_iter(expected.into_iter().map(|(k, l)| Diagnostic::new(k, l)));
            let errors = match parse_text(text) {
                Ok(ast) => {
                    panic!("Expected parsing errors, got success instead.\nText: {text}\nResult: {ast:?}")
                },
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

            fn assert_array_mismatch(text: &str, kind: ArrayKind, open: bool, location: Span, eof_expected: bool) {
                let mut expected = vec![(DiagnosticKind::UnmatchedBrace { kind, open }, location)];
                if !eof_expected {
                    expected.push((DiagnosticKind::UnexpectedEof, text.len()..text.len()));
                }

                assert_errors(text, expected);
            }

            fn assert_array_mismatches(kind: ArrayKind) {
                let (l, r) = kind.delimiters();
                assert_array_mismatch(&format!("{l} {l} {r}"), kind, true, 0..1, false);
                assert_array_mismatch(&format!("{r} {l} {r}"), kind, false, 0..1, true);
                assert_array_mismatch(&format!("{l} {r} {l}"), kind, true, 4..5, false);
                assert_array_mismatch(&format!("{l} {r} {r}"), kind, false, 4..5, true);
            }

            fn assert_multi_array_mismatches(matched_kind: ArrayKind, unmatched_kind: ArrayKind) {
                let (ml, mr) = matched_kind.delimiters();
                let (ul, ur) = unmatched_kind.delimiters();

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
        }

        #[test]
        fn comments() {
            assert_parsed("; comment", vec![]);
            assert_parsed("/* block comment */", vec![]);
            assert_parsed("/*symbol*/", vec![new_expression(
                ExpressionValue::Symbol("/*symbol*/"),
                0..10,
            )]);
            assert_parsed("*/", vec![new_expression(ExpressionValue::Symbol("*/"), 0..2)]);

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
                        new_expression(ExpressionValue::Symbol("print"), 10..15),
                        new_expression(ExpressionValue::String("Auto-run action"), 16..33),
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
                assert_errors(directive, vec![
                    (
                        DiagnosticKind::MissingDirectiveArgument { missing: expected_token, description },
                        0..directive.len(),
                    ),
                    (DiagnosticKind::UnexpectedEof, directive.len()..directive.len()),
                ]);
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
                (DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Array, open: false }, 25..26),
                (DiagnosticKind::UnbalancedConditional, 27..49),
                (DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Array, open: false }, 41..42),
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
                    (DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Command, open: true }, 15..16),
                    (DiagnosticKind::UnbalancedConditional, 38..61),
                    (DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Command, open: false }, 53..54),
                ],
            );
        }
    }
}
