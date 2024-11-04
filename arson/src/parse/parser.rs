use std::iter::Peekable;

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
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'src> {
    pub kind: ExpressionKind<'src>,
    pub location: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError<'src> {
    LexingError(LexError),
    UnexpectedEof,
    UnmatchedBrace(Span, ArrayKind),
    BadDirective(Span),
    IncorrectToken {
        expected: TokenKind<'static>,
        actual: Token<'src>,
    },
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

enum ParseNodeResult<'src> {
    Node(Expression<'src>),
    ArrayEnd(Span),
    SkipToken,
    Eof,
}

#[derive(Clone)]
struct Parser<'src> {
    array_stack: Vec<ArrayMarker>,
    errors: Vec<ParseError<'src>>,
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

impl<'src> Parser<'src> {
    pub fn new() -> Self {
        Self {
            array_stack: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn parse_exprs<I: Iterator<Item = Token<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
    ) -> (Vec<Expression<'src>>, Span) {
        let mut exprs = Vec::new();

        loop {
            match self.parse_node(tokens) {
                ParseNodeResult::Node(node) => exprs.push(node),
                ParseNodeResult::ArrayEnd(end_location) => return (exprs, end_location),
                ParseNodeResult::SkipToken => continue,
                ParseNodeResult::Eof => match exprs.last().cloned() {
                    Some(last) => return (exprs, last.location.clone()),
                    None => return (exprs, 0..0),
                },
            };
        }
    }

    fn parse_array<I: Iterator<Item = Token<'src>>>(
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

    fn open_array<I: Iterator<Item = Token<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
        start: &Span,
        kind: ArrayKind,
    ) -> (Vec<Expression<'src>>, Span) {
        let (array, end) = self.parse_array(tokens, kind, start.clone());
        (array, start.start..end.end)
    }

    fn close_array(&mut self, kind: ArrayKind, end: Span) -> ParseNodeResult<'src> {
        match self.array_stack.last() {
            Some(last) => {
                if kind != last.kind {
                    return self.recover_mismatched_array(kind, end);
                }
            },
            None => {
                self.errors.push(ParseError::UnmatchedBrace(end, kind));
                return ParseNodeResult::SkipToken;
            },
        }

        return ParseNodeResult::ArrayEnd(end);
    }

    fn recover_mismatched_array(&mut self, kind: ArrayKind, end: Span) -> ParseNodeResult<'src> {
        todo!()
    }

    fn parse_node<I: Iterator<Item = Token<'src>>>(&mut self, tokens: &mut Peekable<I>) -> ParseNodeResult<'src> {
        let token = match tokens.next() {
            None => {
                if let Some(unmatched) = self.array_stack.last() {
                    self.errors
                        .push(ParseError::UnmatchedBrace(unmatched.location.clone(), unmatched.kind));
                }

                return ParseNodeResult::Eof;
            },
            Some(token) => token,
        };

        let (kind, location) = match token.kind {
            TokenKind::Integer(value) => (ExpressionKind::Integer(value), token.location),
            TokenKind::Float(value) => (ExpressionKind::Float(value), token.location),
            TokenKind::String(value) => (ExpressionKind::String(value), token.location),
            TokenKind::Symbol(value) => (ExpressionKind::Symbol(value), token.location),
            TokenKind::Variable(value) => (ExpressionKind::Variable(value), token.location),
            TokenKind::Unhandled => (ExpressionKind::Unhandled, token.location),

            TokenKind::ArrayOpen => {
                let (array, location) = self.open_array(tokens, &token.location, ArrayKind::Array);
                (ExpressionKind::Array(array), location)
            },
            TokenKind::CommandOpen => {
                let (array, location) = self.open_array(tokens, &token.location, ArrayKind::Command);
                (ExpressionKind::Command(array), location)
            },
            TokenKind::PropertyOpen => {
                let (array, location) = self.open_array(tokens, &token.location, ArrayKind::Property);
                (ExpressionKind::Property(array), location)
            },
            TokenKind::ArrayClose => return self.close_array(ArrayKind::Array, token.location),
            TokenKind::CommandClose => return self.close_array(ArrayKind::Command, token.location),
            TokenKind::PropertyClose => return self.close_array(ArrayKind::Property, token.location),

            TokenKind::Ifdef => todo!(),
            TokenKind::Ifndef => todo!(),
            TokenKind::Else => todo!(),
            TokenKind::Endif => todo!(),

            TokenKind::Define => {
                let (name, name_location) = next_token!(self, tokens, Symbol("dummy"));
                let name = StrExpression::new(name, name_location.clone());

                let start_location = next_token!(self, tokens, ArrayOpen);
                let (body, body_location) = self.open_array(tokens, &start_location, ArrayKind::Array);
                let body = ArrayExpression::new(body, body_location.clone());

                (
                    ExpressionKind::Define(name, body),
                    token.location.start..body_location.end,
                )
            },
            TokenKind::Undefine => {
                let (name, name_location) = next_token!(self, tokens, Symbol("dummy"));
                let name = StrExpression::new(name, name_location.clone());
                (ExpressionKind::Undefine(name), token.location.start..name_location.end)
            },
            TokenKind::Include => {
                let (path, path_location) = next_token!(self, tokens, Symbol("dummy"));
                let path = StrExpression::new(path, path_location.clone());
                (ExpressionKind::Include(path), token.location.start..path_location.end)
            },
            TokenKind::IncludeOptional => {
                let (path, path_location) = next_token!(self, tokens, Symbol("dummy"));
                let path = StrExpression::new(path, path_location.clone());
                (
                    ExpressionKind::IncludeOptional(path),
                    token.location.start..path_location.end,
                )
            },
            TokenKind::Merge => {
                let (name, name_location) = next_token!(self, tokens, Symbol("dummy"));
                let name = StrExpression::new(name, name_location.clone());
                (ExpressionKind::Merge(name), token.location.start..name_location.end)
            },
            TokenKind::Autorun => {
                let start_location = next_token!(self, tokens, CommandOpen);
                let (body, body_location) = self.open_array(tokens, &start_location, ArrayKind::Command);
                let body = ArrayExpression::new(body, body_location.clone());
                (ExpressionKind::Autorun(body), token.location.start..body_location.end)
            },

            TokenKind::BadDirective(_) => {
                self.errors.push(ParseError::BadDirective(token.location));
                return ParseNodeResult::SkipToken;
            },

            TokenKind::Comment => return ParseNodeResult::SkipToken,
            TokenKind::BlockCommentStart(_) => {
                self.skip_block_comment(tokens);
                return ParseNodeResult::SkipToken;
            },
            TokenKind::BlockCommentEnd(_) => return ParseNodeResult::SkipToken,
        };

        return ParseNodeResult::Node(Expression { kind, location });
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

    fn incorrect_token(&mut self, expected: TokenKind<'static>, actual: Token<'src>) -> ParseNodeResult<'src> {
        self.errors
            .push(ParseError::IncorrectToken { expected, actual });
        return ParseNodeResult::SkipToken;
    }

    fn unexpected_eof(&mut self) -> ParseNodeResult<'src> {
        self.errors.push(ParseError::UnexpectedEof);
        ParseNodeResult::Eof
    }
}

fn filter_token_errors(tokens: Vec<Result<Token<'_>, LexError>>) -> Result<Vec<Token<'_>>, Vec<ParseError>> {
    let mut errors = Vec::new();
    let tokens = tokens
        .into_iter()
        .filter_map(|t| match t {
            Ok(token) => Some(token),
            Err(error) => {
                errors.push(ParseError::LexingError(error));
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
    let (exprs, _) = parser.parse_exprs(&mut tokens.into_iter().peekable());
    match parser.errors.is_empty() {
        true => Ok(exprs),
        false => Err(parser.errors),
    }
}
