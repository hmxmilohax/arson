use std::iter::Peekable;

use logos::Span;

use super::lexer::{Token, TokenKind};

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
    UnexpectedEof,
    UnmatchedBrace(Span, ArrayKind),
    UnexpectedConditional(Span),
    UnmatchedConditional(Span),
    UnbalancedConditional(Span),
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

struct ConditionalMarker<'src> {
    location: Span,
    array_depth: usize,
    is_positive: bool,
    symbol: StrExpression<'src>,
    false_branch: Option<ArrayExpression<'src>>,
}

enum ParseNodeResult<'src> {
    Node(Expression<'src>),
    ArrayEnd(Span),
    SkipToken,
    Eof,
}

struct Parser<'src> {
    array_stack: Vec<ArrayMarker>,
    conditional_stack: Vec<ConditionalMarker<'src>>,
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
            conditional_stack: Vec::new(),
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
                ParseNodeResult::Eof => {
                    self.verify_eof();
                    match exprs.last().cloned() {
                        Some(last) => return (exprs, last.location.clone()),
                        None => return (exprs, 0..0),
                    }
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

    fn parse_conditional<I: Iterator<Item = Token<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
        start: Span,
        positive: bool,
        symbol: StrExpression<'src>,
    ) -> (ExpressionKind<'src>, Span) {
        self.conditional_stack.push(ConditionalMarker {
            location: start.clone(),
            array_depth: self.array_stack.len(),
            is_positive: positive,
            symbol,
            false_branch: None,
        });

        let (true_exprs, true_location) = self.parse_conditional_block(tokens, start);
        let conditional = self
            .conditional_stack
            .pop()
            .expect("conditional was added just above");

        if self.array_stack.len() != conditional.array_depth {
            self.errors
                .push(ParseError::UnbalancedConditional(true_location.clone()));
        }

        let location = match &conditional.false_branch {
            Some(false_branch) => conditional.location.start..false_branch.location.end,
            None => conditional.location.start..true_location.end,
        };

        (
            ExpressionKind::Conditional {
                is_positive: conditional.is_positive,
                symbol: conditional.symbol,
                true_branch: ArrayExpression::new(true_exprs, true_location),
                false_branch: conditional.false_branch,
            },
            location,
        )
    }

    fn parse_conditional_block<I: Iterator<Item = Token<'src>>>(
        &mut self,
        tokens: &mut Peekable<I>,
        start: Span,
    ) -> (Vec<Expression<'src>>, Span) {
        let (exprs, next_directive_location) = self.parse_exprs(tokens);
        let location = start.start..next_directive_location.end;
        (exprs, location)
    }

    fn verify_eof(&mut self) {
        let mut unexpected_eof = false;
        for unmatched in &self.array_stack {
            self.errors
                .push(ParseError::UnmatchedBrace(unmatched.location.clone(), unmatched.kind));
            unexpected_eof = true;
        }

        for unmatched in &self.conditional_stack {
            self.errors
                .push(ParseError::UnmatchedConditional(unmatched.location.clone()));
            unexpected_eof = true;
        }

        if unexpected_eof && !self.errors.contains(&ParseError::UnexpectedEof) {
            self.errors.push(ParseError::UnexpectedEof);
        }
    }

    fn parse_node<I: Iterator<Item = Token<'src>>>(&mut self, tokens: &mut Peekable<I>) -> ParseNodeResult<'src> {
        let token = match tokens.next() {
            None => return ParseNodeResult::Eof,
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

            TokenKind::Ifdef => {
                let (name, name_location) = next_token!(self, tokens, Symbol("dummy"));
                self.parse_conditional(tokens, token.location, true, StrExpression::new(name, name_location))
            },
            TokenKind::Ifndef => {
                let (name, name_location) = next_token!(self, tokens, Symbol("dummy"));
                self.parse_conditional(tokens, token.location, false, StrExpression::new(name, name_location))
            },
            TokenKind::Else => {
                let (exprs, location) = self.parse_conditional_block(tokens, token.location.clone());
                match self.conditional_stack.last_mut() {
                    Some(conditional) => {
                        conditional.false_branch = Some(ArrayExpression::new(exprs, location));
                        return ParseNodeResult::ArrayEnd(token.location);
                    },
                    None => todo!("#else error recovery"),
                }
            },
            TokenKind::Endif => match self.conditional_stack.last() {
                Some(_) => return ParseNodeResult::ArrayEnd(token.location),
                None => todo!("#endif error recovery"),
            },

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

pub fn parse(tokens: Vec<Token<'_>>) -> Result<Vec<Expression>, Vec<ParseError>> {
    let mut parser = Parser::new();
    let (exprs, _) = parser.parse_exprs(&mut tokens.into_iter().peekable());
    match parser.errors.is_empty() {
        true => Ok(exprs),
        false => Err(parser.errors),
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::lexer;

    use super::*;

    const fn new_expression(kind: ExpressionKind<'_>, location: Span) -> Expression<'_> {
        Expression { kind, location }
    }

    fn assert_parsed(text: &str, exprs: Vec<Expression>) {
        let tokens = lexer::lex(text).expect("no token errors expected in this test");
        let result = match parse(tokens) {
            Ok(exprs) => exprs,
            Err(errs) => panic!("Errors encountered while parsing: {errs:?}"),
        };
        assert_eq!(result, exprs);
    }

    fn assert_errors(text: &str, errs: Vec<ParseError<'_>>) {
        let tokens = lexer::lex(text).expect("no token errors expected in this test");
        let result = match parse(tokens) {
            Ok(exprs) => panic!("Expected parsing errors, got AST instead: {exprs:?}"),
            Err(errs) => errs,
        };
        assert_eq!(result, errs);
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
    }

    fn assert_directive_symbol_error(name: &str) {
        let text = name.to_owned() + " 1";
        assert_errors(
            &text,
            vec![ParseError::IncorrectToken {
                expected: TokenKind::Symbol("dummy"),
                actual: Token::new(TokenKind::Integer(1), name.len() + 1..name.len() + 2),
            }],
        );
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

        assert_errors(
            "#define kDefine 1",
            vec![ParseError::IncorrectToken {
                expected: TokenKind::ArrayOpen,
                actual: Token::new(TokenKind::Integer(1), 16..17),
            }],
        );
        assert_errors(
            "#autorun kDefine",
            vec![ParseError::IncorrectToken {
                expected: TokenKind::CommandOpen,
                actual: Token::new(TokenKind::Symbol("kDefine"), 9..16),
            }],
        );

        assert_errors("#bad", vec![ParseError::BadDirective(0..4)]);
    }

    #[test]
    fn conditionals() {
        assert_parsed(
            r#"
            #ifdef kDefine
                (array1 10)
            #else
                (array2 5)
            #endif
            "#,
            vec![new_expression(
                ExpressionKind::Conditional {
                    is_positive: true,
                    symbol: StrExpression::new("kDefine", 20..27),
                    true_branch: ArrayExpression::new(
                        vec![new_expression(
                            ExpressionKind::Array(vec![
                                new_expression(ExpressionKind::Symbol("array1"), 45..51),
                                new_expression(ExpressionKind::Integer(10), 52..54),
                            ]),
                            44..55,
                        )],
                        13..73,
                    ),
                    false_branch: Some(ArrayExpression::new(
                        vec![new_expression(
                            ExpressionKind::Array(vec![
                                new_expression(ExpressionKind::Symbol("array2"), 91..97),
                                new_expression(ExpressionKind::Integer(5), 98..99),
                            ]),
                            90..100,
                        )],
                        68..119,
                    )),
                },
                13..119,
            )],
        );
        assert_parsed(
            r#"
            #ifndef kDefine
                (array 10)
            #endif
            "#,
            vec![new_expression(
                ExpressionKind::Conditional {
                    is_positive: false,
                    symbol: StrExpression::new("kDefine", 21..28),
                    true_branch: ArrayExpression::new(
                        vec![new_expression(
                            ExpressionKind::Array(vec![
                                new_expression(ExpressionKind::Symbol("array"), 46..51),
                                new_expression(ExpressionKind::Integer(10), 52..54),
                            ]),
                            45..55,
                        )],
                        13..74,
                    ),
                    false_branch: None,
                },
                13..74,
            )],
        );

        assert_directive_symbol_error("#ifdef");
        assert_directive_symbol_error("#ifndef");

        assert_errors(
            r#"
            #ifndef kDefine
                (array 10)
            "#,
            vec![ParseError::UnmatchedConditional(14..21)],
        );
        assert_errors(
            r#"
            #ifdef kDefine
                (array1 10)
            #else
            "#,
            vec![ParseError::UnmatchedConditional(71..76)],
        );
        assert_errors(
            r#"
            #else
                (array2 5)
            #endif
            "#,
            vec![ParseError::UnexpectedConditional(14..19)],
        );
        assert_errors(
            r#"
                (array 10)
            #endif
            "#,
            vec![ParseError::UnexpectedConditional(42..48)],
        );

        assert_errors(
            r#"
            (
            #ifdef kDefine
                array1 10)
            #else
                array2 5)
            #endif
            )
            "#,
            vec![
                ParseError::UnbalancedConditional(29..90),
                ParseError::UnbalancedConditional(85..137)
            ],
        );
    }
}
