// SPDX-License-Identifier: LGPL-3.0-or-later

//! Formats DTA using a token-based formatter.
//!
//! Because using tokens offers less rich information about how the file is structured,
//! this formatter doesn't have the same characteristics as the expression-based one,
//! and is best used as a fallback.

use std::fmt::{self, Write};

use arson_parse::{ArrayKind, Token, TokenValue, Tokenizer};

use crate::{Indentation, Options, COMMAND_SAME_LINE_ARGS};

/// Formats the given input text to a new string using the token-based formatter.
///
/// # Example
///
/// ```rust
/// let input = "(1 2 3 4) (a b c d) (outer (inner \"some text\")) (foo (bar \"some text\") (baz 10.0))";
/// let options = arson_fmtlib::Options::default();
/// let formatted = arson_fmtlib::token::format_to_string(input, options);
/// assert_eq!(&formatted, "\
///     (1 2 3 4)\
///   \n(a b c d)\
///   \n(outer\
///   \n   (inner \"some text\")\
///   \n)\
///   \n(foo\
///   \n   (bar \"some text\")\
///   \n   (baz 10.0)\
///   \n)\
/// ")
/// ```
pub fn format_to_string(input: &str, options: Options) -> String {
    Formatter::new(input, options).to_string()
}

/// The token-based formatter for DTA text.
///
/// # Example
///
/// ```rust
/// let input = "(1 2 3 4) (a b c d) (outer (inner \"some text\")) (foo (bar \"some text\") (baz 10.0))";
/// let options = arson_fmtlib::Options::default();
/// let formatter = arson_fmtlib::token::Formatter::new(input, options);
///
/// let formatted = formatter.to_string();
/// assert_eq!(&formatted, "\
///     (1 2 3 4)\
///   \n(a b c d)\
///   \n(outer\
///   \n   (inner \"some text\")\
///   \n)\
///   \n(foo\
///   \n   (bar \"some text\")\
///   \n   (baz 10.0)\
///   \n)\
/// ")
/// ```
pub struct Formatter<'src> {
    input: &'src str,
    options: Options,
}

impl<'src> Formatter<'src> {
    /// Creates a new [`Formatter`] with the given input and options.
    pub fn new(input: &'src str, options: Options) -> Self {
        Self { options, input }
    }
}

impl fmt::Display for Formatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        InnerFormatter::new(self.options.clone(), self.input).format_input(f)
    }
}

struct InnerFormatter<'src> {
    options: Options,

    input: &'src str,
    tokens: Tokenizer<'src>,

    indent_level: usize,
    indent_text: String,
}

impl<'src> InnerFormatter<'src> {
    fn new(options: Options, input: &'src str) -> Self {
        let indent_text = match options.indentation {
            Indentation::Tabs(_) => "\t".to_owned(),
            Indentation::Spaces(count) => std::iter::repeat_n(' ', count).collect(),
        };

        Self {
            options,

            input,
            tokens: Tokenizer::new(input),

            indent_level: 0,
            indent_text,
        }
    }

    fn format_input(&mut self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        while let Some(token) = self.tokens.next() {
            self.format_token(&token, f)?;
        }

        Ok(())
    }

    fn write_token_indented(&self, token: &Token<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        self.write_indent(f)?;
        self.write_token_unindented(token, f)
    }

    fn write_token_line(&mut self, token: &Token<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        self.write_token_indented(token, f)?;
        self.format_possible_comment(token, f)?;
        self.write_possible_line(f)
    }

    fn write_token_spaced(&self, token: &Token<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        f.write_char(' ')?;
        self.write_token_unindented(token, f)
    }

    fn write_token_unindented(&self, token: &Token<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        f.write_str(&self.input[token.location.clone()])
    }

    fn write_possible_line(&self, f: &mut impl fmt::Write) -> fmt::Result {
        if self.tokens.peek().is_some() {
            f.write_char('\n')?;
        }
        Ok(())
    }

    fn format_token(&mut self, token: &Token<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match token.value {
            TokenValue::Integer(_) => self.write_token_line(token, f)?,
            TokenValue::Float(_) => self.write_token_line(token, f)?,
            TokenValue::String(_) => self.write_token_line(token, f)?,

            TokenValue::Symbol(_) => self.write_token_line(token, f)?,
            TokenValue::Variable(_) => self.write_token_line(token, f)?,
            TokenValue::Unhandled => self.write_token_line(token, f)?,

            TokenValue::ArrayOpen => self.format_array_open(token, ArrayKind::Array, f)?,
            TokenValue::CommandOpen => self.format_array_open(token, ArrayKind::Command, f)?,
            TokenValue::PropertyOpen => self.format_array_open(token, ArrayKind::Property, f)?,

            TokenValue::ArrayClose => self.format_array_close(token, f)?,
            TokenValue::CommandClose => self.format_array_close(token, f)?,
            TokenValue::PropertyClose => self.format_array_close(token, f)?,

            TokenValue::Define => self.format_define(token, f)?,
            TokenValue::Undefine => self.format_symbol_directive_line(token, f)?,
            TokenValue::Include => self.format_symbol_directive_line(token, f)?,
            TokenValue::IncludeOptional => self.format_symbol_directive_line(token, f)?,
            TokenValue::Merge => self.format_symbol_directive_line(token, f)?,
            TokenValue::Autorun => self.format_autorun(token, f)?,

            TokenValue::Ifdef => self.format_symbol_directive_line(token, f)?,
            TokenValue::Ifndef => self.format_symbol_directive_line(token, f)?,
            TokenValue::Else => self.write_token_line(token, f)?,
            TokenValue::Endif => self.write_token_line(token, f)?,

            TokenValue::Comment(_) => self.write_token_line(token, f)?,
            TokenValue::BlockComment(_) => self.write_token_line(token, f)?,

            TokenValue::Error(_) => self.write_token_line(token, f)?,
        }

        Ok(())
    }

    fn format_array_open(
        &mut self,
        open_token: &Token<'_>,
        kind: ArrayKind,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        self.write_indent(f)?;
        self.format_array_open_unindented(open_token, kind, f)
    }

    fn probe_array(&mut self) -> Result<String, Vec<Token<'src>>> {
        let mut is_short = true;
        let mut short_str = String::new();
        let mut array_tokens = Vec::new();

        macro_rules! try_write {
            ($($arg:tt)+) => {
                match $($arg)+ {
                    Ok(ok) => ok,
                    Err(_) => return Err(array_tokens),
                }
            }
        }

        while let Some(token) = self.tokens.peek() {
            match token.value {
                TokenValue::ArrayClose => {
                    let token = self.tokens.next().unwrap();
                    try_write!(self.write_token_unindented(&token, &mut short_str));
                    try_write!(self.format_possible_comment(&token, &mut short_str));
                    break;
                },
                TokenValue::CommandClose => {
                    let token = self.tokens.next().unwrap();
                    try_write!(self.write_token_unindented(&token, &mut short_str));
                    try_write!(self.format_possible_comment(&token, &mut short_str));
                    break;
                },
                TokenValue::PropertyClose => {
                    let token = self.tokens.next().unwrap();
                    try_write!(self.write_token_unindented(&token, &mut short_str));
                    try_write!(self.format_possible_comment(&token, &mut short_str));
                    break;
                },

                TokenValue::ArrayOpen | TokenValue::CommandOpen | TokenValue::PropertyOpen => {
                    is_short = false;
                    break;
                },

                TokenValue::Ifdef | TokenValue::Ifndef | TokenValue::Else | TokenValue::Endif => {
                    is_short = false;
                    break;
                },

                TokenValue::Define
                | TokenValue::Undefine
                | TokenValue::Include
                | TokenValue::IncludeOptional
                | TokenValue::Merge
                | TokenValue::Autorun => {
                    is_short = false;
                    break;
                },

                TokenValue::Comment(_) | TokenValue::Error(_) => {
                    is_short = false;
                    break;
                },
                TokenValue::BlockComment(text) if text.contains('\n') => {
                    is_short = false;
                    break;
                }

                _ => {
                    let token = self.tokens.next().unwrap();

                    match short_str.is_empty() {
                        true => try_write!(self.write_token_unindented(&token, &mut short_str)),
                        false => try_write!(self.write_token_spaced(&token, &mut short_str)),
                    }

                    array_tokens.push(token);

                    if short_str.len() >= self.options.max_array_width {
                        is_short = false;
                        break;
                    }
                },
            }
        }

        if is_short {
            if self.tokens.peek().is_some() {
                short_str.push('\n');
            }

            Ok(short_str)
        } else {
            Err(array_tokens)
        }
    }

    fn format_array_open_unindented(
        &mut self,
        open_token: &Token<'_>,
        kind: ArrayKind,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        self.write_token_unindented(open_token, f)?;
        // Checked later, checking here would format things incorrectly
        // self.format_possible_comments(&open_token, f)?;

        let mut array_tokens = match self.probe_array() {
            Ok(short_str) => return f.write_str(&short_str),
            Err(tokens) => tokens,
        };

        self.indent_level += 1;

        fn pop_front<T>(vec: &mut Vec<T>) -> Option<T> {
            if vec.is_empty() {
                None
            } else {
                let remaining = vec.split_off(1);
                std::mem::replace(vec, remaining).pop()
            }
        }

        if let Some(ref first) = pop_front(&mut array_tokens) {
            match first.value {
                TokenValue::Symbol(name) => {
                    // Display leading symbol on the same line as the array opening
                    self.write_token_unindented(first, f)?;
                    self.format_possible_comment(first, f)?;

                    if matches!(kind, ArrayKind::Command) {
                        // Additional arguments which should be displayed on the same line
                        if let Some(arg_count) = (*COMMAND_SAME_LINE_ARGS).get(name) {
                            let mut count = array_tokens.len().min(*arg_count);

                            while count > 0 {
                                count -= 1;

                                let token = pop_front(&mut array_tokens).unwrap();
                                match token.value {
                                    TokenValue::Integer(_)
                                    | TokenValue::Float(_)
                                    | TokenValue::String(_)
                                    | TokenValue::Symbol(_)
                                    | TokenValue::Variable(_)
                                    | TokenValue::Unhandled => {
                                        self.write_token_spaced(&token, f)?;
                                        self.format_possible_comment(&token, f)?;
                                    },
                                    _ => {
                                        f.write_char('\n')?;
                                        self.write_token_indented(&token, f)?;
                                        self.format_possible_comment(&token, f)?;
                                        break;
                                    },
                                }
                            }
                        }
                    }

                    f.write_char('\n')?;
                },
                TokenValue::Integer(_) => {
                    // More than likely this is used as a data key if we're in a large array,
                    // print on the same line as the opening
                    self.write_token_unindented(first, f)?;
                    self.format_possible_comment(first, f)?;
                    f.write_char('\n')?;
                },
                TokenValue::Variable(_) => {
                    // Display leading variable on the same line as the array opening
                    self.write_token_unindented(first, f)?;
                    self.format_possible_comment(first, f)?;

                    if matches!(kind, ArrayKind::Command) {
                        // Also display the next following symbol on the same line
                        if let Some(next) = array_tokens.first() {
                            if matches!(next.value, TokenValue::Symbol(_)) {
                                self.write_token_spaced(next, f)?;
                                self.format_possible_comment(first, f)?;
                                pop_front(&mut array_tokens);
                            }
                        }
                    }

                    f.write_char('\n')?;
                },
                TokenValue::String(_) if matches!(kind, ArrayKind::Command) => {
                    // The first argument of a command being a string means to look up an object,
                    // format it similarly to variables
                    self.write_token_unindented(first, f)?;
                    self.format_possible_comment(first, f)?;

                    // Also display the next following symbol on the same line
                    if let Some(next) = array_tokens.first() {
                        if matches!(next.value, TokenValue::Symbol(_)) {
                            self.write_token_spaced(next, f)?;
                            self.format_possible_comment(next, f)?;
                            pop_front(&mut array_tokens);
                        }
                    }

                    f.write_char('\n')?;
                },
                _ => {
                    f.write_char('\n')?;
                    self.write_token_line(first, f)?;
                },
            }
        } else {
            f.write_char('\n')?;
        }

        for token in array_tokens {
            self.write_token_line(&token, f)?;
        }

        Ok(())
    }

    fn format_array_close(&mut self, close_token: &Token<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Unindent before writing token, or else it'll be indented wrong
        self.indent_level = self.indent_level.saturating_sub(1);
        self.write_token_line(close_token, f)
    }

    fn format_possible_comment(&mut self, last: &Token<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        if let Some(comment) = self.tokens.peek() {
            if matches!(comment.value, TokenValue::Comment(_) | TokenValue::BlockComment(_)) {
                let between = &self.input[last.location.end..comment.location.start];
                if !between.contains('\n') {
                    let token = self.tokens.next().unwrap();
                    self.write_token_spaced(&token, f)?;
                }
            }
        }

        Ok(())
    }

    fn format_symbol_directive(
        &mut self,
        directive_token: &Token<'_>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        self.write_token_indented(directive_token, f)?;
        self.format_possible_comment(directive_token, f)?;

        if let Some(next) = self.tokens.peek() {
            if matches!(next.value, TokenValue::Symbol(_)) {
                let next = self.tokens.next().unwrap();
                self.write_token_spaced(&next, f)?;
                self.format_possible_comment(&next, f)?;
            }
        }

        Ok(())
    }

    fn format_symbol_directive_line(
        &mut self,
        directive_token: &Token<'_>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        self.format_symbol_directive(directive_token, f)?;
        self.write_possible_line(f)
    }

    fn format_define(&mut self, directive_token: &Token<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.format_symbol_directive(directive_token, f)?;

        if let Some(next) = self.tokens.peek() {
            if !matches!(next.value, TokenValue::ArrayOpen) {
                return f.write_char('\n');
            }

            let open_token = self.tokens.next().unwrap();
            self.write_token_spaced(&open_token, f)?;

            match self.probe_array() {
                Ok(short_str) => return f.write_str(&short_str),
                Err(tokens) => {
                    f.write_char('\n')?;

                    self.indent_level += 1;

                    for token in tokens {
                        self.write_token_line(&token, f)?;
                    }
                },
            }
        }

        Ok(())
    }

    fn format_autorun(&mut self, directive_token: &Token<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write_token_indented(directive_token, f)?;
        self.format_possible_comment(directive_token, f)?;

        if let Some(next) = self.tokens.peek() {
            if matches!(next.value, TokenValue::CommandOpen) {
                let next = self.tokens.next().unwrap();
                f.write_char(' ')?;
                return self.format_array_open_unindented(&next, ArrayKind::Command, f);
            }

            return f.write_char('\n');
        }

        Ok(())
    }

    fn write_indent(&self, f: &mut impl fmt::Write) -> fmt::Result {
        for _i in 0..self.indent_level {
            f.write_str(&self.indent_text)?;
        }

        Ok(())
    }
}
