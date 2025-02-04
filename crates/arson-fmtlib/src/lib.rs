// SPDX-License-Identifier: LGPL-3.0-or-later

//! A formatter library for DTA files.

#![warn(missing_docs)]

use arson_parse::ParseError;

mod consts;
pub mod expr;
pub mod token;

/// The indentation to use when formatting.
#[derive(Debug, Clone, Copy)]
pub enum Indentation {
    /// Use tabs when formatting.
    /// The inner size value is how many characters a tab should be considered to be.
    Tabs(usize),

    /// Use spaces when formatting.
    Spaces(usize),
}

/// Options for formatting.
#[derive(Debug, Clone)]
pub struct Options {
    /// The indentation style to use.
    pub indentation: Indentation,
    /// The maximum width of arrays in the output.
    pub max_array_width: usize,
    /// The maximum width of lines in the output.
    pub max_line_width: usize,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            indentation: Indentation::Spaces(3),
            max_array_width: 75,
            max_line_width: 90,
        }
    }
}

/// Formats the given input text to a new string.
///
/// This will first attempt to do a full parse on the input text and use an expression-based formatter.
/// Failing that, it will fall back to a more forgiving but less capable token-based formatter
/// which formats on a best-effort basis.
///
/// # Example
///
/// ```rust
/// let input = "(1 2 3 4) (a b c d) (outer (inner \"some text\")) (foo (bar \"some text\") (baz 10.0))";
/// let options = arson_fmtlib::Options::default();
/// let formatted = arson_fmtlib::format_to_string(input, options).unwrap();
/// assert_eq!(&formatted, "\
///     (1 2 3 4)\
///   \n(a b c d)\
///   \n(outer (inner \"some text\"))\
///   \n(foo\
///   \n   (bar \"some text\")\
///   \n   (baz 10.0)\
///   \n)\
/// ")
/// ```
pub fn format_to_string(input: &str, options: Options) -> Result<String, (String, ParseError)> {
    match expr::Formatter::new(input, options.clone()) {
        Ok(f) => Ok(f.to_string()),
        Err(error) => Err((token::Formatter::new(input, options).to_string(), error)),
    }
}

/// The formatter for DTA text.
///
/// This formatter does output through the [`std::fmt::Display`] trait
/// to make it possible to output to both [`std::fmt::Write`] and [`std::io::Write`]
/// without going through an intermediate [`String`].
/// Use [`format`], [`write`], or [`to_string`](ToString::to_string) to perform the
/// actual formatting and output.
///
/// # Example
///
/// ```rust
/// let input = "(1 2 3 4) (a b c d) (outer (inner \"some text\")) (foo (bar \"some text\") (baz 10.0))";
/// let options = arson_fmtlib::Options::default();
/// let formatter = arson_fmtlib::expr::Formatter::new(input, options).unwrap();
///
/// let formatted = formatter.to_string();
/// assert_eq!(&formatted, "\
///     (1 2 3 4)\
///   \n(a b c d)\
///   \n(outer (inner \"some text\"))\
///   \n(foo\
///   \n   (bar \"some text\")\
///   \n   (baz 10.0)\
///   \n)\
/// ")
/// ```
pub enum Formatter<'src> {
    /// Expression-based formatter (see [`expr::Formatter`]).
    Expression(expr::Formatter<'src>),
    /// Token-based formatter (see [`token::Formatter`]).
    Token(token::Formatter<'src>),
}

impl<'src> Formatter<'src> {
    /// Creates a new [`Formatter`] with the given input and options.
    pub fn new(input: &'src str, options: Options) -> (Self, Option<ParseError>) {
        match expr::Formatter::new(input, options.clone()) {
            Ok(formatter) => (Self::Expression(formatter), None),
            Err(error) => {
                let formatter = token::Formatter::new(input, options);
                (Self::Token(formatter), Some(error))
            },
        }
    }
}

impl std::fmt::Display for Formatter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expression(formatter) => formatter.fmt(f),
            Self::Token(formatter) => formatter.fmt(f),
        }
    }
}
