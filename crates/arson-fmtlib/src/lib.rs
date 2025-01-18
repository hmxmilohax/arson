// SPDX-License-Identifier: LGPL-3.0-or-later

//! A formatter library for DTA files.

#![warn(missing_docs)]

use std::cell::Cell;
use std::collections::HashMap;
use std::fmt::{self, Write};
use std::sync::LazyLock;

use arson_parse::{ArrayKind, Expression, ExpressionValue, ParseError};

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
            max_array_width: 60,
            max_line_width: 90,
        }
    }
}

/// Formats the given input text to a new string.
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
pub fn format_to_string(input: &str, options: Options) -> Result<String, ParseError> {
    Formatter::new(input, options).map(|f| f.to_string())
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
/// let formatter = arson_fmtlib::Formatter::new(input, options).unwrap();
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
#[derive(Debug)]
pub struct Formatter<'src> {
    options: Options,

    input: &'src str,
    ast: Vec<Expression<'src>>,

    indent_level: Cell<isize>,
    indent_text: String,
}

impl<'src> Formatter<'src> {
    /// Creates a new [`Formatter`] with the given input and options.
    pub fn new(input: &'src str, options: Options) -> Result<Formatter<'src>, ParseError> {
        let ast = arson_parse::parse_text(input)?;
        let indent_text = match options.indentation {
            Indentation::Tabs(_) => "\t".to_owned(),
            Indentation::Spaces(count) => std::iter::repeat_n(' ', count).collect(),
        };

        Ok(Self {
            options,
            input,
            ast,
            indent_level: Cell::new(0),
            indent_text,
        })
    }
}

impl fmt::Display for Formatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.ast.is_empty() {
            return Ok(());
        }

        // At the top level of the AST, print everything on its own line
        self.format_expr_noindent(&self.ast[0], f)?;
        for expr in &self.ast[1..] {
            f.write_char('\n')?;
            self.format_expr_noindent(expr, f)?;
        }

        Ok(())
    }
}

struct IndentGuard<'src> {
    formatter: &'src Formatter<'src>,
    saved: isize,
}

impl Drop for IndentGuard<'_> {
    fn drop(&mut self) {
        self.formatter.indent_level.set(self.saved);
    }
}

impl<'src> Formatter<'src> {
    fn write_original(&self, expr: &Expression<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.input[expr.location.clone()])
    }

    fn format_array(
        &self,
        array: &[Expression<'src>],
        kind: ArrayKind,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let (l, r) = kind.delimiters();
        f.write_char(l)?;
        self.format_array_(array, kind, f)?;
        f.write_char(r)
    }

    fn format_array_(
        &self,
        array: &[Expression<'src>],
        kind: ArrayKind,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        fn is_any_array(expr: &Expression<'_>) -> bool {
            matches!(
                expr.value,
                ExpressionValue::Array(_) | ExpressionValue::Command(_) | ExpressionValue::Property(_)
            ) || is_conditional(expr)
        }

        fn is_conditional(expr: &Expression<'_>) -> bool {
            matches!(expr.value, ExpressionValue::Conditional { .. })
        }

        if array.is_empty() {
            return Ok(());
        }

        // If there is only one element, and it is not an array itself, always print it as-is
        if array.len() == 1 && !is_any_array(&array[0]) {
            return self.format_expr_noindent(&array[0], f);
        }

        // Attempt compact array first, so long as there is no more than one inner array
        if array.iter().filter(|n| is_any_array(n)).count() <= 1 {
            // Max width - 2, to account for array delimiters
            let max_len = self.options.max_array_width - 2;
            let mut limit_buffer = String::new();

            struct ExprFormatter<'src>(&'src Formatter<'src>, &'src Expression<'src>);

            impl fmt::Display for ExprFormatter<'_> {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    self.0.format_expr_noindent(self.1, f)
                }
            }

            write!(limit_buffer, "{}", ExprFormatter(self, &array[0]))?;
            for expr in &array[1..] {
                write!(limit_buffer, " {}", ExprFormatter(self, expr))?;
                if limit_buffer.len() > max_len {
                    break;
                }
            }

            if limit_buffer.len() <= max_len {
                return f.write_str(&limit_buffer);
            }
        }

        // Inspect first element of the array
        let Some((first, mut remaining)) = array.split_first() else {
            return Ok(());
        };

        // Display leading symbol on the same line as the array opening
        match first.value {
            ExpressionValue::Symbol(name) => {
                self.format_expr_noindent(first, f)?;

                // Additional arguments which should be displayed on the same line
                if matches!(kind, ArrayKind::Command) {
                    static COMMAND_SAME_LINE_ARGS: LazyLock<HashMap<&str, usize>> = LazyLock::new(|| {
                        HashMap::from_iter([
                            ("foreach", 2),     // {foreach $var $array {...} ...}
                            ("foreach_int", 3), // {foreach_int $var 0 5 {...} ...}
                            ("if", 1),          // {if {condition} {...} ...}
                            ("if_else", 1),     // {if_else {condition} {...} {...}}
                            ("unless", 1),      // {unless {condition} {...} ...}
                            ("with", 1),        // {with $object {...} ...}
                            ("while", 1),       // {while {condition} {...} ...}
                            ("func", 1),        // {func name ($arg1 ...) {...} ...}
                        ])
                    });

                    if let Some(&arg_count) = (*COMMAND_SAME_LINE_ARGS).get(name) {
                        let count = remaining.len().min(arg_count);
                        let args;
                        (args, remaining) = remaining.split_at(count);

                        for arg in args {
                            f.write_char(' ')?;
                            self.format_expr_noindent(arg, f)?;
                        }
                    }
                }
            },
            _ => {
                remaining = array;
            },
        };

        // Bump up indentation for inner elements
        let guard = self.bump_indent(1);

        f.write_char('\n')?;
        for expr in remaining {
            self.format_expr(expr, f)?;
            f.write_char('\n')?;
        }

        // Restore and write indentation for closing delimiter
        drop(guard);

        // Write indentation for closing delimiter and restore (implicit)
        self.write_indent(f)
    }

    fn format_block(&self, array: &[Expression<'src>], f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Bump up indentation for inner elements
        let guard = self.bump_indent(1);

        for expr in array {
            self.format_expr(expr, f)?;
            f.write_char('\n')?;
        }

        // Restore indentation
        drop(guard);

        Ok(())
    }

    fn format_conditional_block(
        &self,
        array: &[Expression<'src>],
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let guard = self.bump_indent(-1);
        self.format_block(&array, f)?;
        drop(guard);

        Ok(())
    }

    fn format_expr(&self, expr: &Expression<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write_indent(f)?;
        self.format_expr_noindent(expr, f)
    }

    fn format_expr_noindent(&self, expr: &Expression<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &expr.value {
            ExpressionValue::Integer(_) => self.write_original(expr, f),
            ExpressionValue::Float(_) => self.write_original(expr, f),
            ExpressionValue::String(_) => self.write_original(expr, f),

            ExpressionValue::Symbol(_) => self.write_original(expr, f),
            ExpressionValue::Variable(_) => self.write_original(expr, f),
            ExpressionValue::Unhandled => self.write_original(expr, f),

            ExpressionValue::Array(body) => self.format_array(body, ArrayKind::Array, f),
            ExpressionValue::Command(body) => self.format_array(body, ArrayKind::Command, f),
            ExpressionValue::Property(body) => self.format_array(body, ArrayKind::Property, f),

            ExpressionValue::Define(name, body) => {
                write!(f, "#define {} ", name.text)?;
                self.format_array(&body.exprs, ArrayKind::Array, f)
            },
            ExpressionValue::Undefine(name) => write!(f, "#undef {}", name.text),
            ExpressionValue::Include(name) => write!(f, "#include {}", name.text),
            ExpressionValue::IncludeOptional(name) => write!(f, "#include_opt {}", name.text),
            ExpressionValue::Merge(name) => write!(f, "#merge {}", name.text),
            ExpressionValue::Autorun(body) => {
                f.write_str("#autorun ")?;
                self.format_array(&body.exprs, ArrayKind::Command, f)
            },

            ExpressionValue::Conditional { is_positive, symbol, true_branch, false_branch } => {
                match is_positive {
                    true => write!(f, "#ifdef {}", symbol.text)?,
                    false => write!(f, "#ifndef {}", symbol.text)?,
                };
                f.write_char('\n')?;

                self.format_conditional_block(&true_branch.exprs, f)?;
                if let Some(false_branch) = false_branch {
                    // TODO: this is a hack to keep directives indented properly after writing the block
                    if self.indent_level.get() > 0 {
                        f.write_str(&self.indent_text)?;
                    }
                    f.write_str("#else")?;
                    f.write_char('\n')?;
                    self.format_conditional_block(&false_branch.exprs, f)?;
                }

                // TODO: this is a hack to keep directives indented properly after writing the block
                if self.indent_level.get() > 0 {
                    f.write_str(&self.indent_text)?;
                }
                f.write_str("#endif")
            },

            ExpressionValue::Comment(_) => self.write_original(expr, f),
            ExpressionValue::BlockComment(_) => self.write_original(expr, f),
        }
    }

    #[must_use = "indentation is restored when guard is dropped"]
    fn bump_indent(&self, amount: isize) -> IndentGuard<'_> {
        let saved = self.indent_level.get();
        self.indent_level.set(saved + amount);
        IndentGuard { formatter: self, saved }
    }

    fn write_indent(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for _i in 0..self.indent_level.get() {
            f.write_str(&self.indent_text)?;
        }

        Ok(())
    }
}
