// SPDX-License-Identifier: LGPL-3.0-or-later

//! Formats DTA using an expression-based formatter.

use std::cell::Cell;
use std::fmt::{self, Write};

use arson_parse::{ArrayKind, Expression, ExpressionValue, ParseError};

use crate::{Indentation, Options, COMMAND_SAME_LINE_ARGS};

/// Formats the given input text to a new string using the expression-based formatter.
///
/// # Example
///
/// ```rust
/// let input = "(1 2 3 4) (a b c d) (outer (inner \"some text\")) (foo (bar \"some text\") (baz 10.0))";
/// let options = arson_fmtlib::Options::default();
/// let formatted = arson_fmtlib::expr::format_to_string(input, options).unwrap();
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

/// The expression-based formatter for DTA text.
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
#[derive(Debug)]
pub struct Formatter<'src> {
    options: Options,

    input: &'src str,
    ast: Vec<Expression<'src>>,

    indent_level: Cell<usize>,
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
    saved: usize,
}

impl Drop for IndentGuard<'_> {
    fn drop(&mut self) {
        self.formatter.indent_level.set(self.saved);
    }
}

struct ExprFormatter<'src>(&'src Formatter<'src>, &'src Expression<'src>);

impl fmt::Display for ExprFormatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.format_expr_noindent(self.1, f)
    }
}

fn is_any_array(expr: &Expression<'_>) -> bool {
    matches!(
        expr.value,
        ExpressionValue::Array(_) | ExpressionValue::Command(_) | ExpressionValue::Property(_)
    )
}

fn is_populated_array(expr: &Expression<'_>) -> bool {
    match &expr.value {
        ExpressionValue::Array(array)
        | ExpressionValue::Command(array)
        | ExpressionValue::Property(array) => array.len() > 1,
        _ => false,
    }
}

fn is_directive(expr: &Expression<'_>) -> bool {
    matches!(
        expr.value,
        ExpressionValue::Define(_, _)
            | ExpressionValue::Undefine(_)
            | ExpressionValue::Include(_)
            | ExpressionValue::IncludeOptional(_)
            | ExpressionValue::Merge(_)
            | ExpressionValue::Autorun(_)
    )
}

fn is_conditional(expr: &Expression<'_>) -> bool {
    matches!(expr.value, ExpressionValue::Conditional { .. })
}

impl<'src> Formatter<'src> {
    fn write_original(&self, expr: &Expression<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.input[expr.location.clone()])
    }

    fn probe_node(&self, buffer: &mut String, expr: &Expression<'src>) -> bool {
        match &expr.value {
            ExpressionValue::Array(array) => self.probe_array_(buffer, array, ArrayKind::Array),
            ExpressionValue::Command(array) => self.probe_array_(buffer, array, ArrayKind::Command),
            ExpressionValue::Property(array) => self.probe_array_(buffer, array, ArrayKind::Property),
            _ => write!(buffer, "{}", ExprFormatter(self, expr)).is_ok(),
        }
    }

    fn probe_array_(&self, buffer: &mut String, array: &[Expression<'src>], kind: ArrayKind) -> bool {
        let (l, r) = kind.delimiters();
        buffer.push(l);
        let result = self.probe_array(array).inspect(|short| buffer.push_str(short));
        buffer.push(r);
        result.is_some()
    }

    fn probe_array(&self, array: &[Expression<'src>]) -> Option<String> {
        if array.is_empty() {
            return Some(String::new());
        }

        // If there is only one element, try to print it as-is
        if array.len() == 1 {
            let first = &array[0];
            if !is_any_array(first) && !is_conditional(first) {
                let mut short = String::new();
                write!(short, "{}", ExprFormatter(self, &array[0])).ok()?;
                return Some(short);
            }
        }

        let (arrays, large_arrays, directives, conditionals) =
            array
                .iter()
                .fold((0, 0, 0, 0), |(mut arr, mut larry, mut dir, mut cond), n| {
                    arr += is_any_array(n) as usize;
                    larry += is_populated_array(n) as usize;
                    dir += is_directive(n) as usize;
                    cond += is_conditional(n) as usize;
                    (arr, larry, dir, cond)
                });

        // Attempt compact array
        if (arrays == 1 && large_arrays == 1 || (arrays <= 3 && large_arrays < 1))
            && directives < 1
            && conditionals < 1
        {
            // Max width - 2, to account for array delimiters
            let max_len = self.options.max_array_width - 2;
            let mut limit_buffer = String::new();

            let mut is_small = self.probe_node(&mut limit_buffer, &array[0]);
            for expr in &array[1..] {
                limit_buffer.push(' ');
                is_small &= self.probe_node(&mut limit_buffer, expr);
                if !is_small || limit_buffer.len() > max_len {
                    break;
                }
            }

            if is_small && limit_buffer.len() <= max_len {
                return Some(limit_buffer);
            }
        }

        None
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
        if array.is_empty() {
            return Ok(());
        }

        // Attempt short representation of the array
        if let Some(short) = self.probe_array(array) {
            return f.write_str(&short);
        }

        // Inspect first element of the array
        let Some((first, mut remaining)) = array.split_first() else {
            return Ok(());
        };

        match first.value {
            ExpressionValue::Symbol(name) => {
                // Display leading symbol on the same line as the array opening
                self.format_expr_noindent(first, f)?;

                // Additional arguments which should be displayed on the same line
                if matches!(kind, ArrayKind::Command) {
                    if let Some(&arg_count) = (*COMMAND_SAME_LINE_ARGS).get(name) {
                        let count = remaining.len().min(arg_count);
                        let (args, _remaining) = remaining.split_at(count);

                        if let Some(short) = self.probe_array(args) {
                            f.write_char(' ')?;
                            f.write_str(&short)?;
                            remaining = _remaining;
                        }
                    }
                }
            },
            ExpressionValue::Integer(_) if remaining.iter().any(|n| is_any_array(n)) => {
                // Display integers used as data keys or case values on the
                // same line as the array opening
                self.format_expr_noindent(first, f)?;
            },
            ExpressionValue::Variable(_) | ExpressionValue::Command(_) | ExpressionValue::Property(_) => {
                // Display leading variable on the same line as the array opening
                self.format_expr_noindent(first, f)?;

                if matches!(kind, ArrayKind::Command) {
                    // Also display the next following symbol on the same line
                    if let Some((next, _remaining)) = remaining.split_first() {
                        if matches!(next.value, ExpressionValue::Symbol(_)) {
                            f.write_char(' ')?;
                            self.format_expr_noindent(next, f)?;
                            remaining = _remaining;
                        }
                    }
                }
            },
            ExpressionValue::String(_) if matches!(kind, ArrayKind::Command) => {
                // The first argument of a command being a string means to look up an object,
                // format it similarly to variables
                self.format_expr_noindent(first, f)?;

                // Also display the next following symbol on the same line
                if let Some((next, _remaining)) = remaining.split_first() {
                    if matches!(next.value, ExpressionValue::Symbol(_)) {
                        f.write_char(' ')?;
                        self.format_expr_noindent(next, f)?;
                        remaining = _remaining;
                    }
                }
            },
            _ => {
                remaining = array;
            },
        };

        self.format_array_long(remaining, f)
    }

    fn format_array_long(&self, array: &[Expression<'src>], f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Bump up indentation for inner elements
        let guard = self.bump_indent(1);

        f.write_char('\n')?;
        for expr in array {
            self.format_expr(expr, f)?;
            f.write_char('\n')?;
        }

        // Restore and write indentation for closing delimiter
        drop(guard);

        // Write indentation for closing delimiter and restore
        self.write_indent(f)
    }

    fn format_define_body(&self, body: &[Expression<'src>], f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (l, r) = ArrayKind::Array.delimiters();
        f.write_char(l)?;

        if !body.is_empty() {
            // Attempt short representation
            if let Some(short) = self.probe_array(body) {
                f.write_str(&short)?;
            } else {
                self.format_array_long(body, f)?;
            }
        }

        f.write_char(r)
    }

    fn format_conditional_block(
        &self,
        array: &[Expression<'src>],
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        // Only indent if block contains inner conditionals
        let guard = match array.iter().any(|n| is_conditional(n)) {
            true => self.bump_indent(1),
            false => self.bump_indent(0),
        };

        for expr in array {
            self.format_expr(expr, f)?;
            f.write_char('\n')?;
        }

        // Restore indentation
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
                self.format_define_body(&body.exprs, f)
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
                    self.write_indent(f)?;
                    f.write_str("#else")?;
                    f.write_char('\n')?;
                    self.format_conditional_block(&false_branch.exprs, f)?;
                }

                self.write_indent(f)?;
                f.write_str("#endif")
            },

            ExpressionValue::Comment(_) => self.write_original(expr, f),
            ExpressionValue::BlockComment(_) => self.write_original(expr, f),
        }
    }

    #[must_use = "indentation is restored when guard is dropped"]
    fn bump_indent(&self, amount: usize) -> IndentGuard<'_> {
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
