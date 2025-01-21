// SPDX-License-Identifier: LGPL-3.0-or-later

//! Formats DTA using an expression-based formatter.

use std::fmt::{self, Write};
use std::iter::Peekable;

use arson_parse::{ArrayKind, Expression, ExpressionValue, ParseError};

use crate::{Indentation, Options, BLOCK_COMMANDS, COMMAND_SAME_LINE_ARGS};

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
}

impl<'src> Formatter<'src> {
    /// Creates a new [`Formatter`] with the given input and options.
    pub fn new(input: &'src str, options: Options) -> Result<Formatter<'src>, ParseError> {
        let ast = arson_parse::parse_text(input)?;
        Ok(Self { options, input, ast })
    }
}

impl fmt::Display for Formatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        InnerFormatter::new(self).format_input(&self.ast, f)
    }
}

struct InnerFormatter<'src> {
    options: Options,

    input: &'src str,

    indent_level: usize,
    indent_text: String,
}

impl<'src> InnerFormatter<'src> {
    fn new(outer: &'src Formatter<'src>) -> Self {
        let indent_text = match outer.options.indentation {
            Indentation::Tabs(_) => "\t".to_owned(),
            Indentation::Spaces(count) => std::iter::repeat_n(' ', count).collect(),
        };

        Self {
            options: outer.options.clone(),

            input: outer.input,

            indent_level: 0,
            indent_text,
        }
    }
}

struct IndentGuard<'a, 'src> {
    f: &'a mut InnerFormatter<'src>,
    saved: usize,
}

impl Drop for IndentGuard<'_, '_> {
    fn drop(&mut self) {
        self.f.indent_level = self.saved;
    }
}

#[derive(Debug, Default)]
struct ArrayProbeStats {
    all_arrays: usize,
    medium_arrays: usize,
    large_arrays: usize,

    arrays: usize,
    commands: usize,
    properties: usize,

    directives: usize,
    conditionals: usize,
}

fn is_any_array(expr: &Expression<'_>) -> bool {
    matches!(
        expr.value,
        ExpressionValue::Array(_) | ExpressionValue::Command(_) | ExpressionValue::Property(_)
    )
}

fn is_array_longer_than(expr: &Expression<'_>, limit: usize) -> bool {
    match &expr.value {
        ExpressionValue::Array(array)
        | ExpressionValue::Command(array)
        | ExpressionValue::Property(array) => array.len() > limit,
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

impl<'src> InnerFormatter<'src> {
    fn write_original(&self, expr: &Expression<'src>, f: &mut impl fmt::Write) -> fmt::Result {
        f.write_str(&self.input[expr.location.clone()])
    }

    fn write_expr_spaced(&self, expr: &Expression<'src>, f: &mut impl fmt::Write) -> fmt::Result {
        f.write_char(' ')?;
        self.write_original(expr, f)
    }

    fn format_input(&mut self, array: &[Expression<'src>], f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = array.iter().peekable();
        self.format_block_loop(&mut iter, f)
    }

    fn format_expr(&mut self, expr: &Expression<'src>, f: &mut impl fmt::Write) -> fmt::Result {
        self.write_indent(f)?;
        self.format_expr_unindented(expr, f)
    }

    fn format_expr_unindented(&mut self, expr: &Expression<'src>, f: &mut impl fmt::Write) -> fmt::Result {
        match &expr.value {
            ExpressionValue::Integer(_) => self.write_original(expr, f)?,
            ExpressionValue::Float(_) => self.write_original(expr, f)?,
            ExpressionValue::String(_) => self.write_original(expr, f)?,

            ExpressionValue::Symbol(_) => self.write_original(expr, f)?,
            ExpressionValue::Variable(_) => self.write_original(expr, f)?,
            ExpressionValue::Unhandled => self.write_original(expr, f)?,

            ExpressionValue::Array(body) => self.format_array(body, ArrayKind::Array, f)?,
            ExpressionValue::Command(body) => self.format_array(body, ArrayKind::Command, f)?,
            ExpressionValue::Property(body) => self.format_array(body, ArrayKind::Property, f)?,

            ExpressionValue::Define(name, body) => {
                write!(f, "#define {} ", name.text)?;
                self.format_define_body(&body.exprs, f)?;
            },
            ExpressionValue::Undefine(name) => write!(f, "#undef {}", name.text)?,
            ExpressionValue::Include(name) => write!(f, "#include {}", name.text)?,
            ExpressionValue::IncludeOptional(name) => write!(f, "#include_opt {}", name.text)?,
            ExpressionValue::Merge(name) => write!(f, "#merge {}", name.text)?,
            ExpressionValue::Autorun(body) => {
                f.write_str("#autorun ")?;
                self.format_array(&body.exprs, ArrayKind::Command, f)?;
            },

            ExpressionValue::Conditional { is_positive, symbol, true_branch, false_branch } => {
                match is_positive {
                    true => write!(f, "#ifdef {}", symbol.text)?,
                    false => write!(f, "#ifndef {}", symbol.text)?,
                };

                let last = Expression::new(ExpressionValue::Symbol(symbol.text), symbol.location.clone());
                self.format_conditional_block(&true_branch.exprs, &last, f)?;

                if let Some(false_branch) = false_branch {
                    self.write_indent(f)?;
                    f.write_str("#else")?;

                    let last_location = false_branch.location.start..false_branch.location.start + 5;
                    let last = Expression::new(ExpressionValue::Symbol("#else"), last_location);
                    self.format_conditional_block(&false_branch.exprs, &last, f)?;
                }

                self.write_indent(f)?;
                f.write_str("#endif")?;
            },

            ExpressionValue::BlankLine => (), // f.write_str("\n\n")?,
            ExpressionValue::Comment(_) => self.write_original(expr, f)?,
            ExpressionValue::BlockComment(_) => self.write_original(expr, f)?,
        }

        Ok(())
    }

    fn probe_node(&mut self, expr: &Expression<'src>, buffer: &mut String) -> bool {
        match &expr.value {
            ExpressionValue::Array(array) => self.probe_array_(buffer, array, ArrayKind::Array),
            ExpressionValue::Command(array) => self.probe_array_(buffer, array, ArrayKind::Command),
            ExpressionValue::Property(array) => self.probe_array_(buffer, array, ArrayKind::Property),

            ExpressionValue::Define(name, body) => {
                write!(buffer, "#define {} ", name.text).is_ok()
                    && self.probe_array_(buffer, &body.exprs, ArrayKind::Array)
            },
            ExpressionValue::Autorun(body) => {
                buffer.push_str("#autorun ");
                self.probe_array_(buffer, &body.exprs, ArrayKind::Command)
            },

            ExpressionValue::Conditional { .. } => false,

            // Ignore blank lines, as they have no semantic bearing on
            // whether an array could be large or small
            ExpressionValue::BlankLine => true,

            ExpressionValue::Comment(_) => false,
            ExpressionValue::BlockComment(text) => {
                buffer.push_str(text);
                !text.contains('\n')
            },

            _ => self.format_expr_unindented(expr, buffer).is_ok(),
        }
    }

    fn probe_array_(&mut self, buffer: &mut String, array: &[Expression<'src>], kind: ArrayKind) -> bool {
        let (l, r) = kind.delimiters();
        buffer.push(l);
        let result = self.probe_array(array, kind).inspect(|short| buffer.push_str(short));
        buffer.push(r);
        result.is_ok()
    }

    fn probe_array(
        &mut self,
        array: &[Expression<'src>],
        kind: ArrayKind,
    ) -> Result<String, ArrayProbeStats> {
        if array.is_empty() {
            return Ok(String::new());
        }

        // If there is only one element, try to print it as-is
        if array.len() == 1 && !is_any_array(&array[0]) {
            let mut short = String::new();
            if self.probe_node(&array[0], &mut short) {
                return Ok(short);
            }
        }

        let stats = array.iter().fold(ArrayProbeStats::default(), |mut stats, n| {
            stats.all_arrays += is_any_array(n) as usize;
            stats.medium_arrays += is_array_longer_than(n, 1) as usize;
            stats.large_arrays += is_array_longer_than(n, 3) as usize;

            stats.arrays += matches!(n.value, ExpressionValue::Array(_)) as usize;
            stats.commands += matches!(n.value, ExpressionValue::Command(_)) as usize;
            stats.properties += matches!(n.value, ExpressionValue::Property(_)) as usize;

            stats.directives += is_directive(n) as usize;
            stats.conditionals += is_conditional(n) as usize;

            stats
        });

        let try_compact = match kind {
            // Arrays should be compacted if either:
            // - they contain one inner array with many elements
            // - they contain fewer than 4 small arrays and have no medium/large arrays
            ArrayKind::Array => {
                (stats.all_arrays == 1 && stats.medium_arrays == 1)
                    || (stats.all_arrays <= 3 && stats.medium_arrays < 1)
            },

            // Commands should be compacted if both:
            // - they are not recognized specifically as commands which execute blocks
            // - they contain no large arrays
            ArrayKind::Command => {
                let large = match &array[0].value {
                    ExpressionValue::Symbol(name) if (*BLOCK_COMMANDS).contains(name) => true,

                    ExpressionValue::Symbol(_)
                    | ExpressionValue::String(_)
                    | ExpressionValue::Variable(_)
                    | ExpressionValue::Command(_)
                    | ExpressionValue::Property(_) => 'a: {
                        // First element is (most likely) retrieving an object; if the method being
                        // called is a `foreach` or `with`, it's most likely executing a block
                        if let Some(name) = array.get(1) {
                            if let ExpressionValue::Symbol(name) = &name.value {
                                break 'a name.starts_with("foreach_") || name.starts_with("with_");
                            }
                        }

                        false
                    },

                    _ => false,
                };
                !large && stats.large_arrays < 1
            },

            // Properties should be compacted if they contain no large arrays
            ArrayKind::Property => stats.large_arrays < 1,
        };

        // Attempt compact array
        if try_compact && stats.directives < 1 && stats.conditionals < 1 {
            // Max width - 2, to account for array delimiters
            let max_len = self.options.max_array_width - 2;
            let mut limit_buffer = String::new();

            let mut is_small = true;
            for expr in array {
                if !limit_buffer.is_empty() {
                    limit_buffer.push(' ');
                }

                is_small &= self.probe_node(expr, &mut limit_buffer);
                if !is_small || limit_buffer.len() > max_len {
                    break;
                }
            }

            if is_small && limit_buffer.len() <= max_len {
                return Ok(limit_buffer);
            }
        }

        Err(stats)
    }

    fn format_array(
        &mut self,
        array: &[Expression<'src>],
        kind: ArrayKind,
        f: &mut impl fmt::Write,
    ) -> fmt::Result {
        let (l, r) = kind.delimiters();
        f.write_char(l)?;
        self.format_array_(array, kind, f)?;
        f.write_char(r)
    }

    fn format_array_(
        &mut self,
        array: &[Expression<'src>],
        kind: ArrayKind,
        f: &mut impl fmt::Write,
    ) -> fmt::Result {
        if array.is_empty() {
            return Ok(());
        }

        // Attempt short representation of the array
        let stats = match self.probe_array(array, kind) {
            Ok(short) => return f.write_str(&short),
            Err(stats) => stats,
        };

        // Inspect first element of the array
        let Some((first, mut remaining)) = array.split_first() else {
            return Ok(());
        };

        let mut last = first;
        match first.value {
            ExpressionValue::Symbol(name) => {
                // Display leading symbol on the same line as the array opening
                self.write_original(first, f)?;

                if matches!(kind, ArrayKind::Command) {
                    // Additional arguments which should be displayed on the same line
                    if let Some(arg_count) = (*COMMAND_SAME_LINE_ARGS).get(name) {
                        self.format_command_args(*arg_count, &mut remaining, &mut last, f)?;
                    }

                    // Otherwise, format it like an object
                    self.format_object_args(&mut remaining, &mut last, f)?;
                }
            },
            ExpressionValue::Integer(_) if remaining.iter().any(|n| is_any_array(n)) => {
                // Display integers used as data keys or case values on the
                // same line as the array opening
                self.write_original(first, f)?;
            },
            ExpressionValue::Variable(_) | ExpressionValue::Command(_) | ExpressionValue::Property(_) => {
                // Display leading variable on the same line as the array opening
                self.write_original(first, f)?;

                if matches!(kind, ArrayKind::Command) {
                    self.format_object_args(&mut remaining, &mut last, f)?;
                }
            },
            ExpressionValue::String(_) if matches!(kind, ArrayKind::Command) || stats.arrays > 0 => {
                // The first argument of a command being a string means to look up an object.
                // Additionally, if the rest of the array contains more arrays, the string
                // is most likely an object key (weird choice though)
                self.write_original(first, f)?;

                if matches!(kind, ArrayKind::Command) {
                    self.format_object_args(&mut remaining, &mut last, f)?;
                }
            },
            _ => {
                return self.format_array_long(array, None, f);
            },
        };

        self.format_array_long(remaining, Some(last), f)
    }

    fn format_object_args<'a>(
        &mut self,
        remaining: &mut &'a [Expression<'src>],
        last: &mut &'a Expression<'src>,
        f: &mut impl fmt::Write,
    ) -> fmt::Result {
        // Display the first symbol following the object on the same line
        let Some((next, _remaining)) = remaining.split_first() else {
            return Ok(());
        };

        if let ExpressionValue::Symbol(name) = &next.value {
            self.write_expr_spaced(next, f)?;
            *last = next;
            *remaining = _remaining;

            // Display the argument after that on the same line
            // if this is a `foreach` or `with` func
            if name.starts_with("foreach_") || name.starts_with("with_") {
                self.format_command_args(1, remaining, last, f)?;
            }
        }

        Ok(())
    }

    fn format_command_args<'a>(
        &mut self,
        mut count: usize,
        remaining: &mut &'a [Expression<'src>],
        last: &mut &'a Expression<'src>,
        f: &mut impl fmt::Write,
    ) -> fmt::Result {
        while count > 0 {
            count -= 1;

            let Some((arg, _remaining)) = remaining.split_first() else {
                break;
            };

            let mut buffer = String::new();
            if !self.probe_node(arg, &mut buffer) {
                break;
            }

            f.write_char(' ')?;
            f.write_str(&buffer)?;

            *last = arg;
            *remaining = _remaining;
        }

        Ok(())
    }

    fn format_array_long(
        &mut self,
        array: &[Expression<'src>],
        last: Option<&Expression<'src>>,
        f: &mut impl fmt::Write,
    ) -> fmt::Result {
        // Bump up indentation for inner elements
        let guard = self.bump_indent(1);

        guard.f.format_block(array, last, f)?;

        // Restore and write indentation for closing delimiter
        drop(guard);

        // Write indentation for closing delimiter and restore
        self.write_indent(f)
    }

    fn format_block(
        &mut self,
        array: &[Expression<'src>],
        last: Option<&Expression<'src>>,
        f: &mut impl fmt::Write,
    ) -> fmt::Result {
        let mut iter = array.iter().peekable();

        if let Some(last) = last {
            self.format_possible_comment(last, &mut iter, f)?;
        }

        f.write_char('\n')?;
        self.format_block_loop(&mut iter, f)?;
        f.write_char('\n')?;

        Ok(())
    }

    fn format_block_loop(
        &mut self,
        iter: &mut Peekable<std::slice::Iter<'_, Expression<'src>>>,
        f: &mut impl fmt::Write,
    ) -> fmt::Result {
        if let Some(expr) = iter.peek() {
            // Skip starting blank line to avoid excess whitespace
            if matches!(expr.value, ExpressionValue::BlankLine) {
                iter.next().unwrap();
            }
        }

        // Shenanigans are happening here to make blank line formatting work correctly.
        // This formatting code isn't the greatest lol
        let mut first = true;
        while let Some(expr) = iter.next() {
            // Don't indent blank lines
            if matches!(expr.value, ExpressionValue::BlankLine) {
                if !first && iter.peek().is_some() {
                    f.write_char('\n')?;
                }
            } else {
                if !first {
                    f.write_char('\n')?;
                }

                self.format_expr(expr, f)?;
                self.format_possible_comment(expr, iter, f)?;
            }

            first = false;
        }

        Ok(())
    }

    fn format_possible_comment(
        &mut self,
        last: &Expression<'src>,
        iter: &mut Peekable<std::slice::Iter<'_, Expression<'src>>>,
        f: &mut impl fmt::Write,
    ) -> fmt::Result {
        if let Some(comment) = iter.peek() {
            if matches!(
                comment.value,
                ExpressionValue::Comment(_) | ExpressionValue::BlockComment(_)
            ) {
                let between = &self.input[last.location.end..comment.location.start];
                if !between.contains('\n') {
                    let token = iter.next().unwrap();
                    self.write_expr_spaced(token, f)?;
                }
            }
        }

        Ok(())
    }

    fn format_define_body(&mut self, body: &[Expression<'src>], f: &mut impl fmt::Write) -> fmt::Result {
        let (l, r) = ArrayKind::Array.delimiters();
        f.write_char(l)?;

        if !body.is_empty() {
            // Attempt short representation
            if let Ok(short) = self.probe_array(body, ArrayKind::Array) {
                f.write_str(&short)?;
            } else {
                self.format_array_long(body, None, f)?;
            }
        }

        f.write_char(r)
    }

    fn format_conditional_block(
        &mut self,
        array: &[Expression<'src>],
        last: &Expression<'src>,
        f: &mut impl fmt::Write,
    ) -> fmt::Result {
        // Only indent if block contains inner conditionals
        let guard = match array.iter().any(|n| is_conditional(n)) {
            true => self.bump_indent(1),
            false => self.bump_indent(0),
        };

        guard.f.format_block(array, Some(last), f)?;

        // Restore indentation
        drop(guard);

        Ok(())
    }

    #[must_use = "indentation is restored when guard is dropped"]
    fn bump_indent(&mut self, amount: usize) -> IndentGuard<'_, 'src> {
        let saved = self.indent_level;
        self.indent_level = saved + amount;
        IndentGuard { f: self, saved }
    }

    fn write_indent(&self, f: &mut impl fmt::Write) -> fmt::Result {
        for _i in 0..self.indent_level {
            f.write_str(&self.indent_text)?;
        }

        Ok(())
    }
}
