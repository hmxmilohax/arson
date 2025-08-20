// SPDX-License-Identifier: LGPL-3.0-or-later

use std::borrow::Cow;
use std::cell::Cell;
use std::fmt::{self, Write};

use crate::{Node, NodeValue};

#[derive(Debug, Clone, Copy)]
pub(crate) enum ArrayKind {
    Array,
    Command,
    Property,
}

#[derive(Debug, Clone, Copy)]
pub enum ArrayIndentation {
    Tabs,
    Spaces(usize),
}

/// Options for array pretty-printing.
#[derive(Debug, Clone)]
pub struct ArrayDisplayOptions {
    /// The indentation style to use.
    pub indentation: ArrayIndentation,
    /// The maximum width of arrays in the output.
    pub max_array_width: usize,
}

impl Default for ArrayDisplayOptions {
    fn default() -> Self {
        Self {
            indentation: ArrayIndentation::Spaces(3),
            max_array_width: 60,
        }
    }
}

impl ArrayDisplayOptions {
    fn indent_text(&self) -> String {
        match self.indentation {
            ArrayIndentation::Tabs => "\t".to_owned(),
            ArrayIndentation::Spaces(count) => std::iter::repeat_n(' ', count).collect(),
        }
    }
}

#[derive(Debug)]
pub struct ArrayDisplay<'a> {
    nodes: &'a [Node],
    kind: ArrayKind,
    options: ArrayDisplayOptions,

    indent_level: Cell<usize>,
    indent_text: Cow<'a, str>,
}

impl<'a> ArrayDisplay<'a> {
    pub(crate) fn new(nodes: &'a [Node], kind: ArrayKind, options: ArrayDisplayOptions) -> Self {
        let indent_text = options.indent_text();
        Self {
            nodes,
            kind,
            options,

            indent_level: Cell::new(0),
            indent_text: Cow::Owned(indent_text),
        }
    }

    pub(crate) fn new_default(nodes: &'a [Node], kind: ArrayKind) -> Self {
        Self::new(nodes, kind, ArrayDisplayOptions::default())
    }

    fn new_inner(other: &'a Self, nodes: &'a [Node], kind: ArrayKind) -> Self {
        Self {
            nodes,
            kind,

            options: other.options.clone(),
            indent_level: other.indent_level.clone(),
            indent_text: Cow::Borrowed(&other.indent_text),
        }
    }

    fn write_array_compact(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.nodes.is_empty() {
            Self::write_node_compact(&self.nodes[0], f)?;
            for node in &self.nodes[1..] {
                f.write_char(' ')?;
                Self::write_node_compact(node, f)?;
            }
        }

        Ok(())
    }

    fn write_node_compact(node: &Node, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use std::fmt::Display;

        match node.unevaluated() {
            NodeValue::String(value) => {
                // Apply escapes where necessary
                let value = value.replace('\"', "\\q").replace('\n', "\\n");
                write!(f, "\"{value}\"")
            },
            NodeValue::Object(value) => write!(f, "<object {value}>"),
            value => value.fmt(f),
        }
    }

    fn write_array_pretty(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.nodes.is_empty() {
            return Ok(());
        }

        // If there is only one element, and it is not an array itself, always print it as-is
        if self.nodes.len() == 1 && !self.nodes[0].is_any_array() {
            return Self::write_node_compact(&self.nodes[0], f);
        }

        // Attempt compact array first, so long as there is no more than one inner array
        if self.nodes.iter().filter(|n| n.is_any_array()).count() <= 1 {
            // Max width - 2, to account for array delimiters
            let max_len = self.options.max_array_width - 2;
            let mut limit_buffer = String::new();

            struct NodeWrap<'a>(&'a Node);

            impl std::fmt::Display for NodeWrap<'_> {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    ArrayDisplay::write_node_compact(self.0, f)
                }
            }

            write!(limit_buffer, "{}", NodeWrap(&self.nodes[0]))?;
            for node in &self.nodes[1..] {
                write!(limit_buffer, " {}", NodeWrap(node))?;
                if limit_buffer.len() > max_len {
                    break;
                }
            }

            if limit_buffer.len() <= max_len {
                return f.write_str(&limit_buffer);
            }
        }

        // Bump up indentation for inner elements
        let original_indent = self.indent_level.get();
        self.indent_level.set(original_indent + 1);

        let mut iter = self.nodes.iter();

        // Display leading symbol on the same line as the array opening
        match iter.next() {
            Some(node) if node.is_symbol() => writeln!(f, "{node}")?,
            Some(node) => {
                f.write_char('\n')?;
                self.write_node_pretty(node, f)?
            },
            None => return Ok(()),
        }

        for node in iter {
            self.write_node_pretty(node, f)?;
        }

        // Restore and write indentation for closing delimiter
        self.indent_level.set(original_indent);
        self.write_indent(f)?;

        Ok(())
    }

    fn write_node_pretty(&'a self, node: &Node, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write_indent(f)?;
        match node.unevaluated() {
            NodeValue::Array(array) => {
                let Ok(borrow) = array.borrow() else {
                    return writeln!(f, "(<borrowed>)");
                };
                self.write_inner_array_pretty(&borrow, ArrayKind::Array, f)
            },
            NodeValue::Command(array) => self.write_inner_array_pretty(array, ArrayKind::Command, f),
            NodeValue::Property(array) => self.write_inner_array_pretty(array, ArrayKind::Property, f),
            _ => {
                Self::write_node_compact(node, f)?;
                writeln!(f)
            },
        }
    }

    fn write_inner_array_pretty(
        &'a self,
        nodes: &'a [Node],
        kind: ArrayKind,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let display = Self::new_inner(self, nodes, kind);
        writeln!(f, "{display:#}")
    }

    fn write_indent(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for _i in 0..self.indent_level.get() {
            f.write_str(&self.indent_text)?;
        }

        Ok(())
    }
}

impl fmt::Display for ArrayDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (l, r) = match self.kind {
            ArrayKind::Array => ('(', ')'),
            ArrayKind::Command => ('{', '}'),
            ArrayKind::Property => ('[', ']'),
        };

        f.write_char(l)?;

        if f.alternate() {
            self.write_array_pretty(f)?;
        } else {
            self.write_array_compact(f)?;
        }

        f.write_char(r)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{arson_array, Context, NodeArray, NodeCommand, NodeProperty, Variable};

    fn assert_display(array: impl fmt::Display, standard: &str, pretty: &str) {
        assert_eq!(array.to_string(), standard);
        assert_eq!(format!("{array:#}"), pretty);
    }

    fn assert_display_node(node: NodeValue, expected: &str) {
        assert_display(arson_array![node], expected, expected)
    }

    #[test]
    fn integer() {
        use std::num::Wrapping;

        assert_display_node(NodeValue::Integer(Wrapping(1)), "(1)");
        assert_display_node(NodeValue::Integer(Wrapping(458243)), "(458243)");
        assert_display_node(NodeValue::Integer(Wrapping(-235725)), "(-235725)");
        assert_display_node(NodeValue::Integer(Wrapping(i64::MAX)), "(9223372036854775807)");
        assert_display_node(NodeValue::Integer(Wrapping(i64::MIN)), "(-9223372036854775808)");
    }

    #[test]
    fn float() {
        assert_display_node(NodeValue::Float(0.000_000_000_001), "(1e-12)");
        assert_display_node(NodeValue::Float(0.000_000_001), "(1e-9)");
        assert_display_node(NodeValue::Float(0.000_001), "(1e-6)");
        assert_display_node(NodeValue::Float(0.001), "(0.001)");
        assert_display_node(NodeValue::Float(1.0), "(1.0)");
        assert_display_node(NodeValue::Float(1_000.0), "(1000.0)");
        assert_display_node(NodeValue::Float(1_000_000.0), "(1000000.0)");
        assert_display_node(NodeValue::Float(1_000_000_000.0), "(1000000000.0)");
        assert_display_node(NodeValue::Float(1_000_000_000_000.0), "(1000000000000.0)");
    }

    #[test]
    fn string() {
        assert_display_node(NodeValue::String("asdf".to_owned().into()), "(\"asdf\")");
        assert_display_node(NodeValue::String("\"asdf\"".to_owned().into()), "(\"\\qasdf\\q\")");
        assert_display_node(NodeValue::String("asdf\n".to_owned().into()), "(\"asdf\\n\")");
    }

    #[test]
    fn symbol() {
        let mut context = Context::new();
        let sym = context.add_required_symbol("sym");
        let sym_space = context.add_required_symbol("sym with\nwhitespace");

        assert_display_node(NodeValue::Symbol(sym), "(sym)");
        assert_display_node(NodeValue::Symbol(sym_space), "('sym with\nwhitespace')");
    }

    #[test]
    fn variable() {
        let mut context = Context::new();
        let var = Variable::new_required("var", &mut context);
        let dollar_var = Variable::new_required("$var", &mut context);

        assert_display_node(NodeValue::Variable(var), "($var)");
        assert_display_node(NodeValue::Variable(dollar_var), "($$var)");
    }

    #[test]
    fn unhandled() {
        assert_display_node(NodeValue::Unhandled, "(kDataUnhandled)");
    }

    #[test]
    fn arrays() {
        assert_display(arson_array![1, 2, 3], "(1 2 3)", "(1 2 3)");
        assert_display(NodeCommand::from(arson_array![1, 2, 3]), "{1 2 3}", "{1 2 3}");
        assert_display(NodeProperty::from(arson_array![1, 2, 3]), "[1 2 3]", "[1 2 3]");
    }

    #[test]
    fn inner_arrays() {
        let mut context = Context::new();
        let sym1 = context.add_required_symbol("sym1");
        let sym2 = context.add_required_symbol("sym2");

        assert_display(
            arson_array![sym1.clone(), arson_array![sym2.clone(), 100]],
            "(sym1 (sym2 100))",
            "(sym1 (sym2 100))",
        );
        assert_display(
            arson_array![sym1.clone(), NodeCommand::from(arson_array![sym2.clone(), 100])],
            "(sym1 {sym2 100})",
            "(sym1 {sym2 100})",
        );
        assert_display(
            arson_array![sym1.clone(), NodeProperty::from(arson_array![sym2.clone(), 100])],
            "(sym1 [sym2 100])",
            "(sym1 [sym2 100])",
        );
    }

    #[test]
    fn multiple_arrays() {
        let mut context = Context::new();

        let sym1 = context.add_required_symbol("sym1");
        let sym2 = context.add_required_symbol("sym2");

        #[rustfmt::skip] // preserve correlation between array formatting and display output
        assert_display(
            arson_array![sym1.clone(),
                arson_array![10, 20, 30],
                NodeCommand::from(arson_array![sym2.clone(), 100]),
                NodeProperty::from(arson_array![sym2.clone()]),
            ],
            "(sym1 (10 20 30) {sym2 100} [sym2])",
            "(sym1\
            \n   (10 20 30)\
            \n   {sym2 100}\
            \n   [sym2]\
            \n)",
        );
    }

    #[test]
    fn big_example() {
        let mut context = Context::new();

        let sym1 = context.add_required_symbol("sym1");
        let sym2 = context.add_required_symbol("sym2");
        let sym_asdf = context.add_required_symbol("asdf");
        let sym_jkl = context.add_required_symbol("jkl");

        #[rustfmt::skip] // preserve correlation between array formatting and display output
        assert_display(
            arson_array![
                arson_array![sym1.clone(), 5],
                arson_array![sym2.clone(),
                    arson_array![sym_asdf.clone(), 100],
                    arson_array![sym_jkl.clone(), 250],
                    arson_array![1,
                        arson_array![5, "foo"],
                        arson_array![10, "bar"],
                    ],
                ],
                arson_array![3, 3],
                arson_array![4, 4],
            ],
            "((sym1 5) (sym2 (asdf 100) (jkl 250) (1 (5 \"foo\") (10 \"bar\"))) (3 3) (4 4))",
            "(\
            \n   (sym1 5)\
            \n   (sym2\
            \n      (asdf 100)\
            \n      (jkl 250)\
            \n      (\
            \n         1\
            \n         (5 \"foo\")\
            \n         (10 \"bar\")\
            \n      )\
            \n   )\
            \n   (3 3)\
            \n   (4 4)\
            \n)"
        );
    }

    #[test]
    fn indent_style() {
        fn assert_indent(array: NodeArray, expected: &str, indentation: ArrayIndentation) {
            let options = ArrayDisplayOptions { indentation, ..Default::default() };
            let display = array.display_with_options(options);
            assert_eq!(format!("{display:#}"), expected);
        }

        assert_indent(
            arson_array![arson_array![1, 2, 3], arson_array![4, 5, 6]],
            "(\
            \n   (1 2 3)\
            \n   (4 5 6)\
            \n)",
            ArrayIndentation::Spaces(3),
        );

        assert_indent(
            arson_array![arson_array![1, 2, 3], arson_array![4, 5, 6]],
            "(\
            \n        (1 2 3)\
            \n        (4 5 6)\
            \n)",
            ArrayIndentation::Spaces(8),
        );

        assert_indent(
            arson_array![arson_array![1, 2, 3], arson_array![4, 5, 6]],
            "(\
            \n\t(1 2 3)\
            \n\t(4 5 6)\
            \n)",
            ArrayIndentation::Tabs,
        );
    }

    #[test]
    fn max_line_width() {
        fn assert_width(array: NodeArray, expected: &str, width: usize) {
            let options = ArrayDisplayOptions { max_array_width: width, ..Default::default() };
            let display = array.display_with_options(options);
            assert_eq!(format!("{display:#}"), expected);
        }

        assert_width(
            arson_array!["1234567890", "1234567890", "1234567890", "1234567890", "1234567890"],
            "(\
            \n   \"1234567890\"\
            \n   \"1234567890\"\
            \n   \"1234567890\"\
            \n   \"1234567890\"\
            \n   \"1234567890\"\
            \n)",
            60,
        );
        assert_width(
            arson_array!["1234567890", "1234567890", "1234567890", "1234567890", "1234567890"],
            "(\"1234567890\" \"1234567890\" \"1234567890\" \"1234567890\" \"1234567890\")",
            80,
        );
        assert_width(
            arson_array!["1234567890", "1234567890"],
            "(\
            \n   \"1234567890\"\
            \n   \"1234567890\"\
            \n)",
            25,
        );
        assert_width(
            arson_array!["1234567890", "1234567890"],
            "(\"1234567890\" \"1234567890\")",
            30,
        );
    }
}
