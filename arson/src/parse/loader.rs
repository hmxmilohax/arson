// SPDX-License-Identifier: LGPL-3.0-or-later

use std::{
    fs::{self, File},
    io,
    marker::PhantomData,
    path::Path,
};

use crate::{Context, Node, NodeArray, NodeCommand, NodeProperty, NodeValue, NodeVariable};

use super::parser::{self, Expression, ExpressionKind, ParseError};

#[derive(thiserror::Error, Debug)]
pub enum LoadError {
    #[error("IO error: {0}")]
    IO(#[from] io::Error),

    #[error("Failed to parse the given file")]
    Parse(Vec<ParseError>),
}

impl From<io::ErrorKind> for LoadError {
    fn from(value: io::ErrorKind) -> Self {
        Self::from(io::Error::from(value))
    }
}

struct Loader<'ctx, 'src> {
    context: &'ctx mut Context,
    phantom: PhantomData<&'src ()>,
}

enum NodeResult {
    Value(Node),
    Skip,
}

impl<'ctx, 'src> Loader<'ctx, 'src> {
    fn new(context: &'ctx mut Context) -> Self {
        Self { context, phantom: PhantomData }
    }

    fn load_array(&mut self, ast: impl Iterator<Item = Expression<'src>>) -> NodeArray {
        let mut array = NodeArray::with_capacity(4);

        for expr in ast {
            match self.load_node(expr) {
                NodeResult::Value(node) => array.push(node),
                NodeResult::Skip => continue,
            }
        }

        array.shrink_to_fit();
        array
    }

    fn load_node(&mut self, expr: Expression<'src>) -> NodeResult {
        let node = match expr.kind {
            ExpressionKind::Integer(value) => Node::from(value),
            ExpressionKind::Float(value) => Node::from(value),
            ExpressionKind::String(value) => Node::from(value.to_owned()),
            ExpressionKind::Symbol(value) => Node::from(self.context.add_symbol(value)),
            ExpressionKind::Variable(value) => Node::from(NodeVariable::from(self.context.add_symbol(value))),
            ExpressionKind::Unhandled => Node::from(NodeValue::Unhandled),

            ExpressionKind::Array(exprs) => Node::from(self.load_array(exprs.into_iter())),
            ExpressionKind::Command(exprs) => Node::from(NodeCommand::from(self.load_array(exprs.into_iter()))),
            ExpressionKind::Property(exprs) => Node::from(NodeProperty::from(self.load_array(exprs.into_iter()))),

            ExpressionKind::Define(name, exprs) => {
                let name = self.context.add_symbol(name.text);
                let define = self.load_array(exprs.exprs.into_iter());
                self.context.add_macro(name, define);
                return NodeResult::Skip;
            },
            ExpressionKind::Undefine(name) => {
                if let Some(name) = self.context.get_symbol(name.text) {
                    self.context.remove_macro(&name);
                }
                return NodeResult::Skip;
            },
            ExpressionKind::Include(_path) => {
                todo!("#include loading")
            },
            ExpressionKind::IncludeOptional(_path) => {
                todo!("#include_opt loading")
            },
            ExpressionKind::Merge(_name) => {
                todo!("#merge loading")
            },
            ExpressionKind::Autorun(_exprs) => {
                todo!("#autorun loading")
            },

            ExpressionKind::Conditional { is_positive, symbol, true_branch, false_branch } => {
                let defined = match self.context.get_symbol(symbol.text) {
                    Some(name) => self.context.get_macro(&name).is_some(),
                    None => false,
                };

                let array = match defined == is_positive {
                    true => self.load_array(true_branch.exprs.into_iter()),
                    false => match false_branch {
                        Some(false_branch) => self.load_array(false_branch.exprs.into_iter()),
                        None => return NodeResult::Skip,
                    },
                };

                Node::from(array)
            },
        };

        NodeResult::Value(node)
    }
}

pub fn load_path(context: &mut Context, path: &Path) -> Result<NodeArray, LoadError> {
    load_text(context, &fs::read_to_string(path)?)
}

pub fn load_file(context: &mut Context, file: File) -> Result<NodeArray, LoadError> {
    let text = io::read_to_string(file)?;
    load_text(context, &text)
}

pub fn load_text(context: &mut Context, text: &str) -> Result<NodeArray, LoadError> {
    let ast = match parser::parse_text(text) {
        Ok(ast) => ast,
        Err(errors) => return Err(LoadError::Parse(errors)),
    };
    Ok(load_ast(context, ast.into_iter()))
}

pub fn load_ast<'src>(context: &mut Context, ast: impl Iterator<Item = Expression<'src>>) -> NodeArray {
    let mut loader = Loader::new(context);
    loader.load_array(ast)
}
