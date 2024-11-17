// SPDX-License-Identifier: LGPL-3.0-or-later

use std::{io, marker::PhantomData, rc::Rc};

use crate::{fs::VirtualPath, Context, Node, NodeArray, NodeCommand, NodeProperty, NodeValue, RawNodeValue, Variable};

use super::parser::{self, Expression, ExpressionKind, ParseError};

#[derive(Clone)]
pub struct LoadOptions {
    pub allow_include: bool,
}

#[derive(thiserror::Error, Debug)]
pub enum LoadError {
    #[error("IO error: {0}")]
    IO(#[from] io::Error),

    #[error("Failed to parse the given file")]
    Parse(Vec<ParseError>),

    #[error("A required macro definition was not found")]
    MacroNotFound,

    #[error("Inclusion is disallowed by the given load options")]
    IncludeNotAllowed,

    #[error("Encountered errors while including other files")]
    Inner {
        recovered: NodeArray,
        errors: Vec<LoadError>,
    },
}

struct Loader<'ctx, 'src> {
    context: &'ctx mut Context,
    options: LoadOptions,
    phantom: PhantomData<&'src ()>,
}

enum NodeResult<'define> {
    Value(Node),
    Include(NodeArray),
    MergeFile(NodeArray),
    MergeMacro(&'define NodeArray),
    Skip,
}

impl<'ctx, 'src> Loader<'ctx, 'src> {
    fn new(context: &'ctx mut Context, options: LoadOptions) -> Self {
        Self { context, options, phantom: PhantomData }
    }

    fn load_array(&mut self, ast: impl Iterator<Item = Expression<'src>>) -> Result<NodeArray, LoadError> {
        let mut array = NodeArray::with_capacity(4);
        let mut errors = vec![];

        for expr in ast {
            match self.load_node(expr) {
                Ok(result) => match result {
                    NodeResult::Value(node) => array.push(node),
                    NodeResult::Include(mut file) => array.append(&mut file),
                    NodeResult::MergeFile(file) => Self::merge_array(&mut array, &file),
                    NodeResult::MergeMacro(define) => Self::merge_array(&mut array, define),
                    NodeResult::Skip => continue,
                },
                Err(error) => {
                    errors.push(error);
                    continue;
                },
            }
        }

        array.shrink_to_fit();

        if !errors.is_empty() {
            Err(LoadError::Inner { recovered: array, errors })
        } else {
            Ok(array)
        }
    }

    // This is implemented here instead of on NodeArray itself,
    // since [`Rc::get_mut`] requires that no other references to the stored array exist,
    // and the only place that this can be ensured is within the loader.
    fn merge_array(target: &mut NodeArray, source: &NodeArray) {
        for node in source.iter() {
            let RawNodeValue::Array(array) = node.unevaluated() else {
                continue;
            };
            let Ok(tag) = array.get(0) else {
                continue;
            };

            match target.find_array_opt(tag.unevaluated()) {
                // Some(mut found) => match Rc::get_mut(&mut found) {
                //     Some(found) => Self::merge_array(found, array),
                //     None => continue,
                // },
                Some(mut found) => {
                    let found =
                        Rc::get_mut(&mut found).expect("no external array references should exist during loading");
                    Self::merge_array(found, array);
                },
                None => target.push(NodeValue::Array(array.clone())),
            }
        }
    }

    fn load_node(&mut self, expr: Expression<'src>) -> Result<NodeResult<'_>, LoadError> {
        let node = match expr.kind {
            ExpressionKind::Integer(value) => value.into(),
            ExpressionKind::Float(value) => value.into(),
            ExpressionKind::String(value) => value.into(),
            ExpressionKind::Symbol(value) => self.context.add_symbol(value).into(),
            ExpressionKind::Variable(value) => Variable::from(self.context.add_symbol(value)).into(),
            ExpressionKind::Unhandled => Node::UNHANDLED,

            ExpressionKind::Array(exprs) => self.load_array(exprs.into_iter())?.into(),
            ExpressionKind::Command(exprs) => NodeCommand::from(self.load_array(exprs.into_iter())?).into(),
            ExpressionKind::Property(exprs) => NodeProperty::from(self.load_array(exprs.into_iter())?).into(),

            ExpressionKind::Define(name, exprs) => {
                let name = self.context.add_symbol(name.text);
                let define = self.load_array(exprs.exprs.into_iter())?;
                self.context.add_macro(name, define);
                return Ok(NodeResult::Skip);
            },
            ExpressionKind::Undefine(name) => {
                if let Some(name) = self.context.get_symbol(name.text) {
                    self.context.remove_macro(&name);
                }
                return Ok(NodeResult::Skip);
            },
            ExpressionKind::Include(path) => {
                if !self.options.allow_include {
                    return Err(LoadError::IncludeNotAllowed);
                }

                let file = self.load_path(path.text)?;
                return Ok(NodeResult::Include(file));
            },
            ExpressionKind::IncludeOptional(path) => {
                if !self.options.allow_include {
                    return Ok(NodeResult::Skip);
                }

                match self.load_path_optional(path.text)? {
                    Some(file) => return Ok(NodeResult::Include(file)),
                    None => return Ok(NodeResult::Skip),
                }
            },
            ExpressionKind::Merge(name) => {
                if let Some(symbol) = self.context.get_symbol(name.text) {
                    match self.context.get_macro(&symbol) {
                        Some(define) => return Ok(NodeResult::MergeMacro(define)),
                        None => return Err(LoadError::MacroNotFound),
                    }
                } else {
                    if !self.options.allow_include {
                        return Err(LoadError::IncludeNotAllowed);
                    }

                    let file = self.load_path(name.text)?;
                    return Ok(NodeResult::MergeFile(file));
                }
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
                    true => self.load_array(true_branch.exprs.into_iter())?,
                    false => match false_branch {
                        Some(false_branch) => self.load_array(false_branch.exprs.into_iter())?,
                        None => return Ok(NodeResult::Skip),
                    },
                };

                Node::from(array)
            },
        };

        Ok(NodeResult::Value(node))
    }

    fn load_path_optional(&mut self, path: &str) -> Result<Option<NodeArray>, LoadError> {
        match self.context.file_system().exists(VirtualPath::new(path)) {
            true => self.load_path(path).map(|a| Some(a)),
            false => Ok(None),
        }
    }

    fn load_path(&mut self, path: &str) -> Result<NodeArray, LoadError> {
        load_path(self.context, self.options.clone(), VirtualPath::new(path))
    }
}

pub fn load_path(context: &mut Context, options: LoadOptions, path: &VirtualPath) -> Result<NodeArray, LoadError> {
    let file_system = context.file_system_mut();

    let mut file = file_system.open_execute(path)?;
    let text = io::read_to_string(file.as_mut())?;

    file_system.set_cwd(path)?;

    load_text(context, options, &text)
}

pub fn load_text(context: &mut Context, options: LoadOptions, text: &str) -> Result<NodeArray, LoadError> {
    let ast = match parser::parse_text(text) {
        Ok(ast) => ast,
        Err(errors) => return Err(LoadError::Parse(errors)),
    };
    load_ast(context, options, ast.into_iter())
}

pub fn load_ast<'src>(
    context: &mut Context,
    options: LoadOptions,
    ast: impl Iterator<Item = Expression<'src>>,
) -> Result<NodeArray, LoadError> {
    let mut loader = Loader::new(context, options);
    loader.load_array(ast)
}
