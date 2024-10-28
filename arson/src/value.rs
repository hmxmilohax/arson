// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::{Error, Symbol};

pub type NodeInteger = i64;
pub type NodeFloat = f64;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum NodeType {
    Integer,
    Float,
    String,
    Symbol,
}

#[derive(Debug, Clone)]
pub enum Node {
    Integer(NodeInteger),
    Float(NodeFloat),
    String(String),
    Symbol(Symbol),
}

/// Helper macro for evaluating whether a node matches a single type.
/// Used to prevent desyncs between the match arm and the [`Error::TypeMismatch::expected`] field.
macro_rules! evaluate_single_type {
    ($node:ident, $type:ident) => {
        match $node {
            Node::$type(value) => Ok(value),
            _ => return Err(Error::TypeMismatch { expected: NodeType::$type, actual: $node.get_type() }),
        }
    };
    ($node:ident, *$type:ident) => {
        match $node {
            Node::$type(value) => Ok(*value),
            _ => return Err(Error::TypeMismatch { expected: NodeType::$type, actual: $node.get_type() }),
        }
    };
}

impl Node {
    pub const fn get_type(&self) -> NodeType {
        match self {
            Self::Integer(_) => NodeType::Integer,
            Self::Float(_) => NodeType::Float,
            Self::String(_) => NodeType::String,
            Self::Symbol(_) => NodeType::Symbol,
        }
    }

    pub const fn integer(&self) -> Result<NodeInteger, Error> {
        match self {
            Self::Integer(value) => Ok(*value),
            Self::Float(value) => Ok(*value as NodeInteger),
            _ => Err(Error::unhandled(self)),
        }
    }

    pub const fn integer_strict(&self) -> Result<NodeInteger, Error> {
        evaluate_single_type!(self, *Integer)
    }

    pub const fn float(&self) -> Result<NodeFloat, Error> {
        match self {
            Self::Integer(value) => Ok(*value as NodeFloat),
            Self::Float(value) => Ok(*value),
            _ => Err(Error::unhandled(self)),
        }
    }

    pub const fn float_strict(&self) -> Result<NodeFloat, Error> {
        evaluate_single_type!(self, *Float)
    }

    pub const fn symbol(&self) -> Result<&Symbol, Error> {
        evaluate_single_type!(self, Symbol)
    }

    pub const fn string(&self) -> Result<&String, Error> {
        evaluate_single_type!(self, String)
    }
}

macro_rules! impl_from {
    ($variant:ident, $from_type:ty) => {
        impl From<$from_type> for Node {
            fn from(value: $from_type) -> Self {
                Node::$variant(value)
            }
        }
    };
    ($variant:ident, $from_type:ty as $cast:ty) => {
        impl From<$from_type> for Node {
            fn from(value: $from_type) -> Self {
                Node::$variant(value as $cast)
            }
        }
    };
}

impl_from!(Integer, NodeInteger);
impl_from!(Integer, i32 as NodeInteger);
impl_from!(Float, NodeFloat);
impl_from!(Float, f32 as NodeFloat);
impl_from!(Symbol, Symbol);
impl_from!(String, String);
