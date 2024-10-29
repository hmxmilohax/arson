// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::{Error, Symbol};

pub type NodeInteger = i64;
pub type NodeFloat = f64;

macro_rules! define_node_types {
    ($($type:ident($value:ty)$(,)?)+) => {
        /// The type of value contained within a node.
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
        pub enum NodeType {
            $($type,)+
        }

        #[derive(Debug, Clone)]
        pub enum Node {
            $($type($value),)+
        }
    }
}

define_node_types! {
    Integer(NodeInteger),
    Float(NodeFloat),
    String(String),
    Symbol(Symbol),
}

/// Helper macro for evaluating whether a node matches a single type.
/// Used to prevent desyncs between the match arm and the [`Error::TypeMismatch::expected`] field.
macro_rules! evaluate_single_type {
    ($node:ident, $type:ident) => {
        evaluate_single_type!($node, $type, value => value)
    };
    ($node:ident, $type:ident, $value:ident => $expr:expr) => {
        match $node {
            Node::$type($value) => Ok($expr),
            _ => {
                return Err(Error::TypeMismatch {
                    expected: NodeType::$type,
                    actual: $node.get_type(),
                })
            },
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
        evaluate_single_type!(self, Integer, value => *value)
    }

    pub const fn float(&self) -> Result<NodeFloat, Error> {
        match self {
            Self::Integer(value) => Ok(*value as NodeFloat),
            Self::Float(value) => Ok(*value),
            _ => Err(Error::unhandled(self)),
        }
    }

    pub const fn float_strict(&self) -> Result<NodeFloat, Error> {
        evaluate_single_type!(self, Float, value => *value)
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
        impl_from!($variant, $from_type, value => value);
    };
    ($variant:ident, $from_type:ty, $value:ident => $expr:expr) => {
        impl From<$from_type> for Node {
            fn from($value: $from_type) -> Self {
                Node::$variant($expr)
            }
        }
    };
}

impl_from!(Integer, NodeInteger);
impl_from!(Float, NodeFloat);
impl_from!(Symbol, Symbol);
impl_from!(String, String);

impl_from!(Integer, i32, value => value as NodeInteger);
impl_from!(Float, f32, value => value as NodeFloat);
impl_from!(Symbol, &Symbol, value => value.clone());
impl_from!(String, &String, value => value.clone());
impl_from!(String, &str, value => value.to_owned());
