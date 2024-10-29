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
/// Used to prevent desyncs between match arms and error information.
macro_rules! evaluate_type {
    ($node:ident, $type:ident($value:ident) => $expr:expr) => {
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
    ($node:ident; $($type:ident($value:ident) => $expr:expr$(,)?)+) => {
        match $node {
            $(Node::$type($value) => Ok($expr),)+
            _ => {
                return Err(Error::UnhandledType {
                    expected: vec![$(NodeType::$type,)+],
                    actual: $node.get_type(),
                })
            },
        }
    };
}

impl Node {
    pub fn get_type(&self) -> NodeType {
        match self {
            Self::Integer(_) => NodeType::Integer,
            Self::Float(_) => NodeType::Float,
            Self::String(_) => NodeType::String,
            Self::Symbol(_) => NodeType::Symbol,
        }
    }

    pub fn integer(&self) -> Result<NodeInteger, Error> {
        evaluate_type! {
            self;
            Integer(value) => *value,
            Float(value) => *value as NodeInteger,
        }
    }

    pub fn integer_strict(&self) -> Result<NodeInteger, Error> {
        evaluate_type!(self, Integer(value) => *value)
    }

    pub fn float(&self) -> Result<NodeFloat, Error> {
        evaluate_type! {
            self;
            Integer(value) => *value as NodeFloat,
            Float(value) => *value,
        }
    }

    pub fn float_strict(&self) -> Result<NodeFloat, Error> {
        evaluate_type!(self, Float(value) => *value)
    }

    pub fn symbol(&self) -> Result<&Symbol, Error> {
        evaluate_type!(self, Symbol(value) => value)
    }

    pub fn string(&self) -> Result<&String, Error> {
        evaluate_type!(self, String(value) => value)
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
