// SPDX-License-Identifier: LGPL-3.0-or-later

use std::rc::Rc;

use crate::{Error, Object, Symbol};

/// A function which is callable by a [`NodeCommand`].
pub type HandleFn = fn(args: &NodeArray) -> HandleResult;
/// The result of a [`HandleFn`].
pub type HandleResult = Result<Node, Error>;

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

    Object(Rc<dyn Object>),
    Function(HandleFn),

    Array(Rc<NodeArray>),
    Command(Rc<NodeCommand>),
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

            Self::Object(_) => NodeType::Object,
            Self::Function(_) => NodeType::Function,

            Self::Array(_) => NodeType::Array,
            Self::Command(_) => NodeType::Command,
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

    pub fn object(&self) -> Result<&Rc<dyn Object>, Error> {
        evaluate_type!(self, Object(value) => value)
    }

    pub fn function(&self) -> Result<HandleFn, Error> {
        evaluate_type!(self, Function(value) => *value)
    }

    pub fn array(&self) -> Result<&Rc<NodeArray>, Error> {
        evaluate_type!(self, Array(value) => value)
    }

    pub fn command(&self) -> Result<&Rc<NodeCommand>, Error> {
        evaluate_type!(self, Command(value) => value)
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

#[derive(Debug, Clone)]
pub struct NodeArray {
    nodes: Vec<Node>,
}

impl NodeArray {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn push(&mut self, node: Node) {
        self.nodes.push(node)
    }

    pub fn pop(&mut self) -> Option<Node> {
        self.nodes.pop()
    }

    pub fn insert(&mut self, index: usize, node: Node) {
        self.nodes.insert(index, node)
    }

    pub fn remove(&mut self, index: usize) -> Node {
        self.nodes.remove(index)
    }

    pub fn node(&self, index: usize) -> Result<&Node, Error> {
        match self.nodes.get(index) {
            Some(value) => Ok(value),
            None => Err(Error::IndexOutOfRange { index, range: 0..self.nodes.len() }),
        }
    }

    pub fn node_mut(&mut self, index: usize) -> Result<&mut Node, Error> {
        //? workaround for mutable borrow rules
        let range = 0..self.nodes.len();

        match self.nodes.get_mut(index) {
            Some(value) => Ok(value),
            None => Err(Error::IndexOutOfRange { index, range }),
        }
    }

    pub fn integer(&self, index: usize) -> Result<NodeInteger, Error> {
        self.node(index)?.integer()
    }

    pub fn float(&self, index: usize) -> Result<NodeFloat, Error> {
        self.node(index)?.float()
    }

    pub fn symbol(&self, index: usize) -> Result<&Symbol, Error> {
        self.node(index)?.symbol()
    }

    pub fn string(&self, index: usize) -> Result<&String, Error> {
        self.node(index)?.string()
    }

    pub fn object(&self, index: usize) -> Result<&Rc<dyn Object>, Error> {
        self.node(index)?.object()
    }

    pub fn function(&self, index: usize) -> Result<HandleFn, Error> {
        self.node(index)?.function()
    }

    pub fn array(&self, index: usize) -> Result<&Rc<NodeArray>, Error> {
        self.node(index)?.array()
    }

    pub fn command(&self, index: usize) -> Result<&Rc<NodeCommand>, Error> {
        self.node(index)?.command()
    }
}

#[derive(Debug, Clone)]
pub struct NodeCommand {
    nodes: NodeArray,
}

impl std::ops::Deref for NodeCommand {
    type Target = NodeArray;

    fn deref(&self) -> &Self::Target {
        &self.nodes
    }
}
