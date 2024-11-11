// SPDX-License-Identifier: LGPL-3.0-or-later

use std::rc::Rc;

use super::{Context, Error, Object, Symbol};

/// A function which is callable by a [`NodeCommand`].
pub type HandleFn = fn(context: &mut Context, args: &NodeArray) -> HandleResult;
/// The result of a [`HandleFn`].
pub type HandleResult = crate::Result<Node>;

pub type NodeInteger = i64;
pub type NodeFloat = f64;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeVariable {
    pub symbol: Symbol,
}

impl From<Symbol> for NodeVariable {
    fn from(value: Symbol) -> Self {
        Self { symbol: value }
    }
}

macro_rules! define_node_types {
    ($($(#[$attr:meta])* $type:ident$(($value:ty))?$(,)?)+) => {
        /// The type of value contained within a node.
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
        pub enum NodeType {
            $($(#[$attr])* $type,)+
        }

        #[derive(Debug, Clone)]
        pub enum NodeValue {
            $($(#[$attr])* $type$(($value))?,)+
        }

        impl NodeValue {
            pub fn get_type(&self) -> NodeType {
                match self {
                    $(NodeValue::$type$((param_sink!($value, _)))? => NodeType::$type,)+
                }
            }
        }
    }
}

// type aliases to avoid syntax errors in the macro below
type StringBox = Rc<String>;
type ObjectBox = Rc<dyn Object>;
type ArrayBox = Rc<NodeArray>;
type CommandBox = Rc<NodeCommand>;
type PropertyBox = Rc<NodeProperty>;

define_node_types! {
    Integer(NodeInteger),
    Float(NodeFloat),
    String(StringBox),

    Symbol(Symbol),
    Variable(NodeVariable),
    Unhandled,

    Object(ObjectBox),
    Function(HandleFn),

    Array(ArrayBox),
    Command(CommandBox),
    Property(PropertyBox),
}

/// Helper macro for evaluating whether a node matches a single type.
/// Used to prevent desyncs between match arms and error information.
macro_rules! evaluate_type {
    ($node:ident, $type:ident($value:ident) => $expr:expr) => {
        match $node {
            NodeValue::$type($value) => Ok($expr),
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
            $(NodeValue::$type($value) => Ok($expr),)+
            _ => {
                return Err(Error::UnhandledType {
                    expected: vec![$(NodeType::$type,)+],
                    actual: $node.get_type(),
                })
            },
        }
    };
}

impl NodeValue {
    pub const fn handled() -> Self {
        Self::Integer(0)
    }

    pub fn integer(&self) -> crate::Result<NodeInteger> {
        evaluate_type! {
            self;
            Integer(value) => *value,
            Float(value) => *value as NodeInteger,
        }
    }

    pub fn integer_strict(&self) -> crate::Result<NodeInteger> {
        evaluate_type!(self, Integer(value) => *value)
    }

    pub fn float(&self) -> crate::Result<NodeFloat> {
        evaluate_type! {
            self;
            Integer(value) => *value as NodeFloat,
            Float(value) => *value,
        }
    }

    pub fn float_strict(&self) -> crate::Result<NodeFloat> {
        evaluate_type!(self, Float(value) => *value)
    }

    pub fn string(&self) -> crate::Result<&StringBox> {
        evaluate_type!(self, String(value) => value)
    }

    pub fn symbol(&self) -> crate::Result<&Symbol> {
        evaluate_type!(self, Symbol(value) => value)
    }

    pub fn variable(&self) -> crate::Result<&NodeVariable> {
        evaluate_type!(self, Variable(value) => value)
    }

    pub fn is_unhandled(&self) -> bool {
        matches!(self, Self::Unhandled)
    }

    pub fn object(&self) -> crate::Result<&ObjectBox> {
        evaluate_type!(self, Object(value) => value)
    }

    pub fn function(&self) -> crate::Result<HandleFn> {
        evaluate_type!(self, Function(value) => *value)
    }

    pub fn array(&self) -> crate::Result<&ArrayBox> {
        evaluate_type!(self, Array(value) => value)
    }

    pub fn command(&self) -> crate::Result<&CommandBox> {
        evaluate_type!(self, Command(value) => value)
    }

    pub fn property(&self) -> crate::Result<&PropertyBox> {
        evaluate_type!(self, Property(value) => value)
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    value: NodeValue,
}

impl Node {
    pub const fn handled() -> Self {
        Self { value: NodeValue::handled() }
    }

    pub const fn unhandled() -> Self {
        Self { value: NodeValue::Unhandled }
    }

    pub fn evaluate(&self, context: &mut Context) -> crate::Result<NodeValue> {
        let evaluated = match &self.value {
            NodeValue::Variable(name) => context.get_variable(&name.symbol).value,
            NodeValue::Command(command) => context.execute(command)?.value,
            NodeValue::Property(_property) => todo!("property node evaluation"),
            value => value.clone(),
        };
        Ok(evaluated)
    }

    #[inline]
    pub const fn unevaluated(&self) -> &NodeValue {
        &self.value
    }

    pub fn integer(&self, context: &mut Context) -> crate::Result<NodeInteger> {
        self.evaluate(context)?.integer()
    }

    pub fn integer_strict(&self, context: &mut Context) -> crate::Result<NodeInteger> {
        self.evaluate(context)?.integer_strict()
    }

    pub fn float(&self, context: &mut Context) -> crate::Result<NodeFloat> {
        self.evaluate(context)?.float()
    }

    pub fn float_strict(&self, context: &mut Context) -> crate::Result<NodeFloat> {
        self.evaluate(context)?.float_strict()
    }

    pub fn string(&self, context: &mut Context) -> crate::Result<StringBox> {
        self.evaluate(context)?.string().cloned()
    }

    pub fn symbol(&self, context: &mut Context) -> crate::Result<Symbol> {
        self.evaluate(context)?.symbol().cloned()
    }

    // No context arg here; variables are evaluated away
    pub fn variable(&self) -> crate::Result<NodeVariable> {
        self.unevaluated().variable().cloned()
    }

    pub fn is_unhandled(&self) -> bool {
        self.unevaluated().is_unhandled()
    }

    pub fn object(&self, context: &mut Context) -> crate::Result<ObjectBox> {
        self.evaluate(context)?.object().cloned()
    }

    pub fn function(&self, context: &mut Context) -> crate::Result<HandleFn> {
        self.evaluate(context)?.function()
    }

    pub fn array(&self, context: &mut Context) -> crate::Result<ArrayBox> {
        self.evaluate(context)?.array().cloned()
    }

    // No context arg here; commands are evaluated away
    pub fn command(&self) -> crate::Result<CommandBox> {
        self.unevaluated().command().cloned()
    }

    // No context arg here; properties are evaluated away
    pub fn property(&self) -> crate::Result<PropertyBox> {
        self.unevaluated().property().cloned()
    }
}

macro_rules! impl_from {
    ($variant:ident, $from_type:ty) => {
        impl_from!($variant, $from_type, value => value);
    };
    ($variant:ident, $from_type:ty, $value:ident => $expr:expr) => {
        impl From<$from_type> for NodeValue {
            fn from($value: $from_type) -> Self {
                NodeValue::$variant($expr)
            }
        }

        impl From<$from_type> for Node {
            fn from($value: $from_type) -> Self {
                Self { value: NodeValue::from($value) }
            }
        }
    };
}

impl From<NodeValue> for Node {
    fn from(value: NodeValue) -> Self {
        Self { value }
    }
}

impl_from!(Integer, NodeInteger);
impl_from!(Integer, i32, value => value as NodeInteger);
impl_from!(Float, NodeFloat);
impl_from!(Float, f32, value => value as NodeFloat);
impl_from!(String, StringBox);
impl_from!(String, String, value => Rc::new(value));
impl_from!(String, &String, value => Rc::new(value.clone()));
impl_from!(String, &str, value => Rc::new(value.to_owned()));

impl_from!(Symbol, Symbol);
impl_from!(Symbol, &Symbol, value => value.clone());
impl_from!(Variable, NodeVariable);
impl_from!(Variable, &NodeVariable, value => value.clone());

impl_from!(Object, ObjectBox);
impl_from!(Object, &ObjectBox, value => value.clone());
impl_from!(Function, HandleFn);

impl_from!(Array, ArrayBox);
impl_from!(Array, NodeArray, value => Rc::new(value));
impl_from!(Array, &ArrayBox, value => value.clone());
impl_from!(Command, CommandBox);
impl_from!(Command, NodeCommand, value => Rc::new(value));
impl_from!(Command, &CommandBox, value => value.clone());
impl_from!(Property, PropertyBox);
impl_from!(Property, NodeProperty, value => Rc::new(value));
impl_from!(Property, &PropertyBox, value => value.clone());

#[derive(Debug, Clone)]
pub struct NodeArray {
    nodes: Vec<Node>,
}

impl NodeArray {
    pub const fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self { nodes: Vec::with_capacity(capacity) }
    }

    pub fn node(&self, index: usize) -> crate::Result<&Node> {
        match self.nodes.get(index) {
            Some(value) => Ok(value),
            None => Err(Error::IndexOutOfRange { index, range: 0..self.nodes.len() }),
        }
    }

    pub fn evaluate(&self, context: &mut Context, index: usize) -> crate::Result<NodeValue> {
        self.node(index)?.evaluate(context)
    }

    pub fn unevaluated(&self, index: usize) -> crate::Result<&NodeValue> {
        Ok(self.node(index)?.unevaluated())
    }

    pub fn integer(&self, context: &mut Context, index: usize) -> crate::Result<NodeInteger> {
        self.node(index)?.integer(context)
    }

    pub fn integer_strict(&self, context: &mut Context, index: usize) -> crate::Result<NodeInteger> {
        self.node(index)?.integer_strict(context)
    }

    pub fn float(&self, context: &mut Context, index: usize) -> crate::Result<NodeFloat> {
        self.node(index)?.float(context)
    }

    pub fn float_strict(&self, context: &mut Context, index: usize) -> crate::Result<NodeFloat> {
        self.node(index)?.float_strict(context)
    }

    pub fn string(&self, context: &mut Context, index: usize) -> crate::Result<StringBox> {
        self.node(index)?.string(context)
    }

    pub fn symbol(&self, context: &mut Context, index: usize) -> crate::Result<Symbol> {
        self.node(index)?.symbol(context)
    }

    pub fn variable(&self, index: usize) -> crate::Result<NodeVariable> {
        self.node(index)?.variable()
    }

    pub fn object(&self, context: &mut Context, index: usize) -> crate::Result<ObjectBox> {
        self.node(index)?.object(context)
    }

    pub fn function(&self, context: &mut Context, index: usize) -> crate::Result<HandleFn> {
        self.node(index)?.function(context)
    }

    pub fn array(&self, context: &mut Context, index: usize) -> crate::Result<ArrayBox> {
        self.node(index)?.array(context)
    }

    pub fn command(&self, index: usize) -> crate::Result<CommandBox> {
        self.node(index)?.command()
    }

    pub fn property(&self, index: usize) -> crate::Result<PropertyBox> {
        self.node(index)?.property()
    }

    pub fn find_array(&self, tag: &Symbol) -> crate::Result<&NodeArray> {
        for node in self.iter() {
            let NodeValue::Array(array) = node.unevaluated() else {
                continue;
            };
            let Ok(node) = array.unevaluated(0) else {
                continue;
            };
            let Ok(symbol) = node.symbol() else {
                continue;
            };

            if symbol == tag {
                return Ok(array);
            }
        }

        Err(Error::EntryNotFound(tag.clone()))
    }
}

impl Default for NodeArray {
    fn default() -> Self {
        Self::new()
    }
}

impl std::ops::Deref for NodeArray {
    type Target = Vec<Node>;

    fn deref(&self) -> &Self::Target {
        &self.nodes
    }
}

impl std::ops::DerefMut for NodeArray {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.nodes
    }
}

#[derive(Debug, Clone)]
pub struct NodeCommand {
    nodes: NodeArray,
}

impl NodeCommand {
    pub const fn new() -> Self {
        Self { nodes: NodeArray::new() }
    }
}

impl Default for NodeCommand {
    fn default() -> Self {
        Self::new()
    }
}

impl From<NodeArray> for NodeCommand {
    fn from(value: NodeArray) -> Self {
        Self { nodes: value }
    }
}

impl std::ops::Deref for NodeCommand {
    type Target = NodeArray;

    fn deref(&self) -> &Self::Target {
        &self.nodes
    }
}

impl std::ops::DerefMut for NodeCommand {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.nodes
    }
}

#[derive(Debug, Clone)]
pub struct NodeProperty {
    nodes: NodeArray,
}

impl NodeProperty {
    pub const fn new() -> Self {
        Self { nodes: NodeArray::new() }
    }
}

impl Default for NodeProperty {
    fn default() -> Self {
        Self::new()
    }
}

impl From<NodeArray> for NodeProperty {
    fn from(value: NodeArray) -> Self {
        Self { nodes: value }
    }
}

impl std::ops::Deref for NodeProperty {
    type Target = NodeArray;

    fn deref(&self) -> &Self::Target {
        &self.nodes
    }
}

impl std::ops::DerefMut for NodeProperty {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.nodes
    }
}
