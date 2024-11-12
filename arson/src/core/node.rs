// SPDX-License-Identifier: LGPL-3.0-or-later

use std::{
    borrow::Borrow,
    ops::{Deref, DerefMut},
    rc::Rc,
    slice::SliceIndex,
};

use super::{Context, Error, Object, Symbol};

/// A function which is callable by a [`NodeCommand`].
pub type HandleFn = fn(context: &mut Context, args: &NodeSlice) -> HandleResult;
/// The result of a [`HandleFn`].
pub type HandleResult = crate::Result<NodeValue>;

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
        /// The type of value contained within a [`Node`], [`NodeValue`], or [`RawNodeValue`].
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
        pub enum NodeType {
            $($(#[$attr])* $type,)+
        }

        /// A raw, unevaluated value stored within a [`Node`].
        #[derive(Debug, Clone)]
        pub enum RawNodeValue {
            $($(#[$attr])* $type$(($value))?,)+
        }

        impl RawNodeValue {
            pub fn get_type(&self) -> NodeType {
                match self {
                    $(Self::$type$((param_sink!($value, _)))? => NodeType::$type,)+
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

/// A node value which has been evaluated.
#[derive(Debug, Clone)]
pub enum NodeValue {
    Integer(NodeInteger),
    Float(NodeFloat),
    String(StringBox),
    Symbol(Symbol),
    Unhandled,

    Object(ObjectBox),
    Function(HandleFn),
    Array(ArrayBox),
}

impl NodeValue {
    pub fn get_type(&self) -> NodeType {
        match self {
            NodeValue::Integer(_) => NodeType::Integer,
            NodeValue::Float(_) => NodeType::Float,
            NodeValue::String(_) => NodeType::String,
            NodeValue::Symbol(_) => NodeType::Symbol,
            NodeValue::Unhandled => NodeType::Unhandled,

            NodeValue::Object(_) => NodeType::Object,
            NodeValue::Function(_) => NodeType::Function,
            NodeValue::Array(_) => NodeType::Array,
        }
    }
}

/// Helper macro for evaluating a node against a set of types,
/// returning an error if none of them match.
///
/// Used to prevent desyncs between match arms and error information.
#[macro_export]
macro_rules! evaluate_node {
    (
        $node:expr;
        $($type:ident::$variant:ident($value:ident) => $expr:expr$(,)?)+
    ) => {
        match $node {
            $($type::$variant($value) => $expr,)+
            unhandled => {
                let expected = vec![$($crate::NodeType::$variant,)+];
                if expected.len() == 1 {
                    return Err($crate::Error::TypeMismatch {
                        expected: expected[0],
                        actual: unhandled.get_type(),
                    })
                } else {
                    return Err($crate::Error::UnhandledType {
                        expected,
                        actual: unhandled.get_type(),
                    })
                }
            },
        }
    };
}

/// Helper macro for evaluating an optional node result against a set of types, using a fallback case for
/// a [`None`] result, and returning an error on the [`Some`] path if no patterns match.
///
/// Used for conciseness and to prevent desyncs between match arms and error information.
#[macro_export]
macro_rules! evaluate_node_opt {
    (
        Some($some_ident:ident @ $node:expr) => $some_expr:expr;
        None => $none_expr:expr$(,)?;
        $($type:ident::$variant:ident($value:ident) => $expr:expr,)+
    ) => {
        match $node {
            Some($some_ident) => evaluate_node!($some_expr; $($type::$variant($value) => $expr,)+),
            None => $none_expr,
        }
    };
}

macro_rules! common_getters {
    () => {
        pub fn integer(&self) -> crate::Result<NodeInteger> {
            evaluate_node! {
                self;
                Self::Integer(value) => Ok(*value),
                Self::Float(value) => Ok(*value as NodeInteger),
            }
        }

        pub fn integer_strict(&self) -> crate::Result<NodeInteger> {
            evaluate_node!(self; Self::Integer(value) => Ok(*value))
        }

        pub fn boolean(&self) -> crate::Result<bool> {
            evaluate_node! {
                self;
                Self::Integer(value) => Ok(*value != 0),
                Self::String(value) => Ok(!value.is_empty()),
                Self::Symbol(value) => Ok(!value.name().is_empty()),
            }
        }

        pub fn boolean_strict(&self) -> crate::Result<bool> {
            Ok(self.integer_strict()? != 0)
        }

        pub fn float(&self) -> crate::Result<NodeFloat> {
            evaluate_node! {
                self;
                Self::Integer(value) => Ok(*value as NodeFloat),
                Self::Float(value) => Ok(*value),
            }
        }

        pub fn float_strict(&self) -> crate::Result<NodeFloat> {
            evaluate_node!(self; Self::Float(value) => Ok(*value))
        }

        pub fn string(&self) -> crate::Result<&StringBox> {
            evaluate_node!(self; Self::String(value) => Ok(value))
        }

        pub fn symbol(&self) -> crate::Result<&Symbol> {
            evaluate_node!(self; Self::Symbol(value) => Ok(value))
        }

        pub fn object(&self) -> crate::Result<&ObjectBox> {
            evaluate_node!(self; Self::Object(value) => Ok(value))
        }

        pub fn function(&self) -> crate::Result<HandleFn> {
            evaluate_node!(self; Self::Function(value) => Ok(*value))
        }

        pub fn array(&self) -> crate::Result<&ArrayBox> {
            evaluate_node!(self; Self::Array(value) => Ok(value))
        }
    };
}

impl NodeValue {
    /// Generic value to be returned when a script call has been handled,
    /// but no specific value is returned from the method handling the call.
    pub const HANDLED: NodeValue = Self::Integer(0);

    /// Boolean TRUE value.
    pub const TRUE: NodeValue = Self::Integer(1);
    /// Boolean FALSE value.
    pub const FALSE: NodeValue = Self::Integer(0);

    common_getters!();
}

impl PartialEq for NodeValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (NodeValue::Integer(left), NodeValue::Integer(right)) => left == right,
            (NodeValue::Float(left), NodeValue::Float(right)) => left == right,
            (NodeValue::String(left), NodeValue::String(right)) => left == right,
            (NodeValue::Symbol(left), NodeValue::Symbol(right)) => left == right,
            (NodeValue::Unhandled, NodeValue::Unhandled) => true,

            (NodeValue::Object(left), NodeValue::Object(right)) => Rc::ptr_eq(left, right),
            (NodeValue::Function(left), NodeValue::Function(right)) => left == right,
            (NodeValue::Array(_left), NodeValue::Array(_right)) => todo!("array comparison (yes or no?)"),

            _ => false,
        }
    }
}

impl PartialOrd for NodeValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (NodeValue::Integer(left), NodeValue::Integer(right)) => left.partial_cmp(right),
            (NodeValue::Float(left), NodeValue::Float(right)) => left.partial_cmp(right),
            (NodeValue::String(left), NodeValue::String(right)) => left.partial_cmp(right),
            (NodeValue::Symbol(left), NodeValue::Symbol(right)) => left.partial_cmp(right),
            (NodeValue::Unhandled, NodeValue::Unhandled) => Some(std::cmp::Ordering::Equal),

            (NodeValue::Object(left), NodeValue::Object(right)) => {
                let left = Rc::as_ptr(left) as *const ();
                let right = Rc::as_ptr(right) as *const ();
                left.partial_cmp(&right)
            },
            (NodeValue::Function(left), NodeValue::Function(right)) => left.partial_cmp(right),
            (NodeValue::Array(_left), NodeValue::Array(_right)) => todo!("array comparison (yes or no?)"),

            _ => None,
        }
    }
}

impl RawNodeValue {
    /// Generic value to be returned when a script call has been handled,
    /// but no specific value is returned from the method handling the call.
    pub const HANDLED: RawNodeValue = Self::Integer(0);

    /// Boolean TRUE value.
    pub const TRUE: RawNodeValue = Self::Integer(1);
    /// Boolean FALSE value.
    pub const FALSE: RawNodeValue = Self::Integer(0);

    common_getters!();

    pub fn variable(&self) -> crate::Result<&NodeVariable> {
        evaluate_node!(self; Self::Variable(value) => Ok(value))
    }

    pub fn command(&self) -> crate::Result<&CommandBox> {
        evaluate_node!(self; Self::Command(value) => Ok(value))
    }

    pub fn property(&self) -> crate::Result<&PropertyBox> {
        evaluate_node!(self; Self::Property(value) => Ok(value))
    }

    pub fn evaluate(&self, context: &mut Context) -> crate::Result<NodeValue> {
        let evaluated = match &self {
            Self::Integer(value) => NodeValue::from(value),
            Self::Float(value) => NodeValue::from(value),
            Self::String(value) => NodeValue::from(value),

            Self::Symbol(value) => NodeValue::from(value),
            Self::Variable(name) => context.get_variable(&name.symbol),
            Self::Unhandled => NodeValue::Unhandled,

            Self::Object(value) => NodeValue::from(value),
            Self::Function(value) => NodeValue::from(value),

            Self::Array(value) => NodeValue::from(value),
            Self::Command(value) => context.execute(value)?,
            Self::Property(_property) => todo!("property node evaluation"),
        };
        Ok(evaluated)
    }
}

impl From<NodeValue> for RawNodeValue {
    fn from(value: NodeValue) -> Self {
        match value {
            NodeValue::Integer(value) => RawNodeValue::Integer(value),
            NodeValue::Float(value) => RawNodeValue::Float(value),
            NodeValue::String(value) => RawNodeValue::String(value),
            NodeValue::Symbol(value) => RawNodeValue::Symbol(value),
            NodeValue::Unhandled => RawNodeValue::Unhandled,

            NodeValue::Object(value) => RawNodeValue::Object(value),
            NodeValue::Function(value) => RawNodeValue::Function(value),
            NodeValue::Array(value) => RawNodeValue::Array(value),
        }
    }
}

impl PartialEq for RawNodeValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RawNodeValue::Integer(left), RawNodeValue::Integer(right)) => left == right,
            (RawNodeValue::Float(left), RawNodeValue::Float(right)) => left == right,
            (RawNodeValue::String(left), RawNodeValue::String(right)) => left == right,

            (RawNodeValue::Symbol(left), RawNodeValue::Symbol(right)) => left == right,
            (RawNodeValue::Variable(left), RawNodeValue::Variable(right)) => left == right,
            (RawNodeValue::Unhandled, RawNodeValue::Unhandled) => true,

            (RawNodeValue::Object(left), RawNodeValue::Object(right)) => Rc::ptr_eq(left, right),
            (RawNodeValue::Function(left), RawNodeValue::Function(right)) => left == right,

            (RawNodeValue::Array(_left), RawNodeValue::Array(_right)) => todo!("array comparison (yes or no?)"),
            (RawNodeValue::Command(_left), RawNodeValue::Command(_right)) => todo!("array comparison (yes or no?)"),
            (RawNodeValue::Property(_left), RawNodeValue::Property(_right)) => todo!("array comparison (yes or no?)"),

            _ => false,
        }
    }
}

impl PartialOrd for RawNodeValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (RawNodeValue::Integer(left), RawNodeValue::Integer(right)) => left.partial_cmp(right),
            (RawNodeValue::Float(left), RawNodeValue::Float(right)) => left.partial_cmp(right),
            (RawNodeValue::String(left), RawNodeValue::String(right)) => left.partial_cmp(right),

            (RawNodeValue::Symbol(left), RawNodeValue::Symbol(right)) => left.partial_cmp(right),
            (RawNodeValue::Variable(left), RawNodeValue::Variable(right)) => left.partial_cmp(right),
            (RawNodeValue::Unhandled, RawNodeValue::Unhandled) => Some(std::cmp::Ordering::Equal),

            (RawNodeValue::Object(left), RawNodeValue::Object(right)) => {
                let left = Rc::as_ptr(left) as *const ();
                let right = Rc::as_ptr(right) as *const ();
                left.partial_cmp(&right)
            },
            (RawNodeValue::Function(left), RawNodeValue::Function(right)) => left.partial_cmp(right),

            (RawNodeValue::Array(_left), RawNodeValue::Array(_right)) => todo!("array comparison (yes or no?)"),
            (RawNodeValue::Command(_left), RawNodeValue::Command(_right)) => todo!("array comparison (yes or no?)"),
            (RawNodeValue::Property(_left), RawNodeValue::Property(_right)) => todo!("array comparison (yes or no?)"),

            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    value: RawNodeValue,
}

impl Node {
    /// Generic value to be returned when a script call has been handled,
    /// but no specific value is returned from the method handling the call.
    pub const HANDLED: Node = Self { value: RawNodeValue::HANDLED };

    /// Boolean TRUE value.
    pub const TRUE: Node = Self { value: RawNodeValue::TRUE };
    /// Boolean FALSE value.
    pub const FALSE: Node = Self { value: RawNodeValue::FALSE };

    pub fn evaluate(&self, context: &mut Context) -> crate::Result<NodeValue> {
        self.value.evaluate(context)
    }

    #[inline]
    pub const fn unevaluated(&self) -> &RawNodeValue {
        &self.value
    }

    pub fn integer(&self, context: &mut Context) -> crate::Result<NodeInteger> {
        self.evaluate(context)?.integer()
    }

    pub fn integer_strict(&self, context: &mut Context) -> crate::Result<NodeInteger> {
        self.evaluate(context)?.integer_strict()
    }

    pub fn boolean(&self, context: &mut Context) -> crate::Result<bool> {
        self.evaluate(context)?.boolean()
    }

    pub fn boolean_strict(&self, context: &mut Context) -> crate::Result<bool> {
        self.evaluate(context)?.boolean_strict()
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

    pub fn variable(&self) -> crate::Result<NodeVariable> {
        self.unevaluated().variable().cloned()
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

    pub fn command(&self) -> crate::Result<CommandBox> {
        self.unevaluated().command().cloned()
    }

    pub fn property(&self) -> crate::Result<PropertyBox> {
        self.unevaluated().property().cloned()
    }
}

macro_rules! impl_from_raw {
    ($variant:ident, $from_type:ty) => {
        impl_from_raw!($variant, $from_type, value => value);
    };
    ($variant:ident, $from_type:ty, $value:ident => $expr:expr) => {
        impl From<$from_type> for RawNodeValue {
            fn from($value: $from_type) -> Self {
                Self::$variant($expr)
            }
        }

        impl From<$from_type> for Node {
            fn from($value: $from_type) -> Self {
                Self { value: RawNodeValue::from($value) }
            }
        }
    };
}

macro_rules! impl_from {
    ($variant:ident, $from_type:ty) => {
        impl_from!($variant, $from_type, value => value);
    };
    ($variant:ident, $from_type:ty, $value:ident => $expr:expr) => {
        impl From<$from_type> for NodeValue {
            fn from($value: $from_type) -> Self {
                Self::$variant($expr)
            }
        }

        impl_from_raw!($variant, $from_type, $value => $expr);
    };
}

impl From<NodeValue> for Node {
    fn from(value: NodeValue) -> Self {
        Self { value: RawNodeValue::from(value) }
    }
}

impl From<RawNodeValue> for Node {
    fn from(value: RawNodeValue) -> Self {
        Self { value }
    }
}

impl_from!(Integer, NodeInteger);
impl_from!(Integer, &NodeInteger, value => *value);
impl_from!(Integer, i32, value => value as NodeInteger);
impl_from!(Integer, &i32, value => *value as NodeInteger);
impl_from!(Integer, bool, value => value as NodeInteger);
impl_from!(Integer, &bool, value => *value as NodeInteger);
impl_from!(Float, NodeFloat);
impl_from!(Float, &NodeFloat, value => *value);
impl_from!(Float, f32, value => value as NodeFloat);
impl_from!(Float, &f32, value => *value as NodeFloat);
impl_from!(String, StringBox);
impl_from!(String, &StringBox, value => value.clone());
impl_from!(String, String, value => Rc::new(value));
impl_from!(String, &String, value => Rc::new(value.clone()));
impl_from!(String, &str, value => Rc::new(value.to_owned()));

impl_from!(Symbol, Symbol);
impl_from!(Symbol, &Symbol, value => value.clone());

impl_from!(Object, ObjectBox);
impl_from!(Object, &ObjectBox, value => value.clone());
impl_from!(Function, HandleFn);
impl_from!(Function, &HandleFn, value => *value);

impl_from!(Array, ArrayBox);
impl_from!(Array, &ArrayBox, value => value.clone());
impl_from!(Array, NodeArray, value => Rc::new(value));

impl_from_raw!(Variable, NodeVariable);
impl_from_raw!(Variable, &NodeVariable, value => value.clone());
impl_from_raw!(Command, CommandBox);
impl_from_raw!(Command, NodeCommand, value => Rc::new(value));
impl_from_raw!(Command, &CommandBox, value => value.clone());
impl_from_raw!(Property, PropertyBox);
impl_from_raw!(Property, NodeProperty, value => Rc::new(value));
impl_from_raw!(Property, &PropertyBox, value => value.clone());

/// A contiguous collection of [`Node`]s.
#[derive(Debug, Clone, Default)]
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

    pub fn slice<I: SliceIndex<[Node], Output = [Node]>>(&self, index: I) -> crate::Result<&NodeSlice> {
        NodeSlice::new(&self.nodes).slice(index)
    }
}

impl FromIterator<Node> for NodeArray {
    fn from_iter<T: IntoIterator<Item = Node>>(iter: T) -> Self {
        Self { nodes: Vec::from_iter(iter) }
    }
}

impl FromIterator<NodeValue> for NodeArray {
    fn from_iter<T: IntoIterator<Item = NodeValue>>(iter: T) -> Self {
        Self {
            nodes: Vec::from_iter(iter.into_iter().map(|v| Node::from(v))),
        }
    }
}
impl FromIterator<RawNodeValue> for NodeArray {
    fn from_iter<T: IntoIterator<Item = RawNodeValue>>(iter: T) -> Self {
        Self {
            nodes: Vec::from_iter(iter.into_iter().map(|v| Node::from(v))),
        }
    }
}

impl Deref for NodeArray {
    type Target = Vec<Node>;

    fn deref(&self) -> &Self::Target {
        &self.nodes
    }
}

impl DerefMut for NodeArray {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.nodes
    }
}

impl Borrow<NodeSlice> for NodeArray {
    fn borrow(&self) -> &NodeSlice {
        NodeSlice::new(&self.nodes)
    }
}

/// A [[`Node`]] slice with the same additional methods as [`NodeArray`].
#[repr(transparent)]
#[derive(Debug)]
pub struct NodeSlice {
    nodes: [Node],
}

impl NodeSlice {
    pub fn new(nodes: &[Node]) -> &NodeSlice {
        // SAFETY: NodeSlice transparently contains a [Node], so its layout is identical
        unsafe { &*(nodes as *const [Node] as *const NodeSlice) }
    }

    pub fn slice<I: SliceIndex<[Node], Output = [Node]>>(&self, index: I) -> crate::Result<&NodeSlice> {
        match self.nodes.get(index) {
            Some(value) => Ok(Self::new(value)),
            None => Err(Error::OutOfRange(0..self.nodes.len())),
        }
    }
}

impl Deref for NodeSlice {
    type Target = [Node];

    fn deref(&self) -> &Self::Target {
        &self.nodes
    }
}

impl<'slice> IntoIterator for &'slice NodeSlice {
    type Item = <&'slice [Node] as IntoIterator>::Item;
    type IntoIter = <&'slice [Node] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        #[allow(clippy::into_iter_on_ref)]
        self.nodes.into_iter()
    }
}

macro_rules! array_impl {
    () => {
        pub fn get<I: SliceIndex<[Node]>>(&self, index: I) -> crate::Result<&I::Output> {
            match self.nodes.get(index) {
                Some(value) => Ok(value),
                None => Err(Error::OutOfRange(0..self.nodes.len())),
            }
        }

        pub fn get_opt<I: SliceIndex<[Node]>>(&self, index: I) -> Option<&I::Output> {
            self.nodes.get(index)
        }

        pub fn evaluate(&self, context: &mut Context, index: usize) -> crate::Result<NodeValue> {
            self.get(index)?.evaluate(context)
        }

        pub fn unevaluated(&self, index: usize) -> crate::Result<&RawNodeValue> {
            Ok(self.get(index)?.unevaluated())
        }

        pub fn integer(&self, context: &mut Context, index: usize) -> crate::Result<NodeInteger> {
            self.get(index)?.integer(context)
        }

        pub fn integer_strict(&self, context: &mut Context, index: usize) -> crate::Result<NodeInteger> {
            self.get(index)?.integer_strict(context)
        }

        pub fn boolean(&self, context: &mut Context, index: usize) -> crate::Result<bool> {
            self.get(index)?.boolean(context)
        }

        pub fn boolean_strict(&self, context: &mut Context, index: usize) -> crate::Result<bool> {
            self.get(index)?.boolean_strict(context)
        }

        pub fn float(&self, context: &mut Context, index: usize) -> crate::Result<NodeFloat> {
            self.get(index)?.float(context)
        }

        pub fn float_strict(&self, context: &mut Context, index: usize) -> crate::Result<NodeFloat> {
            self.get(index)?.float_strict(context)
        }

        pub fn string(&self, context: &mut Context, index: usize) -> crate::Result<StringBox> {
            self.get(index)?.string(context)
        }

        pub fn symbol(&self, context: &mut Context, index: usize) -> crate::Result<Symbol> {
            self.get(index)?.symbol(context)
        }

        pub fn variable(&self, index: usize) -> crate::Result<NodeVariable> {
            self.get(index)?.variable()
        }

        pub fn object(&self, context: &mut Context, index: usize) -> crate::Result<ObjectBox> {
            self.get(index)?.object(context)
        }

        pub fn function(&self, context: &mut Context, index: usize) -> crate::Result<HandleFn> {
            self.get(index)?.function(context)
        }

        pub fn array(&self, context: &mut Context, index: usize) -> crate::Result<ArrayBox> {
            self.get(index)?.array(context)
        }

        pub fn command(&self, index: usize) -> crate::Result<CommandBox> {
            self.get(index)?.command()
        }

        pub fn property(&self, index: usize) -> crate::Result<PropertyBox> {
            self.get(index)?.property()
        }

        pub fn find_array(&self, tag: &Symbol) -> crate::Result<&NodeArray> {
            for node in self.iter() {
                let RawNodeValue::Array(array) = node.unevaluated() else {
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
    };
}

impl NodeArray {
    array_impl!();
}

impl NodeSlice {
    array_impl!();
}

/// An executable/evaluatable command.
#[derive(Debug, Clone, Default)]
pub struct NodeCommand {
    nodes: NodeArray,
}

impl NodeCommand {
    pub fn execute(&self, context: &mut Context) -> crate::Result<NodeValue> {
        context.execute(self)
    }
}

/// A property on an object which can be manipulated.
#[derive(Debug, Clone, Default)]
pub struct NodeProperty {
    nodes: NodeArray,
}

macro_rules! array_wrapper_impl {
    ($name:ident) => {
        impl $name {
            pub const fn new() -> Self {
                Self { nodes: NodeArray::new() }
            }
        }

        impl From<NodeArray> for $name {
            fn from(value: NodeArray) -> Self {
                Self { nodes: value }
            }
        }

        impl FromIterator<Node> for $name {
            fn from_iter<T: IntoIterator<Item = Node>>(iter: T) -> Self {
                Self { nodes: NodeArray::from_iter(iter) }
            }
        }

        impl FromIterator<NodeValue> for $name {
            fn from_iter<T: IntoIterator<Item = NodeValue>>(iter: T) -> Self {
                Self { nodes: NodeArray::from_iter(iter) }
            }
        }
        impl FromIterator<RawNodeValue> for $name {
            fn from_iter<T: IntoIterator<Item = RawNodeValue>>(iter: T) -> Self {
                Self { nodes: NodeArray::from_iter(iter) }
            }
        }

        impl Deref for $name {
            type Target = NodeArray;

            fn deref(&self) -> &Self::Target {
                &self.nodes
            }
        }

        impl DerefMut for $name {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.nodes
            }
        }

        impl Borrow<NodeArray> for $name {
            fn borrow(&self) -> &NodeArray {
                &self.nodes
            }
        }

        impl Borrow<NodeSlice> for $name {
            fn borrow(&self) -> &NodeSlice {
                self.nodes.borrow()
            }
        }
    };
}

array_wrapper_impl!(NodeCommand);
array_wrapper_impl!(NodeProperty);
