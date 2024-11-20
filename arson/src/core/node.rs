// SPDX-License-Identifier: LGPL-3.0-or-later

use std::{
    borrow::{Borrow, BorrowMut},
    cmp::Ordering,
    ops::{Deref, DerefMut},
    rc::Rc,
    slice::SliceIndex,
};

use super::{Context, Error, Object, Symbol, Variable};

/// A function which is callable by a [`NodeCommand`].
pub type HandleFn = fn(context: &mut Context, args: &NodeSlice) -> HandleResult;
/// The result of a [`HandleFn`].
pub type HandleResult = crate::Result<NodeValue>;

pub type NodeInteger = i64;
pub type NodeFloat = f64;

macro_rules! define_node_types {
    (
        make_types!(),
        $(#[$name_attr:meta])*
        pub enum $name:ident {
            $(
                $(#[$type_attr:meta])*
                $type:ident$(($value:ty) $({
                    eq: |$eq_left:ident, $eq_right:ident| $eq_expr:expr,
                    cmp: |$cmp_left:ident, $cmp_right:ident| $cmp_expr:expr,
                })?)?
                $(,)?
            )+
        }
    ) => {
        /// The type of value contained within a [`Node`], [`NodeValue`], or [`RawNodeValue`].
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
        pub enum NodeType {
            $($(#[$type_attr])* $type,)+
        }

        $(
            $(
                impl From<$value> for Node {
                    fn from(value: $value) -> Self {
                        Self { value: RawNodeValue::from(value) }
                    }
                }

                impl From<&$value> for Node {
                    fn from(value: &$value) -> Self {
                        Self::from(value.clone())
                    }
                }
            )?
        )+

        define_node_types! {
            $(#[$name_attr])*
            pub enum $name {
                $(
                    $(#[$type_attr])*
                    $type$(($value) $({
                        eq: |$eq_left, $eq_right| $eq_expr,
                        cmp: |$cmp_left, $cmp_right| $cmp_expr,
                    })?)?
                )+
            }
        }
    };
    (
        $(#[$name_attr:meta])*
        pub enum $name:ident {
            $(
                $(#[$type_attr:meta])*
                $type:ident$(($value:ty) $({
                    eq: |$eq_left:ident, $eq_right:ident| $eq_expr:expr,
                    cmp: |$cmp_left:ident, $cmp_right:ident| $cmp_expr:expr,
                })?)?
                $(,)?
            )+
        }
    ) => {
        $(#[$name_attr])*
        pub enum $name {
            $($(#[$type_attr])* $type$(($value))?,)+
        }

        impl $name {
            pub fn get_type(&self) -> NodeType {
                match self {
                    $(Self::$type$((meta_morph!($value => _)))? => NodeType::$type,)+
                }
            }
        }

        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                match (self, other) {
                    $(
                        (
                            Self::$type$((meta_morph!($value => meta_select!($($eq_left)?, left))))?,
                            Self::$type$((meta_morph!($value => meta_select!($($eq_right)?, right))))?
                        )
                        => meta_select!(
                            $(meta_morph!($value => meta_select!($($eq_expr)?, left == right)))?,
                            true
                        ),
                    )+

                    _ => false,
                }
            }
        }

        impl PartialOrd for $name {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                match (self, other) {
                    $(
                        (
                            Self::$type$((meta_morph!($value => meta_select!($($cmp_left)?, left))))?,
                            Self::$type$((meta_morph!($value => meta_select!($($cmp_right)?, right))))?
                        )
                        => meta_select!(
                            $(meta_morph!($value => meta_select!($($cmp_expr)?, left.partial_cmp(right))))?,
                            Some(Ordering::Equal)
                        ),
                    )+

                    _ => None,
                }
            }
        }

        $(
            $(
                impl From<$value> for $name {
                    fn from(value: $value) -> Self {
                        Self::$type(value)
                    }
                }

                impl From<&$value> for $name {
                    fn from(value: &$value) -> Self {
                        Self::from(value.clone())
                    }
                }

                impl PartialEq<$value> for $name {
                    fn eq(&self, meta_select!($($eq_right)?, other): &$value) -> bool {
                        match self {
                            Self::$type(meta_select!($($eq_left)?, value)) => meta_select!($($eq_expr)?, value == other),
                            _ => false,
                        }
                    }
                }

                impl PartialEq<$name> for $value {
                    fn eq(&self, other: &$name) -> bool {
                        other.eq(self)
                    }
                }

                impl PartialOrd<$value> for $name {
                    fn partial_cmp(&self, meta_select!($($cmp_right)?, other): &$value) -> Option<Ordering> {
                        match self {
                            Self::$type(meta_select!($($cmp_left)?, value)) => meta_select!($($cmp_expr)?, value.partial_cmp(other)),
                            _ => None,
                        }
                    }
                }

                impl PartialOrd<$name> for $value {
                    fn partial_cmp(&self, meta_select!($($cmp_right)?, other): &$name) -> Option<Ordering> {
                        $(let $cmp_left = self;)?
                        match meta_select!($($cmp_right)?, other) {
                            $name::$type(meta_select!($($cmp_right)?, other)) => meta_select!($($cmp_expr)?, self.partial_cmp(other)),
                            _ => None,
                        }
                    }
                }
            )?
        )+
    }
}

// Type aliases to avoid syntax errors in the macro invocations below.
// Strings and arrays are boxed and reference-counted to make cloning cheap.
type StringBox = Rc<String>;
type ObjectBox = Rc<dyn Object>;
type ArrayBox = Rc<NodeArray>;
type CommandBox = Rc<NodeCommand>;
type PropertyBox = Rc<NodeProperty>;

fn rc_cmp<T: ?Sized>(left: &Rc<T>, right: &Rc<T>) -> Option<Ordering> {
    let left = Rc::as_ptr(left) as *const ();
    let right = Rc::as_ptr(right) as *const ();
    left.partial_cmp(&right)
}

define_node_types! {
    make_types!(),

    /// A raw, unevaluated value stored within a [`Node`].
    #[derive(Debug, Clone)]
    pub enum RawNodeValue {
        Integer(NodeInteger),
        Float(NodeFloat),
        String(StringBox),

        Symbol(Symbol),
        Variable(Variable) {
            eq: |left, right| left.symbol() == right.symbol(),
            cmp: |left, right| left.symbol().partial_cmp(right.symbol()),
        },
        Unhandled,

        Object(ObjectBox) {
            eq: |left, right| Rc::ptr_eq(left, right),
            cmp: |left, right| rc_cmp(left, right),
        },
        Function(HandleFn),

        Array(ArrayBox) {
            eq: |left, right| left == right,
            cmp: |left, right| left.partial_cmp(right),
        },
        Command(CommandBox) {
            eq: |left, right| left == right,
            cmp: |left, right| left.partial_cmp(right),
        },
        Property(PropertyBox) {
            eq: |left, right| left == right,
            cmp: |left, right| left.partial_cmp(right),
        },
    }
}

define_node_types! {
    /// A node value which has been evaluated.
    #[derive(Debug, Clone)]
    pub enum NodeValue {
        Integer(NodeInteger),
        Float(NodeFloat),
        String(StringBox),
        Symbol(Symbol),
        Unhandled,

        Object(ObjectBox) {
            eq: |left, right| Rc::ptr_eq(left, right),
            cmp: |left, right| rc_cmp(left, right),
        },
        Function(HandleFn),
        Array(ArrayBox) {
            eq: |left, right| left == right,
            cmp: |left, right| left.partial_cmp(right),
        },
    }
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

macro_rules! impl_from_raw {
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
    ($variant:ident, $from_type:ty, $value:ident => $expr:expr) => {
        impl From<$from_type> for NodeValue {
            fn from($value: $from_type) -> Self {
                Self::$variant($expr)
            }
        }

        impl_from_raw!($variant, $from_type, $value => $expr);
    };
}

impl_from!(Integer, i32, value => value as NodeInteger);
impl_from!(Integer, &i32, value => *value as NodeInteger);
impl_from!(Integer, bool, value => value as NodeInteger);
impl_from!(Integer, &bool, value => *value as NodeInteger);
impl_from!(Float, f32, value => value as NodeFloat);
impl_from!(Float, &f32, value => *value as NodeFloat);
impl_from!(String, String, value => Rc::new(value));
impl_from!(String, &String, value => Rc::new(value.clone()));
impl_from!(String, &str, value => Rc::new(value.to_owned()));

impl_from!(Array, NodeArray, value => Rc::new(value));

impl_from_raw!(Command, NodeCommand, value => Rc::new(value));
impl_from_raw!(Property, NodeProperty, value => Rc::new(value));

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
            Some($some_ident) => $crate::evaluate_node!($some_expr; $($type::$variant($value) => $expr,)+),
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

impl RawNodeValue {
    /// Generic value to be returned when a script call has been handled,
    /// but no specific value is returned from the method handling the call.
    pub const HANDLED: RawNodeValue = Self::Integer(0);

    /// Boolean TRUE value.
    pub const TRUE: RawNodeValue = Self::Integer(1);
    /// Boolean FALSE value.
    pub const FALSE: RawNodeValue = Self::Integer(0);

    common_getters!();

    pub fn variable(&self) -> crate::Result<&Variable> {
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
            Self::Variable(variable) => variable.get(context),
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

#[derive(Debug, Clone)]
pub struct Node {
    value: RawNodeValue,
}

impl Node {
    /// Value returned when a script call has not been handled by a receiver.
    pub const UNHANDLED: Node = Self { value: RawNodeValue::Unhandled };
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

    pub fn variable(&self) -> crate::Result<Variable> {
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

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

#[macro_export]
macro_rules! arson_array {
    () => (
        $crate::NodeArray::new()
    );
    ($elem:expr; $n:expr) => (
        $crate::NodeArray::from(vec![$elem.into(); $n])
    );
    ($($x:expr),+ $(,)?) => (
        $crate::NodeArray::from(vec![$($x.into()),+])
    );
}

/// A contiguous collection of [`Node`]s.
#[derive(Debug, Clone, Default, PartialEq, PartialOrd)]
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

    pub fn capacity(&self) -> usize {
        self.nodes.capacity()
    }

    pub fn push<N: Into<Node>>(&mut self, value: N) {
        self.nodes.push(value.into())
    }

    pub fn pop(&mut self) -> Option<Node> {
        self.nodes.pop()
    }

    pub fn insert<N: Into<Node>>(&mut self, index: usize, value: N) {
        self.nodes.insert(index, value.into())
    }

    pub fn remove(&mut self, index: usize) -> Node {
        self.nodes.remove(index)
    }

    pub fn append(&mut self, other: &mut Self) {
        self.nodes.append(&mut other.nodes)
    }

    pub fn extend_from_slice(&mut self, other: &[Node]) {
        self.nodes.extend_from_slice(other)
    }

    pub fn clear(&mut self) {
        self.nodes.clear()
    }

    pub fn reserve(&mut self, additional: usize) {
        self.nodes.reserve(additional)
    }

    pub fn reserve_exact(&mut self, additional: usize) {
        self.nodes.reserve_exact(additional)
    }

    pub fn resize(&mut self, index: usize, value: Node) {
        self.nodes.resize(index, value)
    }

    pub fn truncate(&mut self, len: usize) {
        self.nodes.truncate(len)
    }

    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.nodes.shrink_to(min_capacity)
    }

    pub fn shrink_to_fit(&mut self) {
        self.nodes.shrink_to_fit()
    }
}

impl From<Vec<Node>> for NodeArray {
    fn from(value: Vec<Node>) -> Self {
        Self { nodes: value }
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
            nodes: Vec::from_iter(iter.into_iter().map(Node::from)),
        }
    }
}

impl FromIterator<RawNodeValue> for NodeArray {
    fn from_iter<T: IntoIterator<Item = RawNodeValue>>(iter: T) -> Self {
        Self {
            nodes: Vec::from_iter(iter.into_iter().map(Node::from)),
        }
    }
}

impl Deref for NodeArray {
    type Target = NodeSlice;

    fn deref(&self) -> &Self::Target {
        NodeSlice::new(&self.nodes)
    }
}

impl DerefMut for NodeArray {
    fn deref_mut(&mut self) -> &mut Self::Target {
        NodeSlice::from_mut(&mut self.nodes)
    }
}

impl Borrow<Vec<Node>> for NodeArray {
    fn borrow(&self) -> &Vec<Node> {
        &self.nodes
    }
}

impl BorrowMut<Vec<Node>> for NodeArray {
    fn borrow_mut(&mut self) -> &mut Vec<Node> {
        &mut self.nodes
    }
}

/// A [[`Node`]] slice with the same additional methods as [`NodeArray`].
#[repr(transparent)]
#[derive(Debug, PartialEq, PartialOrd)]
pub struct NodeSlice {
    nodes: [Node],
}

impl NodeSlice {
    pub fn new(nodes: &[Node]) -> &NodeSlice {
        // SAFETY: NodeSlice transparently contains a [Node], so its layout is identical
        unsafe { &*(nodes as *const [Node] as *const NodeSlice) }
    }

    pub fn from_mut(nodes: &mut [Node]) -> &mut NodeSlice {
        // SAFETY: NodeSlice transparently contains a [Node], so its layout is identical
        unsafe { &mut *(nodes as *mut [Node] as *mut NodeSlice) }
    }

    pub fn slice<I: SliceIndex<[Node], Output = [Node]>>(&self, index: I) -> crate::Result<&NodeSlice> {
        match self.nodes.get(index) {
            Some(value) => Ok(Self::new(value)),
            None => Err(Error::OutOfRange(0..self.nodes.len())),
        }
    }

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

    pub fn variable(&self, index: usize) -> crate::Result<Variable> {
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

    pub fn find_array<T: PartialEq<RawNodeValue>>(&self, tag: &T) -> crate::Result<Rc<NodeArray>> {
        self.find_array_opt(tag).ok_or(Error::EntryNotFound)
    }

    pub fn find_array_opt<T: PartialEq<RawNodeValue>>(&self, tag: &T) -> Option<Rc<NodeArray>> {
        for node in self.iter() {
            let RawNodeValue::Array(array) = node.unevaluated() else {
                continue;
            };
            let Ok(node) = array.unevaluated(0) else {
                continue;
            };

            if *tag == *node {
                return Some(array.clone());
            }
        }

        None
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

macro_rules! define_array_wrapper {
    (
        $(
            $(#[$attr:meta])*
            struct $name:ident;
        )+
    ) => {
        $(
            $(#[$attr])*
            #[derive(Debug, Clone, Default, PartialEq, PartialOrd)]
            pub struct $name {
                nodes: NodeArray,
            }

            impl $name {
                pub const fn new() -> Self {
                    Self { nodes: NodeArray::new() }
                }
            }

            impl<T> From<T> for $name
                where NodeArray: From<T>
            {
                fn from(value: T) -> Self {
                    Self { nodes: NodeArray::from(value) }
                }
            }

            impl<T> FromIterator<T> for $name
                where NodeArray: FromIterator<T>
            {
                fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
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
        )+
    };
}

define_array_wrapper! {
    /// An executable/evaluatable command.
    struct NodeCommand;

    /// A property on an object which can be manipulated.
    struct NodeProperty;
}

impl NodeCommand {
    pub fn execute(&self, context: &mut Context) -> crate::Result<NodeValue> {
        context.execute(self)
    }
}
