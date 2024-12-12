// SPDX-License-Identifier: LGPL-3.0-or-later

use std::cell::{Cell, RefCell};
use std::fmt::{self, Write};
use std::ops::Range;
use std::rc::Rc;
use std::slice::SliceIndex;

use crate::*;

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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ArrayKind {
    Array,
    Command,
    Property,
}

impl ArrayKind {
    pub fn delimiter(&self, open: bool) -> char {
        let delimiters = self.delimiters();
        match open {
            true => delimiters.0,
            false => delimiters.1,
        }
    }

    pub fn delimiters(&self) -> (char, char) {
        match self {
            ArrayKind::Array => ('(', ')'),
            ArrayKind::Command => ('{', '}'),
            ArrayKind::Property => ('[', ']'),
        }
    }
}

impl std::fmt::Display for ArrayKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArrayKind::Array => write!(f, "array"),
            ArrayKind::Command => write!(f, "command"),
            ArrayKind::Property => write!(f, "property"),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ArrayError {
    #[error("Bad array length {actual}, expected {expected}")]
    LengthMismatch { expected: usize, actual: usize },

    #[error("Array index outside of range {0:?}")]
    OutOfRange(Range<usize>),

    #[error("Requested data was not found")]
    NotFound,

    #[error("Array already mutably borrowed: {0:?}")]
    BadBorrow(#[from] std::cell::BorrowError),

    #[error("Array already immutably borrowed: {0:?}")]
    BadMutBorrow(#[from] std::cell::BorrowMutError),
}

#[derive(PartialEq, PartialOrd, Clone)]
pub struct ArrayRef {
    inner: Rc<RefCell<NodeArray>>,
}

impl ArrayRef {
    pub fn new(array: NodeArray) -> Self {
        Self { inner: Rc::new(RefCell::new(array)) }
    }

    pub fn borrow(&self) -> crate::Result<std::cell::Ref<'_, NodeArray>> {
        self.inner.try_borrow().map_err(|e| ArrayError::BadBorrow(e).into())
    }

    pub fn borrow_mut(&self) -> crate::Result<std::cell::RefMut<'_, NodeArray>> {
        self.inner.try_borrow_mut().map_err(|e| ArrayError::BadMutBorrow(e).into())
    }
}

impl std::fmt::Debug for ArrayRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner.try_borrow() {
            Ok(inner) => inner.fmt(f),
            Err(_) => f.debug_tuple("NodeArray").field(&"<borrowed>").finish(),
        }
    }
}

/// A contiguous slice of [`Node`]s.
#[repr(transparent)]
#[derive(PartialEq, PartialOrd)]
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
            None => Err(ArrayError::OutOfRange(0..self.nodes.len()).into()),
        }
    }

    pub fn slice_mut<I: SliceIndex<[Node], Output = [Node]>>(
        &mut self,
        index: I,
    ) -> crate::Result<&mut NodeSlice> {
        let len = self.nodes.len(); // borrow checker workaround
        match self.nodes.get_mut(index) {
            Some(value) => Ok(Self::from_mut(value)),
            None => Err(ArrayError::OutOfRange(0..len).into()),
        }
    }

    pub fn get<I: SliceIndex<[Node]>>(&self, index: I) -> crate::Result<&I::Output> {
        match self.nodes.get(index) {
            Some(value) => Ok(value),
            None => Err(ArrayError::OutOfRange(0..self.nodes.len()).into()),
        }
    }

    pub fn get_opt<I: SliceIndex<[Node]>>(&self, index: I) -> Option<&I::Output> {
        self.nodes.get(index)
    }

    pub fn get_mut<I: SliceIndex<[Node]>>(&mut self, index: I) -> crate::Result<&mut I::Output> {
        let len = self.nodes.len(); // borrow checker workaround
        match self.nodes.get_mut(index) {
            Some(value) => Ok(value),
            None => Err(ArrayError::OutOfRange(0..len).into()),
        }
    }

    pub fn get_mut_opt<I: SliceIndex<[Node]>>(&mut self, index: I) -> Option<&mut I::Output> {
        self.nodes.get_mut(index)
    }
}

// Data retrieval by index
impl NodeSlice {
    pub fn integer<S>(&self, context: &mut Context<S>, index: usize) -> crate::Result<Integer> {
        self.get(index)?.integer(context)
    }

    pub fn float<S>(&self, context: &mut Context<S>, index: usize) -> crate::Result<FloatValue> {
        self.get(index)?.float(context)
    }

    pub fn number<S>(&self, context: &mut Context<S>, index: usize) -> crate::Result<Number> {
        self.get(index)?.number(context)
    }

    pub fn boolean<S>(&self, context: &mut Context<S>, index: usize) -> crate::Result<bool> {
        self.get(index)?.boolean(context)
    }

    pub fn string<S>(&self, context: &mut Context<S>, index: usize) -> crate::Result<Rc<String>> {
        self.get(index)?.string(context)
    }

    pub fn symbol<S>(&self, context: &mut Context<S>, index: usize) -> crate::Result<Symbol> {
        self.get(index)?.symbol(context)
    }

    pub fn variable(&self, index: usize) -> crate::Result<&Variable> {
        self.get(index)?.variable()
    }

    pub fn array<S>(&self, context: &mut Context<S>, index: usize) -> crate::Result<ArrayRef> {
        self.get(index)?.array(context)
    }

    pub fn command(&self, index: usize) -> crate::Result<&Rc<NodeCommand>> {
        self.get(index)?.command()
    }

    pub fn property(&self, index: usize) -> crate::Result<&Rc<NodeProperty>> {
        self.get(index)?.property()
    }

    pub fn unevaluated(&self, index: usize) -> crate::Result<&NodeValue> {
        Ok(self.get(index)?.unevaluated())
    }

    pub fn evaluate<S>(&self, context: &mut Context<S>, index: usize) -> crate::Result<NodeValue> {
        self.get(index)?.evaluate(context)
    }

    pub fn set<S, T: Into<Node>>(&self, context: &mut Context<S>, index: usize, value: T) -> crate::Result {
        self.get(index)?.set(context, value)
    }
}

/// A predicate used for searching in the [`NodeSlice::find_array`] and
/// [`NodeSlice::find_data`] family of functions.
///
/// This trait is primarily just a helper and is not meant to be implemented directly,
/// though nothing will go wrong if you do (barring any logic errors in said implementation).
pub trait FindDataPredicate {
    fn matches(this: &Self, value: &NodeValue) -> bool;
}

// No blanket implementation is provided to allow all values which are comparable to NodeValue,
// because very few of the variants make sense to be used as data keys as-is.
// Instead, an implementation is provided for Fn(&NodeValue) -> bool which allows for custom searches,
// so if a more special need arises, it can be handled using that custom logic.

macro_rules! find_pred_impl {
    ($type:ty => |$predicate:ident, $value:ident| $block:expr) => {
        impl FindDataPredicate for $type {
            fn matches($predicate: &Self, $value: &NodeValue) -> bool {
                $block
            }
        }

        // Using a blanket implementation for this isn't possible due to the
        // impl for Fn(&NodeValue) -> bool, which inherently handles references
        impl FindDataPredicate for &$type {
            fn matches(this: &Self, value: &NodeValue) -> bool {
                FindDataPredicate::matches(*this, value)
            }
        }
    };
}

find_pred_impl!(Symbol => |predicate, value| value == predicate);
// We could allow this, but really, you should use a Symbol instead...
// find_pred_impl!(String => |predicate, value| value == predicate);
find_pred_impl!(Integer => |predicate, value| value == predicate);
find_pred_impl!(IntegerValue => |predicate, value| value == predicate);

impl<F: Fn(&NodeValue) -> bool> FindDataPredicate for F {
    fn matches(this: &Self, value: &NodeValue) -> bool {
        this(value)
    }
}

// Clashes with the Fn(&NodeValue) -> bool implementation
// impl<T: FindDataPredicate> FindDataPredicate for &T {
//     fn matches(&self, value: &NodeValue) -> bool {
//         FindDataPredicate::matches(*self, value)
//     }
// }

pub trait IntoDataPredicate {
    type Target: FindDataPredicate;

    fn into_predicate(self) -> Self::Target;
}

impl<T: FindDataPredicate> IntoDataPredicate for T {
    type Target = T;

    fn into_predicate(self) -> Self::Target {
        self
    }
}

impl<S> IntoDataPredicate for (&mut Context<S>, &str) {
    type Target = Symbol;

    fn into_predicate(self) -> Self::Target {
        self.0.add_symbol(self.1)
    }
}

impl<S> IntoDataPredicate for (&mut Context<S>, &String) {
    type Target = Symbol;

    fn into_predicate(self) -> Self::Target {
        self.0.add_symbol(self.1)
    }
}

// this is kinda stupid lol
pub trait IntoIntoDataPredicate {
    type Target: IntoDataPredicate;

    fn into_predicate<S>(self, context: &mut Context<S>) -> Self::Target;
}

impl<T: IntoDataPredicate> IntoIntoDataPredicate for T {
    type Target = T;

    fn into_predicate<S>(self, _context: &mut Context<S>) -> Self::Target {
        self
    }
}

impl IntoIntoDataPredicate for &str {
    type Target = Symbol;

    fn into_predicate<S>(self, context: &mut Context<S>) -> Self::Target {
        context.add_symbol(self)
    }
}

impl IntoIntoDataPredicate for &String {
    type Target = Symbol;

    fn into_predicate<S>(self, context: &mut Context<S>) -> Self::Target {
        context.add_symbol(self)
    }
}

// Data retrieval by predicate
impl NodeSlice {
    pub fn find_array(&self, predicate: impl IntoDataPredicate) -> crate::Result<ArrayRef> {
        let predicate = predicate.into_predicate();
        for node in self.iter() {
            let NodeValue::Array(array) = node.unevaluated() else {
                continue;
            };
            if let Ok(node) = array.borrow()?.unevaluated(0) {
                if FindDataPredicate::matches(&predicate, node) {
                    return Ok(array.clone());
                }
            };
        }

        Err(ArrayError::NotFound.into())
    }

    pub fn find_array_opt(&self, predicate: impl IntoDataPredicate) -> Option<ArrayRef> {
        let predicate = predicate.into_predicate();
        for node in self.iter() {
            let NodeValue::Array(array) = node.unevaluated() else {
                continue;
            };
            let Ok(borrow) = array.borrow() else {
                continue;
            };
            let Ok(node) = borrow.unevaluated(0) else {
                continue;
            };

            if FindDataPredicate::matches(&predicate, node) {
                return Some(array.clone());
            }
        }

        None
    }

    pub fn find_data(&self, predicate: impl IntoDataPredicate) -> crate::Result<Node> {
        let array = self.find_array(predicate)?;
        let array = array.borrow()?;
        arson_assert_len!(array, 2, "Expected only one value in array");
        array.get(1).cloned()
    }

    pub fn find_data_opt<S>(&self, predicate: impl IntoDataPredicate) -> Option<Node> {
        let array = self.find_array_opt(predicate)?;
        let array = array.borrow().ok()?;
        if array.len() != 2 {
            return None;
        }
        array.get_opt(1).cloned()
    }

    pub fn find_integer<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<Integer> {
        self.find_data(predicate.into_predicate(context))?.integer(context)
    }

    pub fn find_float<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<FloatValue> {
        self.find_data(predicate.into_predicate(context))?.float(context)
    }

    pub fn find_number<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<Number> {
        self.find_data(predicate.into_predicate(context))?.number(context)
    }

    pub fn find_boolean<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<bool> {
        self.find_data(predicate.into_predicate(context))?.boolean(context)
    }

    pub fn find_string<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<Rc<String>> {
        self.find_data(predicate.into_predicate(context))?.string(context)
    }

    pub fn find_symbol<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<Symbol> {
        self.find_data(predicate.into_predicate(context))?.symbol(context)
    }

    pub fn find_variable(&self, predicate: impl IntoDataPredicate) -> crate::Result<Variable> {
        self.find_data(predicate)?.variable().cloned()
    }

    pub fn find_command(&self, predicate: impl IntoDataPredicate) -> crate::Result<Rc<NodeCommand>> {
        self.find_data(predicate)?.command().cloned()
    }

    pub fn find_property(&self, predicate: impl IntoDataPredicate) -> crate::Result<Rc<NodeProperty>> {
        self.find_data(predicate)?.property().cloned()
    }

    pub fn display_evaluated<'a, S>(&'a self, context: &'a mut Context<S>) -> ArrayDisplay<'a, S> {
        ArrayDisplay::new(&self.nodes, ArrayKind::Array, context)
    }
}

// Number utilities
impl NodeSlice {
    pub fn number_chain<S>(
        &self,
        context: &mut Context<S>,
        f_int: impl Fn(Integer, Integer) -> crate::Result<Integer>,
        f_float: impl Fn(FloatValue, FloatValue) -> crate::Result<FloatValue>,
    ) -> ExecuteResult {
        fn integer_chain<S>(
            context: &mut Context<S>,
            args: &NodeSlice,
            left: Integer,
            f_int: impl Fn(Integer, Integer) -> crate::Result<Integer>,
            f_float: impl Fn(FloatValue, FloatValue) -> crate::Result<FloatValue>,
        ) -> ExecuteResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(left.into());
            };

            match node.number(context)? {
                Number::Integer(right) => {
                    integer_chain(context, args.slice(1..)?, f_int(left, right)?, f_int, f_float)
                },
                Number::Float(right) => {
                    float_chain(context, args.slice(1..)?, f_float(left.0 as FloatValue, right)?, f_float)
                },
            }
        }

        fn float_chain<S>(
            context: &mut Context<S>,
            args: &NodeSlice,
            left: FloatValue,
            f_float: impl Fn(FloatValue, FloatValue) -> crate::Result<FloatValue>,
        ) -> ExecuteResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(left.into());
            };

            match node.number(context)? {
                Number::Integer(right) => {
                    float_chain(context, args.slice(1..)?, f_float(left, right.0 as FloatValue)?, f_float)
                },
                Number::Float(right) => {
                    float_chain(context, args.slice(1..)?, f_float(left, right)?, f_float)
                },
            }
        }

        match self.number(context, 0)? {
            Number::Integer(value) => integer_chain(context, self.slice(1..)?, value, f_int, f_float),
            Number::Float(value) => float_chain(context, self.slice(1..)?, value, f_float),
        }
    }
}

impl std::ops::Deref for NodeSlice {
    type Target = [Node];

    fn deref(&self) -> &Self::Target {
        &self.nodes
    }
}

impl std::ops::DerefMut for NodeSlice {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.nodes
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

impl fmt::Debug for NodeSlice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.nodes.fmt(f)
    }
}

impl fmt::Display for NodeSlice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_nodes_unevaluated(&self.nodes, ArrayKind::Array, f)
    }
}

/// A contiguous, growable collection of [`Node`]s.
#[derive(Clone, Default, PartialEq, PartialOrd)]
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

    pub fn merge(&mut self, source: &NodeArray) {
        for node in source.iter() {
            let NodeValue::Array(array) = node.unevaluated() else {
                continue;
            };
            let Ok(borrow) = array.borrow() else {
                continue;
            };
            let Ok(tag) = borrow.get(0) else {
                continue;
            };

            // todo: there's probably a better way to handle finding an array by NodeValue, but for now this will do
            // unsure if this should be the hard-set behavior for NodeValue
            match self.find_array_opt(|value: &NodeValue| match (tag.unevaluated(), value) {
                (NodeValue::Symbol(left), NodeValue::Symbol(right)) => *left == *right,
                (NodeValue::Integer(left), NodeValue::Integer(right)) => *left == *right,
                _ => false,
            }) {
                Some(found) => {
                    if let Ok(mut found_borrow) = found.borrow_mut() {
                        found_borrow.merge(&borrow)
                    }
                },
                None => self.push(NodeValue::Array(array.clone())),
            }
        }
    }

    pub fn display_evaluated<'a, S>(&'a self, context: &'a mut Context<S>) -> ArrayDisplay<'a, S> {
        ArrayDisplay::new(&self.nodes, ArrayKind::Array, context)
    }
}

// Vec forwards
impl NodeArray {
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

impl std::ops::Deref for NodeArray {
    type Target = NodeSlice;

    fn deref(&self) -> &Self::Target {
        NodeSlice::new(&self.nodes)
    }
}

impl std::ops::DerefMut for NodeArray {
    fn deref_mut(&mut self) -> &mut Self::Target {
        NodeSlice::from_mut(&mut self.nodes)
    }
}

impl std::borrow::Borrow<Vec<Node>> for NodeArray {
    fn borrow(&self) -> &Vec<Node> {
        &self.nodes
    }
}

impl std::borrow::BorrowMut<Vec<Node>> for NodeArray {
    fn borrow_mut(&mut self) -> &mut Vec<Node> {
        &mut self.nodes
    }
}

impl IntoIterator for NodeArray {
    type Item = <Vec<Node> as IntoIterator>::Item;
    type IntoIter = <Vec<Node> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        #[allow(clippy::into_iter_on_ref)]
        self.nodes.into_iter()
    }
}

impl<'nodes> IntoIterator for &'nodes NodeArray {
    type Item = <&'nodes Vec<Node> as IntoIterator>::Item;
    type IntoIter = <&'nodes Vec<Node> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        #[allow(clippy::into_iter_on_ref)]
        self.nodes.iter()
    }
}

impl fmt::Debug for NodeArray {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.nodes.fmt(f)
    }
}

impl fmt::Display for NodeArray {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_nodes_unevaluated(&self.nodes, ArrayKind::Array, f)
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
            #[derive(Clone, Default, PartialEq, PartialOrd)]
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

            impl std::ops::Deref for $name {
                type Target = NodeArray;

                fn deref(&self) -> &Self::Target {
                    &self.nodes
                }
            }

            impl std::ops::DerefMut for $name {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.nodes
                }
            }

            impl std::borrow::Borrow<NodeArray> for $name {
                fn borrow(&self) -> &NodeArray {
                    &self.nodes
                }
            }

            impl IntoIterator for $name {
                type Item = <NodeArray as IntoIterator>::Item;
                type IntoIter = <NodeArray as IntoIterator>::IntoIter;

                fn into_iter(self) -> Self::IntoIter {
                    #[allow(clippy::into_iter_on_ref)]
                    self.nodes.into_iter()
                }
            }

            impl<'nodes> IntoIterator for &'nodes $name {
                type Item = <&'nodes NodeArray as IntoIterator>::Item;
                type IntoIter = <&'nodes NodeArray as IntoIterator>::IntoIter;

                fn into_iter(self) -> Self::IntoIter {
                    #[allow(clippy::into_iter_on_ref)]
                    (&self.nodes).into_iter()
                }
            }
        )+
    };
}

define_array_wrapper! {
    /// A script command which can be executed.
    struct NodeCommand;

    /// A property on an object which can be manipulated.
    struct NodeProperty;
}

impl NodeCommand {
    pub fn execute<S>(&self, context: &mut Context<S>) -> ExecuteResult {
        context.execute(self)
    }

    pub fn display_evaluated<'a, S>(&'a self, context: &'a mut Context<S>) -> ArrayDisplay<'a, S> {
        ArrayDisplay::new(&self.nodes, ArrayKind::Command, context)
    }
}

impl fmt::Debug for NodeCommand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.nodes.fmt(f)
    }
}

impl fmt::Display for NodeCommand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_nodes_unevaluated(&self.nodes, ArrayKind::Command, f)
    }
}

impl NodeProperty {
    pub fn display_evaluated<'a, S>(&'a self, context: &'a mut Context<S>) -> ArrayDisplay<'a, S> {
        ArrayDisplay::new(&self.nodes, ArrayKind::Property, context)
    }
}

impl fmt::Debug for NodeProperty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.nodes.fmt(f)
    }
}

impl fmt::Display for NodeProperty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_nodes_unevaluated(&self.nodes, ArrayKind::Property, f)
    }
}

pub struct ArrayDisplay<'a, S> {
    context: Cell<Option<&'a mut Context<S>>>,
    nodes: &'a [Node],
    kind: ArrayKind,
}

impl<'a, S> ArrayDisplay<'a, S> {
    fn new(nodes: &'a [Node], kind: ArrayKind, context: &'a mut Context<S>) -> Self {
        Self { context: Cell::new(Some(context)), nodes, kind }
    }
}

impl<S> fmt::Display for ArrayDisplay<'_, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.context.take() {
            Some(context) => {
                let result = write_nodes_evaluated(self.nodes, self.kind, context, f);
                // Re-store context to ensure repeated uses have the same result
                self.context.set(Some(context));
                result
            },
            None => write_nodes_unevaluated(self.nodes, self.kind, f),
        }
    }
}

fn write_nodes_evaluated<S>(
    nodes: &[Node],
    kind: ArrayKind,
    context: &mut Context<S>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    write_nodes(nodes, kind, f, |node, f| write!(f, "{}", node.display_evaluated(context)))
}

fn write_nodes_unevaluated(nodes: &[Node], kind: ArrayKind, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write_nodes(nodes, kind, f, |node, f| write!(f, "{}", node.unevaluated()))
}

fn write_nodes(
    nodes: &[Node],
    kind: ArrayKind,
    f: &mut fmt::Formatter<'_>,
    mut display: impl FnMut(&Node, &mut fmt::Formatter<'_>) -> fmt::Result,
) -> fmt::Result {
    let (l, r) = kind.delimiters();
    f.write_char(l)?;
    if !nodes.is_empty() {
        display(&nodes[0], f)?;
        for node in &nodes[1..] {
            f.write_char(' ')?;
            display(node, f)?;
        }
    }
    f.write_char(r)
}
