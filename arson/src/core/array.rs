// SPDX-License-Identifier: LGPL-3.0-or-later

use std::{
    cell::{Cell, RefCell},
    fmt::{self, Write},
    ops::Range,
    rc::Rc,
    slice::SliceIndex,
};

use crate::parse::ArrayKind;
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

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ArrayRef {
    inner: Rc<RefCell<NodeArray>>,
}

impl ArrayRef {
    pub fn new(array: NodeArray) -> Self {
        Self { inner: Rc::new(RefCell::new(array)) }
    }

    pub fn borrow(&self) -> crate::Result<std::cell::Ref<'_, NodeArray>> {
        self.inner
            .try_borrow()
            .map_err(|e| ArrayError::BadBorrow(e).into())
    }

    pub fn borrow_mut(&self) -> crate::Result<std::cell::RefMut<'_, NodeArray>> {
        self.inner
            .try_borrow_mut()
            .map_err(|e| ArrayError::BadMutBorrow(e).into())
    }
}

/// A contiguous slice of [`Node`]s.
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
            None => Err(ArrayError::OutOfRange(0..self.nodes.len()).into()),
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
}

// Data retrieval by index
impl NodeSlice {
    pub fn integer(&self, context: &mut Context, index: usize) -> crate::Result<Integer> {
        self.get(index)?.integer(context)
    }

    pub fn float(&self, context: &mut Context, index: usize) -> crate::Result<Float> {
        self.get(index)?.float(context)
    }

    pub fn number(&self, context: &mut Context, index: usize) -> crate::Result<Number> {
        self.get(index)?.number(context)
    }

    pub fn boolean(&self, context: &mut Context, index: usize) -> crate::Result<bool> {
        self.get(index)?.boolean(context)
    }

    pub fn string(&self, context: &mut Context, index: usize) -> crate::Result<Rc<String>> {
        self.get(index)?.string(context)
    }

    pub fn symbol(&self, context: &mut Context, index: usize) -> crate::Result<Symbol> {
        self.get(index)?.symbol(context)
    }

    pub fn variable(&self, index: usize) -> crate::Result<&Variable> {
        self.get(index)?.variable()
    }

    pub fn array(&self, context: &mut Context, index: usize) -> crate::Result<ArrayRef> {
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

    pub fn evaluate(&self, context: &mut Context, index: usize) -> crate::Result<NodeValue> {
        self.get(index)?.evaluate(context)
    }

    pub fn set<T: Into<Node>>(&self, context: &mut Context, index: usize, value: T) -> crate::Result<()> {
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
    ($type:ident => |$predicate:ident, $value:ident| $block:expr) => {
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

// Data retrieval by predicate
impl NodeSlice {
    pub fn find_array(&self, predicate: impl FindDataPredicate) -> crate::Result<ArrayRef> {
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

    pub fn find_array_opt(&self, predicate: impl FindDataPredicate) -> Option<ArrayRef> {
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

    pub fn find_data(&self, predicate: impl FindDataPredicate) -> crate::Result<Node> {
        let array = self.find_array(predicate)?;
        let array = array.borrow()?;
        arson_assert_len!(array, 2, "Expected only one value in array");
        array.get(1).cloned()
    }

    pub fn find_data_opt(&self, predicate: impl FindDataPredicate) -> Option<Node> {
        let array = self.find_array_opt(predicate)?;
        let array = array.borrow().ok()?;
        if array.len() != 2 {
            return None;
        }
        array.get_opt(1).cloned()
    }

    pub fn find_integer(&self, context: &mut Context, predicate: impl FindDataPredicate) -> crate::Result<Integer> {
        self.find_data(predicate)?.integer(context)
    }

    pub fn find_float(&self, context: &mut Context, predicate: impl FindDataPredicate) -> crate::Result<Float> {
        self.find_data(predicate)?.float(context)
    }

    pub fn find_number(&self, context: &mut Context, predicate: impl FindDataPredicate) -> crate::Result<Number> {
        self.find_data(predicate)?.number(context)
    }

    pub fn find_boolean(&self, context: &mut Context, predicate: impl FindDataPredicate) -> crate::Result<bool> {
        self.find_data(predicate)?.boolean(context)
    }

    pub fn find_string(&self, context: &mut Context, predicate: impl FindDataPredicate) -> crate::Result<Rc<String>> {
        self.find_data(predicate)?.string(context)
    }

    pub fn find_symbol(&self, context: &mut Context, predicate: impl FindDataPredicate) -> crate::Result<Symbol> {
        self.find_data(predicate)?.symbol(context)
    }

    pub fn find_variable(&self, predicate: impl FindDataPredicate) -> crate::Result<Variable> {
        self.find_data(predicate)?.variable().cloned()
    }

    pub fn find_command(&self, predicate: impl FindDataPredicate) -> crate::Result<Rc<NodeCommand>> {
        self.find_data(predicate)?.command().cloned()
    }

    pub fn find_property(&self, predicate: impl FindDataPredicate) -> crate::Result<Rc<NodeProperty>> {
        self.find_data(predicate)?.property().cloned()
    }

    pub fn display_evaluated<'a>(&'a self, context: &'a mut Context) -> ArrayDisplay<'_> {
        ArrayDisplay::new_evaluated(&self.nodes, ArrayKind::Array, context)
    }
}

impl std::ops::Deref for NodeSlice {
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

impl fmt::Display for NodeSlice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ArrayDisplay::new_unevaluated(&self.nodes, ArrayKind::Array).fmt(f)
    }
}

/// A contiguous, growable collection of [`Node`]s.
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

    pub fn display_evaluated<'a>(&'a self, context: &'a mut Context) -> ArrayDisplay<'_> {
        ArrayDisplay::new_evaluated(&self.nodes, ArrayKind::Array, context)
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

impl fmt::Display for NodeArray {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ArrayDisplay::new_unevaluated(&self.nodes, ArrayKind::Array).fmt(f)
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
    /// An executable/evaluatable command.
    struct NodeCommand;

    /// A property on an object which can be manipulated.
    struct NodeProperty;
}

impl NodeCommand {
    pub fn execute(&self, context: &mut Context) -> ExecuteResult {
        context.execute(self)
    }

    pub fn display_evaluated<'a>(&'a self, context: &'a mut Context) -> ArrayDisplay<'_> {
        ArrayDisplay::new_evaluated(&self.nodes, ArrayKind::Command, context)
    }
}

impl fmt::Display for NodeCommand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ArrayDisplay::new_unevaluated(&self.nodes, ArrayKind::Command).fmt(f)
    }
}

impl NodeProperty {
    pub fn display_evaluated<'a>(&'a self, context: &'a mut Context) -> ArrayDisplay<'_> {
        ArrayDisplay::new_evaluated(&self.nodes, ArrayKind::Property, context)
    }
}

impl fmt::Display for NodeProperty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ArrayDisplay::new_unevaluated(&self.nodes, ArrayKind::Property).fmt(f)
    }
}

pub struct ArrayDisplay<'a> {
    context: Cell<Option<&'a mut Context>>,
    nodes: &'a [Node],
    kind: ArrayKind,
}

impl<'a> ArrayDisplay<'a> {
    fn new_unevaluated(nodes: &'a [Node], kind: ArrayKind) -> Self {
        Self { context: Cell::new(None), nodes, kind }
    }

    fn new_evaluated(nodes: &'a [Node], kind: ArrayKind, context: &'a mut Context) -> Self {
        Self { context: Cell::new(Some(context)), nodes, kind }
    }

    fn write_nodes(
        &self,
        f: &mut fmt::Formatter<'_>,
        mut display: impl FnMut(&Node, &mut fmt::Formatter<'_>) -> fmt::Result,
    ) -> fmt::Result {
        let (l, r) = self.kind.delimiters();
        f.write_char(l)?;
        if !self.nodes.is_empty() {
            display(&self.nodes[0], f)?;
            for node in &self.nodes[1..] {
                f.write_char(' ')?;
                display(node, f)?;
            }
        }
        f.write_char(r)
    }
}

impl<'a> fmt::Display for ArrayDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.context.replace(None) {
            Some(context) => self.write_nodes(f, |node, f| write!(f, "{}", node.display_evaluated(context))),
            None => self.write_nodes(f, |node, f| write!(f, "{}", node.unevaluated())),
        }
    }
}
