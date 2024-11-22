// SPDX-License-Identifier: LGPL-3.0-or-later

use std::{
    borrow::{Borrow, BorrowMut},
    ops::{Deref, DerefMut},
    slice::SliceIndex,
};

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

    pub fn set<T: Into<NodeValue>>(&self, context: &mut Context, index: usize, value: T) -> crate::Result<()> {
        self.get(index)?.set(context, value)
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

    pub fn find_array<T: PartialEq<RawNodeValue>>(&self, tag: &T) -> crate::Result<ArrayBox> {
        self.find_array_opt(tag).ok_or(Error::EntryNotFound)
    }

    pub fn find_array_opt<T: PartialEq<RawNodeValue>>(&self, tag: &T) -> Option<ArrayBox> {
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
