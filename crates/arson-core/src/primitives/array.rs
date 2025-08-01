// SPDX-License-Identifier: LGPL-3.0-or-later

use std::borrow::Cow;
use std::cell::{Cell, RefCell};
use std::fmt::{self, Write};
use std::ops::Range;
use std::rc::Rc;

use super::NodeSliceIndex;
use crate::prelude::*;
use crate::{FloatValue, Integer, IntegerValue, IntoSymbol, Number, NumericError};

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

#[macro_export]
macro_rules! arson_slice {
    () => (
        $crate::NodeSlice::new()
    );
    ($elem:expr; $n:expr) => (
        $crate::NodeSlice::from(&std::array::from_fn::<_, $n, _>(|_| $elem.into()))
    );
    ($($x:expr),+ $(,)?) => (
        $crate::NodeSlice::from(&[$($x.into()),+])
    );
}

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum ArrayError {
    #[error("array already mutably borrowed")]
    BadBorrow(#[from] std::cell::BorrowError),

    #[error("array already immutably borrowed")]
    BadMutBorrow(#[from] std::cell::BorrowMutError),

    #[error("tag {0} was not found in the array")]
    TagNotFound(String),
}

#[derive(PartialEq, PartialOrd, Clone)]
pub struct ArrayRef {
    inner: Rc<RefCell<NodeArray>>,
}

pub type ArrayBorrow<'a> = std::cell::Ref<'a, NodeArray>;
pub type ArrayBorrowMut<'a> = std::cell::RefMut<'a, NodeArray>;

impl ArrayRef {
    pub fn new(array: NodeArray) -> Self {
        Self { inner: Rc::new(RefCell::new(array)) }
    }

    pub fn borrow(&self) -> crate::Result<ArrayBorrow<'_>> {
        self.inner.try_borrow().map_err(|e| ArrayError::BadBorrow(e).into())
    }

    pub fn borrow_mut(&self) -> crate::Result<ArrayBorrowMut<'_>> {
        self.inner.try_borrow_mut().map_err(|e| ArrayError::BadMutBorrow(e).into())
    }

    pub fn total_cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Unlike other places in the code, there's no good way of handling or propogating
        // borrow errors without destroying the intent of the method, so this is
        // one of the few places where a potential panic is deliberately left in.
        self.inner.borrow().total_cmp(&other.inner.borrow())
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

impl std::fmt::Display for ArrayRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner.try_borrow() {
            Ok(inner) => inner.fmt(f),
            Err(_) => f.write_str("(<borrowed>)"),
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
    pub fn empty() -> &'static NodeSlice {
        Self::from(&[])
    }

    pub fn from(nodes: &[Node]) -> &NodeSlice {
        // SAFETY: NodeSlice transparently contains a [Node], so its layout is identical
        unsafe { &*(nodes as *const [Node] as *const NodeSlice) }
    }

    pub fn from_mut(nodes: &mut [Node]) -> &mut NodeSlice {
        // SAFETY: NodeSlice transparently contains a [Node], so its layout is identical
        unsafe { &mut *(nodes as *mut [Node] as *mut NodeSlice) }
    }

    pub fn slice<I: NodeSliceIndex<Output = [Node]>>(&self, index: I) -> crate::Result<&NodeSlice> {
        index.get(&self.nodes).map(Self::from)
    }

    pub fn slice_mut<I: NodeSliceIndex<Output = [Node]>>(
        &mut self,
        index: I,
    ) -> crate::Result<&mut NodeSlice> {
        index.get_mut(&mut self.nodes).map(Self::from_mut)
    }

    pub fn get<I: NodeSliceIndex>(&self, index: I) -> crate::Result<&I::Output> {
        index.get(&self.nodes)
    }

    pub fn get_opt<I: NodeSliceIndex>(&self, index: I) -> Option<&I::Output> {
        index.get_opt(&self.nodes)
    }

    pub fn get_mut<I: NodeSliceIndex>(&mut self, index: I) -> crate::Result<&mut I::Output> {
        index.get_mut(&mut self.nodes)
    }

    pub fn get_mut_opt<I: NodeSliceIndex>(&mut self, index: I) -> Option<&mut I::Output> {
        index.get_mut_opt(&mut self.nodes)
    }

    pub fn sort(&mut self) {
        self.nodes.sort_by(Node::total_cmp)
    }

    pub fn total_cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Modified from SlicePartialOrd::partial_cmp in core::slice

        let min = self.len().min(other.len());

        let left = &self[..min];
        let right = &other[..min];

        for i in 0..min {
            match left[i].total_cmp(&right[i]) {
                std::cmp::Ordering::Equal => (),
                non_eq => return non_eq,
            }
        }

        self.len().cmp(&other.len())
    }

    pub fn display_with_options(&self, options: ArrayDisplayOptions) -> ArrayDisplay<'_> {
        ArrayDisplay::new(&self.nodes, ArrayKind::Array, options)
    }
}

macro_rules! value_or {
    ($value:expr, $context:ident, $map:expr, $default:ident) => {
        value_or!($value, $context, $map, $default => $default.clone())
    };
    ($value:expr, $context:ident, $map:expr, $default:ident => $make_default:expr) => {
        if let Some(node) = $value {
            $map(node, $context, $default)
        } else {
            Ok($make_default)
        }
    };
    ($value:expr, $map:expr, $default:ident) => {
        if let Some(node) = $value {
            $map(node, $default)
        } else {
            $default.clone()
        }
    };
}

macro_rules! value_or_else {
    ($value:expr, $context:ident, $map:expr, $default:expr) => {
        if let Some(node) = $value {
            $map(node, $context, $default)
        } else {
            $default($context)
        }
    };
    ($value:expr, $map:expr, $default:ident) => {
        if let Some(node) = $value {
            $map(node, $default)
        } else {
            $default()
        }
    };
}

macro_rules! index_value_or {
    ($self:ident, $context:ident, $index:ident, $map:expr, $default:ident) => {
        value_or!($self.get_opt($index), $context, $map, $default)
    };
    ($self:ident, $context:ident, $index:ident, $map:expr, $default:ident => $make_default:expr) => {
        value_or!($self.get_opt($index), $context, $map, $default => $make_default)
    };
    ($self:ident, $index:ident, $map:expr, $default:ident) => {
        value_or!($self.get_opt($index), $map, $default)
    };
}

macro_rules! index_value_or_else {
    ($self:ident, $context:ident, $index:ident, $map:expr, $default:ident) => {
        value_or_else!($self.get_opt($index), $context, $map, $default)
    };
    ($self:ident, $index:ident, $map:expr, $default:ident) => {
        value_or_else!($self.get_opt($index), $map, $default)
    };
}

// Data retrieval by index
impl NodeSlice {
    pub fn unevaluated(&self, index: usize) -> crate::Result<&NodeValue> {
        self.get(index).map(|n| n.unevaluated())
    }

    pub fn evaluate(&self, context: &mut Context, index: usize) -> crate::Result<NodeValue> {
        self.get(index)?.evaluate(context)
    }

    pub fn integer(&self, context: &mut Context, index: usize) -> crate::Result<Integer> {
        self.get(index)?.integer(context)
    }

    pub fn float(&self, context: &mut Context, index: usize) -> crate::Result<FloatValue> {
        self.get(index)?.float(context)
    }

    pub fn number(&self, context: &mut Context, index: usize) -> crate::Result<Number> {
        self.get(index)?.number(context)
    }

    pub fn size_integer(&self, context: &mut Context, index: usize) -> crate::Result<usize> {
        self.get(index)?.size_integer(context)
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

    pub fn force_symbol(&self, context: &mut Context, index: usize) -> crate::Result<Symbol> {
        self.get(index)?.force_symbol(context)
    }

    pub fn array_tag(&self, context: &mut Context, index: usize) -> crate::Result<ArrayTag> {
        self.get(index)?.array_tag(context)
    }

    pub fn variable(&self, index: usize) -> crate::Result<&Variable> {
        self.get(index)?.variable()
    }

    pub fn set_variable(
        &self,
        context: &mut Context,
        index: usize,
        value: impl Into<Node>,
    ) -> crate::Result<Option<Node>> {
        self.get(index)?.set_variable(context, value)
    }

    pub fn object(&self, context: &mut Context, index: usize) -> crate::Result<ObjectRef> {
        self.get(index)?.object(context)
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
}

// Optional data retrieval by index
impl NodeSlice {
    pub fn unevaluated_opt(&self, index: usize) -> Option<&NodeValue> {
        self.get_opt(index).map(|n| n.unevaluated())
    }

    pub fn evaluate_opt(&self, context: &mut Context, index: usize) -> Option<crate::Result<NodeValue>> {
        self.get_opt(index).map(|n| n.evaluate(context))
    }

    pub fn integer_opt(&self, context: &mut Context, index: usize) -> Option<crate::Result<Integer>> {
        self.get_opt(index).and_then(|n| n.integer_opt(context).transpose())
    }

    pub fn float_opt(&self, context: &mut Context, index: usize) -> Option<crate::Result<FloatValue>> {
        self.get_opt(index).and_then(|n| n.float_opt(context).transpose())
    }

    pub fn number_opt(&self, context: &mut Context, index: usize) -> Option<crate::Result<Number>> {
        self.get_opt(index).and_then(|n| n.number_opt(context).transpose())
    }

    pub fn size_integer_opt(&self, context: &mut Context, index: usize) -> Option<crate::Result<usize>> {
        self.get_opt(index).and_then(|n| n.size_integer_opt(context).transpose())
    }

    pub fn string_opt(&self, context: &mut Context, index: usize) -> Option<crate::Result<Rc<String>>> {
        self.get_opt(index).and_then(|n| n.string_opt(context).transpose())
    }

    pub fn symbol_opt(&self, context: &mut Context, index: usize) -> Option<crate::Result<Symbol>> {
        self.get_opt(index).and_then(|n| n.symbol_opt(context).transpose())
    }

    pub fn force_symbol_opt(&self, context: &mut Context, index: usize) -> Option<crate::Result<Symbol>> {
        self.get_opt(index).and_then(|n| n.force_symbol_opt(context).transpose())
    }

    pub fn array_tag_opt(&self, context: &mut Context, index: usize) -> Option<crate::Result<ArrayTag>> {
        self.get_opt(index).and_then(|n| n.array_tag_opt(context).transpose())
    }

    pub fn variable_opt(&self, index: usize) -> Option<&Variable> {
        self.get_opt(index).and_then(|n| n.variable_opt())
    }

    pub fn set_variable_opt(&self, context: &mut Context, index: usize, value: impl Into<Node>) {
        if let Some(node) = self.get_opt(index) {
            node.set_variable_opt(context, value);
        }
    }

    pub fn object_opt(&self, context: &mut Context, index: usize) -> Option<crate::Result<ObjectRef>> {
        self.get_opt(index).and_then(|n| n.object_opt(context).transpose())
    }

    pub fn array_opt(&self, context: &mut Context, index: usize) -> Option<crate::Result<ArrayRef>> {
        self.get_opt(index).and_then(|n| n.array_opt(context).transpose())
    }

    pub fn command_opt(&self, index: usize) -> Option<&Rc<NodeCommand>> {
        self.get_opt(index)?.command_opt()
    }

    pub fn property_opt(&self, index: usize) -> Option<&Rc<NodeProperty>> {
        self.get_opt(index)?.property_opt()
    }
}

// Data retrieval by index, with defaults
impl NodeSlice {
    pub fn unevaluated_or(&self, index: usize, default: &NodeValue) -> NodeValue {
        self.get_opt(index)
            .map(|n| n.unevaluated().clone())
            .unwrap_or_else(|| default.clone())
    }

    pub fn evaluate_or(
        &self,
        context: &mut Context,
        index: usize,
        default: &NodeValue,
    ) -> crate::Result<NodeValue> {
        self.get_opt(index)
            .map(|n| n.evaluate(context))
            .unwrap_or_else(|| Ok(default.clone()))
    }

    pub fn integer_or(
        &self,
        context: &mut Context,
        index: usize,
        default: Integer,
    ) -> crate::Result<Integer> {
        index_value_or!(self, context, index, Node::integer_or, default)
    }

    pub fn float_or(
        &self,
        context: &mut Context,
        index: usize,
        default: FloatValue,
    ) -> crate::Result<FloatValue> {
        index_value_or!(self, context, index, Node::float_or, default)
    }

    pub fn number_or(&self, context: &mut Context, index: usize, default: Number) -> crate::Result<Number> {
        index_value_or!(self, context, index, Node::number_or, default)
    }

    pub fn size_integer_or(
        &self,
        context: &mut Context,
        index: usize,
        default: usize,
    ) -> crate::Result<usize> {
        index_value_or!(self, context, index, Node::size_integer_or, default)
    }

    pub fn string_or(
        &self,
        context: &mut Context,
        index: usize,
        default: &Rc<String>,
    ) -> crate::Result<Rc<String>> {
        index_value_or!(self, context, index, Node::string_or, default)
    }

    pub fn symbol_or(
        &self,
        context: &mut Context,
        index: usize,
        default: impl IntoSymbol,
    ) -> crate::Result<Symbol> {
        index_value_or!(self, context, index, Node::symbol_or, default => default.into_symbol(context)?)
    }

    pub fn force_symbol_or(
        &self,
        context: &mut Context,
        index: usize,
        default: impl IntoSymbol,
    ) -> crate::Result<Symbol> {
        index_value_or!(self, context, index, Node::force_symbol_or, default => default.into_symbol(context)?)
    }

    pub fn array_tag_or(
        &self,
        context: &mut Context,
        index: usize,
        default: &ArrayTag,
    ) -> crate::Result<ArrayTag> {
        index_value_or!(self, context, index, Node::array_tag_or, default)
    }

    pub fn variable_or(&self, index: usize, default: &Variable) -> Variable {
        index_value_or!(self, index, Node::variable_or, default)
    }

    pub fn object_or(
        &self,
        context: &mut Context,
        index: usize,
        default: &ObjectRef,
    ) -> crate::Result<ObjectRef> {
        index_value_or!(self, context, index, Node::object_or, default)
    }

    pub fn array_or(
        &self,
        context: &mut Context,
        index: usize,
        default: &ArrayRef,
    ) -> crate::Result<ArrayRef> {
        index_value_or!(self, context, index, Node::array_or, default)
    }

    pub fn command_or(&self, index: usize, default: &Rc<NodeCommand>) -> Rc<NodeCommand> {
        index_value_or!(self, index, Node::command_or, default)
    }

    pub fn property_or(&self, index: usize, default: &Rc<NodeProperty>) -> Rc<NodeProperty> {
        index_value_or!(self, index, Node::property_or, default)
    }

    pub fn unevaluated_or_else(&self, index: usize, default: impl FnOnce() -> NodeValue) -> NodeValue {
        self.get_opt(index)
            .map(|n| n.unevaluated().clone())
            .unwrap_or_else(default)
    }

    pub fn evaluate_or_else(
        &self,
        context: &mut Context,
        index: usize,
        default: impl FnOnce(&mut Context) -> crate::Result<NodeValue>,
    ) -> crate::Result<NodeValue> {
        self.get_opt(index)
            .map(|n| n.evaluate(context))
            .unwrap_or_else(|| default(context))
    }

    pub fn integer_or_else(
        &self,
        context: &mut Context,
        index: usize,
        default: impl FnOnce(&mut Context) -> crate::Result<Integer>,
    ) -> crate::Result<Integer> {
        index_value_or_else!(self, context, index, Node::integer_or_else, default)
    }

    pub fn float_or_else(
        &self,
        context: &mut Context,
        index: usize,
        default: impl FnOnce(&mut Context) -> crate::Result<FloatValue>,
    ) -> crate::Result<FloatValue> {
        index_value_or_else!(self, context, index, Node::float_or_else, default)
    }

    pub fn number_or_else(
        &self,
        context: &mut Context,
        index: usize,
        default: impl FnOnce(&mut Context) -> crate::Result<Number>,
    ) -> crate::Result<Number> {
        index_value_or_else!(self, context, index, Node::number_or_else, default)
    }

    pub fn size_integer_or_else(
        &self,
        context: &mut Context,
        index: usize,
        default: impl FnOnce(&mut Context) -> crate::Result<usize>,
    ) -> crate::Result<usize> {
        index_value_or_else!(self, context, index, Node::size_integer_or_else, default)
    }

    pub fn string_or_else(
        &self,
        context: &mut Context,
        index: usize,
        default: impl FnOnce(&mut Context) -> crate::Result<Rc<String>>,
    ) -> crate::Result<Rc<String>> {
        index_value_or_else!(self, context, index, Node::string_or_else, default)
    }

    pub fn symbol_or_else(
        &self,
        context: &mut Context,
        index: usize,
        default: impl FnOnce(&mut Context) -> crate::Result<Symbol>,
    ) -> crate::Result<Symbol> {
        index_value_or_else!(self, context, index, Node::symbol_or_else, default)
    }

    pub fn force_symbol_or_else(
        &self,
        context: &mut Context,
        index: usize,
        default: impl FnOnce(&mut Context) -> crate::Result<Symbol>,
    ) -> crate::Result<Symbol> {
        index_value_or_else!(self, context, index, Node::force_symbol_or_else, default)
    }

    pub fn array_tag_or_else(
        &self,
        context: &mut Context,
        index: usize,
        default: impl FnOnce(&mut Context) -> crate::Result<ArrayTag>,
    ) -> crate::Result<ArrayTag> {
        index_value_or_else!(self, context, index, Node::array_tag_or_else, default)
    }

    pub fn variable_or_else(
        &self,
        index: usize,
        default: impl FnOnce() -> crate::Result<Variable>,
    ) -> crate::Result<Variable> {
        index_value_or_else!(self, index, Node::variable_or_else, default)
    }

    pub fn object_or_else(
        &self,
        context: &mut Context,
        index: usize,
        default: impl FnOnce(&mut Context) -> crate::Result<ObjectRef>,
    ) -> crate::Result<ObjectRef> {
        index_value_or_else!(self, context, index, Node::object_or_else, default)
    }

    pub fn array_or_else(
        &self,
        context: &mut Context,
        index: usize,
        default: impl FnOnce(&mut Context) -> crate::Result<ArrayRef>,
    ) -> crate::Result<ArrayRef> {
        index_value_or_else!(self, context, index, Node::array_or_else, default)
    }

    pub fn command_or_else(
        &self,
        index: usize,
        default: impl FnOnce() -> crate::Result<Rc<NodeCommand>>,
    ) -> crate::Result<Rc<NodeCommand>> {
        index_value_or_else!(self, index, Node::command_or_else, default)
    }

    pub fn property_or_else(
        &self,
        index: usize,
        default: impl FnOnce() -> crate::Result<Rc<NodeProperty>>,
    ) -> crate::Result<Rc<NodeProperty>> {
        index_value_or_else!(self, index, Node::property_or_else, default)
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ArrayTag {
    Integer(Integer),
    Symbol(Symbol),
}

impl PartialEq<NodeValue> for ArrayTag {
    fn eq(&self, other: &NodeValue) -> bool {
        match (self, other) {
            (ArrayTag::Integer(left), NodeValue::Integer(right)) => left == right,
            (ArrayTag::Symbol(left), NodeValue::Symbol(right)) => left == right,
            _ => false,
        }
    }
}

impl PartialEq<Integer> for ArrayTag {
    fn eq(&self, other: &Integer) -> bool {
        match self {
            ArrayTag::Integer(value) => value == other,
            _ => false,
        }
    }
}

impl PartialEq<Symbol> for ArrayTag {
    fn eq(&self, other: &Symbol) -> bool {
        match self {
            ArrayTag::Symbol(value) => value == other,
            _ => false,
        }
    }
}

impl PartialEq<ArrayTag> for NodeValue {
    fn eq(&self, other: &ArrayTag) -> bool {
        other.eq(self)
    }
}

impl PartialEq<ArrayTag> for Integer {
    fn eq(&self, other: &ArrayTag) -> bool {
        other.eq(self)
    }
}

impl PartialEq<ArrayTag> for Symbol {
    fn eq(&self, other: &ArrayTag) -> bool {
        other.eq(self)
    }
}

impl std::fmt::Display for ArrayTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArrayTag::Integer(value) => value.fmt(f),
            ArrayTag::Symbol(value) => value.fmt(f),
        }
    }
}

/// A predicate used for searching in the [`NodeSlice::find_array`] and
/// [`NodeSlice::find_data`] family of functions.
///
/// This trait is primarily just a helper and is not meant to be implemented directly,
/// though nothing will go wrong if you do (barring any logic errors in said implementation).
pub trait FindDataPredicate {
    fn matches(&self, value: &NodeValue) -> bool;
    fn err_string(&self) -> String;
}

// No blanket implementation is provided to allow all values which are comparable to NodeValue,
// because very few of the variants make sense to be used as data keys as-is.
// Instead, an implementation is provided for Fn(&NodeValue) -> bool which allows for custom searches,
// so if a more special need arises, it can be handled using that custom logic.

macro_rules! find_pred_impl {
    (
        $(
            $type:ty {
                matches: |$matches_predicate:ident, $matches_value:ident| $matches_block:expr,
                err_string: |$err_string_self:ident| $err_string_block:expr,
            },
        )+
    ) => {
        $(
            impl FindDataPredicate for $type {
                fn matches(&self, $matches_value: &NodeValue) -> bool {
                    let $matches_predicate = self;
                    $matches_block
                }

                fn err_string(&self) -> String {
                    let $err_string_self = self;
                    $err_string_block
                }
            }
        )+
    };
}

find_pred_impl! {
    Symbol {
        matches: |predicate, value| value == predicate,
        err_string: |this| format!("symbol {this}"),
    },
    // We could allow this, but really, you should use a Symbol instead...
    // String {
    //     matches: |predicate, value| value == predicate,
    //     err_string: |this| format!("string {this}"),
    // },
    Integer {
        matches: |predicate, value| value == predicate,
        err_string: |this| format!("integer {this}"),
    },
    IntegerValue {
        matches: |predicate, value| value == predicate,
        err_string: |this| format!("integer {this}"),
    },
    ArrayTag {
        matches: |predicate, value| value == predicate,
        err_string: |this| match this {
            ArrayTag::Integer(tag) => FindDataPredicate::err_string(tag),
            ArrayTag::Symbol(tag) => FindDataPredicate::err_string(tag),
        },
    },
}

// TODO: Custom predicate function
// struct FindDataFn<F: Fn(&NodeValue) -> bool>(F);

// impl<F: Fn(&NodeValue) -> bool> FindDataPredicate for FindDataFn<F> {
//     fn matches(&self, value: &NodeValue) -> bool {
//         self.0(value)
//     }

//     fn err_string(&self) -> String {
//         "custom predicate".to_owned()
//     }
// }

impl<T: FindDataPredicate> FindDataPredicate for &T {
    fn matches(&self, value: &NodeValue) -> bool {
        FindDataPredicate::matches(*self, value)
    }

    fn err_string(&self) -> String {
        FindDataPredicate::err_string(*self)
    }
}

pub trait IntoDataPredicate {
    type Target: FindDataPredicate;

    fn into_predicate(self) -> crate::Result<Self::Target>;
}

impl<T: FindDataPredicate> IntoDataPredicate for T {
    type Target = T;

    fn into_predicate(self) -> crate::Result<Self::Target> {
        Ok(self)
    }
}

impl<T: AsRef<str>> IntoDataPredicate for (&mut Context, T) {
    type Target = Symbol;

    fn into_predicate(self) -> crate::Result<Self::Target> {
        self.0.add_symbol(self.1.as_ref())
    }
}

// impl<F: Fn(&NodeValue) -> bool> IntoDataPredicate for F {
//     type Target = FindDataFn<F>;

//     fn into_predicate(self) -> crate::Result<Self::Target> {
//         Ok(FindDataFn(self))
//     }
// }

// this is kinda stupid lol
pub trait IntoIntoDataPredicate {
    type Target: IntoDataPredicate;

    fn into_predicate(self, context: &mut Context) -> crate::Result<Self::Target>;
}

impl<T: IntoDataPredicate> IntoIntoDataPredicate for T {
    type Target = T;

    fn into_predicate(self, _context: &mut Context) -> crate::Result<Self::Target> {
        Ok(self)
    }
}

impl IntoIntoDataPredicate for &str {
    type Target = Symbol;

    fn into_predicate(self, context: &mut Context) -> crate::Result<Self::Target> {
        context.add_symbol(self)
    }
}

impl IntoIntoDataPredicate for &String {
    type Target = Symbol;

    fn into_predicate(self, context: &mut Context) -> crate::Result<Self::Target> {
        context.add_symbol(self)
    }
}

macro_rules! predicate_value_or {
    ($self:ident, $context:ident, $predicate:ident, $map:expr, $default:ident) => {
        value_or!(
            &$self.find_data_opt($predicate.into_predicate($context)?),
            $context,
            $map,
            $default
        )
    };
    ($self:ident, $context:ident, $predicate:ident, $map:expr, $default:ident => $make_default:expr) => {
        value_or!(
            &$self.find_data_opt($predicate.into_predicate($context)?),
            $context,
            $map,
            $default => $make_default
        )
    };
    ($self:ident, $predicate:ident, $map:expr, $default:ident) => {
        value_or!(&$self.find_data_opt($predicate), $map, $default)
    };
}

macro_rules! predicate_value_or_else {
    ($self:ident, $context:ident, $predicate:ident, $map:expr, $default:ident) => {
        value_or_else!(
            &$self.find_data_opt($predicate.into_predicate($context)?),
            $context,
            $map,
            $default
        )
    };
    ($self:ident, $context:ident, $predicate:ident, $map:expr, $default:ident => $make_default:expr) => {
        value_or_else!(
            &$self.find_data_opt($predicate.into_predicate($context)),
            $context,
            $map,
            $default => $make_default
        )
    };
    ($self:ident, $predicate:ident, $map:expr, $default:ident) => {
        value_or_else!(&$self.find_data_opt($predicate), $map, $default)
    };
}

// Data retrieval by predicate
impl NodeSlice {
    pub fn find_tag(&self, predicate: impl IntoDataPredicate) -> crate::Result<ArrayRef> {
        let predicate = predicate.into_predicate()?;
        for node in self.iter() {
            let NodeValue::Array(array) = node.unevaluated() else {
                continue;
            };
            if let Ok(node) = array.borrow()?.unevaluated(0) {
                if predicate.matches(node) {
                    return Ok(array.clone());
                }
            };
        }

        Err(ArrayError::TagNotFound(predicate.err_string()).into())
    }

    pub fn find_data(&self, predicate: impl IntoDataPredicate) -> crate::Result<Node> {
        let array = self.find_tag(predicate)?;
        let array = array.borrow()?;
        arson_assert_len!(array, 2, "Expected only one value in array");
        array.get(1).cloned()
    }

    pub fn find_integer(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<Integer> {
        self.find_data(predicate.into_predicate(context)?)?.integer(context)
    }

    pub fn find_float(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<FloatValue> {
        self.find_data(predicate.into_predicate(context)?)?.float(context)
    }

    pub fn find_number(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<Number> {
        self.find_data(predicate.into_predicate(context)?)?.number(context)
    }

    pub fn find_size_integer(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<usize> {
        self.find_data(predicate.into_predicate(context)?)?.size_integer(context)
    }

    pub fn find_boolean(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<bool> {
        self.find_data(predicate.into_predicate(context)?)?.boolean(context)
    }

    pub fn find_string(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<Rc<String>> {
        self.find_data(predicate.into_predicate(context)?)?.string(context)
    }

    pub fn find_symbol(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<Symbol> {
        self.find_data(predicate.into_predicate(context)?)?.symbol(context)
    }

    pub fn find_symbol_forced(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<Symbol> {
        self.find_data(predicate.into_predicate(context)?)?.force_symbol(context)
    }

    pub fn find_array_tag(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<ArrayTag> {
        self.find_data(predicate.into_predicate(context)?)?.array_tag(context)
    }

    pub fn find_variable(&self, predicate: impl IntoDataPredicate) -> crate::Result<Variable> {
        self.find_data(predicate)?.variable().cloned()
    }

    pub fn find_object(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<ObjectRef> {
        self.find_data(predicate.into_predicate(context)?)?.object(context)
    }

    pub fn find_array(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<ArrayRef> {
        self.find_data(predicate.into_predicate(context)?)?.array(context)
    }

    pub fn find_command(&self, predicate: impl IntoDataPredicate) -> crate::Result<Rc<NodeCommand>> {
        self.find_data(predicate)?.command().cloned()
    }

    pub fn find_property(&self, predicate: impl IntoDataPredicate) -> crate::Result<Rc<NodeProperty>> {
        self.find_data(predicate)?.property().cloned()
    }
}

macro_rules! find_opt {
    ($self:ident, $predicate:ident, $context:ident) => {{
        let Ok(predicate) = $predicate.into_predicate($context) else {
            return None;
        };

        $self.find_data_opt(predicate)
    }};
}

// Optional data retrieval by predicate
impl NodeSlice {
    pub fn find_tag_opt(&self, predicate: impl IntoDataPredicate) -> Option<ArrayRef> {
        let Ok(predicate) = predicate.into_predicate() else {
            return None;
        };

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

            if predicate.matches(node) {
                return Some(array.clone());
            }
        }

        None
    }

    pub fn find_data_opt(&self, predicate: impl IntoDataPredicate) -> Option<Node> {
        let array = self.find_tag_opt(predicate)?;
        let array = array.borrow().ok()?;
        if array.len() != 2 {
            return None;
        }
        array.get_opt(1).cloned()
    }

    pub fn find_integer_opt(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<Integer>> {
        find_opt!(self, predicate, context)?.integer_opt(context).transpose()
    }

    pub fn find_float_opt(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<FloatValue>> {
        find_opt!(self, predicate, context)?.float_opt(context).transpose()
    }

    pub fn find_number_opt(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<Number>> {
        find_opt!(self, predicate, context)?.number_opt(context).transpose()
    }

    pub fn find_size_integer_opt(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<usize>> {
        find_opt!(self, predicate, context)?.size_integer_opt(context).transpose()
    }

    pub fn find_string_opt(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<Rc<String>>> {
        find_opt!(self, predicate, context)?.string_opt(context).transpose()
    }

    pub fn find_symbol_opt(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<Symbol>> {
        find_opt!(self, predicate, context)?.symbol_opt(context).transpose()
    }

    pub fn find_symbol_forced_opt(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<Symbol>> {
        find_opt!(self, predicate, context)?.force_symbol_opt(context).transpose()
    }

    pub fn find_array_tag_opt(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<ArrayTag>> {
        find_opt!(self, predicate, context)?.array_tag_opt(context).transpose()
    }

    pub fn find_variable_opt(&self, predicate: impl IntoDataPredicate) -> Option<Variable> {
        self.find_data_opt(predicate)?.variable_opt().cloned()
    }

    pub fn find_object_opt(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<ObjectRef>> {
        find_opt!(self, predicate, context)?.object_opt(context).transpose()
    }

    pub fn find_array_opt(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<ArrayRef>> {
        find_opt!(self, predicate, context)?.array_opt(context).transpose()
    }

    pub fn find_command_opt(&self, predicate: impl IntoDataPredicate) -> Option<Rc<NodeCommand>> {
        self.find_data_opt(predicate)?.command_opt().cloned()
    }

    pub fn find_property_opt(&self, predicate: impl IntoDataPredicate) -> Option<Rc<NodeProperty>> {
        self.find_data_opt(predicate)?.property_opt().cloned()
    }
}

// Data retrieval by predicate, with defaults
impl NodeSlice {
    pub fn find_tag_or(&self, predicate: impl IntoDataPredicate, default: &ArrayRef) -> ArrayRef {
        self.find_tag_opt(predicate).unwrap_or_else(|| default.clone())
    }

    pub fn find_data_or(&self, predicate: impl IntoDataPredicate, default: &Node) -> Node {
        self.find_data_opt(predicate).unwrap_or_else(|| default.clone())
    }

    pub fn find_integer_or(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: Integer,
    ) -> crate::Result<Integer> {
        predicate_value_or!(self, context, predicate, Node::integer_or, default)
    }

    pub fn find_float_or(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: FloatValue,
    ) -> crate::Result<FloatValue> {
        predicate_value_or!(self, context, predicate, Node::float_or, default)
    }

    pub fn find_number_or(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: Number,
    ) -> crate::Result<Number> {
        predicate_value_or!(self, context, predicate, Node::number_or, default)
    }

    pub fn find_size_integer_or(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: usize,
    ) -> crate::Result<usize> {
        predicate_value_or!(self, context, predicate, Node::size_integer_or, default)
    }

    pub fn find_string_or(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: &Rc<String>,
    ) -> crate::Result<Rc<String>> {
        predicate_value_or!(self, context, predicate, Node::string_or, default)
    }

    pub fn find_symbol_or(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: impl IntoSymbol,
    ) -> crate::Result<Symbol> {
        predicate_value_or!(self, context, predicate, Node::symbol_or, default => default.into_symbol(context)?)
    }

    pub fn find_force_symbol_or(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: impl IntoSymbol,
    ) -> crate::Result<Symbol> {
        predicate_value_or!(self, context, predicate, Node::force_symbol_or, default => default.into_symbol(context)?)
    }

    pub fn find_array_tag_or(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: &ArrayTag,
    ) -> crate::Result<ArrayTag> {
        predicate_value_or!(self, context, predicate, Node::array_tag_or, default)
    }

    pub fn find_variable_or(&self, predicate: impl IntoDataPredicate, default: &Variable) -> Variable {
        predicate_value_or!(self, predicate, Node::variable_or, default)
    }

    pub fn find_object_or(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: &ObjectRef,
    ) -> crate::Result<ObjectRef> {
        predicate_value_or!(self, context, predicate, Node::object_or, default)
    }

    pub fn find_array_or(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: &ArrayRef,
    ) -> crate::Result<ArrayRef> {
        predicate_value_or!(self, context, predicate, Node::array_or, default)
    }

    pub fn find_command_or(
        &self,
        predicate: impl IntoDataPredicate,
        default: &Rc<NodeCommand>,
    ) -> Rc<NodeCommand> {
        predicate_value_or!(self, predicate, Node::command_or, default)
    }

    pub fn find_property_or(
        &self,
        predicate: impl IntoDataPredicate,
        default: &Rc<NodeProperty>,
    ) -> Rc<NodeProperty> {
        predicate_value_or!(self, predicate, Node::property_or, default)
    }

    pub fn find_tag_or_else(
        &self,
        predicate: impl IntoDataPredicate,
        default: impl FnOnce() -> crate::Result<ArrayRef>,
    ) -> crate::Result<ArrayRef> {
        if let Some(tag) = self.find_tag_opt(predicate) {
            return Ok(tag);
        }

        default()
    }

    pub fn find_data_or_else(
        &self,
        predicate: impl IntoDataPredicate,
        default: impl FnOnce() -> crate::Result<Node>,
    ) -> crate::Result<Node> {
        if let Some(data) = self.find_data_opt(predicate) {
            return Ok(data);
        }

        default()
    }

    pub fn find_integer_or_else(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce(&mut Context) -> crate::Result<Integer>,
    ) -> crate::Result<Integer> {
        predicate_value_or_else!(self, context, predicate, Node::integer_or_else, default)
    }

    pub fn find_float_or_else(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce(&mut Context) -> crate::Result<FloatValue>,
    ) -> crate::Result<FloatValue> {
        predicate_value_or_else!(self, context, predicate, Node::float_or_else, default)
    }

    pub fn find_number_or_else(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce(&mut Context) -> crate::Result<Number>,
    ) -> crate::Result<Number> {
        predicate_value_or_else!(self, context, predicate, Node::number_or_else, default)
    }

    pub fn find_size_integer_or_else(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce(&mut Context) -> crate::Result<usize>,
    ) -> crate::Result<usize> {
        predicate_value_or_else!(self, context, predicate, Node::size_integer_or_else, default)
    }

    pub fn find_string_or_else(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce(&mut Context) -> crate::Result<Rc<String>>,
    ) -> crate::Result<Rc<String>> {
        predicate_value_or_else!(self, context, predicate, Node::string_or_else, default)
    }

    pub fn find_symbol_or_else(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce(&mut Context) -> crate::Result<Symbol>,
    ) -> crate::Result<Symbol> {
        predicate_value_or_else!(self, context, predicate, Node::symbol_or_else, default)
    }

    pub fn find_force_symbol_or_else(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce(&mut Context) -> crate::Result<Symbol>,
    ) -> crate::Result<Symbol> {
        predicate_value_or_else!(self, context, predicate, Node::force_symbol_or_else, default)
    }

    pub fn find_array_tag_or_else(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce(&mut Context) -> crate::Result<ArrayTag>,
    ) -> crate::Result<ArrayTag> {
        predicate_value_or_else!(self, context, predicate, Node::array_tag_or_else, default)
    }

    pub fn find_variable_or_else(
        &self,
        predicate: impl IntoDataPredicate,
        default: impl FnOnce() -> crate::Result<Variable>,
    ) -> crate::Result<Variable> {
        predicate_value_or_else!(self, predicate, Node::variable_or_else, default)
    }

    pub fn find_object_or_else(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce(&mut Context) -> crate::Result<ObjectRef>,
    ) -> crate::Result<ObjectRef> {
        predicate_value_or_else!(self, context, predicate, Node::object_or_else, default)
    }

    pub fn find_array_or_else(
        &self,
        context: &mut Context,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce(&mut Context) -> crate::Result<ArrayRef>,
    ) -> crate::Result<ArrayRef> {
        predicate_value_or_else!(self, context, predicate, Node::array_or_else, default)
    }

    pub fn find_command_or_else(
        &self,
        predicate: impl IntoDataPredicate,
        default: impl FnOnce() -> crate::Result<Rc<NodeCommand>>,
    ) -> crate::Result<Rc<NodeCommand>> {
        predicate_value_or_else!(self, predicate, Node::command_or_else, default)
    }

    pub fn find_property_or_else(
        &self,
        predicate: impl IntoDataPredicate,
        default: impl FnOnce() -> crate::Result<Rc<NodeProperty>>,
    ) -> crate::Result<Rc<NodeProperty>> {
        predicate_value_or_else!(self, predicate, Node::property_or_else, default)
    }
}

// Number utilities
impl NodeSlice {
    pub fn number_chain(
        &self,
        context: &mut Context,
        f_int: impl Fn(Integer, Integer) -> crate::Result<Integer>,
        f_float: impl Fn(FloatValue, FloatValue) -> crate::Result<FloatValue>,
    ) -> ExecuteResult {
        fn integer_chain(
            context: &mut Context,
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

        fn float_chain(
            context: &mut Context,
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

impl<T> std::borrow::Borrow<T> for NodeSlice
where
    T: ?Sized,
    <NodeSlice as std::ops::Deref>::Target: std::borrow::Borrow<T>,
{
    fn borrow(&self) -> &T {
        std::ops::Deref::deref(self).borrow()
    }
}

impl<T> std::borrow::BorrowMut<T> for NodeSlice
where
    T: ?Sized,
    <NodeSlice as std::ops::Deref>::Target: std::borrow::BorrowMut<T>,
{
    fn borrow_mut(&mut self) -> &mut T {
        std::ops::DerefMut::deref_mut(self).borrow_mut()
    }
}

impl<T> AsRef<T> for NodeSlice
where
    T: ?Sized,
    <NodeSlice as std::ops::Deref>::Target: AsRef<T>,
{
    fn as_ref(&self) -> &T {
        std::ops::Deref::deref(self).as_ref()
    }
}

impl<T> AsMut<T> for NodeSlice
where
    T: ?Sized,
    <NodeSlice as std::ops::Deref>::Target: AsMut<T>,
{
    fn as_mut(&mut self) -> &mut T {
        std::ops::DerefMut::deref_mut(self).as_mut()
    }
}

impl ToOwned for NodeSlice {
    type Owned = NodeArray;

    fn to_owned(&self) -> Self::Owned {
        self.nodes.to_owned().into()
    }
}

impl<'slice> IntoIterator for &'slice NodeSlice {
    type Item = <&'slice [Node] as IntoIterator>::Item;
    type IntoIter = <&'slice [Node] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        #[expect(
            clippy::into_iter_on_ref,
            reason = "intentionally forwarding to into_iter"
        )]
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
        ArrayDisplay::new_default(&self.nodes, ArrayKind::Array).fmt(f)
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

    pub fn display_with_options(&self, options: ArrayDisplayOptions) -> ArrayDisplay<'_> {
        ArrayDisplay::new(&self.nodes, ArrayKind::Array, options)
    }
}

// TODO: replace with std::slice::range when that stabilizes
fn check_range<R>(range: R, length: usize) -> crate::Result<Range<usize>>
where
    R: std::ops::RangeBounds<usize>,
{
    use std::ops::Bound;

    let start = match range.start_bound() {
        Bound::Included(start) => *start,
        Bound::Excluded(start) => match start.checked_add(1) {
            Some(start) => start,
            None => return Err(NumericError::Overflow.into()),
        },
        Bound::Unbounded => 0,
    };

    let end = match range.end_bound() {
        Bound::Included(end) => match end.checked_add(1) {
            Some(end) => end,
            None => return Err(NumericError::Overflow.into()),
        },
        Bound::Excluded(end) => *end,
        Bound::Unbounded => length,
    };

    if start <= end && end <= length {
        Ok(start..end)
    } else {
        Err(NumericError::slice_out_of_range(range, 0..length).into())
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

    pub fn insert<N: Into<Node>>(&mut self, index: usize, value: N) -> crate::Result {
        arson_assert!(index <= self.nodes.len());
        self.nodes.insert(index, value.into());
        Ok(())
    }

    pub fn insert_slice(&mut self, index: usize, values: &[Node]) -> crate::Result {
        self.splice(index..index, values.iter().cloned())?;
        Ok(())
    }

    pub fn remove(&mut self, index: usize) -> crate::Result<Node> {
        arson_assert!(index < self.nodes.len());
        Ok(self.nodes.remove(index))
    }

    pub fn remove_item(&mut self, value: &Node) {
        if let Some(pos) = self.nodes.iter().position(|v| v == value) {
            self.nodes.remove(pos);
        }
    }

    pub fn drain<R>(&mut self, range: R) -> crate::Result<std::vec::Drain<'_, Node>>
    where
        R: std::ops::RangeBounds<usize>,
    {
        check_range(range, self.nodes.len()).map(|range| self.nodes.drain(range))
    }

    pub fn append(&mut self, other: &mut Self) {
        self.nodes.append(&mut other.nodes)
    }

    pub fn extend_from_slice(&mut self, other: &[Node]) {
        self.nodes.extend_from_slice(other)
    }

    pub fn splice<R, I>(
        &mut self,
        range: R,
        replace_with: I,
    ) -> crate::Result<std::vec::Splice<'_, I::IntoIter>>
    where
        R: std::ops::RangeBounds<usize>,
        I: IntoIterator<Item = Node>,
    {
        check_range(range, self.nodes.len()).map(|range| self.nodes.splice(range, replace_with))
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

    pub fn resize_with<F: FnMut() -> Node>(&mut self, index: usize, value: F) {
        self.nodes.resize_with(index, value)
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

// Merge/replace procedures
impl NodeArray {
    pub fn merge_tags(&mut self, source: &NodeArray) {
        for node in source {
            let NodeValue::Array(source_array) = node.unevaluated() else {
                continue;
            };
            let Ok(source_borrow) = source_array.borrow() else {
                continue;
            };
            let Some(source_tag) = source_borrow.get_opt(0) else {
                continue;
            };

            let Some(predicate) = source_tag.unevaluated().array_tag() else {
                continue;
            };

            match self.find_tag_opt(predicate) {
                Some(found) => {
                    if let Ok(mut dest_borrow) = found.borrow_mut() {
                        dest_borrow.merge_tags(&source_borrow)
                    }
                },
                None => self.push(source_array),
            }
        }
    }

    pub fn replace_tags(&mut self, source: &NodeArray) {
        // returns whether an array was found within `dest`,
        // used to determine whether or not to replace the node wholesale
        fn replace(dest: &mut NodeArray, source: &NodeArray) -> bool {
            let mut array_found = false;

            for dest_node in dest {
                let NodeValue::Array(dest_array) = dest_node.unevaluated() else {
                    continue;
                };

                array_found = true;
                let Ok(mut dest_borrow) = dest_array.borrow_mut() else {
                    continue;
                };
                let Some(dest_tag) = dest_borrow.get_opt(0) else {
                    continue;
                };

                let Some(predicate) = dest_tag.unevaluated().array_tag() else {
                    continue;
                };

                let Some(found) = source.find_tag_opt(predicate) else {
                    continue;
                };
                let Ok(source_borrow) = found.borrow() else {
                    continue;
                };

                if !replace(&mut dest_borrow, &source_borrow) {
                    drop(dest_borrow);
                    drop(source_borrow);
                    *dest_node = found.into()
                }
            }

            array_found
        }

        replace(self, source);
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

impl std::iter::Extend<Node> for NodeArray {
    fn extend<T: IntoIterator<Item = Node>>(&mut self, iter: T) {
        self.nodes.extend(iter)
    }
}

impl std::ops::Deref for NodeArray {
    type Target = NodeSlice;

    fn deref(&self) -> &Self::Target {
        NodeSlice::from(&self.nodes)
    }
}

impl std::ops::DerefMut for NodeArray {
    fn deref_mut(&mut self) -> &mut Self::Target {
        NodeSlice::from_mut(&mut self.nodes)
    }
}

impl std::borrow::Borrow<NodeSlice> for NodeArray {
    fn borrow(&self) -> &NodeSlice {
        self
    }
}

impl std::borrow::BorrowMut<NodeSlice> for NodeArray {
    fn borrow_mut(&mut self) -> &mut NodeSlice {
        self
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

impl AsRef<Vec<Node>> for NodeArray {
    fn as_ref(&self) -> &Vec<Node> {
        &self.nodes
    }
}

impl AsMut<Vec<Node>> for NodeArray {
    fn as_mut(&mut self) -> &mut Vec<Node> {
        &mut self.nodes
    }
}

impl IntoIterator for NodeArray {
    type Item = <Vec<Node> as IntoIterator>::Item;
    type IntoIter = <Vec<Node> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.nodes.into_iter()
    }
}

impl<'nodes> IntoIterator for &'nodes NodeArray {
    type Item = <&'nodes Vec<Node> as IntoIterator>::Item;
    type IntoIter = <&'nodes Vec<Node> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        #[expect(
            clippy::into_iter_on_ref,
            reason = "intentionally forwarding to into_iter"
        )]
        (&self.nodes).into_iter()
    }
}

impl<'nodes> IntoIterator for &'nodes mut NodeArray {
    type Item = <&'nodes mut Vec<Node> as IntoIterator>::Item;
    type IntoIter = <&'nodes mut Vec<Node> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        #[expect(
            clippy::into_iter_on_ref,
            reason = "intentionally forwarding to into_iter"
        )]
        (&mut self.nodes).into_iter()
    }
}

impl fmt::Debug for NodeArray {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.nodes.fmt(f)
    }
}

impl fmt::Display for NodeArray {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ArrayDisplay::new_default(&self.nodes, ArrayKind::Array).fmt(f)
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
                    self.nodes.into_iter()
                }
            }

            impl<'nodes> IntoIterator for &'nodes $name {
                type Item = <&'nodes NodeArray as IntoIterator>::Item;
                type IntoIter = <&'nodes NodeArray as IntoIterator>::IntoIter;

                fn into_iter(self) -> Self::IntoIter {
                    (&self.nodes).into_iter()
                }
            }

            impl<'nodes> IntoIterator for &'nodes mut $name {
                type Item = <&'nodes mut NodeArray as IntoIterator>::Item;
                type IntoIter = <&'nodes mut NodeArray as IntoIterator>::IntoIter;

                fn into_iter(self) -> Self::IntoIter {
                    (&mut self.nodes).into_iter()
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
    pub fn execute(&self, context: &mut Context) -> ExecuteResult {
        context.execute(self)
    }

    pub fn display_with_options(&self, options: ArrayDisplayOptions) -> ArrayDisplay<'_> {
        ArrayDisplay::new(&self.nodes, ArrayKind::Command, options)
    }
}

impl fmt::Debug for NodeCommand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.nodes.fmt(f)
    }
}

impl fmt::Display for NodeCommand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ArrayDisplay::new_default(&self.nodes, ArrayKind::Command).fmt(f)
    }
}

impl NodeProperty {
    pub fn display_with_options(&self, options: ArrayDisplayOptions) -> ArrayDisplay<'_> {
        ArrayDisplay::new(&self.nodes, ArrayKind::Property, options)
    }
}

impl fmt::Debug for NodeProperty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.nodes.fmt(f)
    }
}

impl fmt::Display for NodeProperty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ArrayDisplay::new_default(&self.nodes, ArrayKind::Property).fmt(f)
    }
}

#[derive(Debug, Clone, Copy)]
enum ArrayKind {
    Array,
    Command,
    Property,
}

#[derive(Debug, Clone, Copy)]
pub enum ArrayIndentation {
    Tabs,
    Spaces(usize),
}

/// Options for array pretty-printing.
#[derive(Debug, Clone)]
pub struct ArrayDisplayOptions {
    /// The indentation style to use.
    pub indentation: ArrayIndentation,
    /// The maximum width of arrays in the output.
    pub max_array_width: usize,
}

impl Default for ArrayDisplayOptions {
    fn default() -> Self {
        Self {
            indentation: ArrayIndentation::Spaces(3),
            max_array_width: 60,
        }
    }
}

impl ArrayDisplayOptions {
    fn indent_text(&self) -> String {
        match self.indentation {
            ArrayIndentation::Tabs => "\t".to_owned(),
            ArrayIndentation::Spaces(count) => std::iter::repeat_n(' ', count).collect(),
        }
    }
}

#[derive(Debug)]
pub struct ArrayDisplay<'a> {
    nodes: &'a [Node],
    kind: ArrayKind,
    options: ArrayDisplayOptions,

    indent_level: Cell<usize>,
    indent_text: Cow<'a, str>,
}

impl<'a> ArrayDisplay<'a> {
    fn new(nodes: &'a [Node], kind: ArrayKind, options: ArrayDisplayOptions) -> Self {
        let indent_text = options.indent_text();
        Self {
            nodes,
            kind,
            options,

            indent_level: Cell::new(0),
            indent_text: Cow::Owned(indent_text),
        }
    }

    fn new_default(nodes: &'a [Node], kind: ArrayKind) -> Self {
        Self::new(nodes, kind, ArrayDisplayOptions::default())
    }

    fn new_inner(other: &'a Self, nodes: &'a [Node], kind: ArrayKind) -> Self {
        Self {
            nodes,
            kind,

            options: other.options.clone(),
            indent_level: other.indent_level.clone(),
            indent_text: Cow::Borrowed(&other.indent_text),
        }
    }

    fn write_array_compact(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.nodes.is_empty() {
            Self::write_node_compact(&self.nodes[0], f)?;
            for node in &self.nodes[1..] {
                f.write_char(' ')?;
                Self::write_node_compact(node, f)?;
            }
        }

        Ok(())
    }

    fn write_node_compact(node: &Node, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use std::fmt::Display;

        match node.unevaluated() {
            NodeValue::String(value) => {
                // Apply escapes where necessary
                let value = value.replace('\"', "\\q").replace('\n', "\\n");
                write!(f, "\"{value}\"")
            },
            NodeValue::Object(value) => write!(f, "<object {value}>"),
            value => value.fmt(f),
        }
    }

    fn write_array_pretty(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.nodes.is_empty() {
            return Ok(());
        }

        // If there is only one element, and it is not an array itself, always print it as-is
        if self.nodes.len() == 1 && !self.nodes[0].is_any_array() {
            return Self::write_node_compact(&self.nodes[0], f);
        }

        // Attempt compact array first, so long as there is no more than one inner array
        if self.nodes.iter().filter(|n| n.is_any_array()).count() <= 1 {
            // Max width - 2, to account for array delimiters
            let max_len = self.options.max_array_width - 2;
            let mut limit_buffer = String::new();

            struct NodeWrap<'a>(&'a Node);

            impl std::fmt::Display for NodeWrap<'_> {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    ArrayDisplay::write_node_compact(self.0, f)
                }
            }

            write!(limit_buffer, "{}", NodeWrap(&self.nodes[0]))?;
            for node in &self.nodes[1..] {
                write!(limit_buffer, " {}", NodeWrap(node))?;
                if limit_buffer.len() > max_len {
                    break;
                }
            }

            if limit_buffer.len() <= max_len {
                return f.write_str(&limit_buffer);
            }
        }

        // Bump up indentation for inner elements
        let original_indent = self.indent_level.get();
        self.indent_level.set(original_indent + 1);

        let mut iter = self.nodes.iter();

        // Display leading symbol on the same line as the array opening
        match iter.next() {
            Some(node) if node.is_symbol() => writeln!(f, "{node}")?,
            Some(node) => {
                f.write_char('\n')?;
                self.write_node_pretty(node, f)?
            },
            None => return Ok(()),
        }

        for node in iter {
            self.write_node_pretty(node, f)?;
        }

        // Restore and write indentation for closing delimiter
        self.indent_level.set(original_indent);
        self.write_indent(f)?;

        Ok(())
    }

    fn write_node_pretty(&'a self, node: &Node, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write_indent(f)?;
        match node.unevaluated() {
            NodeValue::Array(array) => {
                let Ok(borrow) = array.borrow() else {
                    return writeln!(f, "(<borrowed>)");
                };
                self.write_inner_array_pretty(&borrow, ArrayKind::Array, f)
            },
            NodeValue::Command(array) => self.write_inner_array_pretty(array, ArrayKind::Command, f),
            NodeValue::Property(array) => self.write_inner_array_pretty(array, ArrayKind::Property, f),
            _ => {
                Self::write_node_compact(node, f)?;
                writeln!(f)
            },
        }
    }

    fn write_inner_array_pretty(
        &'a self,
        nodes: &'a [Node],
        kind: ArrayKind,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let display = Self::new_inner(self, nodes, kind);
        writeln!(f, "{display:#}")
    }

    fn write_indent(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for _i in 0..self.indent_level.get() {
            f.write_str(&self.indent_text)?;
        }

        Ok(())
    }
}

impl fmt::Display for ArrayDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (l, r) = match self.kind {
            ArrayKind::Array => ('(', ')'),
            ArrayKind::Command => ('{', '}'),
            ArrayKind::Property => ('[', ']'),
        };

        f.write_char(l)?;

        if f.alternate() {
            self.write_array_pretty(f)?;
        } else {
            self.write_array_compact(f)?;
        }

        f.write_char(r)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn merge_tags() {
        let mut context = Context::new();

        let sym1 = context.add_required_symbol("sym1");
        let sym2 = context.add_required_symbol("sym2");
        let sym_asdf = context.add_required_symbol("asdf");
        let sym_jkl = context.add_required_symbol("jkl");

        /*
        (sym1 5)
        (sym2
            (asdf 100)
            (jkl 250)
            (1
                (5 "foo")
                (10 "baz")
            )
        )
        (4 4)
        */
        #[rustfmt::skip] // using DTA-style formatting here
        let mut dest = arson_array![
            arson_array![sym1.clone(), 5],
            arson_array![sym2.clone(),
                arson_array![sym_asdf.clone(), 100],
                arson_array![sym_jkl.clone(), 250],
                arson_array![1,
                    arson_array![5, "foo"],
                    arson_array![10, "baz"],
                ],
            ],
            arson_array![4, 4],
        ];

        /*
        (sym1 1)
        (sym2
            (asdf 10)
            (jkl 50)
            (1
                (10 "bar")
            )
        )
        (3 3)
        */
        #[rustfmt::skip]
        let source = arson_array![
            arson_array![sym1.clone(), 1],
            arson_array![sym2.clone(),
                arson_array![sym_asdf.clone(), 10],
                arson_array![sym_jkl.clone(), 50],
                arson_array![1,
                    arson_array![10, "bar"],
                ],
            ],
            arson_array![3, 3],
        ];

        /*
        (sym1 5)
        (sym2
            (asdf 100)
            (jkl 250)
            (1
                (5 "foo")
                (10 "baz")
            )
        )
        (4 4)
        (3 3)
        */
        #[rustfmt::skip]
        let dest_expected = arson_array![
            arson_array![sym1.clone(), 5],
            arson_array![sym2.clone(),
                arson_array![sym_asdf.clone(), 100],
                arson_array![sym_jkl.clone(), 250],
                arson_array![1,
                    arson_array![5, "foo"],
                    arson_array![10, "baz"],
                ],
            ],
            arson_array![4, 4],
            arson_array![3, 3],
        ];
        let source_expected = source.clone();

        dest.merge_tags(&source);
        assert_eq!(dest, dest_expected);
        assert_eq!(source, source_expected);
    }

    #[test]
    fn replace_tags() {
        let mut context = Context::new();

        let sym1 = context.add_required_symbol("sym1");
        let sym2 = context.add_required_symbol("sym2");
        let sym_asdf = context.add_required_symbol("asdf");
        let sym_jkl = context.add_required_symbol("jkl");

        /*
        (sym1 5)
        (sym2
            (asdf 100)
            (jkl 250)
            (1
                (5 "foo")
                (10 "baz")
            )
        )
        (4 4)
        */
        #[rustfmt::skip] // using DTA-style formatting here
        let mut dest = arson_array![
            arson_array![sym1.clone(), 5],
            arson_array![sym2.clone(),
                arson_array![sym_asdf.clone(), 100],
                arson_array![sym_jkl.clone(), 250],
                arson_array![1,
                    arson_array![5, "foo"],
                    arson_array![10, "baz"],
                ],
            ],
            arson_array![4, 4],
        ];

        /*
        (sym1 1)
        (sym2
            (asdf 10)
            (jkl 50)
            (1
                (10 "bar")
            )
        )
        (3 3)
        */
        #[rustfmt::skip]
        let source = arson_array![
            arson_array![sym1.clone(), 1],
            arson_array![sym2.clone(),
                arson_array![sym_asdf.clone(), 10],
                arson_array![sym_jkl.clone(), 50],
                arson_array![1,
                    arson_array![10, "bar"],
                ],
            ],
            arson_array![3, 3],
        ];

        /*
        (sym1 1)
        (sym2
            (asdf 10)
            (jkl 50)
            (1
                (5 "foo")
                (10 "bar")
            )
        )
        (4 4)
        */
        #[rustfmt::skip]
        let dest_expected = arson_array![
            arson_array![sym1.clone(), 1],
            arson_array![sym2.clone(),
                arson_array![sym_asdf.clone(), 10],
                arson_array![sym_jkl.clone(), 50],
                arson_array![1,
                    arson_array![5, "foo"],
                    arson_array![10, "bar"],
                ],
            ],
            arson_array![4, 4],
        ];
        let source_expected = source.clone();

        dest.replace_tags(&source);
        assert_eq!(dest, dest_expected);
        assert_eq!(source, source_expected);
    }

    mod display {
        use super::*;

        fn assert_display(array: impl fmt::Display, standard: &str, pretty: &str) {
            assert_eq!(array.to_string(), standard);
            assert_eq!(format!("{array:#}"), pretty);
        }

        fn assert_display_node(node: NodeValue, expected: &str) {
            assert_display(arson_array![node], expected, expected)
        }

        #[test]
        fn integer() {
            use std::num::Wrapping;

            assert_display_node(NodeValue::Integer(Wrapping(1)), "(1)");
            assert_display_node(NodeValue::Integer(Wrapping(458243)), "(458243)");
            assert_display_node(NodeValue::Integer(Wrapping(-235725)), "(-235725)");
            assert_display_node(NodeValue::Integer(Wrapping(i64::MAX)), "(9223372036854775807)");
            assert_display_node(NodeValue::Integer(Wrapping(i64::MIN)), "(-9223372036854775808)");
        }

        #[test]
        fn float() {
            assert_display_node(NodeValue::Float(0.000_000_000_001), "(1e-12)");
            assert_display_node(NodeValue::Float(0.000_000_001), "(1e-9)");
            assert_display_node(NodeValue::Float(0.000_001), "(1e-6)");
            assert_display_node(NodeValue::Float(0.001), "(0.001)");
            assert_display_node(NodeValue::Float(1.0), "(1.0)");
            assert_display_node(NodeValue::Float(1_000.0), "(1000.0)");
            assert_display_node(NodeValue::Float(1_000_000.0), "(1000000.0)");
            assert_display_node(NodeValue::Float(1_000_000_000.0), "(1000000000.0)");
            assert_display_node(NodeValue::Float(1_000_000_000_000.0), "(1000000000000.0)");
        }

        #[test]
        fn string() {
            assert_display_node(NodeValue::String("asdf".to_owned().into()), "(\"asdf\")");
            assert_display_node(NodeValue::String("\"asdf\"".to_owned().into()), "(\"\\qasdf\\q\")");
            assert_display_node(NodeValue::String("asdf\n".to_owned().into()), "(\"asdf\\n\")");
        }

        #[test]
        fn symbol() {
            let mut context = Context::new();
            let sym = context.add_required_symbol("sym");
            let sym_space = context.add_required_symbol("sym with\nwhitespace");

            assert_display_node(NodeValue::Symbol(sym), "(sym)");
            assert_display_node(NodeValue::Symbol(sym_space), "('sym with\nwhitespace')");
        }

        #[test]
        fn variable() {
            let mut context = Context::new();
            let var = Variable::new_required("var", &mut context);
            let dollar_var = Variable::new_required("$var", &mut context);

            assert_display_node(NodeValue::Variable(var), "($var)");
            assert_display_node(NodeValue::Variable(dollar_var), "($$var)");
        }

        #[test]
        fn unhandled() {
            assert_display_node(NodeValue::Unhandled, "(kDataUnhandled)");
        }

        #[test]
        fn arrays() {
            assert_display(arson_array![1, 2, 3], "(1 2 3)", "(1 2 3)");
            assert_display(NodeCommand::from(arson_array![1, 2, 3]), "{1 2 3}", "{1 2 3}");
            assert_display(NodeProperty::from(arson_array![1, 2, 3]), "[1 2 3]", "[1 2 3]");
        }

        #[test]
        fn inner_arrays() {
            let mut context = Context::new();
            let sym1 = context.add_required_symbol("sym1");
            let sym2 = context.add_required_symbol("sym2");

            assert_display(
                arson_array![sym1.clone(), arson_array![sym2.clone(), 100]],
                "(sym1 (sym2 100))",
                "(sym1 (sym2 100))",
            );
            assert_display(
                arson_array![sym1.clone(), NodeCommand::from(arson_array![sym2.clone(), 100])],
                "(sym1 {sym2 100})",
                "(sym1 {sym2 100})",
            );
            assert_display(
                arson_array![sym1.clone(), NodeProperty::from(arson_array![sym2.clone(), 100])],
                "(sym1 [sym2 100])",
                "(sym1 [sym2 100])",
            );
        }

        #[test]
        fn multiple_arrays() {
            let mut context = Context::new();

            let sym1 = context.add_required_symbol("sym1");
            let sym2 = context.add_required_symbol("sym2");

            #[rustfmt::skip] // preserve correlation between array formatting and display output
            assert_display(
                arson_array![sym1.clone(),
                    arson_array![10, 20, 30],
                    NodeCommand::from(arson_array![sym2.clone(), 100]),
                    NodeProperty::from(arson_array![sym2.clone()]),
                ],
                "(sym1 (10 20 30) {sym2 100} [sym2])",
                "(sym1\
               \n   (10 20 30)\
               \n   {sym2 100}\
               \n   [sym2]\
               \n)",
            );
        }

        #[test]
        fn big_example() {
            let mut context = Context::new();

            let sym1 = context.add_required_symbol("sym1");
            let sym2 = context.add_required_symbol("sym2");
            let sym_asdf = context.add_required_symbol("asdf");
            let sym_jkl = context.add_required_symbol("jkl");

            #[rustfmt::skip] // preserve correlation between array formatting and display output
            assert_display(
                arson_array![
                    arson_array![sym1.clone(), 5],
                    arson_array![sym2.clone(),
                        arson_array![sym_asdf.clone(), 100],
                        arson_array![sym_jkl.clone(), 250],
                        arson_array![1,
                            arson_array![5, "foo"],
                            arson_array![10, "bar"],
                        ],
                    ],
                    arson_array![3, 3],
                    arson_array![4, 4],
                ],
                "((sym1 5) (sym2 (asdf 100) (jkl 250) (1 (5 \"foo\") (10 \"bar\"))) (3 3) (4 4))",
                "(\
               \n   (sym1 5)\
               \n   (sym2\
               \n      (asdf 100)\
               \n      (jkl 250)\
               \n      (\
               \n         1\
               \n         (5 \"foo\")\
               \n         (10 \"bar\")\
               \n      )\
               \n   )\
               \n   (3 3)\
               \n   (4 4)\
               \n)"
            );
        }

        #[test]
        fn indent_style() {
            fn assert_indent(array: NodeArray, expected: &str, indentation: ArrayIndentation) {
                let options = ArrayDisplayOptions { indentation, ..Default::default() };
                let display = array.display_with_options(options);
                assert_eq!(format!("{display:#}"), expected);
            }

            assert_indent(
                arson_array![arson_array![1, 2, 3], arson_array![4, 5, 6]],
                "(\
               \n   (1 2 3)\
               \n   (4 5 6)\
               \n)",
                ArrayIndentation::Spaces(3),
            );

            assert_indent(
                arson_array![arson_array![1, 2, 3], arson_array![4, 5, 6]],
                "(\
               \n        (1 2 3)\
               \n        (4 5 6)\
               \n)",
                ArrayIndentation::Spaces(8),
            );

            assert_indent(
                arson_array![arson_array![1, 2, 3], arson_array![4, 5, 6]],
                "(\
               \n\t(1 2 3)\
               \n\t(4 5 6)\
               \n)",
                ArrayIndentation::Tabs,
            );
        }

        #[test]
        fn max_line_width() {
            fn assert_width(array: NodeArray, expected: &str, width: usize) {
                let options = ArrayDisplayOptions { max_array_width: width, ..Default::default() };
                let display = array.display_with_options(options);
                assert_eq!(format!("{display:#}"), expected);
            }

            assert_width(
                arson_array!["1234567890", "1234567890", "1234567890", "1234567890", "1234567890"],
                "(\
               \n   \"1234567890\"\
               \n   \"1234567890\"\
               \n   \"1234567890\"\
               \n   \"1234567890\"\
               \n   \"1234567890\"\
               \n)",
                60,
            );
            assert_width(
                arson_array!["1234567890", "1234567890", "1234567890", "1234567890", "1234567890"],
                "(\"1234567890\" \"1234567890\" \"1234567890\" \"1234567890\" \"1234567890\")",
                80,
            );
            assert_width(
                arson_array!["1234567890", "1234567890"],
                "(\
               \n   \"1234567890\"\
               \n   \"1234567890\"\
               \n)",
                25,
            );
            assert_width(
                arson_array!["1234567890", "1234567890"],
                "(\"1234567890\" \"1234567890\")",
                30,
            );
        }
    }
}
