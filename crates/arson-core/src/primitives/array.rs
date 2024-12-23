// SPDX-License-Identifier: LGPL-3.0-or-later

use std::cell::{Cell, RefCell};
use std::fmt::{self, Write};
use std::ops::Range;
use std::rc::Rc;
use std::slice::SliceIndex;

pub use arson_parse::ArrayKind;

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

#[derive(thiserror::Error, Debug)]
pub enum ArrayError {
    #[error("bad array length {actual}, expected {expected}")]
    LengthMismatch { expected: usize, actual: usize },

    #[error("array index outside of range {0:?}")]
    OutOfRange(Range<usize>),

    #[error("requested data for {0} was not found")]
    NotFound(String),

    #[error("array already mutably borrowed: {0:?}")]
    BadBorrow(#[from] std::cell::BorrowError),

    #[error("array already immutably borrowed: {0:?}")]
    BadMutBorrow(#[from] std::cell::BorrowMutError),
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

    pub fn borrow(&self) -> crate::Result<std::cell::Ref<'_, NodeArray>> {
        self.inner.try_borrow().map_err(|e| ArrayError::BadBorrow(e).into())
    }

    pub fn borrow_mut(&self) -> crate::Result<std::cell::RefMut<'_, NodeArray>> {
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

    pub fn display_evaluated<'a, S>(&'a self, context: &'a mut Context<S>) -> ArrayDisplay<'a, S> {
        ArrayDisplay::new(&self.nodes, ArrayKind::Array, context)
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
            Ok($default())
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

    pub fn evaluate<S>(&self, context: &mut Context<S>, index: usize) -> crate::Result<NodeValue> {
        self.get(index)?.evaluate(context)
    }

    pub fn integer<S>(&self, context: &mut Context<S>, index: usize) -> crate::Result<Integer> {
        self.get(index)?.integer(context)
    }

    pub fn float<S>(&self, context: &mut Context<S>, index: usize) -> crate::Result<FloatValue> {
        self.get(index)?.float(context)
    }

    pub fn number<S>(&self, context: &mut Context<S>, index: usize) -> crate::Result<Number> {
        self.get(index)?.number(context)
    }

    pub fn size_integer<S>(&self, context: &mut Context<S>, index: usize) -> crate::Result<usize> {
        self.get(index)?.size_integer(context)
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

    pub fn force_symbol<S>(&self, context: &mut Context<S>, index: usize) -> crate::Result<Symbol> {
        self.get(index)?.force_symbol(context)
    }

    pub fn array_tag<S>(&self, context: &mut Context<S>, index: usize) -> crate::Result<ArrayTag> {
        self.get(index)?.array_tag(context)
    }

    pub fn variable(&self, index: usize) -> crate::Result<&Variable> {
        self.get(index)?.variable()
    }

    pub fn set_variable<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        value: impl Into<Node>,
    ) -> crate::Result {
        self.get(index)?.set_variable(context, value)
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
}

// Optional data retrieval by index
impl NodeSlice {
    pub fn unevaluated_opt(&self, index: usize) -> Option<&NodeValue> {
        self.get_opt(index).map(|n| n.unevaluated())
    }

    pub fn evaluate_opt<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
    ) -> Option<crate::Result<NodeValue>> {
        self.get_opt(index).map(|n| n.evaluate(context))
    }

    pub fn integer_opt<S>(&self, context: &mut Context<S>, index: usize) -> Option<crate::Result<Integer>> {
        self.get_opt(index).and_then(|n| n.integer_opt(context))
    }

    pub fn float_opt<S>(&self, context: &mut Context<S>, index: usize) -> Option<crate::Result<FloatValue>> {
        self.get_opt(index).and_then(|n| n.float_opt(context))
    }

    pub fn number_opt<S>(&self, context: &mut Context<S>, index: usize) -> Option<crate::Result<Number>> {
        self.get_opt(index).and_then(|n| n.number_opt(context))
    }

    pub fn size_integer_opt<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
    ) -> Option<crate::Result<usize>> {
        self.get_opt(index).and_then(|n| n.size_integer_opt(context))
    }

    pub fn boolean_opt<S>(&self, context: &mut Context<S>, index: usize) -> Option<crate::Result<bool>> {
        self.get_opt(index).and_then(|n| n.boolean_opt(context))
    }

    pub fn string_opt<S>(&self, context: &mut Context<S>, index: usize) -> Option<crate::Result<Rc<String>>> {
        self.get_opt(index).and_then(|n| n.string_opt(context))
    }

    pub fn symbol_opt<S>(&self, context: &mut Context<S>, index: usize) -> Option<crate::Result<Symbol>> {
        self.get_opt(index).and_then(|n| n.symbol_opt(context))
    }

    pub fn force_symbol_opt<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
    ) -> Option<crate::Result<Symbol>> {
        self.get_opt(index).and_then(|n| n.force_symbol_opt(context))
    }

    pub fn array_tag_opt<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
    ) -> Option<crate::Result<ArrayTag>> {
        self.get_opt(index).and_then(|n| n.array_tag_opt(context))
    }

    pub fn variable_opt(&self, index: usize) -> Option<&Variable> {
        self.get_opt(index).and_then(|n| n.variable_opt())
    }

    pub fn set_variable_opt<S>(&self, context: &mut Context<S>, index: usize, value: impl Into<Node>) {
        if let Some(node) = self.get_opt(index) {
            node.set_variable_opt(context, value);
        }
    }

    pub fn array_opt<S>(&self, context: &mut Context<S>, index: usize) -> Option<crate::Result<ArrayRef>> {
        self.get_opt(index)?.array_opt(context)
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

    pub fn evaluate_or<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: &NodeValue,
    ) -> crate::Result<NodeValue> {
        self.get_opt(index)
            .map(|n| n.evaluate(context))
            .unwrap_or_else(|| Ok(default.clone()))
    }

    pub fn integer_or<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: Integer,
    ) -> crate::Result<Integer> {
        index_value_or!(self, context, index, Node::integer_or, default)
    }

    pub fn float_or<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: FloatValue,
    ) -> crate::Result<FloatValue> {
        index_value_or!(self, context, index, Node::float_or, default)
    }

    pub fn number_or<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: Number,
    ) -> crate::Result<Number> {
        index_value_or!(self, context, index, Node::number_or, default)
    }

    pub fn size_integer_or<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: usize,
    ) -> crate::Result<usize> {
        index_value_or!(self, context, index, Node::size_integer_or, default)
    }

    pub fn boolean_or<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: bool,
    ) -> crate::Result<bool> {
        index_value_or!(self, context, index, Node::boolean_or, default)
    }

    pub fn string_or<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: &Rc<String>,
    ) -> crate::Result<Rc<String>> {
        index_value_or!(self, context, index, Node::string_or, default)
    }

    pub fn symbol_or<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: impl IntoSymbol,
    ) -> crate::Result<Symbol> {
        index_value_or!(self, context, index, Node::symbol_or, default => default.into_symbol(context))
    }

    pub fn force_symbol_or<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: impl IntoSymbol,
    ) -> crate::Result<Symbol> {
        index_value_or!(self, context, index, Node::force_symbol_or, default => default.into_symbol(context))
    }

    pub fn array_tag_or<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: &ArrayTag,
    ) -> crate::Result<ArrayTag> {
        index_value_or!(self, context, index, Node::array_tag_or, default)
    }

    pub fn variable_or(&self, index: usize, default: &Variable) -> Variable {
        index_value_or!(self, index, Node::variable_or, default)
    }

    pub fn array_or<S>(
        &self,
        context: &mut Context<S>,
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

    pub fn evaluate_or_else<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: impl FnOnce() -> NodeValue,
    ) -> crate::Result<NodeValue> {
        self.get_opt(index)
            .map(|n| n.evaluate(context))
            .unwrap_or_else(|| Ok(default()))
    }

    pub fn integer_or_else<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: impl FnOnce() -> Integer,
    ) -> crate::Result<Integer> {
        index_value_or_else!(self, context, index, Node::integer_or_else, default)
    }

    pub fn float_or_else<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: impl FnOnce() -> FloatValue,
    ) -> crate::Result<FloatValue> {
        index_value_or_else!(self, context, index, Node::float_or_else, default)
    }

    pub fn number_or_else<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: impl FnOnce() -> Number,
    ) -> crate::Result<Number> {
        index_value_or_else!(self, context, index, Node::number_or_else, default)
    }

    pub fn size_integer_or_else<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: impl FnOnce() -> usize,
    ) -> crate::Result<usize> {
        index_value_or_else!(self, context, index, Node::size_integer_or_else, default)
    }

    pub fn boolean_or_else<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: impl FnOnce() -> bool,
    ) -> crate::Result<bool> {
        index_value_or_else!(self, context, index, Node::boolean_or_else, default)
    }

    pub fn string_or_else<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: impl FnOnce() -> Rc<String>,
    ) -> crate::Result<Rc<String>> {
        index_value_or_else!(self, context, index, Node::string_or_else, default)
    }

    pub fn symbol_or_else<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: impl FnOnce() -> Symbol,
    ) -> crate::Result<Symbol> {
        index_value_or_else!(self, context, index, Node::symbol_or_else, default)
    }

    pub fn force_symbol_or_else<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: impl FnOnce() -> Symbol,
    ) -> crate::Result<Symbol> {
        index_value_or_else!(self, context, index, Node::force_symbol_or_else, default)
    }

    pub fn array_tag_or_else<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: impl FnOnce() -> ArrayTag,
    ) -> crate::Result<ArrayTag> {
        index_value_or_else!(self, context, index, Node::array_tag_or_else, default)
    }

    pub fn variable_or_else(&self, index: usize, default: impl FnOnce() -> Variable) -> Variable {
        index_value_or_else!(self, index, Node::variable_or_else, default)
    }

    pub fn array_or_else<S>(
        &self,
        context: &mut Context<S>,
        index: usize,
        default: impl FnOnce() -> ArrayRef,
    ) -> crate::Result<ArrayRef> {
        index_value_or_else!(self, context, index, Node::array_or_else, default)
    }

    pub fn command_or_else(
        &self,
        index: usize,
        default: impl FnOnce() -> Rc<NodeCommand>,
    ) -> Rc<NodeCommand> {
        index_value_or_else!(self, index, Node::command_or_else, default)
    }

    pub fn property_or_else(
        &self,
        index: usize,
        default: impl FnOnce() -> Rc<NodeProperty>,
    ) -> Rc<NodeProperty> {
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

            // Using a blanket implementation for this isn't possible due to the
            // impl for Fn(&NodeValue) -> bool, which inherently handles references
            impl FindDataPredicate for &$type {
                fn matches(&self, value: &NodeValue) -> bool {
                    FindDataPredicate::matches(*self, value)
                }

                fn err_string(&self) -> String {
                    FindDataPredicate::err_string(*self)
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

impl<F: Fn(&NodeValue) -> bool> FindDataPredicate for F {
    fn matches(&self, value: &NodeValue) -> bool {
        self(value)
    }

    fn err_string(&self) -> String {
        "custom predicate".to_owned()
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

impl<S, T: AsRef<str>> IntoDataPredicate for (&mut Context<S>, T) {
    type Target = Symbol;

    fn into_predicate(self) -> Self::Target {
        self.0.add_symbol(self.1.as_ref())
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

macro_rules! predicate_value_or {
    ($self:ident, $context:ident, $predicate:ident, $map:expr, $default:ident) => {
        value_or!(
            &$self.find_data_opt($predicate.into_predicate($context)),
            $context,
            $map,
            $default
        )
    };
    ($self:ident, $context:ident, $predicate:ident, $map:expr, $default:ident => $make_default:expr) => {
        value_or!(
            &$self.find_data_opt($predicate.into_predicate($context)),
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
            &$self.find_data_opt($predicate.into_predicate($context)),
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
        let predicate = predicate.into_predicate();
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

        Err(ArrayError::NotFound(predicate.err_string()).into())
    }

    pub fn find_data(&self, predicate: impl IntoDataPredicate) -> crate::Result<Node> {
        let array = self.find_tag(predicate)?;
        let array = array.borrow()?;
        arson_assert_len!(array, 2, "Expected only one value in array");
        array.get(1).cloned()
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

    pub fn find_size_integer<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<usize> {
        self.find_data(predicate.into_predicate(context))?.size_integer(context)
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

    pub fn find_symbol_forced<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<Symbol> {
        self.find_data(predicate.into_predicate(context))?.force_symbol(context)
    }

    pub fn find_array_tag<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<ArrayTag> {
        self.find_data(predicate.into_predicate(context))?.array_tag(context)
    }

    pub fn find_variable(&self, predicate: impl IntoDataPredicate) -> crate::Result<Variable> {
        self.find_data(predicate)?.variable().cloned()
    }

    pub fn find_array<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> crate::Result<ArrayRef> {
        self.find_data(predicate.into_predicate(context))?.array(context)
    }

    pub fn find_command(&self, predicate: impl IntoDataPredicate) -> crate::Result<Rc<NodeCommand>> {
        self.find_data(predicate)?.command().cloned()
    }

    pub fn find_property(&self, predicate: impl IntoDataPredicate) -> crate::Result<Rc<NodeProperty>> {
        self.find_data(predicate)?.property().cloned()
    }
}

// Optional data retrieval by predicate
impl NodeSlice {
    pub fn find_tag_opt(&self, predicate: impl IntoDataPredicate) -> Option<ArrayRef> {
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

    pub fn find_integer_opt<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<Integer>> {
        self.find_data_opt(predicate.into_predicate(context))?.integer_opt(context)
    }

    pub fn find_float_opt<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<FloatValue>> {
        self.find_data_opt(predicate.into_predicate(context))?.float_opt(context)
    }

    pub fn find_number_opt<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<Number>> {
        self.find_data_opt(predicate.into_predicate(context))?.number_opt(context)
    }

    pub fn find_size_integer_opt<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<usize>> {
        self.find_data_opt(predicate.into_predicate(context))?
            .size_integer_opt(context)
    }

    pub fn find_boolean_opt<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<bool>> {
        self.find_data_opt(predicate.into_predicate(context))?.boolean_opt(context)
    }

    pub fn find_string_opt<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<Rc<String>>> {
        self.find_data_opt(predicate.into_predicate(context))?.string_opt(context)
    }

    pub fn find_symbol_opt<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<Symbol>> {
        self.find_data_opt(predicate.into_predicate(context))?.symbol_opt(context)
    }

    pub fn find_symbol_forced_opt<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<Symbol>> {
        self.find_data_opt(predicate.into_predicate(context))?
            .force_symbol_opt(context)
    }

    pub fn find_array_tag_opt<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<ArrayTag>> {
        self.find_data_opt(predicate.into_predicate(context))?
            .array_tag_opt(context)
    }

    pub fn find_variable_opt(&self, predicate: impl IntoDataPredicate) -> Option<Variable> {
        self.find_data_opt(predicate)?.variable_opt().cloned()
    }

    pub fn find_array_opt<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
    ) -> Option<crate::Result<ArrayRef>> {
        self.find_data_opt(predicate.into_predicate(context))?.array_opt(context)
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

    pub fn find_integer_or<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: Integer,
    ) -> crate::Result<Integer> {
        predicate_value_or!(self, context, predicate, Node::integer_or, default)
    }

    pub fn find_float_or<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: FloatValue,
    ) -> crate::Result<FloatValue> {
        predicate_value_or!(self, context, predicate, Node::float_or, default)
    }

    pub fn find_number_or<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: Number,
    ) -> crate::Result<Number> {
        predicate_value_or!(self, context, predicate, Node::number_or, default)
    }

    pub fn find_size_integer_or<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: usize,
    ) -> crate::Result<usize> {
        predicate_value_or!(self, context, predicate, Node::size_integer_or, default)
    }

    pub fn find_boolean_or<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: bool,
    ) -> crate::Result<bool> {
        predicate_value_or!(self, context, predicate, Node::boolean_or, default)
    }

    pub fn find_string_or<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: &Rc<String>,
    ) -> crate::Result<Rc<String>> {
        predicate_value_or!(self, context, predicate, Node::string_or, default)
    }

    pub fn find_symbol_or<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: impl IntoSymbol,
    ) -> crate::Result<Symbol> {
        predicate_value_or!(self, context, predicate, Node::symbol_or, default => default.into_symbol(context))
    }

    pub fn find_force_symbol_or<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: impl IntoSymbol,
    ) -> crate::Result<Symbol> {
        predicate_value_or!(self, context, predicate, Node::force_symbol_or, default => default.into_symbol(context))
    }

    pub fn find_array_tag_or<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: &ArrayTag,
    ) -> crate::Result<ArrayTag> {
        predicate_value_or!(self, context, predicate, Node::array_tag_or, default)
    }

    pub fn find_variable_or(&self, predicate: impl IntoDataPredicate, default: &Variable) -> Variable {
        predicate_value_or!(self, predicate, Node::variable_or, default)
    }

    pub fn find_array_or<S>(
        &self,
        context: &mut Context<S>,
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
        default: impl FnOnce() -> ArrayRef,
    ) -> ArrayRef {
        self.find_tag_opt(predicate).unwrap_or_else(default)
    }

    pub fn find_data_or_else(
        &self,
        predicate: impl IntoDataPredicate,
        default: impl FnOnce() -> Node,
    ) -> Node {
        self.find_data_opt(predicate).unwrap_or_else(default)
    }

    pub fn find_integer_or_else<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce() -> Integer,
    ) -> crate::Result<Integer> {
        predicate_value_or_else!(self, context, predicate, Node::integer_or_else, default)
    }

    pub fn find_float_or_else<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce() -> FloatValue,
    ) -> crate::Result<FloatValue> {
        predicate_value_or_else!(self, context, predicate, Node::float_or_else, default)
    }

    pub fn find_number_or_else<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce() -> Number,
    ) -> crate::Result<Number> {
        predicate_value_or_else!(self, context, predicate, Node::number_or_else, default)
    }

    pub fn find_size_integer_or_else<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce() -> usize,
    ) -> crate::Result<usize> {
        predicate_value_or_else!(self, context, predicate, Node::size_integer_or_else, default)
    }

    pub fn find_boolean_or_else<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce() -> bool,
    ) -> crate::Result<bool> {
        predicate_value_or_else!(self, context, predicate, Node::boolean_or_else, default)
    }

    pub fn find_string_or_else<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce() -> Rc<String>,
    ) -> crate::Result<Rc<String>> {
        predicate_value_or_else!(self, context, predicate, Node::string_or_else, default)
    }

    pub fn find_symbol_or_else<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce() -> Symbol,
    ) -> crate::Result<Symbol> {
        predicate_value_or_else!(self, context, predicate, Node::symbol_or_else, default)
    }

    pub fn find_force_symbol_or_else<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce() -> Symbol,
    ) -> crate::Result<Symbol> {
        predicate_value_or_else!(self, context, predicate, Node::force_symbol_or_else, default)
    }

    pub fn find_array_tag_or_else<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce() -> ArrayTag,
    ) -> crate::Result<ArrayTag> {
        predicate_value_or_else!(self, context, predicate, Node::array_tag_or_else, default)
    }

    pub fn find_variable_or_else(
        &self,
        predicate: impl IntoDataPredicate,
        default: impl FnOnce() -> Variable,
    ) -> Variable {
        predicate_value_or_else!(self, predicate, Node::variable_or_else, default)
    }

    pub fn find_array_or_else<S>(
        &self,
        context: &mut Context<S>,
        predicate: impl IntoIntoDataPredicate,
        default: impl FnOnce() -> ArrayRef,
    ) -> crate::Result<ArrayRef> {
        predicate_value_or_else!(self, context, predicate, Node::array_or_else, default)
    }

    pub fn find_command_or_else(
        &self,
        predicate: impl IntoDataPredicate,
        default: impl FnOnce() -> Rc<NodeCommand>,
    ) -> Rc<NodeCommand> {
        predicate_value_or_else!(self, predicate, Node::command_or_else, default)
    }

    pub fn find_property_or_else(
        &self,
        predicate: impl IntoDataPredicate,
        default: impl FnOnce() -> Rc<NodeProperty>,
    ) -> Rc<NodeProperty> {
        predicate_value_or_else!(self, predicate, Node::property_or_else, default)
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

    pub fn display_evaluated<'a, S>(&'a self, context: &'a mut Context<S>) -> ArrayDisplay<'a, S> {
        ArrayDisplay::new(&self.nodes, ArrayKind::Array, context)
    }
}

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
        Err(ArrayError::OutOfRange(0..length).into())
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

            let predicate: &dyn Fn(&NodeValue) -> bool = match source_tag.unevaluated() {
                NodeValue::Symbol(left) => &move |right| *left == *right,
                NodeValue::Integer(left) => &move |right| *left == *right,
                _ => continue,
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

                let predicate: &dyn Fn(&NodeValue) -> bool = match dest_tag.unevaluated() {
                    NodeValue::Symbol(left) => &move |right| *left == *right,
                    NodeValue::Integer(left) => &move |right| *left == *right,
                    _ => continue,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn merge_tags() {
        let mut context = crate::make_test_context(());

        let sym1 = context.add_symbol("sym1");
        let sym2 = context.add_symbol("sym2");
        let sym_asdf = context.add_symbol("asdf");
        let sym_jkl = context.add_symbol("jkl");

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
        (3 3)
        (4 4)
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
        let mut context = crate::make_test_context(());

        let sym1 = context.add_symbol("sym1");
        let sym2 = context.add_symbol("sym2");
        let sym_asdf = context.add_symbol("asdf");
        let sym_jkl = context.add_symbol("jkl");

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
}
