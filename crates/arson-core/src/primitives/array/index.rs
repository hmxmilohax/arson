// SPDX-License-Identifier: LGPL-3.0-or-later

use std::rc::Rc;

use crate::prelude::*;
use crate::{ArrayTag, FloatValue, Integer, IntoSymbol, Number, NumericError};

pub trait NodeSliceIndex {
    type Output: ?Sized;

    fn get(self, slice: &[Node]) -> crate::Result<&Self::Output>;
    fn get_mut(self, slice: &mut [Node]) -> crate::Result<&mut Self::Output>;

    fn get_opt(self, slice: &[Node]) -> Option<&Self::Output>;
    fn get_mut_opt(self, slice: &mut [Node]) -> Option<&mut Self::Output>;
}

impl NodeSliceIndex for usize {
    type Output = Node;

    fn get(self, slice: &[Node]) -> crate::Result<&Self::Output> {
        slice
            .get(self)
            .ok_or_else(|| NumericError::IndexOutOfRange(self, 0..slice.len()).into())
    }

    fn get_mut(self, slice: &mut [Node]) -> crate::Result<&mut Self::Output> {
        let length = slice.len(); // done here due to borrow rules
        slice
            .get_mut(self)
            .ok_or_else(|| NumericError::IndexOutOfRange(self, 0..length).into())
    }

    fn get_opt(self, slice: &[Node]) -> Option<&Self::Output> {
        slice.get(self)
    }

    fn get_mut_opt(self, slice: &mut [Node]) -> Option<&mut Self::Output> {
        slice.get_mut(self)
    }
}

macro_rules! range_error_impl {
    ($($type:tt)+) => {
        impl NodeSliceIndex for $($type)+ {
            type Output = [Node];

            fn get(self, slice: &[Node]) -> crate::Result<&Self::Output> {
                slice.get(self.clone()).ok_or_else(|| {
                    NumericError::slice_out_of_range(self, 0..slice.len()).into()
                })
            }

            fn get_mut(self, slice: &mut [Node]) -> crate::Result<&mut Self::Output> {
                let length = slice.len(); // done here due to borrow rules
                slice.get_mut(self.clone()).ok_or_else(|| {
                    NumericError::slice_out_of_range(self, 0..length).into()
                })
            }

            fn get_opt(self, slice: &[Node]) -> Option<&Self::Output> {
                slice.get(self)
            }

            fn get_mut_opt(self, slice: &mut [Node]) -> Option<&mut Self::Output> {
                slice.get_mut(self)
            }
        }
    }
}

range_error_impl!(std::ops::Range<usize>);
range_error_impl!(std::ops::RangeTo<usize>);
range_error_impl!(std::ops::RangeFrom<usize>);
range_error_impl!(std::ops::RangeFull);
range_error_impl!(std::ops::RangeInclusive<usize>);
range_error_impl!(std::ops::RangeToInclusive<usize>);
range_error_impl!((std::ops::Bound<usize>, std::ops::Bound<usize>));

// unstable
// std::range::Range<usize>
// std::range::RangeInclusive<usize>
// std::range::RangeFrom<usize>

impl NodeSlice {
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

    pub fn slice<I: NodeSliceIndex<Output = [Node]>>(&self, index: I) -> crate::Result<&NodeSlice> {
        index.get(&self.nodes).map(Self::from)
    }

    pub fn slice_mut<I: NodeSliceIndex<Output = [Node]>>(
        &mut self,
        index: I,
    ) -> crate::Result<&mut NodeSlice> {
        index.get_mut(&mut self.nodes).map(Self::from_mut)
    }
}

// helper macros
macro_rules! index_value_or {
    ($self:ident, $context:ident, $index:ident, $map:expr, $default:ident) => {
        index_value_or!($self, $context, $index, $map, $default => $default.clone())
    };
    ($self:ident, $context:ident, $index:ident, $map:expr, $default:ident => $make_default:expr) => {
        if let Some(node) = $self.get_opt($index) {
            $map(node, $context, $default)
        } else {
            Ok($make_default)
        }
    };
    ($self:ident, $index:ident, $map:expr, $default:ident) => {
        if let Some(node) = $self.get_opt($index) {
            $map(node, $default)
        } else {
            $default.clone()
        }
    };
}

macro_rules! index_value_or_else {
    ($self:ident, $context:ident, $index:ident, $map:expr, $default:ident) => {
        if let Some(node) = $self.get_opt($index) {
            $map(node, $context, $default)
        } else {
            $default($context)
        }
    };
    ($self:ident, $index:ident, $map:expr, $default:ident) => {
        if let Some(node) = $self.get_opt($index) {
            $map(node, $default)
        } else {
            $default()
        }
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

    pub fn command(&self, index: usize) -> crate::Result<&NodeCommand> {
        self.get(index)?.command()
    }

    pub fn property(&self, index: usize) -> crate::Result<&NodeProperty> {
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

    pub fn command_opt(&self, index: usize) -> Option<&NodeCommand> {
        self.get_opt(index)?.command_opt()
    }

    pub fn property_opt(&self, index: usize) -> Option<&NodeProperty> {
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

    pub fn command_or(&self, index: usize, default: &NodeCommand) -> NodeCommand {
        index_value_or!(self, index, Node::command_or, default)
    }

    pub fn property_or(&self, index: usize, default: &NodeProperty) -> NodeProperty {
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
        default: impl FnOnce() -> crate::Result<NodeCommand>,
    ) -> crate::Result<NodeCommand> {
        index_value_or_else!(self, index, Node::command_or_else, default)
    }

    pub fn property_or_else(
        &self,
        index: usize,
        default: impl FnOnce() -> crate::Result<NodeProperty>,
    ) -> crate::Result<NodeProperty> {
        index_value_or_else!(self, index, Node::property_or_else, default)
    }
}
