// SPDX-License-Identifier: LGPL-3.0-or-later

use std::rc::Rc;

use crate::{
    arson_assert_len,
    ArrayError,
    ArrayRef,
    Context,
    FloatValue,
    Integer,
    IntegerValue,
    IntoSymbol,
    Node,
    NodeCommand,
    NodeProperty,
    NodeSlice,
    NodeValue,
    Number,
    ObjectRef,
    Symbol,
    Variable,
};

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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
        predicate_value_or!($self, $context, $predicate, $map, $default => $default.clone())
    };
    ($self:ident, $context:ident, $predicate:ident, $map:expr, $default:ident => $make_default:expr) => {
        if let Some(node) = &$self.find_data_opt($predicate.into_predicate($context)?) {
            $map(node, $context, $default)
        } else {
            Ok($make_default)
        }
    };
    ($self:ident, $predicate:ident, $map:expr, $default:ident) => {
        if let Some(node) = &$self.find_data_opt($predicate) {
            $map(node, $default)
        } else {
            $default.clone()
        }
    };
}

macro_rules! predicate_value_or_else {
    ($self:ident, $context:ident, $predicate:ident, $map:expr, $default:ident) => {
        if let Some(node) = &$self.find_data_opt($predicate.into_predicate($context)?) {
            $map(node, $context, $default)
        } else {
            $default($context)
        }
    };
    ($self:ident, $predicate:ident, $map:expr, $default:ident) => {
        if let Some(node) = &$self.find_data_opt($predicate) {
            $map(node, $default)
        } else {
            $default()
        }
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
