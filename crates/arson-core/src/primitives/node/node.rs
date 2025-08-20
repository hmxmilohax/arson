// SPDX-License-Identifier: LGPL-3.0-or-later

use std::cmp::Ordering;
use std::fmt;
use std::rc::Rc;

use crate::prelude::*;
use crate::primitives::*;

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum EvaluationError {
    #[error("expected value of type {expected:?}, got {actual:?} instead")]
    TypeMismatch { expected: NodeKind, actual: NodeKind },

    #[error("value of type {src:?} is not convertible to {dest:?}")]
    NotConvertible { src: NodeKind, dest: NodeKind },

    #[error("value of type {0:?} is not a number")]
    NotNumber(NodeKind),

    #[error("value of type {0:?} is not a valid array tag")]
    NotArrayTag(NodeKind),
}

#[derive(Clone, Default)]
pub struct Node {
    pub(super) value: NodeValue,
}

macro_rules! match_value {
    ($value:expr, $kind:ident($inner:ident) => $expr:expr) => {{
        let value = $value;
        match value {
            NodeValue::$kind($inner) => Ok($expr),
            _ => Err(EvaluationError::TypeMismatch {
                expected: NodeKind::$kind,
                actual: value.get_kind(),
            }
            .into()),
        }
    }};
}

macro_rules! match_option {
    ($value:expr, |$evaluated:ident| $expr:expr, $kind:ident) => {
        match_option_with_err!($value, |$evaluated| $expr, EvaluationError::NotConvertible {
            src: $evaluated.get_kind(),
            dest: NodeKind::$kind
        })
    };
}

macro_rules! match_option_with_err {
    ($value:expr, |$evaluated:ident| $expr:expr, $err:expr) => {{
        let $evaluated = $value;
        match $expr {
            Some(value) => Ok(value),
            None => Err($err.into()),
        }
    }};
}

impl Node {
    /// Value returned when a script call has not been handled by a receiver.
    pub const UNHANDLED: Node = Self { value: NodeValue::Unhandled };
    /// Generic value to be returned when a script call has been handled,
    /// but no specific value is returned from the method handling the call.
    pub const HANDLED: Node = Self { value: NodeValue::HANDLED };

    /// Boolean TRUE value.
    pub const TRUE: Node = Self { value: NodeValue::TRUE };
    /// Boolean FALSE value.
    pub const FALSE: Node = Self { value: NodeValue::FALSE };

    pub const fn get_kind(&self) -> NodeKind {
        self.value.get_kind()
    }

    #[inline]
    pub const fn unevaluated(&self) -> &NodeValue {
        &self.value
    }

    pub fn evaluate(&self, context: &mut Context) -> crate::Result<NodeValue> {
        let evaluated = match self.unevaluated() {
            NodeValue::Integer(value) => NodeValue::Integer(*value),
            NodeValue::Float(value) => NodeValue::Float(*value),
            NodeValue::String(value) => NodeValue::String(value.clone()),
            NodeValue::Symbol(value) => NodeValue::Symbol(value.clone()),
            NodeValue::Variable(variable) => variable.get(context).value,

            NodeValue::Function(value) => NodeValue::Function(value.clone()),
            NodeValue::Object(value) => NodeValue::Object(value.clone()),

            NodeValue::Array(value) => NodeValue::from(value),
            NodeValue::Command(value) => context.execute(value)?.value,
            NodeValue::Property(_property) => todo!("property node evaluation"),

            NodeValue::Unhandled => NodeValue::Unhandled,
        };
        Ok(evaluated)
    }

    pub fn integer(&self, context: &mut Context) -> crate::Result<Integer> {
        match_option!(self.evaluate(context)?, |value| value.integer(), Integer)
    }

    pub fn float(&self, context: &mut Context) -> crate::Result<FloatValue> {
        match_option!(self.evaluate(context)?, |value| value.float(), Float)
    }

    pub fn number(&self, context: &mut Context) -> crate::Result<Number> {
        match_option_with_err!(
            self.evaluate(context)?,
            |value| value.number(),
            EvaluationError::NotNumber(value.get_kind())
        )
    }

    pub fn size_integer(&self, context: &mut Context) -> crate::Result<usize> {
        // inlined match_option!, not making a variant just for this lol
        let value = self.evaluate(context)?;
        match value.size_integer() {
            Some(value) => value, // this needs to be returned as-is instead of wrapped in Ok()
            None => {
                Err(EvaluationError::NotConvertible { src: value.get_kind(), dest: NodeKind::Integer }.into())
            },
        }
    }

    pub fn boolean(&self, context: &mut Context) -> crate::Result<bool> {
        Ok(self.evaluate(context)?.boolean())
    }

    pub fn string(&self, context: &mut Context) -> crate::Result<Rc<String>> {
        match_option!(self.evaluate(context)?, |value| value.string().cloned(), String)
    }

    pub fn symbol(&self, context: &mut Context) -> crate::Result<Symbol> {
        match_value!(self.evaluate(context)?, Symbol(value) => value)
    }

    pub fn force_symbol(&self, context: &mut Context) -> crate::Result<Symbol> {
        let value = self.evaluate(context)?;
        match value.force_symbol(context) {
            Some(value) => value,
            None => {
                Err(EvaluationError::NotConvertible { src: value.get_kind(), dest: NodeKind::Symbol }.into())
            },
        }
    }

    pub fn array_tag(&self, context: &mut Context) -> crate::Result<ArrayTag> {
        match_option_with_err!(
            self.evaluate(context)?,
            |value| value.array_tag(),
            EvaluationError::NotArrayTag(value.get_kind())
        )
    }

    pub fn variable(&self) -> crate::Result<&Variable> {
        match_value!(self.unevaluated(), Variable(value) => value)
    }

    pub fn set_variable(&self, context: &mut Context, value: impl Into<Node>) -> crate::Result<Option<Node>> {
        let old = match self.unevaluated() {
            NodeValue::Variable(var) => var.set(context, value),
            NodeValue::Property(_prop) => todo!("set_variable property access"),
            unhandled => arson_fail!("Cannot set non-variable value {:?}", unhandled),
        };
        Ok(old)
    }

    pub fn object(&self, context: &mut Context) -> crate::Result<ObjectRef> {
        match_value!(self.evaluate(context)?, Object(value) => value)
    }

    pub fn array(&self, context: &mut Context) -> crate::Result<ArrayRef> {
        match_value!(self.evaluate(context)?, Array(value) => value)
    }

    pub fn command(&self) -> crate::Result<&NodeCommand> {
        match_value!(self.unevaluated(), Command(value) => value)
    }

    pub fn property(&self) -> crate::Result<&NodeProperty> {
        match_value!(self.unevaluated(), Property(value) => value)
    }

    pub fn total_cmp(&self, other: &Self) -> Ordering {
        self.value.total_cmp(&other.value)
    }
}

// Quick 'n easy variant checks
impl Node {
    pub const fn is_integer(&self) -> bool {
        self.unevaluated().is_integer()
    }

    pub const fn is_float(&self) -> bool {
        self.unevaluated().is_float()
    }

    pub const fn is_number(&self) -> bool {
        self.unevaluated().is_number()
    }

    pub fn is_size_integer(&self) -> bool {
        self.unevaluated().is_size_integer()
    }

    pub const fn is_string(&self) -> bool {
        self.unevaluated().is_string()
    }

    pub const fn is_symbol(&self) -> bool {
        self.unevaluated().is_symbol()
    }

    pub const fn is_variable(&self) -> bool {
        self.unevaluated().is_variable()
    }

    pub const fn is_object(&self) -> bool {
        self.unevaluated().is_object()
    }

    pub const fn is_array(&self) -> bool {
        self.unevaluated().is_array()
    }

    pub const fn is_command(&self) -> bool {
        self.unevaluated().is_command()
    }

    pub const fn is_property(&self) -> bool {
        self.unevaluated().is_property()
    }

    pub const fn is_any_array(&self) -> bool {
        self.unevaluated().is_any_array()
    }

    pub const fn is_unhandled(&self) -> bool {
        self.unevaluated().is_unhandled()
    }
}

// Optional value retrieval
impl Node {
    pub fn integer_opt(&self, context: &mut Context) -> crate::Result<Option<Integer>> {
        self.evaluate(context).map(|n| n.integer())
    }

    pub fn float_opt(&self, context: &mut Context) -> crate::Result<Option<FloatValue>> {
        self.evaluate(context).map(|n| n.float())
    }

    pub fn number_opt(&self, context: &mut Context) -> crate::Result<Option<Number>> {
        self.evaluate(context).map(|n| n.number())
    }

    pub fn size_integer_opt(&self, context: &mut Context) -> crate::Result<Option<usize>> {
        self.evaluate(context).map(|n| n.size_integer_opt())
    }

    pub fn string_opt(&self, context: &mut Context) -> crate::Result<Option<Rc<String>>> {
        self.evaluate(context).map(|n| n.string().cloned())
    }

    pub fn symbol_opt(&self, context: &mut Context) -> crate::Result<Option<Symbol>> {
        self.evaluate(context).map(|n| n.symbol().cloned())
    }

    pub fn force_symbol_opt(&self, context: &mut Context) -> crate::Result<Option<Symbol>> {
        self.evaluate(context).and_then(|n| n.force_symbol(context).transpose())
    }

    pub fn array_tag_opt(&self, context: &mut Context) -> crate::Result<Option<ArrayTag>> {
        self.evaluate(context).map(|n| n.array_tag())
    }

    pub const fn variable_opt(&self) -> Option<&Variable> {
        self.unevaluated().variable()
    }

    pub fn set_variable_opt(&self, context: &mut Context, value: impl Into<Node>) {
        match self.unevaluated() {
            NodeValue::Variable(var) => _ = var.set(context, value),
            NodeValue::Property(_prop) => todo!("set_variable property access"),
            _ => (),
        }
    }

    pub fn object_opt(&self, context: &mut Context) -> crate::Result<Option<ObjectRef>> {
        self.evaluate(context).map(|n| n.object().cloned())
    }

    pub fn array_opt(&self, context: &mut Context) -> crate::Result<Option<ArrayRef>> {
        self.evaluate(context).map(|n| n.array().cloned())
    }

    pub const fn command_opt(&self) -> Option<&NodeCommand> {
        self.unevaluated().command()
    }

    pub const fn property_opt(&self) -> Option<&NodeProperty> {
        self.unevaluated().property()
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

impl From<&Node> for Node {
    fn from(value: &Node) -> Self {
        value.clone()
    }
}

impl<T> From<T> for Node
where
    NodeValue: From<T>,
{
    fn from(value: T) -> Self {
        Self { value: NodeValue::from(value) }
    }
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.unevaluated().fmt(f)
    }
}
