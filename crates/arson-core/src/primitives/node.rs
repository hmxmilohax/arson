// SPDX-License-Identifier: LGPL-3.0-or-later

use std::borrow::Borrow;
use std::cell::Cell;
use std::cmp::Ordering;
use std::fmt::{self, Display};
use std::num::Wrapping;
use std::rc::Rc;

use crate::*;

/// The kind of value contained within a node.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum NodeKind {
    /// See [`NodeValue::Integer`].
    Integer,
    /// See [`NodeValue::Float`].
    Float,
    /// See [`NodeValue::String`].
    String,
    /// See [`NodeValue::Symbol`].
    Symbol,
    /// See [`NodeValue::Variable`].
    Variable,

    /// See [`NodeValue::Array`].
    Array,
    /// See [`NodeValue::Command`].
    Command,
    /// See [`NodeValue::Property`].
    Property,

    /// See [`NodeValue::Unhandled`].
    Unhandled,
}

macro_rules! define_node_types {
    (
        $(
            $(#[$variant_attr:meta])*
            $variant:ident$(($variant_type:ident$(<$variant_gen:tt>)?) $({
                $(
                    from: {
                        $($from_type:ty => |$from_value:ident| $from_expr:expr,)+
                    },
                )?
                $(
                    eq: |$eq_left:ident, $eq_right:ident| $eq_expr:expr,
                    cmp: |$cmp_left:ident, $cmp_right:ident| $cmp_expr:expr,
                )?
                $(
                    extra_eq: {
                        $($extra_eq_type:ty => |$extra_eq_left:ident, $extra_eq_right:ident| $extra_eq_expr:expr,)+
                    },
                )?
            })?)?
        ),+
        $(,)?
    ) => {
        /// A value stored within a [`Node`].
        #[derive(Debug, Clone)]
        pub enum NodeValue {
            $(
                $(#[$variant_attr])*
                $variant$(($variant_type$(<$variant_gen>)?))?,
            )+
        }

        impl PartialEq for NodeValue {
            fn eq(&self, other: &Self) -> bool {
                match (self, other) {
                    $(
                        (
                            Self::$variant$((meta_morph!($variant_type => meta_select!($($($eq_left)?)?, left))))?,
                            Self::$variant$((meta_morph!($variant_type => meta_select!($($($eq_right)?)?, right))))?
                        )
                        => meta_select!(
                            $(meta_morph!($variant_type => meta_select!($($($eq_expr)?)?, left == right)))?,
                            true
                        ),
                    )+

                    _ => false,
                }
            }
        }

        impl PartialOrd for NodeValue {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                match (self, other) {
                    $(
                        (
                            Self::$variant$((meta_morph!($variant_type => meta_select!($($($cmp_left)?)?, left))))?,
                            Self::$variant$((meta_morph!($variant_type => meta_select!($($($cmp_right)?)?, right))))?
                        ) => meta_select!(
                            $(meta_morph!($variant_type => meta_select!($($($cmp_expr)?)?, left.partial_cmp(right))))?,
                            Some(Ordering::Equal)
                        ),
                    )+

                    _ => None,
                }
            }
        }

        impl<T> From<&T> for NodeValue
        where
            T: Clone,
            NodeValue: From<T>
        {
            fn from(value: &T) -> Self {
                Self::from(value.clone())
            }
        }

        // variant types
        $(
            // variant value
            $(
                // default variant conversions
                impl From<$variant_type$(<$variant_gen>)?> for NodeValue {
                    fn from(value: $variant_type$(<$variant_gen>)?) -> Self {
                        Self::$variant(value)
                    }
                }

                // additional conversions from `from:`
                $( // repeater for options block
                    $( // repeater for `from:`
                        $( // repeater for types
                            impl From<$from_type> for NodeValue {
                                fn from($from_value: $from_type) -> Self {
                                    Self::$variant($from_expr)
                                }
                            }
                        )+
                    )?
                )?

                // `eq:` implementation (with defaults)
                impl PartialEq<$variant_type$(<$variant_gen>)?> for NodeValue {
                    fn eq(&self, meta_select!($($($eq_right)?)?, other): &$variant_type$(<$variant_gen>)?) -> bool {
                        match self {
                            Self::$variant(meta_select!($($($eq_left)?)?, value)) => {
                                meta_select!($($($eq_expr)?)?, value == other)
                            },
                            _ => false,
                        }
                    }
                }

                impl PartialEq<NodeValue> for $variant_type$(<$variant_gen>)? {
                    fn eq(&self, other: &NodeValue) -> bool {
                        other.eq(self)
                    }
                }

                // `extra_eq:` implementations
                $( // repeater for options block
                    $( // repeater for `extra_eq:`
                        $( // repeater for types
                            impl PartialEq<$extra_eq_type> for NodeValue {
                                fn eq(&self, $extra_eq_right: &$extra_eq_type) -> bool {
                                    match self {
                                        Self::$variant($extra_eq_left) => $extra_eq_expr,
                                        _ => false,
                                    }
                                }
                            }

                            impl PartialEq<NodeValue> for $extra_eq_type {
                                fn eq(&self, other: &NodeValue) -> bool {
                                    other.eq(self)
                                }
                            }
                        )+
                    )?
                )?
            )?
        )+
    }
}

define_node_types! {
    /// An integer value (see [`Integer`]).
    Integer(Wrapping<IntegerValue>) {
        from: {
            IntegerValue => |value| Wrapping(value),
            i32 => |value| Wrapping(value as IntegerValue),
            bool => |value| Wrapping(value as IntegerValue),
        },
        extra_eq: {
            IntegerValue => |left, right| left.0 == *right,
        },
    },
    /// A floating-point value (see [`Float`]).
    Float(FloatValue) {
        from: {
            f32 => |value| value as FloatValue,
        },
    },
    /// An immutable string value.
    String(Rc<String>) {
        from: {
            String => |value| Rc::new(value),
            &str => |value| Rc::new(value.to_owned()),
        },
        extra_eq: {
            String => |left, right| left.as_str() == right,
            str => |left, right| left.as_str() == right,
        },
    },
    /// A unique identifier (see [`Symbol`]).
    Symbol(Symbol),
    /// A variable (see [`Variable`]).
    Variable(Variable) {
        eq: |left, right| left.symbol() == right.symbol(),
        cmp: |left, right| left.symbol().partial_cmp(right.symbol()),
    },

    /// An array of values (see [`NodeArray`]).
    Array(ArrayRef) {
        from: {
            NodeArray => |value| ArrayRef::new(value),
        },
        extra_eq: {
            NodeArray => |left, right| ArrayRef::borrow(left).map_or(false, |a| a.eq(right)),
        },
    },
    /// A script command (see [`NodeCommand`]).
    Command(Rc<NodeCommand>) {
        from: {
            NodeCommand => |value| Rc::new(value),
        },
        extra_eq: {
            NodeCommand => |left, right| Borrow::<NodeCommand>::borrow(left) == right,
        },
    },
    /// An object property (see [`NodeProperty`]).
    Property(Rc<NodeProperty>) {
        from: {
            NodeProperty => |value| Rc::new(value),
        },
        extra_eq: {
            NodeProperty => |left, right| Borrow::<NodeProperty>::borrow(left) == right,
        },
    },

    /// An "undefined" value, typically used when a command or function does not
    /// handle a particular request or input and wishes to fall back to another handler.
    Unhandled,
}

impl NodeValue {
    /// Generic value to be returned when a script call has been handled,
    /// but no specific value is returned from the method handling the call.
    pub const HANDLED: NodeValue = Self::Integer(Wrapping(0));

    /// Boolean TRUE value.
    pub const TRUE: NodeValue = Self::Integer(Wrapping(1));
    /// Boolean FALSE value.
    pub const FALSE: NodeValue = Self::Integer(Wrapping(0));

    pub const fn get_kind(&self) -> NodeKind {
        match self {
            Self::Integer(_) => NodeKind::Integer,
            Self::Float(_) => NodeKind::Float,
            Self::String(_) => NodeKind::String,
            Self::Symbol(_) => NodeKind::Symbol,
            Self::Variable(_) => NodeKind::Variable,

            Self::Array(_) => NodeKind::Array,
            Self::Command(_) => NodeKind::Command,
            Self::Property(_) => NodeKind::Property,

            Self::Unhandled => NodeKind::Unhandled,
        }
    }

    pub fn integer(&self) -> Option<Integer> {
        match self {
            Self::Integer(value) => Some(*value),
            Self::Float(value) => Some(Wrapping(*value as IntegerValue)),
            _ => None,
        }
    }

    pub fn float(&self) -> Option<FloatValue> {
        match self {
            Self::Integer(value) => Some(value.0 as FloatValue),
            Self::Float(value) => Some(*value),
            _ => None,
        }
    }

    pub fn number(&self) -> Option<Number> {
        match self {
            Self::Integer(value) => Some(Number::Integer(*value)),
            Self::Float(value) => Some(Number::Float(*value)),
            _ => None,
        }
    }

    pub fn boolean(&self) -> Option<bool> {
        match self {
            NodeValue::Integer(value) => Some(value.0 != 0),
            NodeValue::Float(value) => Some(*value != 0.0),
            NodeValue::String(value) => Some(!value.is_empty()),
            NodeValue::Symbol(value) => Some(!value.name().is_empty()),
            NodeValue::Variable(_) => None,

            NodeValue::Array(value) => Some(ArrayRef::borrow(value).map_or(false, |a| !a.is_empty())),
            NodeValue::Command(_) => None,
            NodeValue::Property(_) => None,

            NodeValue::Unhandled => Some(false),
        }
    }

    pub fn string(&self) -> Option<&Rc<String>> {
        match self {
            Self::String(value) => Some(value),
            Self::Symbol(value) => Some(value.name()),
            _ => None,
        }
    }
}

impl Display for NodeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(value) => Display::fmt(value, f),
            Self::Float(value) => Display::fmt(value, f),
            Self::String(value) => Display::fmt(value, f),
            Self::Symbol(value) => Display::fmt(value, f),
            Self::Variable(value) => Display::fmt(value, f),

            Self::Array(value) => match ArrayRef::borrow(value) {
                Ok(borrow) => borrow.fmt(f),
                Err(err) => write!(f, "<failed to borrow array: {err}>"),
            },
            Self::Command(value) => Display::fmt(value, f),
            Self::Property(value) => Display::fmt(value, f),

            Self::Unhandled => write!(f, "kDataUnhandled"),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum EvaluationError {
    #[error("Expected value of type {expected:?}, got {actual:?} instead")]
    TypeMismatch { expected: NodeKind, actual: NodeKind },

    #[error("Value of type {src:?} is not convertible to {dest:?}")]
    NotConvertible { src: NodeKind, dest: NodeKind },

    #[error("Value of type {0:?} is not a number")]
    NotNumber(NodeKind),

    #[error("Value of type {0:?} is not convertible to a boolean")]
    NotBoolean(NodeKind),
}

#[derive(Debug, Clone)]
pub struct Node {
    value: NodeValue,
}

macro_rules! match_value {
    ($value:expr, $kind:ident($inner:ident) => $expr:expr) => {{
        let value = $value;
        match value {
            NodeValue::$kind($inner) => Ok($expr),
            _ => Err(Error::EvaluationError(EvaluationError::TypeMismatch {
                expected: NodeKind::$kind,
                actual: value.get_kind(),
            })),
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

    pub fn integer<S>(&self, context: &mut Context<S>) -> crate::Result<Integer> {
        let value = self.evaluate(context)?;
        match value.integer() {
            Some(value) => Ok(value),
            None => {
                Err(EvaluationError::NotConvertible { src: value.get_kind(), dest: NodeKind::Integer }.into())
            },
        }
    }

    pub fn float<S>(&self, context: &mut Context<S>) -> crate::Result<FloatValue> {
        let value = self.evaluate(context)?;
        match value.float() {
            Some(value) => Ok(value),
            None => {
                Err(EvaluationError::NotConvertible { src: value.get_kind(), dest: NodeKind::Float }.into())
            },
        }
    }

    pub fn number<S>(&self, context: &mut Context<S>) -> crate::Result<Number> {
        let value = self.evaluate(context)?;
        match value.number() {
            Some(value) => Ok(value),
            None => Err(EvaluationError::NotNumber(value.get_kind()).into()),
        }
    }

    pub fn boolean<S>(&self, context: &mut Context<S>) -> crate::Result<bool> {
        let value = self.evaluate(context)?;
        match value.boolean() {
            Some(b) => Ok(b),
            None => Err(EvaluationError::NotBoolean(value.get_kind()).into()),
        }
    }

    pub fn string<S>(&self, context: &mut Context<S>) -> crate::Result<Rc<String>> {
        let value = self.evaluate(context)?;
        match value.string() {
            Some(value) => Ok(value.clone()),
            None => {
                Err(EvaluationError::NotConvertible { src: value.get_kind(), dest: NodeKind::String }.into())
            },
        }
    }

    pub fn symbol<S>(&self, context: &mut Context<S>) -> crate::Result<Symbol> {
        match_value!(self.evaluate(context)?, Symbol(value) => value)
    }

    pub const fn variable(&self) -> crate::Result<&Variable> {
        match_value!(self.unevaluated(), Variable(value) => value)
    }

    pub fn array<S>(&self, context: &mut Context<S>) -> crate::Result<ArrayRef> {
        match_value!(self.evaluate(context)?, Array(value) => value)
    }

    pub const fn command(&self) -> crate::Result<&Rc<NodeCommand>> {
        match_value!(self.unevaluated(), Command(value) => value)
    }

    pub const fn property(&self) -> crate::Result<&Rc<NodeProperty>> {
        match_value!(self.unevaluated(), Property(value) => value)
    }

    pub const fn is_unhandled(&self) -> bool {
        matches!(self.unevaluated(), NodeValue::Unhandled)
    }

    #[inline]
    pub const fn unevaluated(&self) -> &NodeValue {
        &self.value
    }

    pub fn evaluate<S>(&self, context: &mut Context<S>) -> crate::Result<NodeValue> {
        let evaluated = match self.unevaluated() {
            NodeValue::Integer(value) => NodeValue::Integer(*value),
            NodeValue::Float(value) => NodeValue::Float(*value),
            NodeValue::String(value) => NodeValue::String(value.clone()),

            NodeValue::Symbol(value) => NodeValue::Symbol(value.clone()),
            NodeValue::Variable(variable) => variable.get(context).value,
            NodeValue::Unhandled => NodeValue::Unhandled,

            NodeValue::Array(value) => NodeValue::from(value),
            NodeValue::Command(value) => context.execute(value)?.value,
            NodeValue::Property(_property) => todo!("property node evaluation"),
        };
        Ok(evaluated)
    }

    pub fn set<S, T: Into<Node>>(&self, context: &mut Context<S>, value: T) -> crate::Result {
        match self.unevaluated() {
            NodeValue::Variable(var) => var.set(context, value.into()),
            NodeValue::Property(_prop) => todo!("op_assign property access"),
            unhandled => arson_fail!("Cannot set non-variable value {:?}", unhandled),
        };
        Ok(())
    }

    pub fn display_evaluated<'a, S>(&'a self, context: &'a mut Context<S>) -> NodeDisplay<'a, S> {
        NodeDisplay { context: Cell::new(Some(context)), node: self }
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

impl<T> From<T> for Node
where
    NodeValue: From<T>,
{
    fn from(value: T) -> Self {
        Self { value: NodeValue::from(value) }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.unevaluated().fmt(f)
    }
}

pub struct NodeDisplay<'a, S> {
    context: Cell<Option<&'a mut Context<S>>>,
    node: &'a Node,
}

impl<S> fmt::Display for NodeDisplay<'_, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.context.take() {
            Some(context) => {
                let result = match self.node.evaluate(context) {
                    Ok(evaluated) => evaluated.fmt(f),
                    Err(err) => write!(f, "<error: {err}>"),
                };
                // Re-store context to ensure repeated uses have the same result
                self.context.set(Some(context));
                result
            },
            None => self.node.unevaluated().fmt(f),
        }
    }
}
