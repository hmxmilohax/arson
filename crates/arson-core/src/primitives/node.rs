// SPDX-License-Identifier: LGPL-3.0-or-later

use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt;
use std::num::Wrapping;
use std::rc::Rc;

use crate::prelude::*;
use crate::primitives::*;
use crate::IntoSymbol;

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
                    try_from: {
                        $($try_from_type:ty => |$try_from_value:ident| $try_from_expr:expr,)+
                    },
                )?
                $(
                    eq: |$eq_left:ident, $eq_right:ident| $eq_expr:expr,
                    cmp: |$cmp_left:ident, $cmp_right:ident| $cmp_expr:expr,
                    total_cmp: |$total_cmp_left:ident, $total_cmp_right:ident| $total_cmp_expr:expr,
                )?
                $(
                    variant_eq: {
                        $($variant_eq_type:ident$(($variant_eq_other:ident))? => |$variant_eq_value:ident| $variant_eq_expr:expr,)+
                    },
                    variant_cmp: {
                        $(
                            $variant_cmp_type:ident$(($variant_cmp_other:ident))? => |$variant_cmp_value:ident| {
                                left: $variant_cmp_left_expr:expr,
                                right: $variant_cmp_right_expr:expr,
                            },
                        )+
                    },
                    variant_total_cmp: {
                        $(
                            $variant_total_cmp_type:ident$(($variant_total_cmp_other:ident))? => |$variant_total_cmp_value:ident| {
                                left: $variant_total_cmp_left_expr:expr,
                                right: $variant_total_cmp_right_expr:expr,
                            },
                        )+
                    },
                )?
                $(
                    type_eq: {
                        $($type_eq_type:ty => |$type_eq_left:ident, $type_eq_right:ident| $type_eq_expr:expr,)+
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
                    // variant types
                    $(
                        (
                            Self::$variant$((meta_morph!($variant_type => meta_select!($($($eq_left)?)?, left))))?,
                            Self::$variant$((meta_morph!($variant_type => meta_select!($($($eq_right)?)?, right))))?
                        )
                        => meta_select!(
                            $(meta_morph!($variant_type => meta_select!($($($eq_expr)?)?, left == right)))?,
                            true
                        ),
                        $( // variant value
                            $( // options block
                                $( // `variant_eq`
                                    $( // types to compare against
                                        (Self::$variant($variant_eq_value), Self::$variant_eq_type$(($variant_eq_other))?)
                                        | (Self::$variant_eq_type$(($variant_eq_other))?, Self::$variant($variant_eq_value))
                                        => $variant_eq_expr,
                                    )+
                                )?
                            )?
                        )?
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
                        $(
                            $(
                                $(
                                    $(
                                        (Self::$variant($variant_cmp_value), Self::$variant_cmp_type$(($variant_cmp_other))?)
                                        => $variant_cmp_left_expr,
                                        (Self::$variant_cmp_type$(($variant_cmp_other))?, Self::$variant($variant_cmp_value))
                                        => $variant_cmp_right_expr,
                                    )+
                                )?
                            )?
                        )?
                    )+

                    _ => None,
                }
            }
        }

        impl NodeValue {
            fn total_cmp(&self, other: &Self) -> Ordering {
                match (self, other) {
                    $(
                        (
                            Self::$variant$((meta_morph!($variant_type => meta_select!($($($total_cmp_left)?)?, left))))?,
                            Self::$variant$((meta_morph!($variant_type => meta_select!($($($total_cmp_right)?)?, right))))?
                        ) => meta_select!(
                            $(meta_morph!($variant_type => meta_select!($($($total_cmp_expr)?)?, left.cmp(right))))?,
                            Ordering::Equal
                        ),
                        $(
                            $(
                                $(
                                    $(
                                        (Self::$variant($variant_total_cmp_value), Self::$variant_total_cmp_type$(($variant_total_cmp_other))?)
                                        => $variant_total_cmp_left_expr,
                                        (Self::$variant_total_cmp_type$(($variant_total_cmp_other))?, Self::$variant($variant_total_cmp_value))
                                        => $variant_total_cmp_right_expr,
                                    )+
                                )?
                            )?
                        )?
                    )+

                    (left, right) => left.get_kind().cmp(&right.get_kind()),
                }
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
                $( // options block
                    $( // `from`
                        $( // types to convert from
                            impl From<$from_type> for NodeValue {
                                fn from($from_value: $from_type) -> Self {
                                    Self::$variant($from_expr)
                                }
                            }
                        )+
                    )?
                )?

                // additional conversions from `try_from:`
                $( // options block
                    $( // `try_from`
                        $( // types to convert from
                            impl TryFrom<$try_from_type> for NodeValue {
                                type Error = crate::Error;

                                fn try_from($try_from_value: $try_from_type) -> Result<Self, Self::Error> {
                                    Ok(Self::$variant($try_from_expr))
                                }
                            }

                            impl TryFrom<$try_from_type> for Node {
                                type Error = crate::Error;

                                fn try_from(value: $try_from_type) -> Result<Self, Self::Error> {
                                    Ok(Self { value: NodeValue::try_from(value)? })
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

                // `type_eq:` implementations
                $( // options block
                    $( // `type_eq`
                        $( // types to compare against
                            impl PartialEq<$type_eq_type> for NodeValue {
                                fn eq(&self, $type_eq_right: &$type_eq_type) -> bool {
                                    match self {
                                        Self::$variant($type_eq_left) => $type_eq_expr,
                                        _ => false,
                                    }
                                }
                            }

                            impl PartialEq<NodeValue> for $type_eq_type {
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

            i8 => |value| Wrapping(value as IntegerValue),
            i16 => |value| Wrapping(value as IntegerValue),
            i32 => |value| Wrapping(value as IntegerValue),

            u8 => |value| Wrapping(value as IntegerValue),
            u16 => |value| Wrapping(value as IntegerValue),
            u32 => |value| Wrapping(value as IntegerValue),

            bool => |value| Wrapping(value as IntegerValue),

            NodeKind => |value| {
                // Convert the kind for Unhandled into its value,
                // to make {== {type $value} kDataUnhandled} work correctly
                if matches!(value, NodeKind::Unhandled) {
                    return NodeValue::Unhandled;
                }
                Wrapping(value as IntegerValue)
            },
        },
        try_from: {
            u64 => |value| match IntegerValue::try_from(value) {
                Ok(value) => Wrapping(value),
                Err(error) => return Err(NumericError::IntegerConversion(error).into()),
            },
            isize => |value| match IntegerValue::try_from(value) {
                Ok(value) => Wrapping(value),
                Err(error) => return Err(NumericError::IntegerConversion(error).into()),
            },
            usize => |value| match IntegerValue::try_from(value) {
                Ok(value) => Wrapping(value),
                Err(error) => return Err(NumericError::IntegerConversion(error).into()),
            },
        },
        variant_eq: {
            Float(other) => |value| value.0 as FloatValue == *other,
        },
        variant_cmp: {
            Float(other) => |value| {
                left: (value.0 as FloatValue).partial_cmp(other),
                right: other.partial_cmp(&(value.0 as FloatValue)),
            },
        },
        variant_total_cmp: {
            Float(other) => |value| {
                left: (value.0 as FloatValue).total_cmp(other),
                right: other.total_cmp(&(value.0 as FloatValue)),
            },
        },
        type_eq: {
            IntegerValue => |left, right| left.0 == *right,
        },
    },
    /// A floating-point value (see [`Float`]).
    Float(FloatValue) {
        from: {
            f32 => |value| value as FloatValue,
        },
        eq: |left, right| left == right,
        cmp: |left, right| left.partial_cmp(right),
        total_cmp: |left, right| left.total_cmp(right),
    },
    /// An immutable string value.
    String(Rc<String>) {
        from: {
            String => |value| Rc::new(value),
            &str => |value| Rc::new(value.to_owned()),
        },
        variant_eq: {
            Symbol(other) => |value| value == other.name(),
        },
        variant_cmp: {
            Symbol(other) => |value| {
                left: value.partial_cmp(other.name()),
                right: other.name().partial_cmp(value),
            },
        },
        variant_total_cmp: {
            Symbol(other) => |value| {
                left: value.cmp(other.name()),
                right: other.name().cmp(value),
            },
        },
        type_eq: {
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
        total_cmp: |left, right| left.symbol().cmp(right.symbol()),
    },

    /// An array of values (see [`NodeArray`]).
    Array(ArrayRef) {
        from: {
            NodeArray => |value| ArrayRef::new(value),
        },
        eq: |left, right| left == right,
        cmp: |left, right| left.partial_cmp(right),
        total_cmp: |left, right| left.total_cmp(right),
        type_eq: {
            NodeArray => |left, right| ArrayRef::borrow(left).map_or(false, |a| a.eq(right)),
        },
    },
    /// A script command (see [`NodeCommand`]).
    Command(Rc<NodeCommand>) {
        from: {
            NodeCommand => |value| Rc::new(value),
        },
        eq: |left, right| left == right,
        cmp: |left, right| left.partial_cmp(right),
        total_cmp: |left, right| left.total_cmp(right),
        type_eq: {
            NodeCommand => |left, right| Borrow::<NodeCommand>::borrow(left) == right,
        },
    },
    /// An object property (see [`NodeProperty`]).
    Property(Rc<NodeProperty>) {
        from: {
            NodeProperty => |value| Rc::new(value),
        },
        eq: |left, right| left == right,
        cmp: |left, right| left.partial_cmp(right),
        total_cmp: |left, right| left.total_cmp(right),
        type_eq: {
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

    pub const fn integer(&self) -> Option<Integer> {
        match self {
            Self::Integer(value) => Some(*value),
            Self::Float(value) => Some(Wrapping(*value as IntegerValue)),
            _ => None,
        }
    }

    pub const fn float(&self) -> Option<FloatValue> {
        match self {
            Self::Integer(value) => Some(value.0 as FloatValue),
            Self::Float(value) => Some(*value),
            _ => None,
        }
    }

    pub const fn number(&self) -> Option<Number> {
        match self {
            Self::Integer(value) => Some(Number::Integer(*value)),
            Self::Float(value) => Some(Number::Float(*value)),
            _ => None,
        }
    }

    pub fn size_integer(&self) -> Option<crate::Result<usize>> {
        match self {
            Self::Integer(value) => {
                let result = match usize::try_from(value.0) {
                    Ok(value) => Ok(value),
                    Err(err) => Err(NumericError::IntegerConversion(err).into()),
                };
                Some(result)
            },
            _ => None,
        }
    }

    pub fn size_integer_opt(&self) -> Option<usize> {
        match self {
            Self::Integer(value) => match usize::try_from(value.0) {
                Ok(value) => Some(value),
                Err(_) => None,
            },
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

    pub const fn string(&self) -> Option<&Rc<String>> {
        match self {
            Self::String(value) => Some(value),
            Self::Symbol(value) => Some(value.name()),
            _ => None,
        }
    }

    pub const fn symbol(&self) -> Option<&Symbol> {
        match self {
            Self::Symbol(value) => Some(value),
            _ => None,
        }
    }

    pub fn force_symbol<S>(&self, context: &mut Context<S>) -> Option<Symbol> {
        match self {
            Self::String(value) => Some(context.add_symbol(value)),
            Self::Symbol(value) => Some(value.clone()),
            _ => None,
        }
    }

    pub fn array_tag(&self) -> Option<ArrayTag> {
        match self {
            Self::Integer(tag) => Some(ArrayTag::Integer(*tag)),
            Self::Symbol(tag) => Some(ArrayTag::Symbol(tag.clone())),
            _ => None,
        }
    }

    pub const fn variable(&self) -> Option<&Variable> {
        match self {
            Self::Variable(value) => Some(value),
            _ => None,
        }
    }

    pub const fn array(&self) -> Option<&ArrayRef> {
        match self {
            Self::Array(value) => Some(value),
            _ => None,
        }
    }

    pub fn borrow_array(&self) -> Option<crate::Result<ArrayBorrow<'_>>> {
        match self {
            Self::Array(value) => Some(value.borrow()),
            _ => None,
        }
    }

    pub fn borrow_array_mut(&self) -> Option<crate::Result<ArrayBorrowMut<'_>>> {
        match self {
            Self::Array(value) => Some(value.borrow_mut()),
            _ => None,
        }
    }

    pub const fn command(&self) -> Option<&Rc<NodeCommand>> {
        match self {
            Self::Command(value) => Some(value),
            _ => None,
        }
    }

    pub const fn property(&self) -> Option<&Rc<NodeProperty>> {
        match self {
            Self::Property(value) => Some(value),
            _ => None,
        }
    }
}

// Quick 'n easy variant checks
impl NodeValue {
    pub const fn is_integer(&self) -> bool {
        matches!(self, NodeValue::Integer(_))
    }

    pub const fn is_float(&self) -> bool {
        matches!(self, NodeValue::Float(_))
    }

    pub const fn is_number(&self) -> bool {
        matches!(self, NodeValue::Integer(_) | NodeValue::Float(_))
    }

    pub fn is_size_integer(&self) -> bool {
        matches!(self, NodeValue::Integer(value) if usize::try_from(value.0).is_ok())
    }

    pub const fn is_string(&self) -> bool {
        matches!(self, NodeValue::String(_))
    }

    pub const fn is_symbol(&self) -> bool {
        matches!(self, NodeValue::Symbol(_))
    }

    pub const fn is_variable(&self) -> bool {
        matches!(self, NodeValue::Variable(_))
    }

    pub const fn is_array(&self) -> bool {
        matches!(self, NodeValue::Array(_))
    }

    pub const fn is_command(&self) -> bool {
        matches!(self, NodeValue::Command(_))
    }

    pub const fn is_property(&self) -> bool {
        matches!(self, NodeValue::Property(_))
    }

    pub const fn is_any_array(&self) -> bool {
        self.is_array() || self.is_command() || self.is_property()
    }

    pub const fn is_unhandled(&self) -> bool {
        matches!(self, NodeValue::Unhandled)
    }
}

pub trait SymbolDefault {
    fn into_symbol(self) -> Symbol;
}

impl SymbolDefault for &Symbol {
    fn into_symbol(self) -> Symbol {
        self.clone()
    }
}

impl<S, N: IntoSymbol> SymbolDefault for (&mut Context<S>, N) {
    fn into_symbol(self) -> Symbol {
        self.1.into_symbol(self.0)
    }
}

impl NodeValue {
    pub fn integer_or(&self, default: Integer) -> Integer {
        self.integer().unwrap_or(default)
    }

    pub fn float_or(&self, default: FloatValue) -> FloatValue {
        self.float().unwrap_or(default)
    }

    pub fn number_or(&self, default: Number) -> Number {
        self.number().unwrap_or(default)
    }

    pub fn size_integer_or(&self, default: usize) -> usize {
        self.size_integer_opt().unwrap_or(default)
    }

    pub fn boolean_or(&self, default: bool) -> bool {
        self.boolean().unwrap_or(default)
    }

    pub fn string_or(&self, default: &Rc<String>) -> Rc<String> {
        self.string().cloned().unwrap_or_else(|| default.clone())
    }

    pub fn symbol_or(&self, default: impl SymbolDefault) -> Symbol {
        self.symbol().cloned().unwrap_or_else(|| default.into_symbol())
    }

    pub fn force_symbol_or<S>(&self, context: &mut Context<S>, default: impl IntoSymbol) -> Symbol {
        self.force_symbol(context).unwrap_or_else(|| default.into_symbol(context))
    }

    pub fn array_tag_or(&self, default: &ArrayTag) -> ArrayTag {
        self.array_tag().unwrap_or_else(|| default.clone())
    }

    pub fn variable_or(&self, default: &Variable) -> Variable {
        self.variable().cloned().unwrap_or_else(|| default.clone())
    }

    pub fn array_or(&self, default: &ArrayRef) -> ArrayRef {
        self.array().cloned().unwrap_or_else(|| default.clone())
    }

    pub fn command_or(&self, default: &Rc<NodeCommand>) -> Rc<NodeCommand> {
        self.command().cloned().unwrap_or_else(|| default.clone())
    }

    pub fn property_or(&self, default: &Rc<NodeProperty>) -> Rc<NodeProperty> {
        self.property().cloned().unwrap_or_else(|| default.clone())
    }

    pub fn integer_or_else(&self, default: impl FnOnce() -> Integer) -> Integer {
        self.integer().unwrap_or_else(default)
    }

    pub fn float_or_else(&self, default: impl FnOnce() -> FloatValue) -> FloatValue {
        self.float().unwrap_or_else(default)
    }

    pub fn number_or_else(&self, default: impl FnOnce() -> Number) -> Number {
        self.number().unwrap_or_else(default)
    }

    pub fn size_integer_or_else(&self, default: impl FnOnce() -> usize) -> usize {
        self.size_integer_opt().unwrap_or_else(default)
    }

    pub fn boolean_or_else(&self, default: impl FnOnce() -> bool) -> bool {
        self.boolean().unwrap_or_else(default)
    }

    pub fn string_or_else(&self, default: impl FnOnce() -> Rc<String>) -> Rc<String> {
        self.string().cloned().unwrap_or_else(default)
    }

    pub fn symbol_or_else(&self, default: impl FnOnce() -> Symbol) -> Symbol {
        self.symbol().cloned().unwrap_or_else(default)
    }

    pub fn force_symbol_or_else<S>(
        &self,
        context: &mut Context<S>,
        default: impl FnOnce() -> Symbol,
    ) -> Symbol {
        self.force_symbol(context).unwrap_or_else(default)
    }

    pub fn array_tag_or_else(&self, default: impl FnOnce() -> ArrayTag) -> ArrayTag {
        self.array_tag().unwrap_or_else(default)
    }

    pub fn variable_or_else(&self, default: impl FnOnce() -> Variable) -> Variable {
        self.variable().cloned().unwrap_or_else(default)
    }

    pub fn array_or_else(&self, default: impl FnOnce() -> ArrayRef) -> ArrayRef {
        self.array().cloned().unwrap_or_else(default)
    }

    pub fn command_or_else(&self, default: impl FnOnce() -> Rc<NodeCommand>) -> Rc<NodeCommand> {
        self.command().cloned().unwrap_or_else(default)
    }

    pub fn property_or_else(&self, default: impl FnOnce() -> Rc<NodeProperty>) -> Rc<NodeProperty> {
        self.property().cloned().unwrap_or_else(default)
    }
}

impl Default for NodeValue {
    fn default() -> Self {
        Self::from(0)
    }
}

impl<T> From<&T> for NodeValue
where
    T: Clone,
    NodeValue: From<T>,
{
    fn from(value: &T) -> Self {
        Self::from(value.clone())
    }
}

impl fmt::Display for NodeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use std::fmt::Display;

        match self {
            Self::Integer(value) => Display::fmt(value, f),
            Self::Float(value) => write!(f, "{value:?}"),
            Self::String(value) => write!(f, "\"{}\"", value.replace('\"', "\\q")),
            Self::Symbol(value) => Display::fmt(value, f),
            Self::Variable(value) => Display::fmt(value, f),

            Self::Array(value) => Display::fmt(value, f),
            Self::Command(value) => Display::fmt(value, f),
            Self::Property(value) => Display::fmt(value, f),

            Self::Unhandled => write!(f, "kDataUnhandled"),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum EvaluationError {
    #[error("expected value of type {expected:?}, got {actual:?} instead")]
    TypeMismatch { expected: NodeKind, actual: NodeKind },

    #[error("value of type {src:?} is not convertible to {dest:?}")]
    NotConvertible { src: NodeKind, dest: NodeKind },

    #[error("value of type {0:?} is not a number")]
    NotNumber(NodeKind),

    #[error("value of type {0:?} is not convertible to a boolean")]
    NotBoolean(NodeKind),

    #[error("value of type {0:?} is not a valid array tag")]
    NotArrayTag(NodeKind),
}

#[derive(Clone, Default)]
pub struct Node {
    value: NodeValue,
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

    pub fn integer<S>(&self, context: &mut Context<S>) -> crate::Result<Integer> {
        match_option!(self.evaluate(context)?, |value| value.integer(), Integer)
    }

    pub fn float<S>(&self, context: &mut Context<S>) -> crate::Result<FloatValue> {
        match_option!(self.evaluate(context)?, |value| value.float(), Float)
    }

    pub fn number<S>(&self, context: &mut Context<S>) -> crate::Result<Number> {
        match_option_with_err!(
            self.evaluate(context)?,
            |value| value.number(),
            EvaluationError::NotNumber(value.get_kind())
        )
    }

    pub fn size_integer<S>(&self, context: &mut Context<S>) -> crate::Result<usize> {
        // inlined match_option!, not making a variant just for this lol
        let value = self.evaluate(context)?;
        match value.size_integer() {
            Some(value) => value, // this needs to be returned as-is instead of wrapped in Ok()
            None => {
                Err(EvaluationError::NotConvertible { src: value.get_kind(), dest: NodeKind::Integer }.into())
            },
        }
    }

    pub fn boolean<S>(&self, context: &mut Context<S>) -> crate::Result<bool> {
        match_option_with_err!(
            self.evaluate(context)?,
            |value| value.boolean(),
            EvaluationError::NotBoolean(value.get_kind())
        )
    }

    pub fn string<S>(&self, context: &mut Context<S>) -> crate::Result<Rc<String>> {
        match_option!(self.evaluate(context)?, |value| value.string().cloned(), String)
    }

    pub fn symbol<S>(&self, context: &mut Context<S>) -> crate::Result<Symbol> {
        match_value!(self.evaluate(context)?, Symbol(value) => value)
    }

    pub fn force_symbol<S>(&self, context: &mut Context<S>) -> crate::Result<Symbol> {
        match_option!(self.evaluate(context)?, |value| value.force_symbol(context), Symbol)
    }

    pub fn array_tag<S>(&self, context: &mut Context<S>) -> crate::Result<ArrayTag> {
        match_option_with_err!(
            self.evaluate(context)?,
            |value| value.array_tag(),
            EvaluationError::NotArrayTag(value.get_kind())
        )
    }

    pub fn variable(&self) -> crate::Result<&Variable> {
        match_value!(self.unevaluated(), Variable(value) => value)
    }

    pub fn set_variable<S>(&self, context: &mut Context<S>, value: impl Into<Node>) -> crate::Result {
        match self.unevaluated() {
            NodeValue::Variable(var) => var.set(context, value),
            NodeValue::Property(_prop) => todo!("op_assign property access"),
            unhandled => arson_fail!("Cannot set non-variable value {:?}", unhandled),
        };
        Ok(())
    }

    pub fn array<S>(&self, context: &mut Context<S>) -> crate::Result<ArrayRef> {
        match_value!(self.evaluate(context)?, Array(value) => value)
    }

    pub fn command(&self) -> crate::Result<&Rc<NodeCommand>> {
        match_value!(self.unevaluated(), Command(value) => value)
    }

    pub fn property(&self) -> crate::Result<&Rc<NodeProperty>> {
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
    pub fn integer_opt<S>(&self, context: &mut Context<S>) -> Option<crate::Result<Integer>> {
        self.evaluate(context).map(|n| n.integer()).transpose()
    }

    pub fn float_opt<S>(&self, context: &mut Context<S>) -> Option<crate::Result<FloatValue>> {
        self.evaluate(context).map(|n| n.float()).transpose()
    }

    pub fn number_opt<S>(&self, context: &mut Context<S>) -> Option<crate::Result<Number>> {
        self.evaluate(context).map(|n| n.number()).transpose()
    }

    pub fn size_integer_opt<S>(&self, context: &mut Context<S>) -> Option<crate::Result<usize>> {
        self.evaluate(context).map(|n| n.size_integer_opt()).transpose()
    }

    pub fn boolean_opt<S>(&self, context: &mut Context<S>) -> Option<crate::Result<bool>> {
        self.evaluate(context).map(|n| n.boolean()).transpose()
    }

    pub fn string_opt<S>(&self, context: &mut Context<S>) -> Option<crate::Result<Rc<String>>> {
        self.evaluate(context).map(|n| n.string().cloned()).transpose()
    }

    pub fn symbol_opt<S>(&self, context: &mut Context<S>) -> Option<crate::Result<Symbol>> {
        self.evaluate(context).map(|n| n.symbol().cloned()).transpose()
    }

    pub fn force_symbol_opt<S>(&self, context: &mut Context<S>) -> Option<crate::Result<Symbol>> {
        self.evaluate(context).map(|n| n.force_symbol(context)).transpose()
    }

    pub fn array_tag_opt<S>(&self, context: &mut Context<S>) -> Option<crate::Result<ArrayTag>> {
        self.evaluate(context).map(|n| n.array_tag()).transpose()
    }

    pub const fn variable_opt(&self) -> Option<&Variable> {
        self.unevaluated().variable()
    }

    pub fn set_variable_opt<S>(&self, context: &mut Context<S>, value: impl Into<Node>) {
        match self.unevaluated() {
            NodeValue::Variable(var) => var.set(context, value),
            NodeValue::Property(_prop) => todo!("op_assign property access"),
            _ => (),
        }
    }

    pub fn array_opt<S>(&self, context: &mut Context<S>) -> Option<crate::Result<ArrayRef>> {
        self.evaluate(context).map(|n| n.array().cloned()).transpose()
    }

    pub const fn command_opt(&self) -> Option<&Rc<NodeCommand>> {
        self.unevaluated().command()
    }

    pub const fn property_opt(&self) -> Option<&Rc<NodeProperty>> {
        self.unevaluated().property()
    }
}

// Optional value retrieval, with defaults
impl Node {
    pub fn integer_or<S>(&self, context: &mut Context<S>, default: Integer) -> crate::Result<Integer> {
        self.evaluate(context).map(|v| v.integer_or(default))
    }

    pub fn float_or<S>(&self, context: &mut Context<S>, default: FloatValue) -> crate::Result<FloatValue> {
        self.evaluate(context).map(|v| v.float_or(default))
    }

    pub fn number_or<S>(&self, context: &mut Context<S>, default: Number) -> crate::Result<Number> {
        self.evaluate(context).map(|v| v.number_or(default))
    }

    pub fn size_integer_or<S>(&self, context: &mut Context<S>, default: usize) -> crate::Result<usize> {
        self.evaluate(context).map(|v| v.size_integer_or(default))
    }

    pub fn boolean_or<S>(&self, context: &mut Context<S>, default: bool) -> crate::Result<bool> {
        self.evaluate(context).map(|v| v.boolean_or(default))
    }

    pub fn string_or<S>(&self, context: &mut Context<S>, default: &Rc<String>) -> crate::Result<Rc<String>> {
        self.evaluate(context).map(|v| v.string_or(default))
    }

    pub fn symbol_or<S>(&self, context: &mut Context<S>, default: impl IntoSymbol) -> crate::Result<Symbol> {
        self.evaluate(context).map(|v| v.symbol_or((context, default)))
    }

    pub fn force_symbol_or<S>(
        &self,
        context: &mut Context<S>,
        default: impl IntoSymbol,
    ) -> crate::Result<Symbol> {
        self.evaluate(context).map(|v| v.force_symbol_or(context, default))
    }

    pub fn array_tag_or<S>(&self, context: &mut Context<S>, default: &ArrayTag) -> crate::Result<ArrayTag> {
        self.evaluate(context).map(|v| v.array_tag_or(default))
    }

    pub fn variable_or(&self, default: &Variable) -> Variable {
        self.unevaluated().variable_or(default)
    }

    pub fn array_or<S>(&self, context: &mut Context<S>, default: &ArrayRef) -> crate::Result<ArrayRef> {
        self.evaluate(context).map(|v| v.array_or(default))
    }

    pub fn command_or(&self, default: &Rc<NodeCommand>) -> Rc<NodeCommand> {
        self.unevaluated().command_or(default)
    }

    pub fn property_or(&self, default: &Rc<NodeProperty>) -> Rc<NodeProperty> {
        self.unevaluated().property_or(default)
    }

    pub fn integer_or_else<S>(
        &self,
        context: &mut Context<S>,
        default: impl FnOnce() -> Integer,
    ) -> crate::Result<Integer> {
        self.evaluate(context).map(|v| v.integer_or_else(default))
    }

    pub fn float_or_else<S>(
        &self,
        context: &mut Context<S>,
        default: impl FnOnce() -> FloatValue,
    ) -> crate::Result<FloatValue> {
        self.evaluate(context).map(|v| v.float_or_else(default))
    }

    pub fn number_or_else<S>(
        &self,
        context: &mut Context<S>,
        default: impl FnOnce() -> Number,
    ) -> crate::Result<Number> {
        self.evaluate(context).map(|v| v.number_or_else(default))
    }

    pub fn size_integer_or_else<S>(
        &self,
        context: &mut Context<S>,
        default: impl FnOnce() -> usize,
    ) -> crate::Result<usize> {
        self.evaluate(context).map(|v| v.size_integer_or_else(default))
    }

    pub fn boolean_or_else<S>(
        &self,
        context: &mut Context<S>,
        default: impl FnOnce() -> bool,
    ) -> crate::Result<bool> {
        self.evaluate(context).map(|v| v.boolean_or_else(default))
    }

    pub fn string_or_else<S>(
        &self,
        context: &mut Context<S>,
        default: impl FnOnce() -> Rc<String>,
    ) -> crate::Result<Rc<String>> {
        self.evaluate(context).map(|v| v.string_or_else(default))
    }

    pub fn symbol_or_else<S>(
        &self,
        context: &mut Context<S>,
        default: impl FnOnce() -> Symbol,
    ) -> crate::Result<Symbol> {
        self.evaluate(context).map(|v| v.symbol_or_else(default))
    }

    pub fn force_symbol_or_else<S>(
        &self,
        context: &mut Context<S>,
        default: impl FnOnce() -> Symbol,
    ) -> crate::Result<Symbol> {
        self.evaluate(context).map(|v| v.force_symbol_or_else(context, default))
    }

    pub fn array_tag_or_else<S>(
        &self,
        context: &mut Context<S>,
        default: impl FnOnce() -> ArrayTag,
    ) -> crate::Result<ArrayTag> {
        self.evaluate(context).map(|v| v.array_tag_or_else(default))
    }

    pub fn variable_or_else(&self, default: impl FnOnce() -> Variable) -> Variable {
        self.unevaluated().variable_or_else(default)
    }

    pub fn array_or_else<S>(
        &self,
        context: &mut Context<S>,
        default: impl FnOnce() -> ArrayRef,
    ) -> crate::Result<ArrayRef> {
        self.evaluate(context).map(|v| v.array_or_else(default))
    }

    pub fn command_or_else(&self, default: impl FnOnce() -> Rc<NodeCommand>) -> Rc<NodeCommand> {
        self.unevaluated().command_or_else(default)
    }

    pub fn property_or_else(&self, default: impl FnOnce() -> Rc<NodeProperty>) -> Rc<NodeProperty> {
        self.unevaluated().property_or_else(default)
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

#[cfg(test)]
mod tests {
    use super::*;

    mod display {
        use super::*;

        #[test]
        fn integer() {
            assert_eq!(NodeValue::Integer(Wrapping(1)).to_string(), "1");
            assert_eq!(NodeValue::Integer(Wrapping(458243)).to_string(), "458243");
            assert_eq!(NodeValue::Integer(Wrapping(-235725)).to_string(), "-235725");
            assert_eq!(NodeValue::Integer(Wrapping(i64::MAX)).to_string(), "9223372036854775807");
            assert_eq!(NodeValue::Integer(Wrapping(i64::MIN)).to_string(), "-9223372036854775808");
        }

        #[test]
        fn float() {
            assert_eq!(NodeValue::Float(0.000_000_000_001).to_string(), "1e-12");
            assert_eq!(NodeValue::Float(0.000_000_001).to_string(), "1e-9");
            assert_eq!(NodeValue::Float(0.000_001).to_string(), "1e-6");
            assert_eq!(NodeValue::Float(0.001).to_string(), "0.001");
            assert_eq!(NodeValue::Float(1.0).to_string(), "1.0");
            assert_eq!(NodeValue::Float(1_000.0).to_string(), "1000.0");
            assert_eq!(NodeValue::Float(1_000_000.0).to_string(), "1000000.0");
            assert_eq!(NodeValue::Float(1_000_000_000.0).to_string(), "1000000000.0");
            assert_eq!(NodeValue::Float(1_000_000_000_000.0).to_string(), "1000000000000.0");
        }

        #[test]
        fn string() {
            assert_eq!(NodeValue::String("asdf".to_owned().into()).to_string(), "\"asdf\"");
            assert_eq!(
                NodeValue::String("\"asdf\"".to_owned().into()).to_string(),
                "\"\\qasdf\\q\""
            );
            assert_eq!(NodeValue::String("asdf\n".to_owned().into()).to_string(), "\"asdf\n\"");
        }

        #[test]
        fn symbol() {
            let mut context = Context::new(());
            let sym = context.add_symbol("sym");
            let sym_space = context.add_symbol("sym with\nwhitespace");

            assert_eq!(NodeValue::Symbol(sym).to_string(), "sym");
            assert_eq!(NodeValue::Symbol(sym_space).to_string(), "'sym with\nwhitespace'");
        }

        #[test]
        fn variable() {
            let mut context = Context::new(());
            let var = Variable::new("var", &mut context);
            let dollar_var = Variable::new("$var", &mut context);

            assert_eq!(NodeValue::Variable(var).to_string(), "$var");
            assert_eq!(NodeValue::Variable(dollar_var).to_string(), "$$var");
        }

        #[test]
        fn array() {
            let mut context = Context::new(());
            let sym = context.add_symbol("sym");

            let base_array = arson_array![sym, 1, "text"];

            let array = ArrayRef::new(base_array.clone());
            let command = NodeCommand::from(base_array.clone()).into();
            let property = NodeProperty::from(base_array.clone()).into();

            assert_eq!(NodeValue::Array(array).to_string(), "(sym 1 \"text\")");
            assert_eq!(NodeValue::Command(command).to_string(), "{sym 1 \"text\"}");
            assert_eq!(NodeValue::Property(property).to_string(), "[sym 1 \"text\"]");
        }

        #[test]
        fn unhandled() {
            assert_eq!(NodeValue::Unhandled.to_string(), "kDataUnhandled");
        }
    }
}
