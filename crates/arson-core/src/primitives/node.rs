// SPDX-License-Identifier: LGPL-3.0-or-later

use std::borrow::Borrow;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::num::Wrapping;
use std::rc::Rc;

use crate::prelude::*;
use crate::primitives::*;
use crate::IntoSymbol;

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

    /// See [`NodeValue::Function`].
    Function,
    /// See [`NodeValue::Object`].
    Object,

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
                        $(:default: |$default_from_value:ident| $default_from_expr:expr,)?
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
                            Self::$variant$((meta_morph!(|$variant_type| meta_select!($($($eq_left)?)?, left))))?,
                            Self::$variant$((meta_morph!(|$variant_type| meta_select!($($($eq_right)?)?, right))))?
                        )
                        => meta_select!(
                            $(meta_morph!(|$variant_type| meta_select!($($($eq_expr)?)?, left == right)))?,
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
                            Self::$variant$((meta_morph!(|$variant_type| meta_select!($($($cmp_left)?)?, left))))?,
                            Self::$variant$((meta_morph!(|$variant_type| meta_select!($($($cmp_right)?)?, right))))?
                        ) => meta_select!(
                            $(meta_morph!(|$variant_type| meta_select!($($($cmp_expr)?)?, left.partial_cmp(right))))?,
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
                            Self::$variant$((meta_morph!(|$variant_type| meta_select!($($($total_cmp_left)?)?, left))))?,
                            Self::$variant$((meta_morph!(|$variant_type| meta_select!($($($total_cmp_right)?)?, right))))?
                        ) => meta_select!(
                            $(meta_morph!(|$variant_type| meta_select!($($($total_cmp_expr)?)?, left.cmp(right))))?,
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
                    fn from(meta_select!($($($($default_from_value)?)?)?, value): $variant_type$(<$variant_gen>)?) -> Self {
                        Self::$variant(meta_select!($($($($default_from_expr)?)?)?, value))
                    }
                }

                impl From<&$variant_type$(<$variant_gen>)?> for NodeValue {
                    fn from(value: &$variant_type$(<$variant_gen>)?) -> Self {
                        Self::from(value.clone())
                    }
                }

                impl TryFrom<NodeValue> for $variant_type$(<$variant_gen>)? {
                    type Error = crate::Error;

                    fn try_from(value: NodeValue) -> Result<Self, Self::Error> {
                        match value {
                            NodeValue::$variant(value) => Ok(value),
                            _ => Err(EvaluationError::TypeMismatch {
                                expected: NodeKind::$variant,
                                actual: value.get_kind(),
                            }
                            .into()),
                        }
                    }
                }

                impl TryFrom<Node> for $variant_type$(<$variant_gen>)? {
                    type Error = crate::Error;

                    fn try_from(value: Node) -> Result<Self, Self::Error> {
                        TryFrom::try_from(value.value)
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

                            impl From<&$from_type> for NodeValue {
                                fn from(value: &$from_type) -> Self {
                                    Self::from(value.clone())
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

            std::cmp::Ordering => |value| match value {
                std::cmp::Ordering::Less => Wrapping(-1),
                std::cmp::Ordering::Equal => Wrapping(0),
                std::cmp::Ordering::Greater => Wrapping(1),
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
            :default: |value| match get_interned_string(&value) {
                Some(interned) => interned,
                None => value,
            },
            String => |value| match get_interned_string(&value) {
                Some(interned) => interned,
                None => value.into(),
            },
            &str => |value| match get_interned_string(value) {
                Some(interned) => interned,
                None => Rc::new(value.to_owned()),
            },
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

    /// A callable function (see [`HandleFn`]).
    Function(HandleFn) {
        eq: |left, right| left == right,
        cmp: |_left, _right| None,
        total_cmp: |left, right| left.total_cmp(right),
    },
    Object(ObjectRef) {
        eq: |left, right| Rc::ptr_eq(left, right),
        cmp: |_left, _right| None,
        total_cmp: |left, right| left.total_cmp(right.as_ref()),
        type_eq: {
            dyn Object => |left, right| left == right,
        },
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
            NodeArray => |left, right| ArrayRef::borrow(left).is_ok_and(|a| a.eq(right)),
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

impl<O: Object + Sized> From<O> for NodeValue {
    fn from(value: O) -> Self {
        Self::Object(Rc::new(value))
    }
}

// Interned strings
thread_local! {
    static STRING_POOL: RefCell<HashMap<String, Rc<String>>> = {
        let mut pool = HashMap::new();

        // By default, the empty string and all printable ASCII characters characters
        // are interned, the latter being done for the sake of `str_elem`.

        // The empty string
        pool.insert(String::new(), Rc::new(String::new()));

        #[inline]
        fn insert_chars(pool: &mut HashMap<String, Rc<String>>, chars: impl IntoIterator<Item = char>) {
            for c in chars {
                pool.insert(c.to_string(), Rc::new(c.to_string()));
            }
        }

        // Printable ASCII characters in numerical order
        insert_chars(&mut pool, [' ', '!', '\"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/']);
        insert_chars(&mut pool, '0'..='9');
        insert_chars(&mut pool, [':', ';', '<', '=', '>', '?', '@']);
        insert_chars(&mut pool, 'A'..='Z');
        insert_chars(&mut pool, ['[', '\\', ']', '^', '_', '`']);
        insert_chars(&mut pool, 'a'..='z');
        insert_chars(&mut pool, ['{', '|', '}', '~']);

        RefCell::new(pool)
    };
}

/// Creates and returns an internalized copy of a string.
///
/// Interned strings with lengths of 10 characters or less are used to de-duplicate strings
/// when creating [`Node`] instances that contain strings. By default only
pub fn intern_string(value: &str) -> Rc<String> {
    STRING_POOL.with_borrow_mut(|pool| match pool.get(value).cloned() {
        Some(interned) => interned,
        None => {
            let interned = Rc::new(value.to_owned());
            pool.insert(value.to_owned(), interned.clone());
            interned
        },
    })
}

pub fn is_string_interned(value: &str) -> bool {
    STRING_POOL.with_borrow(|pool| pool.contains_key(value))
}

fn get_interned_string(value: &str) -> Option<Rc<String>> {
    // Skip for long strings
    if value.len() > 10 {
        return None;
    }

    STRING_POOL.with_borrow_mut(|pool| pool.get(value).cloned())
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

            Self::Function(_) => NodeKind::Function,
            Self::Object(_) => NodeKind::Object,

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
            Self::Integer(value) => usize::try_from(value.0).ok(),
            _ => None,
        }
    }

    pub fn boolean(&self) -> bool {
        match self {
            NodeValue::Integer(value) => value.0 != 0,
            NodeValue::Float(value) => *value != 0.0,
            NodeValue::String(value) => !value.is_empty(),
            NodeValue::Symbol(value) => !value.name().is_empty(),
            NodeValue::Variable(_) => false,

            NodeValue::Function(_) => true,
            NodeValue::Object(_) => true,

            NodeValue::Array(value) => ArrayRef::borrow(value).is_ok_and(|a| !a.is_empty()),
            NodeValue::Command(value) => !value.is_empty(),
            NodeValue::Property(value) => !value.is_empty(),

            NodeValue::Unhandled => false,
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

    pub fn force_symbol(&self, context: &mut Context) -> Option<crate::Result<Symbol>> {
        match self {
            Self::String(value) => Some(context.add_symbol(value)),
            Self::Symbol(value) => Some(Ok(value.clone())),
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

    pub const fn object(&self) -> Option<&ObjectRef> {
        match self {
            Self::Object(value) => Some(value),
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

    pub const fn is_object(&self) -> bool {
        matches!(self, NodeValue::Object(_))
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
    fn into_symbol(self) -> crate::Result<Symbol>;
}

impl SymbolDefault for &Symbol {
    fn into_symbol(self) -> crate::Result<Symbol> {
        Ok(self.clone())
    }
}

impl<N: IntoSymbol> SymbolDefault for (&mut Context, N) {
    fn into_symbol(self) -> crate::Result<Symbol> {
        self.1.into_symbol(self.0)
    }
}

macro_rules! get_value_or_else {
    ($value:expr, $default:expr) => {{
        if let Some(value) = $value {
            Ok(value)
        } else {
            $default
        }
    }};
    ($value:expr => Result, $default:expr) => {{
        if let Some(value) = $value {
            value
        } else {
            $default
        }
    }};
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

    pub fn string_or(&self, default: &Rc<String>) -> Rc<String> {
        self.string().cloned().unwrap_or_else(|| default.clone())
    }

    pub fn symbol_or(&self, default: impl SymbolDefault) -> crate::Result<Symbol> {
        get_value_or_else!(self.symbol().cloned(), default.into_symbol())
    }

    pub fn force_symbol_or(&self, context: &mut Context, default: impl IntoSymbol) -> crate::Result<Symbol> {
        get_value_or_else!(self.force_symbol(context) => Result, default.into_symbol(context))
    }

    pub fn array_tag_or(&self, default: &ArrayTag) -> ArrayTag {
        self.array_tag().unwrap_or_else(|| default.clone())
    }

    pub fn variable_or(&self, default: &Variable) -> Variable {
        self.variable().cloned().unwrap_or_else(|| default.clone())
    }

    pub fn object_or(&self, default: &ObjectRef) -> ObjectRef {
        self.object().cloned().unwrap_or_else(|| default.clone())
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

    pub fn integer_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<Integer>,
    ) -> crate::Result<Integer> {
        get_value_or_else!(self.integer(), default())
    }

    pub fn float_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<FloatValue>,
    ) -> crate::Result<FloatValue> {
        get_value_or_else!(self.float(), default())
    }

    pub fn number_or_else(&self, default: impl FnOnce() -> crate::Result<Number>) -> crate::Result<Number> {
        get_value_or_else!(self.number(), default())
    }

    pub fn size_integer_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<usize>,
    ) -> crate::Result<usize> {
        get_value_or_else!(self.size_integer_opt(), default())
    }

    pub fn string_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<Rc<String>>,
    ) -> crate::Result<Rc<String>> {
        get_value_or_else!(self.string().cloned(), default())
    }

    pub fn symbol_or_else(&self, default: impl FnOnce() -> crate::Result<Symbol>) -> crate::Result<Symbol> {
        get_value_or_else!(self.symbol().cloned(), default())
    }

    pub fn force_symbol_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<Symbol>,
    ) -> crate::Result<Symbol> {
        if let Some(value) = self.force_symbol(context) {
            value
        } else {
            default(context)
        }
    }

    pub fn array_tag_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<ArrayTag>,
    ) -> crate::Result<ArrayTag> {
        get_value_or_else!(self.array_tag(), default())
    }

    pub fn variable_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<Variable>,
    ) -> crate::Result<Variable> {
        get_value_or_else!(self.variable().cloned(), default())
    }

    pub fn object_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<ObjectRef>,
    ) -> crate::Result<ObjectRef> {
        get_value_or_else!(self.object().cloned(), default())
    }

    pub fn array_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<ArrayRef>,
    ) -> crate::Result<ArrayRef> {
        get_value_or_else!(self.array().cloned(), default())
    }

    pub fn command_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<Rc<NodeCommand>>,
    ) -> crate::Result<Rc<NodeCommand>> {
        get_value_or_else!(self.command().cloned(), default())
    }

    pub fn property_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<Rc<NodeProperty>>,
    ) -> crate::Result<Rc<NodeProperty>> {
        get_value_or_else!(self.property().cloned(), default())
    }
}

impl Default for NodeValue {
    fn default() -> Self {
        Self::from(0)
    }
}

impl From<&NodeValue> for NodeValue {
    fn from(value: &NodeValue) -> Self {
        value.clone()
    }
}

impl fmt::Display for NodeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use std::fmt::Display;

        match self {
            Self::Integer(value) => Display::fmt(value, f),
            Self::Float(value) => {
                // Debug display used since it acts as a general format specifier
                write!(f, "{value:?}")
            },
            Self::String(value) => Display::fmt(value, f),
            Self::Symbol(value) => Display::fmt(value, f),
            Self::Variable(value) => Display::fmt(value, f),

            Self::Function(value) => Display::fmt(value, f),
            Self::Object(value) => Display::fmt(value, f),

            Self::Array(value) => Display::fmt(value, f),
            Self::Command(value) => Display::fmt(value, f),
            Self::Property(value) => Display::fmt(value, f),

            Self::Unhandled => write!(f, "kDataUnhandled"),
        }
    }
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

    pub const fn command_opt(&self) -> Option<&Rc<NodeCommand>> {
        self.unevaluated().command()
    }

    pub const fn property_opt(&self) -> Option<&Rc<NodeProperty>> {
        self.unevaluated().property()
    }
}

// Optional value retrieval, with defaults
impl Node {
    pub fn integer_or(&self, context: &mut Context, default: Integer) -> crate::Result<Integer> {
        self.evaluate(context).map(|v| v.integer_or(default))
    }

    pub fn float_or(&self, context: &mut Context, default: FloatValue) -> crate::Result<FloatValue> {
        self.evaluate(context).map(|v| v.float_or(default))
    }

    pub fn number_or(&self, context: &mut Context, default: Number) -> crate::Result<Number> {
        self.evaluate(context).map(|v| v.number_or(default))
    }

    pub fn size_integer_or(&self, context: &mut Context, default: usize) -> crate::Result<usize> {
        self.evaluate(context).map(|v| v.size_integer_or(default))
    }

    pub fn string_or(&self, context: &mut Context, default: &Rc<String>) -> crate::Result<Rc<String>> {
        self.evaluate(context).map(|v| v.string_or(default))
    }

    pub fn symbol_or(&self, context: &mut Context, default: impl IntoSymbol) -> crate::Result<Symbol> {
        self.evaluate(context).and_then(|v| v.symbol_or((context, default)))
    }

    pub fn force_symbol_or(&self, context: &mut Context, default: impl IntoSymbol) -> crate::Result<Symbol> {
        self.evaluate(context).and_then(|v| v.force_symbol_or(context, default))
    }

    pub fn array_tag_or(&self, context: &mut Context, default: &ArrayTag) -> crate::Result<ArrayTag> {
        self.evaluate(context).map(|v| v.array_tag_or(default))
    }

    pub fn variable_or(&self, default: &Variable) -> Variable {
        self.unevaluated().variable_or(default)
    }

    pub fn object_or(&self, context: &mut Context, default: &ObjectRef) -> crate::Result<ObjectRef> {
        self.evaluate(context).map(|v| v.object_or(default))
    }

    pub fn array_or(&self, context: &mut Context, default: &ArrayRef) -> crate::Result<ArrayRef> {
        self.evaluate(context).map(|v| v.array_or(default))
    }

    pub fn command_or(&self, default: &Rc<NodeCommand>) -> Rc<NodeCommand> {
        self.unevaluated().command_or(default)
    }

    pub fn property_or(&self, default: &Rc<NodeProperty>) -> Rc<NodeProperty> {
        self.unevaluated().property_or(default)
    }

    pub fn integer_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<Integer>,
    ) -> crate::Result<Integer> {
        get_value_or_else!(self.evaluate(context)?.integer(), default(context))
    }

    pub fn float_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<FloatValue>,
    ) -> crate::Result<FloatValue> {
        get_value_or_else!(self.evaluate(context)?.float(), default(context))
    }

    pub fn number_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<Number>,
    ) -> crate::Result<Number> {
        get_value_or_else!(self.evaluate(context)?.number(), default(context))
    }

    pub fn size_integer_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<usize>,
    ) -> crate::Result<usize> {
        get_value_or_else!(self.evaluate(context)?.size_integer() => Result, default(context))
    }

    pub fn string_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<Rc<String>>,
    ) -> crate::Result<Rc<String>> {
        get_value_or_else!(self.evaluate(context)?.string().cloned(), default(context))
    }

    pub fn symbol_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<Symbol>,
    ) -> crate::Result<Symbol> {
        get_value_or_else!(self.evaluate(context)?.symbol().cloned(), default(context))
    }

    pub fn force_symbol_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<Symbol>,
    ) -> crate::Result<Symbol> {
        get_value_or_else!(self.evaluate(context)?.force_symbol(context) => Result, default(context))
    }

    pub fn array_tag_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<ArrayTag>,
    ) -> crate::Result<ArrayTag> {
        get_value_or_else!(self.evaluate(context)?.array_tag(), default(context))
    }

    pub fn variable_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<Variable>,
    ) -> crate::Result<Variable> {
        self.unevaluated().variable_or_else(default)
    }

    pub fn object_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<ObjectRef>,
    ) -> crate::Result<ObjectRef> {
        get_value_or_else!(self.evaluate(context)?.object().cloned(), default(context))
    }

    pub fn array_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<ArrayRef>,
    ) -> crate::Result<ArrayRef> {
        get_value_or_else!(self.evaluate(context)?.array().cloned(), default(context))
    }

    pub fn command_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<Rc<NodeCommand>>,
    ) -> crate::Result<Rc<NodeCommand>> {
        self.unevaluated().command_or_else(default)
    }

    pub fn property_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<Rc<NodeProperty>>,
    ) -> crate::Result<Rc<NodeProperty>> {
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
            assert_eq!(NodeValue::String("asdf".to_owned().into()).to_string(), "asdf");
            assert_eq!(NodeValue::String("\"asdf\"".to_owned().into()).to_string(), "\"asdf\"");
            assert_eq!(NodeValue::String("asdf\n".to_owned().into()).to_string(), "asdf\n");
        }

        #[test]
        fn symbol() {
            let mut context = Context::new();
            let sym_foo = context.add_required_symbol("foo");
            let sym_space = context.add_required_symbol("sym with\nwhitespace");
            let sym_empty = context.add_required_symbol("");

            assert_eq!(NodeValue::Symbol(sym_foo).to_string(), "foo");
            assert_eq!(NodeValue::Symbol(sym_space).to_string(), "'sym with\nwhitespace'");
            assert_eq!(NodeValue::Symbol(sym_empty).to_string(), "''");
        }

        #[test]
        fn variable() {
            let mut context = Context::new();
            let var = Variable::new_required("var", &mut context);
            let dollar_var = Variable::new_required("$var", &mut context);

            assert_eq!(NodeValue::Variable(var).to_string(), "$var");
            assert_eq!(NodeValue::Variable(dollar_var).to_string(), "$$var");
        }

        #[test]
        fn array() {
            let mut context = Context::new();
            let sym = context.add_required_symbol("sym");

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
