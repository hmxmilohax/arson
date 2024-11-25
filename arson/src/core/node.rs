// SPDX-License-Identifier: LGPL-3.0-or-later

use std::{
    cell::Cell,
    cmp::Ordering,
    fmt::{self, Display},
    rc::Rc,
};

use crate::*;

/// A function which is callable by a [`NodeCommand`].
pub type HandleFn = fn(context: &mut Context, args: &NodeSlice) -> HandleResult;
/// The result of a [`HandleFn`].
pub type HandleResult = crate::Result<NodeValue>;

/// The integer value type used within nodes.
pub type Integer = i64;
/// The floating-point value type used within nodes.
pub type Float = f64;

/// A numerical value, either integer or floating-point.
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Number {
    /// An integer value; see [`Integer`].
    Integer(Integer),
    /// A floating-point value; see [`Float`].
    Float(Float),
}

/// The kind of value contained within a node.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum NodeKind {
    Integer,
    Float,
    String,
    Symbol,
    Variable,

    Unhandled,

    Array,
    Command,
    Property,
}

macro_rules! define_node_types {
    (
        $(#[$name_attr:meta])*
        pub enum $name:ident {
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
                })?)?
            ),+
            $(,)?
        }
    ) => {
        $(#[$name_attr])*
        pub enum $name {
            $($(#[$variant_attr])* $variant$(($variant_type$(<$variant_gen>)?))?,)+
        }

        impl PartialEq for $name {
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

        impl PartialOrd for $name {
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

        impl<T> From<&T> for $name
        where
            T: Clone,
            $name: From<T>
        {
            fn from(value: &T) -> Self {
                Self::from(value.clone())
            }
        }

        $(
            $(
                impl From<$variant_type$(<$variant_gen>)?> for $name {
                    fn from(value: $variant_type$(<$variant_gen>)?) -> Self {
                        Self::$variant(value)
                    }
                }

                $(
                    $(
                        $(
                            impl From<$from_type> for $name {
                                fn from($from_value: $from_type) -> Self {
                                    Self::$variant($from_expr)
                                }
                            }
                        )+
                    )?
                )?

                impl PartialEq<$variant_type$(<$variant_gen>)?> for $name {
                    fn eq(&self, meta_select!($($($eq_right)?)?, other): &$variant_type$(<$variant_gen>)?) -> bool {
                        match self {
                            Self::$variant(meta_select!($($($eq_left)?)?, value)) => {
                                meta_select!($($($eq_expr)?)?, value == other)
                            },
                            _ => false,
                        }
                    }
                }

                impl PartialEq<$name> for $variant_type$(<$variant_gen>)? {
                    fn eq(&self, other: &$name) -> bool {
                        other.eq(self)
                    }
                }
            )?
        )+
    }
}

define_node_types! {
    /// A raw, unevaluated value stored within a [`Node`].
    #[derive(Debug, Clone)]
    pub enum RawNodeValue {
        Integer(Integer) {
            from: {
                i32 => |value| value as Integer,
                bool => |value| value as Integer,
            },
        },
        Float(Float) {
            from: {
                f32 => |value| value as Float,
            },
        },
        String(Rc<String>) {
            from: {
                String => |value| Rc::new(value),
                &str => |value| Rc::new(value.to_owned()),
            },
        },
        Symbol(Symbol),
        Variable(Variable) {
            eq: |left, right| left.symbol() == right.symbol(),
            cmp: |left, right| left.symbol().partial_cmp(right.symbol()),
        },

        Array(Rc<NodeArray>) {
            from: {
                NodeArray => |value| Rc::new(value),
            },
        },
        Command(Rc<NodeCommand>) {
            from: {
                NodeCommand => |value| Rc::new(value),
            },
        },
        Property(Rc<NodeProperty>) {
            from: {
                NodeProperty => |value| Rc::new(value),
            },
        },

        Unhandled,
    }
}

define_node_types! {
    /// A node value which has been evaluated.
    #[derive(Debug, Clone)]
    pub enum NodeValue {
        Integer(Integer) {
            from: {
                i32 => |value| value as Integer,
                bool => |value| value as Integer,
            },
        },
        Float(Float) {
            from: {
                f32 => |value| value as Float,
            },
        },
        String(Rc<String>) {
            from: {
                String => |value| Rc::new(value),
                &str => |value| Rc::new(value.to_owned()),
            },
        },
        Symbol(Symbol),

        Array(Rc<NodeArray>) {
            from: {
                NodeArray => |value| Rc::new(value),
            },
        },

        Unhandled,
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    value: RawNodeValue,
}

impl RawNodeValue {
    /// Generic value to be returned when a script call has been handled,
    /// but no specific value is returned from the method handling the call.
    pub const HANDLED: RawNodeValue = Self::Integer(0);

    /// Boolean TRUE value.
    pub const TRUE: RawNodeValue = Self::Integer(1);
    /// Boolean FALSE value.
    pub const FALSE: RawNodeValue = Self::Integer(0);

    pub const fn get_kind(&self) -> NodeKind {
        match self {
            RawNodeValue::Integer(_) => NodeKind::Integer,
            RawNodeValue::Float(_) => NodeKind::Float,
            RawNodeValue::String(_) => NodeKind::String,
            RawNodeValue::Symbol(_) => NodeKind::Symbol,
            RawNodeValue::Variable(_) => NodeKind::Variable,

            RawNodeValue::Array(_) => NodeKind::Array,
            RawNodeValue::Command(_) => NodeKind::Command,
            RawNodeValue::Property(_) => NodeKind::Property,

            RawNodeValue::Unhandled => NodeKind::Unhandled,
        }
    }

    pub fn integer(&self) -> Option<Integer> {
        match self {
            Self::Integer(value) => Some(*value),
            Self::Float(value) => Some(*value as Integer),
            _ => None,
        }
    }

    pub fn integer_strict(&self) -> Option<Integer> {
        match self {
            Self::Integer(value) => Some(*value),
            _ => None,
        }
    }

    pub fn float(&self) -> Option<Float> {
        match self {
            Self::Integer(value) => Some(*value as Float),
            Self::Float(value) => Some(*value),
            _ => None,
        }
    }

    pub fn float_strict(&self) -> Option<Float> {
        match self {
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

    pub fn string(&self) -> Option<&Rc<String>> {
        match self {
            Self::String(value) => Some(value),
            _ => None,
        }
    }

    pub fn symbol(&self) -> Option<&Symbol> {
        match self {
            Self::Symbol(value) => Some(value),
            _ => None,
        }
    }

    pub fn variable(&self) -> Option<&Variable> {
        match self {
            Self::Variable(value) => Some(value),
            _ => None,
        }
    }

    pub fn array(&self) -> Option<&Rc<NodeArray>> {
        match self {
            Self::Array(value) => Some(value),
            _ => None,
        }
    }

    pub fn command(&self) -> Option<&Rc<NodeCommand>> {
        match self {
            Self::Command(value) => Some(value),
            _ => None,
        }
    }

    pub fn property(&self) -> Option<&Rc<NodeProperty>> {
        match self {
            Self::Property(value) => Some(value),
            _ => None,
        }
    }

    pub fn is_unhandled(&self) -> bool {
        matches!(self, Self::Unhandled)
    }
}

impl From<NodeValue> for RawNodeValue {
    fn from(value: NodeValue) -> Self {
        match value {
            NodeValue::Integer(value) => RawNodeValue::Integer(value),
            NodeValue::Float(value) => RawNodeValue::Float(value),
            NodeValue::String(value) => RawNodeValue::String(value),
            NodeValue::Symbol(value) => RawNodeValue::Symbol(value),
            NodeValue::Unhandled => RawNodeValue::Unhandled,
            NodeValue::Array(value) => RawNodeValue::Array(value),
        }
    }
}

impl Display for RawNodeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RawNodeValue::Integer(value) => Display::fmt(value, f),
            RawNodeValue::Float(value) => Display::fmt(value, f),
            RawNodeValue::String(value) => Display::fmt(value, f),
            RawNodeValue::Symbol(value) => Display::fmt(value, f),
            RawNodeValue::Variable(value) => write!(f, "${}", value.symbol()),

            RawNodeValue::Array(value) => Display::fmt(value, f),
            RawNodeValue::Command(value) => Display::fmt(value, f),
            RawNodeValue::Property(value) => Display::fmt(value, f),

            RawNodeValue::Unhandled => write!(f, "kDataUnhandled"),
        }
    }
}

impl NodeValue {
    /// Generic value to be returned when a script call has been handled,
    /// but no specific value is returned from the method handling the call.
    pub const HANDLED: NodeValue = Self::Integer(0);

    /// Boolean TRUE value.
    pub const TRUE: NodeValue = Self::Integer(1);
    /// Boolean FALSE value.
    pub const FALSE: NodeValue = Self::Integer(0);

    pub const fn get_kind(&self) -> NodeKind {
        match self {
            NodeValue::Integer(_) => NodeKind::Integer,
            NodeValue::Float(_) => NodeKind::Float,
            NodeValue::String(_) => NodeKind::String,
            NodeValue::Symbol(_) => NodeKind::Symbol,

            NodeValue::Array(_) => NodeKind::Array,

            NodeValue::Unhandled => NodeKind::Unhandled,
        }
    }

    pub fn integer(&self) -> Option<Integer> {
        match self {
            Self::Integer(value) => Some(*value),
            Self::Float(value) => Some(*value as Integer),
            _ => None,
        }
    }

    pub fn integer_strict(&self) -> Option<Integer> {
        match self {
            Self::Integer(value) => Some(*value),
            _ => None,
        }
    }

    pub fn float(&self) -> Option<Float> {
        match self {
            Self::Integer(value) => Some(*value as Float),
            Self::Float(value) => Some(*value),
            _ => None,
        }
    }

    pub fn float_strict(&self) -> Option<Float> {
        match self {
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

    pub fn boolean(&self) -> bool {
        match self {
            Self::Integer(value) => *value != 0,
            Self::Float(value) => *value != 0.0,
            Self::String(value) => !value.is_empty(),
            Self::Symbol(value) => !value.name().is_empty(),
            Self::Array(value) => !value.is_empty(),
            Self::Unhandled => false,
        }
    }

    pub fn boolean_strict(&self) -> Option<bool> {
        Some(self.integer_strict()? != 0)
    }

    pub fn string(&self) -> Option<&Rc<String>> {
        match self {
            Self::String(value) => Some(value),
            _ => None,
        }
    }

    pub fn symbol(&self) -> Option<&Symbol> {
        match self {
            Self::Symbol(value) => Some(value),
            _ => None,
        }
    }

    pub fn array(&self) -> Option<&Rc<NodeArray>> {
        match self {
            Self::Array(value) => Some(value),
            _ => None,
        }
    }

    pub fn is_unhandled(&self) -> bool {
        matches!(self, Self::Unhandled)
    }
}

impl Display for NodeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeValue::Integer(value) => Display::fmt(value, f),
            NodeValue::Float(value) => Display::fmt(value, f),
            NodeValue::String(value) => Display::fmt(value, f),
            NodeValue::Symbol(value) => Display::fmt(value, f),

            NodeValue::Array(value) => Display::fmt(value, f),

            NodeValue::Unhandled => write!(f, "kDataUnhandled"),
        }
    }
}

impl Node {
    /// Value returned when a script call has not been handled by a receiver.
    pub const UNHANDLED: Node = Self { value: RawNodeValue::Unhandled };
    /// Generic value to be returned when a script call has been handled,
    /// but no specific value is returned from the method handling the call.
    pub const HANDLED: Node = Self { value: RawNodeValue::HANDLED };

    /// Boolean TRUE value.
    pub const TRUE: Node = Self { value: RawNodeValue::TRUE };
    /// Boolean FALSE value.
    pub const FALSE: Node = Self { value: RawNodeValue::FALSE };

    pub const fn get_kind(&self) -> NodeKind {
        self.value.get_kind()
    }

    fn evaluate_kind<R>(
        &self,
        context: &mut Context,
        expected: NodeKind,
        f: impl Fn(&NodeValue) -> Option<R>,
    ) -> crate::Result<R> {
        self.evaluate(context).and_then(|v| match f(&v) {
            Some(value) => Ok(value),
            None => Err(Error::TypeMismatch { expected, actual: v.get_kind() }),
        })
    }

    fn unevaluated_kind<R>(&self, expected: NodeKind, f: impl Fn(&RawNodeValue) -> Option<R>) -> crate::Result<R> {
        match f(self.unevaluated()) {
            Some(value) => Ok(value),
            None => Err(Error::TypeMismatch { expected, actual: self.get_kind() }),
        }
    }

    pub fn integer(&self, context: &mut Context) -> crate::Result<Integer> {
        self.evaluate_kind(context, NodeKind::Integer, NodeValue::integer)
    }

    pub fn integer_strict(&self, context: &mut Context) -> crate::Result<Integer> {
        self.evaluate_kind(context, NodeKind::Integer, NodeValue::integer_strict)
    }

    pub fn float(&self, context: &mut Context) -> crate::Result<Float> {
        self.evaluate_kind(context, NodeKind::Float, NodeValue::float)
    }

    pub fn float_strict(&self, context: &mut Context) -> crate::Result<Float> {
        self.evaluate_kind(context, NodeKind::Float, NodeValue::float_strict)
    }

    pub fn number(&self, context: &mut Context) -> crate::Result<Number> {
        self.evaluate_kind(context, NodeKind::Float, NodeValue::number)
    }

    pub fn boolean(&self, context: &mut Context) -> crate::Result<bool> {
        Ok(self.evaluate(context)?.boolean())
    }

    pub fn boolean_strict(&self, context: &mut Context) -> crate::Result<bool> {
        self.evaluate_kind(context, NodeKind::Integer, NodeValue::boolean_strict)
    }

    pub fn string(&self, context: &mut Context) -> crate::Result<Rc<String>> {
        self.evaluate_kind(context, NodeKind::String, |value| value.string().cloned())
    }

    pub fn symbol(&self, context: &mut Context) -> crate::Result<Symbol> {
        self.evaluate_kind(context, NodeKind::Symbol, |value| value.symbol().cloned())
    }

    pub fn variable(&self) -> crate::Result<Variable> {
        self.unevaluated_kind(NodeKind::Variable, |value| value.variable().cloned())
    }

    pub fn array(&self, context: &mut Context) -> crate::Result<Rc<NodeArray>> {
        self.evaluate_kind(context, NodeKind::Array, |value| value.array().cloned())
    }

    pub fn command(&self) -> crate::Result<Rc<NodeCommand>> {
        self.unevaluated_kind(NodeKind::Variable, |value| value.command().cloned())
    }

    pub fn property(&self) -> crate::Result<Rc<NodeProperty>> {
        self.unevaluated_kind(NodeKind::Variable, |value| value.property().cloned())
    }

    pub fn is_unhandled(&self) -> bool {
        self.unevaluated().is_unhandled()
    }

    #[inline]
    pub const fn unevaluated(&self) -> &RawNodeValue {
        &self.value
    }

    pub fn evaluate(&self, context: &mut Context) -> crate::Result<NodeValue> {
        let evaluated = match self.unevaluated() {
            RawNodeValue::Integer(value) => NodeValue::Integer(*value),
            RawNodeValue::Float(value) => NodeValue::Float(*value),
            RawNodeValue::String(value) => NodeValue::String(value.clone()),

            RawNodeValue::Symbol(value) => NodeValue::Symbol(value.clone()),
            RawNodeValue::Variable(variable) => variable.get(context),
            RawNodeValue::Unhandled => NodeValue::Unhandled,

            RawNodeValue::Array(value) => NodeValue::from(value),
            RawNodeValue::Command(value) => context.execute(value)?,
            RawNodeValue::Property(_property) => todo!("property node evaluation"),
        };
        Ok(evaluated)
    }

    pub fn set_variable<T: Into<NodeValue>>(&self, context: &mut Context, value: T) -> crate::Result<()> {
        match self.unevaluated() {
            RawNodeValue::Variable(var) => var.set(context, value.into()),
            RawNodeValue::Property(_prop) => todo!("op_assign property access"),
            unhandled => arson_fail!("Cannot set non-variable value {:?}", unhandled),
        };
        Ok(())
    }

    pub fn display_evaluated<'a>(&'a self, context: &'a mut Context) -> NodeDisplay<'_> {
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
    RawNodeValue: From<T>,
{
    fn from(value: T) -> Self {
        Self { value: RawNodeValue::from(value) }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.unevaluated().fmt(f)
    }
}

pub struct NodeDisplay<'a> {
    context: Cell<Option<&'a mut Context>>,
    node: &'a Node,
}

impl<'a> fmt::Display for NodeDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.context.replace(None) {
            Some(context) => match self.node.evaluate(context) {
                Ok(evaluated) => evaluated.fmt(f),
                Err(err) => write!(f, "<error: {err}>"),
            },
            None => self.node.unevaluated().fmt(f),
        }
    }
}
