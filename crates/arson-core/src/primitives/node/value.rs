// SPDX-License-Identifier: LGPL-3.0-or-later

use std::borrow::Borrow;
use std::cmp::Ordering;
use std::num::Wrapping;
use std::rc::Rc;

use crate::prelude::*;
use crate::primitives::*;

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
            pub(super) fn total_cmp(&self, other: &Self) -> Ordering {
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
            :default: |value| match super::get_interned_string(&value) {
                Some(interned) => interned,
                None => value,
            },
            String => |value| match super::get_interned_string(&value) {
                Some(interned) => interned,
                None => value.into(),
            },
            &str => |value| match super::get_interned_string(value) {
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

impl<O: Object + Sized> From<O> for NodeValue {
    fn from(value: O) -> Self {
        Self::Object(Rc::new(value))
    }
}

impl std::fmt::Display for NodeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
