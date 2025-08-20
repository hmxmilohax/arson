// SPDX-License-Identifier: LGPL-3.0-or-later

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
