// SPDX-License-Identifier: LGPL-3.0-or-later

/// The integer value type used in parsing.
pub type IntegerValue = i64;

/// The floating-point value type used in parsing.
pub type FloatValue = f64;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ArrayKind {
    Array,
    Command,
    Property,
}

impl ArrayKind {
    pub fn delimiters(&self) -> (char, char) {
        match self {
            ArrayKind::Array => ('(', ')'),
            ArrayKind::Command => ('{', '}'),
            ArrayKind::Property => ('[', ']'),
        }
    }
}

impl std::fmt::Display for ArrayKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArrayKind::Array => write!(f, "array"),
            ArrayKind::Command => write!(f, "command"),
            ArrayKind::Property => write!(f, "property"),
        }
    }
}
