// SPDX-License-Identifier: LGPL-3.0-or-later

use std::num::Wrapping;

/// The integer value type used within nodes.
pub type IntegerValue = i64;

/// The floating-point value type used within nodes.
pub type FloatValue = f64;

/// An integer value with wrapping/overflow semantics.
pub type Integer = Wrapping<IntegerValue>;

/// A numerical value, either integer or floating-point.
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Number {
    /// An integer value (see [`Integer`]).
    Integer(Integer),
    /// A floating-point value (see [`Float`]).
    Float(FloatValue),
}

impl From<Integer> for Number {
    fn from(value: Integer) -> Self {
        Self::Integer(value)
    }
}

impl From<FloatValue> for Number {
    fn from(value: FloatValue) -> Self {
        Self::Float(value)
    }
}
