// SPDX-License-Identifier: LGPL-3.0-or-later

use std::num::Wrapping;
use std::ops::Range;

pub use arson_parse::{FloatValue, IntegerValue};

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

impl Number {
    pub fn integer(&self) -> Integer {
        match self {
            Number::Integer(value) => *value,
            Number::Float(value) => Wrapping(*value as IntegerValue),
        }
    }

    pub fn float(&self) -> FloatValue {
        match self {
            Number::Integer(value) => value.0 as FloatValue,
            Number::Float(value) => *value,
        }
    }
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

#[derive(thiserror::Error, Debug)]
pub enum NumericError {
    #[error("an undesired integer overflow occurred")]
    Overflow,

    #[error("value outside of range {0:?}")]
    OutOfRange(Range<usize>),

    #[error("{0}")]
    IntegerConversion(#[from] std::num::TryFromIntError),

    #[error("{0}")]
    IntegerParse(#[from] std::num::ParseIntError),

    #[error("{0}")]
    FloatParse(#[from] std::num::ParseFloatError),
}
