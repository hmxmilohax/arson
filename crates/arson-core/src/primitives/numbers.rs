// SPDX-License-Identifier: LGPL-3.0-or-later

use std::num::Wrapping;
use std::ops::{self, RangeBounds};

#[cfg(feature = "text-loading")]
pub use arson_parse::{FloatValue, IntegerValue};

#[cfg(not(feature = "text-loading"))]
pub type IntegerValue = i64;
#[cfg(not(feature = "text-loading"))]
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

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum NumericError {
    Overflow,
    IndexOutOfRange(usize, ops::Range<usize>),
    SliceOutOfRange {
        slice_start: ops::Bound<usize>,
        slice_end: ops::Bound<usize>,
        expected_range: ops::Range<usize>,
    },

    IntegerConversion(#[from] std::num::TryFromIntError),
    IntegerParse(#[from] std::num::ParseIntError),
    FloatParse(#[from] std::num::ParseFloatError),
}

impl NumericError {
    pub fn slice_out_of_range(
        slice_range: impl RangeBounds<usize>,
        expected_range: ops::Range<usize>,
    ) -> Self {
        Self::SliceOutOfRange {
            slice_start: slice_range.start_bound().cloned(),
            slice_end: slice_range.end_bound().cloned(),
            expected_range,
        }
    }
}

impl std::fmt::Display for NumericError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumericError::Overflow => f.write_str("an undesired integer overflow occurred"),
            NumericError::IndexOutOfRange(index, range) => {
                write!(f, "index {index} outside of range {range:?}")
            },
            NumericError::SliceOutOfRange {
                slice_start: start,
                slice_end: end,
                expected_range: range,
            } => {
                // "slice range {start:?}..{end:?} outside of range {range:?}"

                f.write_str("slice range ")?;

                match start {
                    ops::Bound::Included(start) => write!(f, "{start}")?,
                    ops::Bound::Excluded(start) => write!(f, "{}", start + 1)?,
                    ops::Bound::Unbounded => (),
                }
                f.write_str("..")?;
                match end {
                    ops::Bound::Included(end) => write!(f, "={end}")?,
                    ops::Bound::Excluded(end) => write!(f, "{end}")?,
                    ops::Bound::Unbounded => (),
                }

                write!(f, " outside of range {range:?}")
            },
            NumericError::IntegerConversion(inner) => inner.fmt(f),
            NumericError::IntegerParse(inner) => inner.fmt(f),
            NumericError::FloatParse(inner) => inner.fmt(f),
        }
    }
}
