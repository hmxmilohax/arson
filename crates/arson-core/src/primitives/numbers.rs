// SPDX-License-Identifier: LGPL-3.0-or-later

use std::num::Wrapping;
use std::ops::{self, RangeBounds};

#[cfg(feature = "text-loading")]
pub use arson_parse::{FloatValue, IntegerValue};

use crate::Node;

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

#[derive(thiserror::Error, Debug)]
pub enum NumericError {
    Overflow,
    IndexOutOfRange(usize, ops::Range<usize>),
    SliceOutOfRange {
        slice_start: ops::Bound<usize>,
        slice_end: ops::Bound<usize>,
        actual_range: ops::Range<usize>,
    },

    IntegerConversion(#[from] std::num::TryFromIntError),
    IntegerParse(#[from] std::num::ParseIntError),
    FloatParse(#[from] std::num::ParseFloatError),
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
                actual_range: range,
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

pub trait NodeSliceIndex {
    type Output: ?Sized;

    fn get(self, slice: &[Node]) -> crate::Result<&Self::Output>;
    fn get_mut(self, slice: &mut [Node]) -> crate::Result<&mut Self::Output>;

    fn get_opt(self, slice: &[Node]) -> Option<&Self::Output>;
    fn get_mut_opt(self, slice: &mut [Node]) -> Option<&mut Self::Output>;
}

impl NodeSliceIndex for usize {
    type Output = Node;

    fn get(self, slice: &[Node]) -> crate::Result<&Self::Output> {
        slice
            .get(self)
            .ok_or_else(|| NumericError::IndexOutOfRange(self, 0..slice.len()).into())
    }

    fn get_mut(self, slice: &mut [Node]) -> crate::Result<&mut Self::Output> {
        let length = slice.len(); // done here due to borrow rules
        slice
            .get_mut(self)
            .ok_or_else(|| NumericError::IndexOutOfRange(self, 0..length).into())
    }

    fn get_opt(self, slice: &[Node]) -> Option<&Self::Output> {
        slice.get(self)
    }

    fn get_mut_opt(self, slice: &mut [Node]) -> Option<&mut Self::Output> {
        slice.get_mut(self)
    }
}

macro_rules! range_error_impl {
    ($($type:tt)+) => {
        impl NodeSliceIndex for $($type)+ {
            type Output = [Node];

            fn get(self, slice: &[Node]) -> crate::Result<&Self::Output> {
                slice.get(self.clone()).ok_or_else(|| {
                    NumericError::SliceOutOfRange {
                        slice_start: self.start_bound().cloned(),
                        slice_end: self.end_bound().cloned(),
                        actual_range: 0..slice.len(),
                    }
                    .into()
                })
            }

            fn get_mut(self, slice: &mut [Node]) -> crate::Result<&mut Self::Output> {
                let length = slice.len(); // done here due to borrow rules
                slice.get_mut(self.clone()).ok_or_else(|| {
                    NumericError::SliceOutOfRange {
                        slice_start: self.start_bound().cloned(),
                        slice_end: self.end_bound().cloned(),
                        actual_range: 0..length,
                    }
                    .into()
                })
            }

            fn get_opt(self, slice: &[Node]) -> Option<&Self::Output> {
                slice.get(self)
            }

            fn get_mut_opt(self, slice: &mut [Node]) -> Option<&mut Self::Output> {
                slice.get_mut(self)
            }
        }
    }
}

range_error_impl!(ops::Range<usize>);
range_error_impl!(ops::RangeTo<usize>);
range_error_impl!(ops::RangeFrom<usize>);
range_error_impl!(ops::RangeFull);
range_error_impl!(ops::RangeInclusive<usize>);
range_error_impl!(ops::RangeToInclusive<usize>);
range_error_impl!((ops::Bound<usize>, ops::Bound<usize>));

// unstable
// std::range::Range<usize>
// std::range::RangeInclusive<usize>
// std::range::RangeFrom<usize>
