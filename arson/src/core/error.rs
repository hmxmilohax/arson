// SPDX-License-Identifier: LGPL-3.0-or-later

use std::ops::Range;

use crate::LoadError;

use super::{NodeType, Symbol};

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Type mismatch: expected {expected:?}, got {actual:?}")]
    TypeMismatch { expected: NodeType, actual: NodeType },

    #[error("Type mismatch: expected one of {expected:?}, got {actual:?}")]
    UnhandledType { expected: Vec<NodeType>, actual: NodeType },

    #[error("Left operand {left:?} is not compatible with right operand {right:?}")]
    BadOperand { left: NodeType, right: NodeType },

    #[error("Bad array length {actual}, expected {expected}")]
    LengthMismatch { expected: usize, actual: usize },

    #[error("Index outside of range {0:?}")]
    OutOfRange(Range<usize>),

    #[error("Error loading file: {0}")]
    LoadError(#[from] LoadError),

    #[error("Entry for symbol {0} not found")]
    EntryNotFound(Symbol),

    #[error("Duplicate entry for symbol {0}")]
    DuplicateEntry(Symbol),

    #[error("{0}")]
    Failure(String),
}

impl Error {
    pub fn bad_operand(left: NodeType, right: NodeType) -> Self {
        Error::BadOperand { left, right }
    }
}

pub type Result<T> = std::result::Result<T, self::Error>;

#[macro_export]
macro_rules! arson_assert {
    ($cond:expr $(,)?) => {
        if !$cond {
            $crate::arson_fail!("Assertion failed: {}", stringify!($cond));
        }
    };
    ($cond:expr, $($arg:tt)+) => {
        if !$cond {
            $crate::arson_fail!($($arg)+);
        }
    };
}

#[macro_export]
macro_rules! arson_fail {
    ($($arg:tt)+) => {
        return Err($crate::Error::Failure(format!($($arg)+)))
    };
}

#[macro_export]
macro_rules! arson_assert_len {
    ($array:ident, $len:literal) => {
        if $array.len() != $len {
            return Err($crate::Error::LengthMismatch { expected: $len, actual: $array.len() });
        }
    };
}
