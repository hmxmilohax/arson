// SPDX-License-Identifier: LGPL-3.0-or-later

use std::ops::Range;

use crate::LoadError;

use super::{NodeKind, Symbol};

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Type mismatch: expected {expected:?}, got {actual:?}")]
    TypeMismatch { expected: NodeKind, actual: NodeKind },

    #[error("Bad array length {actual}, expected {expected}")]
    LengthMismatch { expected: usize, actual: usize },

    #[error("Index outside of range {0:?}")]
    OutOfRange(Range<usize>),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("Error loading file: {0}")]
    LoadError(#[from] LoadError),

    #[error("Requested entry was not found")]
    EntryNotFound,

    #[error("Duplicate entry for symbol {0}")]
    DuplicateEntry(Symbol),

    #[error("{0}")]
    Failure(String),
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
    ($array:ident, $len:expr) => {
        if $array.len() != $len {
            return Err($crate::Error::LengthMismatch { expected: $len, actual: $array.len() });
        }
    };
    ($array:ident, $len:expr, $msg:literal) => {
        if $array.len() != $len {
            $crate::arson_fail!(concat!($msg, ": expected {}, got {}"), $len, $array.len());
        }
    };
}
