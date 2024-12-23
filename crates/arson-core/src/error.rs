// SPDX-License-Identifier: LGPL-3.0-or-later

use std::backtrace::Backtrace;

use crate::{ArrayError, EvaluationError, ExecutionError, LoadError, NumericError};

pub type Result<T = ()> = std::result::Result<T, self::Error>;

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum ErrorKind {
    #[error("{0}")]
    EvaluationError(#[from] EvaluationError),

    #[error("{0}")]
    ExecutionError(#[from] ExecutionError),

    #[error("{0}")]
    NumericError(#[source] NumericError),

    #[error("{0}")]
    ArrayError(#[from] ArrayError),

    #[error("{0}")]
    IoError(#[from] std::io::Error),

    #[error("{0}")]
    LoadError(#[from] LoadError),
}

impl From<std::io::ErrorKind> for crate::ErrorKind {
    fn from(value: std::io::ErrorKind) -> Self {
        Self::IoError(value.into())
    }
}

impl<E: Into<NumericError>> From<E> for crate::ErrorKind {
    fn from(value: E) -> Self {
        Self::NumericError(value.into())
    }
}

/// Data is separated out and boxed to keep the size of [`Error`] down,
/// and consequently the size of [`Result`].
#[derive(Debug)]
struct ErrorData {
    kind: ErrorKind,
    location: Backtrace,
}

pub struct Error {
    data: Box<ErrorData>,
}

impl self::Error {
    pub fn new(kind: ErrorKind) -> Self {
        Self {
            data: Box::new(ErrorData { kind, location: Backtrace::capture() }),
        }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.data.kind
    }

    pub fn location(&self) -> &Backtrace {
        &self.data.location
    }
}

impl std::error::Error for self::Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.data.kind)
    }
}

impl std::fmt::Debug for self::Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Skip straight to displaying `data` for simplicity
        self.data.fmt(f)
    }
}

impl std::fmt::Display for self::Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.data.kind.fmt(f)
    }
}

impl<E: Into<ErrorKind>> From<E> for self::Error {
    fn from(value: E) -> Self {
        Self::new(value.into())
    }
}

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
        return Err(
            $crate::ExecutionError::Failure(format!($($arg)+)).into()
        )
    };
}

#[macro_export]
macro_rules! arson_assert_len {
    ($array:ident, $len:expr) => {
        if $array.len() != $len {
            return Err($crate::ArrayError::LengthMismatch {
                expected: $len,
                actual: $array.len(),
            }.into());
        }
    };
    ($array:ident, $len:expr, $($arg:expr)+) => {
        if $array.len() != $len {
            $crate::arson_fail!("{}: expected {}, got {}", format!($($arg)+), $len, $array.len());
        }
    };
}
