// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::{ArrayError, EvaluationError, ExecutionError, NumericError};

pub type Result<T = ()> = std::result::Result<T, self::Error>;

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Evaluation error: {0}")]
    EvaluationError(#[from] EvaluationError),

    #[error("Execution error: {0}")]
    ExecutionError(#[from] ExecutionError),

    #[error("Numeric error: {0}")]
    NumericError(#[from] NumericError),

    #[error("Array error: {0}")]
    ArrayError(#[from] ArrayError),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

impl From<std::num::TryFromIntError> for Error {
    fn from(value: std::num::TryFromIntError) -> Self {
        Self::NumericError(value.into())
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(value: std::num::ParseIntError) -> Self {
        Self::NumericError(value.into())
    }
}

impl From<std::num::ParseFloatError> for Error {
    fn from(value: std::num::ParseFloatError) -> Self {
        Self::NumericError(value.into())
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
            $crate::Error::ExecutionError(
                $crate::ExecutionError::Failure(format!($($arg)+))
            )
        )
    };
}

#[macro_export]
macro_rules! arson_assert_len {
    ($array:ident, $len:expr) => {
        if $array.len() != $len {
            return Err($crate::Error::ArrayError($crate::ArrayError::LengthMismatch {
                expected: $len,
                actual: $array.len(),
            }));
        }
    };
    ($array:ident, $len:expr, $($arg:expr)+) => {
        if $array.len() != $len {
            $crate::arson_fail!("{}: expected {}, got {}", format!($($arg)+), $len, $array.len());
        }
    };
}
