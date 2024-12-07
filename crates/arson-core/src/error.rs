// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::{ArrayError, EvaluationError, ExecutionError}; //, LoadError};

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Evaluation error: {0}")]
    EvaluationError(#[from] EvaluationError),

    #[error("Execution error: {0}")]
    ExecutionError(#[from] ExecutionError),

    #[error("Array error: {0}")]
    ArrayError(#[from] ArrayError),

    // #[error("Error loading script file: {0}")]
    // LoadError(#[from] LoadError),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

pub type Result<T = ()> = std::result::Result<T, self::Error>;

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
    ($array:ident, $len:expr, $msg:literal) => {
        if $array.len() != $len {
            $crate::arson_fail!(concat!($msg, ": expected {}, got {}"), $len, $array.len());
        }
    };
}
