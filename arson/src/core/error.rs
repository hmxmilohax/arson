// SPDX-License-Identifier: LGPL-3.0-or-later

use super::{NodeType, Symbol};

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Type mismatch: expected {expected:?}, got {actual:?}")]
    TypeMismatch { expected: NodeType, actual: NodeType },

    #[error("Type mismatch: expected one of {expected:?}, got {actual:?}")]
    UnhandledType { expected: Vec<NodeType>, actual: NodeType },

    #[error("Index {index} outside of range {range:?}")]
    IndexOutOfRange {
        index: usize,
        range: std::ops::Range<usize>,
    },

    #[error("Duplicate entry for symbol {0}")]
    DuplicateEntry(Symbol),

    #[error("{0}")]
    Failure(String),
}

pub type Result<T> = std::result::Result<T, self::Error>;
