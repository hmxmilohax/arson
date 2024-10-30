// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::NodeType;

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

    #[error("{0}")]
    Failure(String),
}
