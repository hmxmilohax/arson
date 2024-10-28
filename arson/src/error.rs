// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::{Node, NodeType};

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Type mismatch: expected {expected:?}, got {actual:?}")]
    TypeMismatch { expected: NodeType, actual: NodeType },
    #[error("Unhandled type {0:?}")]
    UnhandledType(NodeType),
    #[error("{0}")]
    Failure(String),
}

impl Error {
    pub const fn unhandled(node: &Node) -> Self {
        Self::UnhandledType(node.get_type())
    }
}
