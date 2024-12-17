// SPDX-License-Identifier: LGPL-3.0-or-later

/// Transform one fragment into another.
macro_rules! meta_morph {
    ($_:tt => $($i:tt)*) => {
        $($i)*
    };
}

/// Select the first fragment if it exists, otherwise the second.
macro_rules! meta_select {
    ($first:tt, $($second:tt)*) => {
        $first
    };
    // to avoid local ambiguity issues
    (meta_morph!($_:tt => $($first:tt)*), $($second:tt)*) => {
        $($first)*
    };
    (, $($second:tt)*) => {
        $($second)*
    };
}

mod builtin;
mod primitives;

pub use primitives::*;

mod context;
mod error;

pub use context::*;
pub use error::*;

pub mod prelude {
    pub use super::context::{Context, ExecuteResult};
    pub use super::primitives::{
        ArrayRef,
        Node,
        NodeArray,
        NodeCommand,
        NodeKind,
        NodeProperty,
        NodeSlice,
        NodeValue,
        Symbol,
        SymbolMap,
        Variable,
        VariableStack,
    };
    pub use super::{arson_array, arson_assert, arson_assert_len, arson_fail};
}
