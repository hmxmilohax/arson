// SPDX-License-Identifier: LGPL-3.0-or-later

#![cfg_attr(error_generic_member_access, feature(error_generic_member_access))]

/// Transform one fragment into another.
macro_rules! meta_morph {
    (|$_:tt| $($i:tt)*) => {
        $($i)*
    };
}

/// Select the first fragment if it exists, otherwise the second.
macro_rules! meta_select {
    ($first:tt, $($second:tt)*) => {
        $first
    };
    // to avoid local ambiguity issues
    (meta_morph!(|$_:tt| $($first:tt)*), $($second:tt)*) => {
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
#[cfg(feature = "text-loading")]
mod loader;

pub use context::*;
pub use error::*;
#[cfg(feature = "text-loading")]
pub use loader::*;

pub mod prelude {
    pub use super::context::{Context, ContextState, ExecuteResult};
    #[cfg(feature = "text-loading")]
    pub use super::loader::{LoadError, LoadOptions};
    pub use super::primitives::{
        ArrayRef,
        HandleFn,
        Node,
        NodeArray,
        NodeCommand,
        NodeKind,
        NodeProperty,
        NodeSlice,
        NodeValue,
        Object,
        ObjectRef,
        Symbol,
        SymbolMap,
        Variable,
        VariableSave,
        VariableStack,
    };
    pub use super::{arson_array, arson_assert, arson_assert_len, arson_fail, arson_slice};
}
