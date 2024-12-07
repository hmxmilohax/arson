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

pub mod prolog {
    pub use super::{
        context::Context,
        primitives::{Node, NodeArray, NodeCommand, NodeProperty, NodeSlice, NodeValue, Symbol, Variable},
    };
}
