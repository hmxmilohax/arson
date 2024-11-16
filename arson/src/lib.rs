// SPDX-License-Identifier: LGPL-3.0-or-later

// Some macro metaprogramming hacks

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
mod core;
pub mod fs;
pub mod parse;
pub mod stdlib;

pub use core::*;

pub use parse::loader::{LoadError, LoadOptions};
