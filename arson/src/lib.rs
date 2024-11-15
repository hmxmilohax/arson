// SPDX-License-Identifier: LGPL-3.0-or-later

/// Macro hack to assist in scenarios where something must be
/// conditionally used based on the existence of an optional fragment.
macro_rules! param_sink {
    ($_:tt, $($i:tt)*) => {
        $($i)*
    };
}

mod builtin;
mod core;
pub mod fs;
pub mod parse;
pub mod stdlib;

pub use core::*;

pub use parse::loader::LoadError;
