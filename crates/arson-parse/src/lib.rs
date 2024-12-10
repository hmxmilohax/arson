// SPDX-License-Identifier: LGPL-3.0-or-later

/// Transform one fragment into another.
macro_rules! meta_morph {
    ($_:tt => $($i:tt)*) => {
        $($i)*
    };
}

#[cfg(feature = "loading")]
pub use arson_core::{ArrayKind, FloatValue, IntegerValue};

#[cfg(not(feature = "loading"))]
mod core_shim;
mod diagnostics;
mod lexer;
#[cfg(feature = "loading")]
mod loader;
mod parser;

// Re-export so dependers don't have to sync versions
#[cfg(feature = "reporting")]
pub use codespan_reporting as reporting;
#[cfg(not(feature = "loading"))]
pub use core_shim::*;
pub use diagnostics::*;
pub use lexer::*;
#[cfg(feature = "loading")]
pub use loader::*;
pub use parser::*;

pub mod prolog {
    pub use super::lexer::{Token, TokenValue, Tokenizer};
    #[cfg(feature = "loading")]
    pub use super::loader::{LoadError, LoadOptions};
    pub use super::parser::{Expression, ExpressionValue};
}
