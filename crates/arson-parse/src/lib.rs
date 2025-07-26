// SPDX-License-Identifier: LGPL-3.0-or-later

/// Transform one fragment into another.
macro_rules! meta_morph {
    (|$_:tt| $($i:tt)*) => {
        $($i)*
    };
}

/// Returns the result of a pattern match, panicking if the pattern wasn't matched.
macro_rules! match_unwrap {
    ($expression:expr, $pattern:pat => $result:expr) => {
        match $expression {
            $pattern => $result,
            _ => panic!("pattern \"{}\" was not matched", stringify!($pattern)),
        }
    };
}

mod diagnostics;
#[cfg(feature = "encoding")]
pub mod encoding;
mod lexer;
mod parser;

pub use diagnostics::*;
pub use lexer::*;
pub use parser::*;

pub mod prelude {
    pub use super::diagnostics::{Diagnostic, DiagnosticKind};
    pub use super::lexer::{Token, TokenValue, Tokenizer};
    pub use super::parser::{Expression, ExpressionValue};
}

#[cfg(feature = "reporting")]
// Re-export so dependers don't have to sync versions
pub use codespan_reporting as reporting;

pub const MAX_SYMBOL_LENGTH: usize = 50;
