// SPDX-License-Identifier: LGPL-3.0-or-later

/// Transform one fragment into another.
macro_rules! meta_morph {
    ($_:tt => $($i:tt)*) => {
        $($i)*
    };
}

mod diagnostics;
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
