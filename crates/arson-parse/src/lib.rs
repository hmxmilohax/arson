// SPDX-License-Identifier: LGPL-3.0-or-later

/// Transform one fragment into another.
macro_rules! meta_morph {
    ($_:tt => $($i:tt)*) => {
        $($i)*
    };
}

mod lexer;
mod loader;
mod parser;

pub use lexer::*;
pub use loader::*;
pub use parser::*;

pub mod prolog {
    pub use super::loader::{load_path, load_text, LoadError, LoadOptions};
}
