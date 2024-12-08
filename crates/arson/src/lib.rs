// SPDX-License-Identifier: LGPL-3.0-or-later

pub use arson_core as core;
pub use arson_fs as fs;
pub use arson_parse as parse;
pub use arson_stdlib as stdlib;

pub use arson_core::*;
pub use arson_parse::prolog::*;

pub mod prolog {
    pub use super::core::prolog::*;
    pub use super::fs::prolog::*;
    pub use super::parse::prolog::*;
    pub use super::stdlib::prolog::*;
}
