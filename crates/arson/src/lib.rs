// SPDX-License-Identifier: LGPL-3.0-or-later

pub use arson_core::*;
pub use arson_parse::{load_path, load_text, LoadOptions};
pub use {arson_core as core, arson_fs as fs, arson_parse as parse, arson_stdlib as stdlib};

pub mod prolog {
    pub use arson_core::prolog::*;
    pub use arson_fs::prolog::*;
    pub use arson_parse::prolog::*;
    pub use arson_stdlib::prolog::*;
}
