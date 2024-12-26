// SPDX-License-Identifier: LGPL-3.0-or-later

pub use arson_core as core;
pub use arson_core::*;
#[cfg(feature = "arson-fs")]
pub use arson_fs as fs;
#[cfg(feature = "arson-parse")]
pub use arson_parse as parse;
#[cfg(feature = "arson-stdlib")]
pub use arson_stdlib as stdlib;

pub mod prelude {
    pub use arson_core::prelude::*;
    #[cfg(feature = "arson-fs")]
    pub use arson_fs::prelude::*;
    #[cfg(feature = "arson-stdlib")]
    pub use arson_stdlib::prelude::*;
}
