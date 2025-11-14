// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_core::{Context, LoadOptions};

pub mod fs;
pub mod math;
pub mod process;
pub mod stdio;

pub struct StdlibOptions {
    pub file_load: LoadOptions,
}

pub fn register_funcs(context: &mut Context, options: StdlibOptions) {
    fs::register_funcs(context, options.file_load);
    math::register_funcs(context);
    process::register_funcs(context);
    stdio::register_funcs(context);
}

pub mod prelude {
    pub use super::StdlibOptions;
}
