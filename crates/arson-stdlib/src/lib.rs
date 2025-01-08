// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_core::Context;

pub mod fs;
pub mod math;
pub mod process;
pub mod stdio;

mod options;
pub use options::*;

pub fn register_funcs(context: &mut Context) {
    assert!(
        context.get_state::<StdlibOptions>().is_ok(),
        "StdlibOptions state must be registered before registering stdlib"
    );

    fs::register_funcs(context);
    math::register_funcs(context);
    process::register_funcs(context);
    stdio::register_funcs(context);
}

pub mod prelude {
    pub use super::options::*;
}
