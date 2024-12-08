// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_core::Context;

pub mod fs;
pub mod math;
pub mod stdio;

mod state;
pub use state::*;

pub fn register_funcs<S: StdlibState>(context: &mut Context<S>) {
    fs::register_funcs(context);
    math::register_funcs(context);
    stdio::register_funcs(context);
}

pub mod prolog {
    pub use super::state::*;
}
