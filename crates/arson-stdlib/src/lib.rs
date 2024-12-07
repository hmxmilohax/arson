// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_core::Context;
use arson_fs::FsState;

pub mod fs;
pub mod math;
pub mod stdio;

pub fn register_funcs<S: FsState>(context: &mut Context<S>) {
    fs::register_funcs(context);
    math::register_funcs(context);
    stdio::register_funcs(context);
}
