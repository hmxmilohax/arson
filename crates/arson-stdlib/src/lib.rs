// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_core::Context;

pub mod io;
pub mod math;

pub fn register_funcs(context: &mut Context) {
    io::register_funcs(context);
    math::register_funcs(context);
}
