// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::core::*;

pub mod io;
pub mod math;

pub fn register_funcs(context: &mut Context) {
    io::register_funcs(context);
    math::register_funcs(context);
}
