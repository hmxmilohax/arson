// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::Context;

pub mod math;

pub fn register_funcs(context: &mut Context) {
    math::register_funcs(context);
}
