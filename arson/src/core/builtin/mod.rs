// SPDX-License-Identifier: LGPL-3.0-or-later

use super::Context;

pub mod operators;

pub fn register_funcs(context: &mut Context) {
    operators::register_funcs(context);
}
