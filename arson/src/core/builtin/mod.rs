// SPDX-License-Identifier: LGPL-3.0-or-later

use super::Context;

pub mod arithmetic;

pub fn register_funcs(context: &mut Context) {
    arithmetic::register_funcs(context);
}
