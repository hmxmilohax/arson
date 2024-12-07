// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::*;

mod flow;
mod operators;

pub fn register_funcs<S>(context: &mut Context<S>) {
    flow::register_funcs(context);
    operators::register_funcs(context);
}
