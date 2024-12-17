// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::prelude::*;

mod array;
mod flow;
mod operators;

pub fn register_funcs<S>(context: &mut Context<S>) {
    context.add_macro_define("ARSON");

    context.add_macro("TRUE", arson_array![1]);
    context.add_macro("FALSE", arson_array![0]);

    array::register_funcs(context);
    flow::register_funcs(context);
    operators::register_funcs(context);
}
