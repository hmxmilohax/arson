// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::prelude::*;

mod array;
mod flow;
mod numeric;
mod operator;
mod value;

pub fn register_funcs<S>(context: &mut Context<S>) {
    context.add_macro_define("ARSON");

    array::register_funcs(context);
    flow::register_funcs(context);
    numeric::register_funcs(context);
    operator::register_funcs(context);
    value::register_funcs(context);
}
