// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::prelude::*;
use crate::SymbolTable;

mod array;
mod flow;
mod numeric;
mod operator;
mod value;

pub(crate) struct BuiltinState {
    pub unquote: Symbol,
    pub unquote_abbrev: Symbol,
}

impl BuiltinState {
    pub(crate) fn new(symbol_table: &mut SymbolTable) -> Self {
        Self {
            unquote: symbol_table.add("unquote"),
            unquote_abbrev: symbol_table.add(","),
        }
    }
}

pub fn register_funcs<S>(context: &mut Context<S>) {
    context.add_macro_define("ARSON");

    array::register_funcs(context);
    flow::register_funcs(context);
    numeric::register_funcs(context);
    operator::register_funcs(context);
    value::register_funcs(context);
}
