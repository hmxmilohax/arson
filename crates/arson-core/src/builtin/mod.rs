// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::prelude::*;
use crate::SymbolTable;

mod array;
mod flow;
mod function;
mod numeric;
mod operator;
mod string;
mod variable;

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

pub fn register_funcs(context: &mut Context) {
    context.add_macro_define("ARSON");

    array::register_funcs(context);
    flow::register_funcs(context);
    function::register_funcs(context);
    numeric::register_funcs(context);
    operator::register_funcs(context);
    string::register_funcs(context);
    variable::register_funcs(context);
}
