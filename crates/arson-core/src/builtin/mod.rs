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

    r#type::register_funcs(context);
}

mod r#type {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.add_macro("kDataInt", arson_array![NodeKind::Integer]);
        context.add_macro("kDataFloat", arson_array![NodeKind::Float]);
        context.add_macro("kDataString", arson_array![NodeKind::String]);
        context.add_macro("kDataSymbol", arson_array![NodeKind::Symbol]);
        context.add_macro("kDataVar", arson_array![NodeKind::Variable]);
    
        // TODO: Not yet implemented
        // context.add_macro("kDataFunc", arson_array![NodeKind::Function]);
        // context.add_macro("kDataObject", arson_array![NodeKind::Object]);
    
        context.add_macro("kDataArray", arson_array![NodeKind::Array]);
        context.add_macro("kDataCommand", arson_array![NodeKind::Command]);
        context.add_macro("kDataProperty", arson_array![NodeKind::Property]);
    
        // Already handled by the parser
        // context.add_macro("kDataUnhandled, arson_array![NodeKind::Unhandled]);
    
        context.register_func("type", self::r#type);
    }

    pub fn r#type<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let value = args.evaluate(context, 0)?;
        Ok(value.get_kind().into())
    }
}
