// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::prelude::*;

pub fn register_funcs(context: &mut Context) {
    context.add_macro("kDataInt", arson_array![NodeKind::Integer]);
    context.add_macro("kDataFloat", arson_array![NodeKind::Float]);
    context.add_macro("kDataString", arson_array![NodeKind::String]);
    context.add_macro("kDataSymbol", arson_array![NodeKind::Symbol]);
    context.add_macro("kDataVar", arson_array![NodeKind::Variable]);

    context.add_macro("kDataFunc", arson_array![NodeKind::Function]);
    context.add_macro("kDataObject", arson_array![NodeKind::Object]);

    context.add_macro("kDataArray", arson_array![NodeKind::Array]);
    context.add_macro("kDataCommand", arson_array![NodeKind::Command]);
    context.add_macro("kDataProperty", arson_array![NodeKind::Property]);

    // Already handled by the parser, but may as well for completeness
    context.add_macro("kDataUnhandled", arson_array![NodeKind::Unhandled]);

    context.register_func("type", self::r#type);

    context.register_func("set", self::set);
    context.register_func("set_var", self::set_var);
    context.register_func("set_this", self::set_this);

    context.register_func("var", self::var);
}

fn r#type(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 1);
    let value = args.evaluate(context, 0)?;
    Ok(value.get_kind().into())
}

fn set(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 2);
    let value = args.evaluate(context, 1)?;
    args.set_variable(context, 0, value.clone())?;
    Ok(value.into())
}

fn set_var(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 2);

    let name = args.force_symbol(context, 0)?;
    let value = args.evaluate(context, 1)?;
    context.set_variable(name, value.clone());

    Ok(value.into())
}

fn set_this(_context: &mut Context, _args: &NodeSlice) -> ExecuteResult {
    todo!("set_this")
}

fn var(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 1);
    let name = args.force_symbol(context, 0)?;
    Ok(context.get_variable(name).into())
}
