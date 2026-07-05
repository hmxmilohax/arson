// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_core::prelude::*;

pub fn register_funcs(context: &mut Context) {
    context.register_func("print", self::print);
    context.register_func("sprint", self::sprint);
}

pub fn print(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    if !args.is_empty() {
        println!("{}", args.concat_evaluated(context));
    }

    Ok(Node::HANDLED)
}

pub fn sprint(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    if !args.is_empty() {
        Ok(args.concat_evaluated(context).to_string().into())
    } else {
        Ok(String::new().into())
    }
}
