// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_core::prelude::*;

pub fn register_funcs<S>(context: &mut Context<S>) {
    context.register_func("print", self::print);
}

pub fn print<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
    if !args.is_empty() {
        // Manual enumeration of nodes to avoid adding a separator,
        // to match the original `print`
        print!("> ");
        for arg in args {
            print!("{}", arg.display_evaluated(context))
        }
        println!();
    }

    Ok(Node::HANDLED)
}
