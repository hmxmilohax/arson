// SPDX-License-Identifier: LGPL-3.0-or-later

use std::cell::Cell;

use arson_core::prelude::*;

pub struct VarargFormatter<'a> {
    context: Cell<Option<&'a mut Context>>,
    args: &'a NodeSlice,
}

impl<'a> VarargFormatter<'a> {
    pub fn new(context: &'a mut Context, args: &'a NodeSlice) -> Self {
        Self { context: Cell::new(Some(context)), args }
    }
}

impl std::fmt::Display for VarargFormatter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Manual enumeration of nodes to avoid adding a separator,
        // to match the original `print`

        match self.context.take() {
            Some(context) => {
                for arg in self.args {
                    match arg.evaluate(context) {
                        Ok(value) => value.fmt(f)?,
                        Err(err) => write!(f, "<error: {err}>")?,
                    }
                }
                self.context.set(Some(context));
            },
            None => {
                for arg in self.args {
                    arg.unevaluated().fmt(f)?;
                }
            },
        }

        Ok(())
    }
}

pub fn register_funcs(context: &mut Context) {
    context.register_func("print", self::print);
}

pub fn print(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    if !args.is_empty() {
        println!("> {}", VarargFormatter::new(context, args));
    }

    Ok(Node::HANDLED)
}
