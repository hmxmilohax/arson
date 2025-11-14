// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::prelude::*;

pub fn register_funcs(context: &mut Context) {
    control::register_funcs(context);
    r#loop::register_funcs(context);
    scope::register_funcs(context);
}

mod control {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("if", self::r#if);
        context.register_func("if_else", self::if_else);
        context.register_func("unless", self::unless);
    }

    fn r#if(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        if args.boolean(context, 0)? {
            for node in args.get(1..)? {
                node.command()?.execute(context)?;
            }
        }

        Ok(Node::HANDLED)
    }

    fn if_else(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 3);
        if args.boolean(context, 0)? {
            Ok(args.evaluate(context, 1)?.into())
        } else {
            Ok(args.evaluate(context, 2)?.into())
        }
    }

    fn unless(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        if !args.boolean(context, 0)? {
            for node in args.get(1..)? {
                node.command()?.execute(context)?;
            }
        }

        Ok(Node::HANDLED)
    }
}

mod r#loop {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("while", self::r#while);
        context.register_func("foreach", self::foreach);
        context.register_func("foreach_int", self::foreach_int);
    }

    fn r#while(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        while args.boolean(context, 0)? {
            for node in args.get(1..)? {
                node.command()?.execute(context)?;
            }
        }

        Ok(Node::HANDLED)
    }

    fn foreach(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        let variable = args.variable(0)?;

        for node in args.array(context, 1)?.borrow()?.iter() {
            let value = node.evaluate(context)?;
            variable.set(context, value);
            for node in args.get(2..)? {
                node.command()?.execute(context)?;
            }
        }

        Ok(Node::HANDLED)
    }

    fn foreach_int(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        fn run_loop(
            context: &mut Context,
            block: &NodeSlice,
            variable: &Variable,
            values: impl Iterator<Item = i64>,
        ) -> ExecuteResult {
            for value in values {
                variable.set(context, value);
                for node in block {
                    node.command()?.execute(context)?;
                }
            }

            Ok(Node::HANDLED)
        }

        let variable = args.variable(0)?;
        let start = args.integer(context, 1)?.0;
        let end = args.integer(context, 2)?.0;
        let block = args.slice(3..)?;

        if start > end {
            run_loop(context, block, variable, (end..start).rev())
        } else {
            run_loop(context, block, variable, start..end)
        }
    }
}

mod scope {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("do", self::r#do);
        // context.register_func("with", self::with_block);
    }

    fn r#do(context: &mut Context, mut args: &NodeSlice) -> ExecuteResult {
        let mut saved_variables = VariableStack::new(context);
        saved_variables.push_initializers(&mut args)?;

        let result = saved_variables.context().execute_block(args);
        drop(saved_variables); // ensure drop does not occur until after execution
        result
    }
}
