// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::prelude::*;

pub fn register_funcs<S>(context: &mut Context<S>) {
    control::register_funcs(context);
    loops::register_funcs(context);
    vars::register_funcs(context);
}

pub mod control {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("if", self::if_block);
        context.register_func("if_else", self::if_else_block);
        context.register_func("unless", self::unless_block);
    }

    pub fn if_block<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        if args.boolean(context, 0)? {
            for node in args.get(1..)? {
                node.command()?.execute(context)?;
            }
        }

        Ok(Node::HANDLED)
    }

    pub fn if_else_block<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 3);
        if args.boolean(context, 0)? {
            args.command(1)?.execute(context)
        } else {
            args.command(2)?.execute(context)
        }
    }

    pub fn unless_block<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        if !args.boolean(context, 0)? {
            for node in args.get(1..)? {
                node.command()?.execute(context)?;
            }
        }

        Ok(Node::HANDLED)
    }
}

pub mod loops {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("while", self::while_block);
        context.register_func("foreach", self::foreach_block);
        context.register_func("foreach_int", self::foreach_int);
    }

    pub fn while_block<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        while args.boolean(context, 0)? {
            for node in args.get(1..)? {
                node.command()?.execute(context)?;
            }
        }

        Ok(Node::HANDLED)
    }

    pub fn foreach_block<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
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

    pub fn foreach_int<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        fn run_loop<S>(
            context: &mut Context<S>,
            args: &NodeSlice,
            variable: &Variable,
            values: impl Iterator<Item = i64>,
        ) -> ExecuteResult {
            for value in values {
                variable.set(context, value);
                for node in args.get(3..)? {
                    node.command()?.execute(context)?;
                }
            }

            Ok(Node::HANDLED)
        }

        let variable = args.variable(0)?;
        let start = args.integer(context, 1)?.0;
        let end = args.integer(context, 2)?.0;
        let args = args.slice(3..)?;

        if start > end {
            run_loop(context, args, variable, (end..start).rev())
        } else {
            run_loop(context, args, variable, start..end)
        }
    }
}

pub mod vars {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("do", self::do_block);
        context.register_func("with", self::with_block);
    }

    pub fn do_block<S>(context: &mut Context<S>, mut args: &NodeSlice) -> ExecuteResult {
        let mut saved_variables = VariableStack::new(context);
        saved_variables.push_initializers(&mut args)?;

        let result = saved_variables.context().execute_block(args);
        drop(saved_variables); // ensure drop does not occur until after execution
        result
    }

    pub fn with_block<S>(_context: &mut Context<S>, _args: &NodeSlice) -> ExecuteResult {
        todo!("`with` func")
    }
}
