// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::core::*;

pub fn register_funcs(context: &mut Context) {
    control::register_funcs(context);
    loops::register_funcs(context);
    vars::register_funcs(context);
}

pub mod control {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func_by_name("if", self::if_block);
        context.register_func_by_name("if_else", self::if_else_block);
        context.register_func_by_name("unless", self::unless_block);
    }

    pub fn if_block(context: &mut Context, args: &NodeSlice) -> HandleResult {
        if args.boolean(context, 0)? {
            for node in args.get(1..)? {
                node.command()?.execute(context)?;
            }
        }

        Ok(NodeValue::HANDLED)
    }

    pub fn if_else_block(context: &mut Context, args: &NodeSlice) -> HandleResult {
        if args.boolean(context, 0)? {
            args.command(1)?.execute(context)
        } else {
            args.command(2)?.execute(context)
        }
    }

    pub fn unless_block(context: &mut Context, args: &NodeSlice) -> HandleResult {
        if !args.boolean(context, 0)? {
            for node in args.get(1..)? {
                node.command()?.execute(context)?;
            }
        }

        Ok(NodeValue::HANDLED)
    }
}

pub mod loops {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func_by_name("while", self::while_block);
        context.register_func_by_name("foreach", self::foreach_block);
        context.register_func_by_name("foreach_int", self::foreach_int);
    }

    pub fn while_block(context: &mut Context, args: &NodeSlice) -> HandleResult {
        while args.boolean(context, 0)? {
            for node in args.get(1..)? {
                node.command()?.execute(context)?;
            }
        }

        Ok(NodeValue::HANDLED)
    }

    pub fn foreach_block(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let variable = args.variable(0)?;

        for node in args.array(context, 1)?.iter() {
            let value = node.evaluate(context)?;
            variable.set(context, value);
            for node in args.get(2..)? {
                node.command()?.execute(context)?;
            }
        }

        Ok(NodeValue::HANDLED)
    }

    pub fn foreach_int(context: &mut Context, args: &NodeSlice) -> HandleResult {
        fn run_loop(
            context: &mut Context,
            args: &NodeSlice,
            variable: Variable,
            values: impl Iterator<Item = i64>,
        ) -> HandleResult {
            for value in values {
                variable.set(context, value.into());
                for node in args.get(3..)? {
                    node.command()?.execute(context)?;
                }
            }

            Ok(NodeValue::HANDLED)
        }

        let variable = args.variable(0)?;
        let start = args.integer(context, 1)?;
        let end = args.integer(context, 2)?;
        let args = args.slice(3..)?;

        if start > end {
            run_loop(context, args, variable, (end..start).rev())
        } else {
            run_loop(context, args, variable, start..end)
        }
    }
}

pub mod vars {
    use crate::arson_assert_len;

    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func_by_name("do", self::do_block);
        context.register_func_by_name("with", self::with_block);
    }

    pub fn do_block(context: &mut Context, mut args: &NodeSlice) -> HandleResult {
        let mut saved_variables = VariableStack::new();
        while let NodeValue::Array(initializer) = args.evaluate(context, 0)? {
            args = args.slice(1..)?;

            let variable = initializer.variable(0)?;
            saved_variables.save(context, &variable);

            if let Some(value) = initializer.get_opt(1) {
                arson_assert_len!(initializer, 2, "multiple values present in `do` variable initializer");
                let value = value.evaluate(context)?;
                variable.set(context, value);
            }
        }

        for node in args.slice(..args.len() - 1)? {
            node.command()?.execute(context)?;
        }

        let result = args.evaluate(context, args.len() - 1)?;
        saved_variables.restore(context);
        Ok(result)
    }

    pub fn with_block(_context: &mut Context, _args: &NodeSlice) -> HandleResult {
        todo!("`with` func")
    }
}
