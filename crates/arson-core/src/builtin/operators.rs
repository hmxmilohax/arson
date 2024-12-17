// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::prelude::*;
use crate::{Number, Integer};

pub fn register_funcs<S>(context: &mut Context<S>) {
    unary::register_funcs(context);
    binary::register_funcs(context);
    bitwise::register_funcs(context);
    logical::register_funcs(context);
    comparison::register_funcs(context);
}

pub mod unary {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("++", self::increment);
        context.register_func("--", self::decrement);
        // context.register_func_by_name("+", self::promote); // No need for a promotion operator
        // context.register_func_by_name("-", self::negate); // "-" registered by `binary`
        context.register_func("~", self::not);
    }

    pub fn increment<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);

        // Forward to arithmetic operator
        let add_args = [args.get(0)?.clone(), Node::from(1)];
        let result = binary::add(context, NodeSlice::new(&add_args))?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn decrement<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);

        // Forward to arithmetic operator
        let subtract_args = [args.get(0)?.clone(), Node::from(-1)];
        let result = binary::subtract(context, NodeSlice::new(&subtract_args))?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn negate<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        match args.number(context, 0)? {
            Number::Integer(value) => Ok((-value).into()),
            Number::Float(value) => Ok((-value).into()),
        }
    }

    pub fn not<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let result = !args.integer(context, 0)?;
        Ok(result.into())
    }
}

pub mod binary {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("+", self::add);
        context.register_func("+=", self::add_assign);
        context.register_func("-", self::subtract);
        context.register_func("-=", self::subtract_assign);
        context.register_func("*", self::multiply);
        context.register_func("*=", self::multiply_assign);
        context.register_func("/", self::divide);
        context.register_func("/=", self::divide_assign);
        context.register_func("%", self::modulo);
        context.register_func("%=", self::modulo_assign);
    }

    pub fn add<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        args.number_chain(context, |left, right| Ok(left + right), |left, right| Ok(left + right))
    }

    pub fn subtract<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        if args.len() == 1 {
            return unary::negate(context, args);
        }

        args.number_chain(context, |left, right| Ok(left - right), |left, right| Ok(left - right))
    }

    pub fn multiply<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        args.number_chain(context, |left, right| Ok(left * right), |left, right| Ok(left * right))
    }

    pub fn divide<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        args.number_chain(
            context,
            |left, right| {
                arson_assert!(right.0 != 0, "attempted to divide by zero");
                Ok(left / right)
            },
            |left, right| Ok(left / right),
        )
    }

    pub fn modulo<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        args.number_chain(
            context,
            |left, right| {
                arson_assert!(right.0 != 0, "attempted to modulo by zero");
                Ok(left % right)
            },
            |left, right| Ok(left % right),
        )
    }

    pub fn add_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::add(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn subtract_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::subtract(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn multiply_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::multiply(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn divide_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::divide(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn modulo_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::modulo(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }
}

pub mod bitwise {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("&", self::and);
        context.register_func("&=", self::and_assign);
        context.register_func("|", self::or);
        context.register_func("|=", self::or_assign);
        context.register_func("^", self::xor);
        context.register_func("^=", self::xor_assign);
    }

    pub fn bitwise_op<S>(
        context: &mut Context<S>,
        args: &NodeSlice,
        f: fn(Integer, Integer) -> Integer,
    ) -> ExecuteResult {
        let result = args.integer(context, 0)?;
        let result = args
            .slice(1..)?
            .iter()
            .try_fold(result, |current, n| n.integer(context).map(|value| f(current, value)))?;

        Ok(result.into())
    }

    pub fn and<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        bitwise_op(context, args, |current, value| current & value)
    }

    pub fn or<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        bitwise_op(context, args, |current, value| current | value)
    }

    pub fn xor<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.integer(context, 0)? ^ args.integer(context, 1)?;
        Ok(result.into())
    }

    pub fn and_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::and(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn or_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::or(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn xor_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::xor(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }
}

pub mod logical {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("&&", self::and);
        context.register_func("||", self::or);
        context.register_func("^^", self::xor);
        context.register_func("!", self::not);
    }

    pub fn and<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        for node in args {
            if !node.boolean(context)? {
                return Ok(Node::FALSE);
            }
        }

        Ok(Node::TRUE)
    }

    pub fn or<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        for node in args {
            if node.boolean(context)? {
                return Ok(Node::TRUE);
            }
        }

        Ok(Node::FALSE)
    }

    pub fn xor<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.boolean(context, 0)? ^ args.boolean(context, 1)?;
        Ok(result.into())
    }

    pub fn not<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let result = !args.boolean(context, 0)?;
        Ok(result.into())
    }
}

pub mod comparison {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("==", self::equal);
        context.register_func("!=", self::not_equal);
        context.register_func(">", self::greater_than);
        context.register_func(">=", self::greater_equal);
        context.register_func("<", self::less_than);
        context.register_func("<=", self::less_equal);
    }

    pub fn equal<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? == args.evaluate(context, 1)?;
        Ok(result.into())
    }

    pub fn not_equal<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? != args.evaluate(context, 1)?;
        Ok(result.into())
    }

    pub fn greater_than<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? > args.evaluate(context, 1)?;
        Ok(result.into())
    }

    pub fn greater_equal<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? >= args.evaluate(context, 1)?;
        Ok(result.into())
    }

    pub fn less_than<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? < args.evaluate(context, 1)?;
        Ok(result.into())
    }

    pub fn less_equal<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? <= args.evaluate(context, 1)?;
        Ok(result.into())
    }
}
