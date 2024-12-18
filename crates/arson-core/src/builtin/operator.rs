// SPDX-License-Identifier: LGPL-3.0-or-later

use std::ops::{Div, Rem};

use crate::prelude::*;
use crate::{FloatValue, Integer, Number};

pub fn register_funcs<S>(context: &mut Context<S>) {
    arithmetic::register_funcs(context);
    bitwise::register_funcs(context);
    logical::register_funcs(context);
    compare::register_funcs(context);
}

mod arithmetic {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("++", self::increment);
        context.register_func("--", self::decrement);

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

        context.register_func("mod", self::modulo);
        context.register_func("div_rem", self::divide_remainder);
    }

    fn increment<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);

        // Forward to addition operator
        let add_args = [args.get(0)?.clone(), Node::from(1)];
        let result = self::add(context, NodeSlice::new(&add_args))?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn decrement<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);

        // Forward to subtraction operator
        let subtract_args = [args.get(0)?.clone(), Node::from(-1)];
        let result = self::subtract(context, NodeSlice::new(&subtract_args))?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn add<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        args.number_chain(context, |left, right| Ok(left + right), |left, right| Ok(left + right))
    }

    fn subtract<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        if args.len() == 1 {
            // unary negate
            match args.number(context, 0)? {
                Number::Integer(value) => return Ok((-value).into()),
                Number::Float(value) => return Ok((-value).into()),
            }
        }

        args.number_chain(context, |left, right| Ok(left - right), |left, right| Ok(left - right))
    }

    fn multiply<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        args.number_chain(context, |left, right| Ok(left * right), |left, right| Ok(left * right))
    }

    fn divide<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        args.number_chain(
            context,
            |left, right| {
                arson_assert!(right.0 != 0, "attempted to divide by zero");
                Ok(left / right)
            },
            |left, right| Ok(left / right),
        )
    }

    fn modulo<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        args.number_chain(
            context,
            |left, right| {
                arson_assert!(right.0 != 0, "attempted to modulo by zero");
                Ok(left % right)
            },
            |left, right| Ok(left % right),
        )
    }

    fn add_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::add(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn subtract_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::subtract(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn multiply_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::multiply(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn divide_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::divide(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn modulo_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::modulo(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn divide_remainder<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);

        fn div_rem<T: Copy + Div + Rem>(left: T, right: T) -> ExecuteResult
        where
            NodeValue: From<<T as Div>::Output> + From<<T as Rem>::Output>,
        {
            let quotient = left / right;
            let remainder = left % right;
            Ok(NodeArray::from_iter([NodeValue::from(quotient), NodeValue::from(remainder)]).into())
        }

        let left = args.number(context, 0)?;
        let right = args.number(context, 1)?;

        match (left, right) {
            (Number::Integer(left), Number::Integer(right)) => div_rem(left, right),
            (Number::Float(left), Number::Float(right)) => div_rem(left, right),
            (Number::Integer(left), Number::Float(right)) => div_rem(left.0 as FloatValue, right),
            (Number::Float(left), Number::Integer(right)) => div_rem(left, right.0 as FloatValue),
        }
    }
}

mod bitwise {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("&", self::and);
        context.register_func("&=", self::and_assign);
        context.register_func("|", self::or);
        context.register_func("|=", self::or_assign);
        context.register_func("^", self::xor);
        context.register_func("^=", self::xor_assign);
        context.register_func("~", self::not);

        context.register_func("mask_eq", self::mask_assign);
    }

    fn bitwise_op<S>(
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

    fn and<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        bitwise_op(context, args, |current, value| current & value)
    }

    fn or<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        bitwise_op(context, args, |current, value| current | value)
    }

    fn xor<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.integer(context, 0)? ^ args.integer(context, 1)?;
        Ok(result.into())
    }

    fn and_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::and(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn or_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::or(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn xor_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::xor(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn not<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let result = !args.integer(context, 0)?;
        Ok(result.into())
    }

    fn mask_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.integer(context, 0)? & !args.integer(context, 1)?;
        args.set_variable(context, 0, result)?;
        Ok(result.into())
    }
}

mod logical {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("&&", self::and);
        context.register_func("||", self::or);
        context.register_func("^^", self::xor);
        context.register_func("!", self::not);
    }

    fn and<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        for node in args {
            if !node.boolean(context)? {
                return Ok(Node::FALSE);
            }
        }

        Ok(Node::TRUE)
    }

    fn or<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        for node in args {
            if node.boolean(context)? {
                return Ok(Node::TRUE);
            }
        }

        Ok(Node::FALSE)
    }

    fn xor<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.boolean(context, 0)? ^ args.boolean(context, 1)?;
        Ok(result.into())
    }

    fn not<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let result = !args.boolean(context, 0)?;
        Ok(result.into())
    }
}

mod compare {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("==", self::equal);
        context.register_func("!=", self::not_equal);
        context.register_func(">", self::greater_than);
        context.register_func(">=", self::greater_equal);
        context.register_func("<", self::less_than);
        context.register_func("<=", self::less_equal);
    }

    fn equal<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? == args.evaluate(context, 1)?;
        Ok(result.into())
    }

    fn not_equal<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? != args.evaluate(context, 1)?;
        Ok(result.into())
    }

    fn greater_than<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? > args.evaluate(context, 1)?;
        Ok(result.into())
    }

    fn greater_equal<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? >= args.evaluate(context, 1)?;
        Ok(result.into())
    }

    fn less_than<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? < args.evaluate(context, 1)?;
        Ok(result.into())
    }

    fn less_equal<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? <= args.evaluate(context, 1)?;
        Ok(result.into())
    }
}
