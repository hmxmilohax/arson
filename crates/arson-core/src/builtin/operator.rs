// SPDX-License-Identifier: LGPL-3.0-or-later

use std::ops::{Div, Rem};

use crate::prelude::*;
use crate::{FloatValue, Integer, Number};

pub fn register_funcs(context: &mut Context) {
    arithmetic::register_funcs(context);
    bitwise::register_funcs(context);
    logical::register_funcs(context);
    compare::register_funcs(context);
    escape::register_funcs(context);
}

mod arithmetic {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
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

    fn increment(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);

        // Forward to addition operator
        let result = self::add(context, arson_slice![args.get(0)?, 1])?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn decrement(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);

        // Forward to subtraction operator
        let result = self::subtract(context, arson_slice![args.get(0)?, -1])?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn add(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        args.number_chain(context, |left, right| Ok(left + right), |left, right| Ok(left + right))
    }

    fn subtract(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        if args.len() == 1 {
            // unary negate
            match args.number(context, 0)? {
                Number::Integer(value) => return Ok((-value).into()),
                Number::Float(value) => return Ok((-value).into()),
            }
        }

        args.number_chain(context, |left, right| Ok(left - right), |left, right| Ok(left - right))
    }

    fn multiply(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        args.number_chain(context, |left, right| Ok(left * right), |left, right| Ok(left * right))
    }

    fn divide(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        args.number_chain(
            context,
            |left, right| {
                arson_assert!(right.0 != 0, "attempted to divide by zero");
                Ok(left / right)
            },
            |left, right| Ok(left / right),
        )
    }

    fn modulo(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        args.number_chain(
            context,
            |left, right| {
                arson_assert!(right.0 != 0, "attempted to modulo by zero");
                Ok(left % right)
            },
            |left, right| Ok(left % right),
        )
    }

    fn add_assign(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        let result = self::add(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn subtract_assign(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        let result = self::subtract(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn multiply_assign(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        let result = self::multiply(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn divide_assign(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        let result = self::divide(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn modulo_assign(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        let result = self::modulo(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn divide_remainder(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);

        fn div_rem<T: Copy + Div + Rem>(
            context: &mut Context,
            args: &NodeSlice,
            left: T,
            right: T,
        ) -> ExecuteResult
        where
            NodeValue: From<<T as Div>::Output> + From<<T as Rem>::Output>,
        {
            let quotient = left / right;
            let remainder = left % right;

            args.set_variable(context, 2, quotient)?;
            args.set_variable(context, 3, remainder)?;

            Ok(Node::HANDLED)
        }

        let left = args.number(context, 0)?;
        let right = args.number(context, 1)?;

        match (left, right) {
            (Number::Integer(left), Number::Integer(right)) => div_rem(context, args, left, right),
            (Number::Float(left), Number::Float(right)) => div_rem(context, args, left, right),
            (Number::Integer(left), Number::Float(right)) => {
                div_rem(context, args, left.0 as FloatValue, right)
            },
            (Number::Float(left), Number::Integer(right)) => {
                div_rem(context, args, left, right.0 as FloatValue)
            },
        }
    }
}

mod bitwise {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("<<", self::shift_left);
        context.register_func(">>", self::shift_right);
        context.register_func("rotate_left", self::rotate_left);
        context.register_func("rotate_right", self::rotate_right);

        context.register_func("&", self::and);
        context.register_func("&=", self::and_assign);
        context.register_func("|", self::or);
        context.register_func("|=", self::or_assign);
        context.register_func("^", self::xor);
        context.register_func("^=", self::xor_assign);
        context.register_func("~", self::not);
        context.register_func("mask_eq", self::mask_assign);
    }

    fn shift_left(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let value = args.integer(context, 0)?;
        let amount = args.integer(context, 1)?;
        Ok((value << amount.0 as usize).into())
    }

    fn shift_right(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let value = args.integer(context, 0)?;
        let amount = args.integer(context, 1)?;
        Ok(value.0.wrapping_shr(amount.0 as u32).into())
    }

    fn rotate_left(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let value = args.integer(context, 0)?;
        let amount = args.integer(context, 1)?;
        Ok(value.0.rotate_left(amount.0 as u32).into())
    }

    fn rotate_right(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let value = args.integer(context, 0)?;
        let amount = args.integer(context, 1)?;
        Ok(value.0.rotate_right(amount.0 as u32).into())
    }

    fn bitwise_op(
        context: &mut Context,
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

    fn and(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        bitwise_op(context, args, |current, value| current & value)
    }

    fn or(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        bitwise_op(context, args, |current, value| current | value)
    }

    fn xor(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.integer(context, 0)? ^ args.integer(context, 1)?;
        Ok(result.into())
    }

    fn and_assign(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        let result = self::and(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn or_assign(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        let result = self::or(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn xor_assign(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        let result = self::xor(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn not(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let result = !args.integer(context, 0)?;
        Ok(result.into())
    }

    fn mask_assign(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.integer(context, 0)? & !args.integer(context, 1)?;
        args.set_variable(context, 0, result)?;
        Ok(result.into())
    }
}

mod logical {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("&&", self::and);
        context.register_func("||", self::or);
        context.register_func("^^", self::xor);
        context.register_func("!", self::not);
    }

    fn and(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        for node in args {
            if !node.boolean(context)? {
                return Ok(Node::FALSE);
            }
        }

        Ok(Node::TRUE)
    }

    fn or(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        for node in args {
            if node.boolean(context)? {
                return Ok(Node::TRUE);
            }
        }

        Ok(Node::FALSE)
    }

    fn xor(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.boolean(context, 0)? ^ args.boolean(context, 1)?;
        Ok(result.into())
    }

    fn not(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let result = !args.boolean(context, 0)?;
        Ok(result.into())
    }
}

mod compare {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("==", self::equal);
        context.register_func("!=", self::not_equal);
        context.register_func(">", self::greater_than);
        context.register_func(">=", self::greater_equal);
        context.register_func("<", self::less_than);
        context.register_func("<=", self::less_equal);
    }

    fn equal(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? == args.evaluate(context, 1)?;
        Ok(result.into())
    }

    fn not_equal(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? != args.evaluate(context, 1)?;
        Ok(result.into())
    }

    fn greater_than(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? > args.evaluate(context, 1)?;
        Ok(result.into())
    }

    fn greater_equal(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? >= args.evaluate(context, 1)?;
        Ok(result.into())
    }

    fn less_than(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? < args.evaluate(context, 1)?;
        Ok(result.into())
    }

    fn less_equal(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? <= args.evaluate(context, 1)?;
        Ok(result.into())
    }
}

pub mod escape {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("quote", self::quote);
        context.register_func("quasiquote", self::quasiquote);
        context.register_func("unquote", self::unquote);
        context.register_func("eval", self::eval);

        context.register_func("'", self::quote);
        context.register_func("`", self::quasiquote);
        context.register_func(",", self::unquote);
    }

    pub fn quote(_context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        args.get(0).cloned()
    }

    pub fn quasiquote(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        fn do_quasiquote(context: &mut Context, value: &Node) -> ExecuteResult {
            match value.unevaluated() {
                NodeValue::Array(array) => {
                    let borrow = array.borrow()?;
                    let quoted = quote_array(context, &borrow)?;
                    Ok(quoted.into())
                },
                NodeValue::Command(command) => {
                    if let Some(NodeValue::Symbol(symbol)) = command.unevaluated_opt(0) {
                        if *symbol == context.builtin_state.unquote
                            || *symbol == context.builtin_state.unquote_abbrev
                        {
                            return unquote(context, command.slice(1..)?);
                        }
                    }

                    let quoted = quote_array(context, command)?;
                    Ok(NodeCommand::from(quoted).into())
                },
                value => Ok(value.into()),
            }
        }

        fn quote_array(context: &mut Context, array: &NodeArray) -> crate::Result<NodeArray> {
            let mut quoted = NodeArray::with_capacity(array.len());
            for value in array {
                let value = do_quasiquote(context, value)?;
                quoted.push(value);
            }
            Ok(quoted)
        }

        arson_assert_len!(args, 1);
        return do_quasiquote(context, args.get(0)?);
    }

    pub fn unquote(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.evaluate(context, 0)?.into())
    }

    pub fn eval(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let value: Node = args.evaluate(context, 0)?.into();
        Ok(value.evaluate(context)?.into())
    }
}
