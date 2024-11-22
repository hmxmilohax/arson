// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::*;

use super::number_chain;

pub fn register_funcs(context: &mut Context) {
    unary::register_funcs(context);
    binary::register_funcs(context);
    bitwise::register_funcs(context);
    logical::register_funcs(context);
    comparison::register_funcs(context);
}

pub mod unary {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("++", self::increment);
        context.register_func("--", self::decrement);
        // context.register_func_by_name("+", self::promote); // No need for a promotion operator
        // context.register_func_by_name("-", self::negate); // "-" registered by `binary`
        context.register_func("~", self::not);
    }

    pub fn increment(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);

        // Forward to arithmetic operator
        let add_args = [args.get(0)?.clone(), Node::from(1)];
        let result = binary::add(context, NodeSlice::new(&add_args))?;
        args.set(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn decrement(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);

        // Forward to arithmetic operator
        let subtract_args = [args.get(0)?.clone(), Node::from(-1)];
        let result = binary::subtract(context, NodeSlice::new(&subtract_args))?;
        args.set(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn negate(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        evaluate_node! {
            args.evaluate(context, 0)?;
            NodeValue::Integer(value) => Ok(value.overflowing_neg().0.into()),
            NodeValue::Float(value) => Ok((-value).into()),
        }
    }

    pub fn not(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        let result = !args.integer(context, 0)?;
        Ok(result.into())
    }
}

pub mod binary {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
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

    pub fn add(context: &mut Context, args: &NodeSlice) -> HandleResult {
        number_chain(
            context,
            args,
            |left, right| Ok(left.overflowing_add(right).0),
            |left, right| Ok(left + right),
        )
    }

    pub fn subtract(context: &mut Context, args: &NodeSlice) -> HandleResult {
        if args.len() == 1 {
            return unary::negate(context, args);
        }

        number_chain(
            context,
            args,
            |left, right| Ok(left.overflowing_sub(right).0),
            |left, right| Ok(left - right),
        )
    }

    pub fn multiply(context: &mut Context, args: &NodeSlice) -> HandleResult {
        number_chain(
            context,
            args,
            |left, right| Ok(left.overflowing_mul(right).0),
            |left, right| Ok(left * right),
        )
    }

    pub fn divide(context: &mut Context, args: &NodeSlice) -> HandleResult {
        number_chain(
            context,
            args,
            |left, right| {
                arson_assert!(right != 0, "attempted to divide by zero");
                Ok(left.overflowing_div(right).0)
            },
            |left, right| Ok(left / right),
        )
    }

    pub fn modulo(context: &mut Context, args: &NodeSlice) -> HandleResult {
        number_chain(
            context,
            args,
            |left, right| {
                arson_assert!(right != 0, "attempted to modulo by zero");
                Ok(left.overflowing_rem(right).0)
            },
            |left, right| Ok(left % right),
        )
    }

    pub fn add_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::add(context, args)?;
        args.set(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn subtract_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::subtract(context, args)?;
        args.set(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn multiply_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::multiply(context, args)?;
        args.set(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn divide_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::divide(context, args)?;
        args.set(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn modulo_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::modulo(context, args)?;
        args.set(context, 0, result.clone())?;
        Ok(result)
    }
}

pub mod bitwise {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("&", self::and);
        context.register_func("&=", self::and_assign);
        context.register_func("|", self::or);
        context.register_func("|=", self::or_assign);
        context.register_func("^", self::xor);
        context.register_func("^=", self::xor_assign);
    }

    pub fn bitwise_op(
        context: &mut Context,
        args: &NodeSlice,
        f: fn(NodeInteger, NodeInteger) -> NodeInteger,
    ) -> HandleResult {
        let result = args.integer(context, 0)?;
        let result = args
            .slice(1..)?
            .iter()
            .try_fold(result, |current, n| n.integer(context).map(|value| f(current, value)))?;

        Ok(result.into())
    }

    pub fn and(context: &mut Context, args: &NodeSlice) -> HandleResult {
        bitwise_op(context, args, |current, value| current & value)
    }

    pub fn or(context: &mut Context, args: &NodeSlice) -> HandleResult {
        bitwise_op(context, args, |current, value| current | value)
    }

    pub fn xor(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        let result = args.integer(context, 0)? ^ args.integer(context, 1)?;
        Ok(result.into())
    }

    pub fn and_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::and(context, args)?;
        args.set(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn or_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::or(context, args)?;
        args.set(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn xor_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::xor(context, args)?;
        args.set(context, 0, result.clone())?;
        Ok(result)
    }
}

pub mod logical {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("&&", self::and);
        context.register_func("||", self::or);
        context.register_func("^^", self::xor);
        context.register_func("!", self::not);
    }

    pub fn and(context: &mut Context, args: &NodeSlice) -> HandleResult {
        for node in args {
            if !node.boolean(context)? {
                return Ok(NodeValue::FALSE);
            }
        }

        Ok(NodeValue::TRUE)
    }

    pub fn or(context: &mut Context, args: &NodeSlice) -> HandleResult {
        for node in args {
            if node.boolean(context)? {
                return Ok(NodeValue::TRUE);
            }
        }

        Ok(NodeValue::FALSE)
    }

    pub fn xor(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        let result = args.boolean(context, 0)? ^ args.boolean(context, 1)?;
        Ok(result.into())
    }

    pub fn not(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        let result = !args.boolean(context, 0)?;
        Ok(result.into())
    }
}

pub mod comparison {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("==", self::equal);
        context.register_func("!=", self::not_equal);
        context.register_func(">", self::greater_than);
        context.register_func(">=", self::greater_equal);
        context.register_func("<", self::less_than);
        context.register_func("<=", self::less_equal);
    }

    pub fn equal(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? == args.evaluate(context, 1)?;
        Ok(result.into())
    }

    pub fn not_equal(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? != args.evaluate(context, 1)?;
        Ok(result.into())
    }

    pub fn greater_than(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? > args.evaluate(context, 1)?;
        Ok(result.into())
    }

    pub fn greater_equal(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? >= args.evaluate(context, 1)?;
        Ok(result.into())
    }

    pub fn less_than(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? < args.evaluate(context, 1)?;
        Ok(result.into())
    }

    pub fn less_equal(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        let result = args.evaluate(context, 0)? <= args.evaluate(context, 1)?;
        Ok(result.into())
    }
}
