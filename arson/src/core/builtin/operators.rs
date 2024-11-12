// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::core::*;
use crate::{arson_assert, arson_assert_len, evaluate_node};

use super::{number_chain, op_assign};

pub fn register_funcs(context: &mut Context) {
    unary::register_funcs(context);
    binary::register_funcs(context);
    bitwise::register_funcs(context);
    logical::register_funcs(context);
    comparison::register_funcs(context);
}

mod unary {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func_by_name("++", self::increment);
        context.register_func_by_name("--", self::decrement);
        // context.register_func_by_name("+", self::promote); // No need for a promotion operator
        // context.register_func_by_name("-", self::negate); // "-" registered by binary module
        context.register_func_by_name("~", self::not);
    }

    fn increment(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);

        // Forward to arithmetic operator
        let add_args = [args.get(0)?.clone(), Node::from(1)];
        let result = binary::add(context, NodeSlice::new(&add_args))?;
        super::op_assign(context, args, result)
    }

    fn decrement(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);

        // Forward to arithmetic operator
        let subtract_args = [args.get(0)?.clone(), Node::from(-1)];
        let result = binary::subtract(context, NodeSlice::new(&subtract_args))?;
        super::op_assign(context, args, result)
    }

    pub(super) fn negate(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        evaluate_node! {
            args.evaluate(context, 0)?;
            NodeValue::Integer(value) => Ok(NodeValue::from(value.overflowing_neg().0)),
            NodeValue::Float(value) => Ok(NodeValue::from(-value)),
        }
    }

    fn not(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        let result = !args.integer(context, 0)?;
        Ok(NodeValue::from(result))
    }
}

mod binary {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func_by_name("+", self::add);
        context.register_func_by_name("+=", self::add_assign);
        context.register_func_by_name("-", self::subtract);
        context.register_func_by_name("-=", self::subtract_assign);
        context.register_func_by_name("*", self::multiply);
        context.register_func_by_name("*=", self::multiply_assign);
        context.register_func_by_name("/", self::divide);
        context.register_func_by_name("/=", self::divide_assign);
        context.register_func_by_name("%", self::modulo);
        context.register_func_by_name("%=", self::modulo_assign);

        context.register_func_by_name("mod", self::modulo);
    }

    pub(super) fn add(context: &mut Context, args: &NodeSlice) -> HandleResult {
        number_chain(
            context,
            args,
            |left, right| Ok(left.overflowing_add(right).0),
            |left, right| Ok(left + right),
        )
    }

    pub(super) fn subtract(context: &mut Context, args: &NodeSlice) -> HandleResult {
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

    fn multiply(context: &mut Context, args: &NodeSlice) -> HandleResult {
        number_chain(
            context,
            args,
            |left, right| Ok(left.overflowing_mul(right).0),
            |left, right| Ok(left * right),
        )
    }

    fn divide(context: &mut Context, args: &NodeSlice) -> HandleResult {
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

    fn modulo(context: &mut Context, args: &NodeSlice) -> HandleResult {
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

    fn add_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::add(context, args)?;
        super::op_assign(context, args, result)
    }

    fn subtract_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::subtract(context, args)?;
        super::op_assign(context, args, result)
    }

    fn multiply_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::multiply(context, args)?;
        super::op_assign(context, args, result)
    }

    fn divide_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::divide(context, args)?;
        super::op_assign(context, args, result)
    }

    fn modulo_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::modulo(context, args)?;
        super::op_assign(context, args, result)
    }
}

mod bitwise {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func_by_name("&", self::and);
        context.register_func_by_name("&=", self::and_assign);
        context.register_func_by_name("|", self::or);
        context.register_func_by_name("|=", self::or_assign);
        context.register_func_by_name("^", self::xor);
        context.register_func_by_name("^=", self::xor_assign);

        context.register_func_by_name("mask_eq", self::mask_assign);
        context.register_func_by_name("highest_bit", self::highest_bit);
        context.register_func_by_name("lowest_bit", self::lowest_bit);
        context.register_func_by_name("count_bits", self::count_bits);
    }

    fn bitwise_op(
        context: &mut Context,
        args: &NodeSlice,
        f: fn(NodeInteger, NodeInteger) -> NodeInteger,
    ) -> HandleResult {
        let result = args.integer(context, 0)?;
        let result = args
            .slice(1..)?
            .iter()
            .try_fold(result, |current, n| n.integer(context).map(|value| f(current, value)))?;

        Ok(NodeValue::from(result))
    }

    fn and(context: &mut Context, args: &NodeSlice) -> HandleResult {
        bitwise_op(context, args, |current, value| current & value)
    }

    fn or(context: &mut Context, args: &NodeSlice) -> HandleResult {
        bitwise_op(context, args, |current, value| current | value)
    }

    fn xor(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        let result = args.integer(context, 0)? ^ args.integer(context, 1)?;
        Ok(NodeValue::from(result))
    }

    fn and_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::and(context, args)?;
        super::op_assign(context, args, result)
    }

    fn or_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::or(context, args)?;
        super::op_assign(context, args, result)
    }

    fn xor_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::xor(context, args)?;
        super::op_assign(context, args, result)
    }

    fn mask_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        let result = args.integer(context, 0)? & !args.integer(context, 1)?;
        super::op_assign(context, args, NodeValue::from(result))
    }

    fn first_active_bit<I: Iterator<Item = u32>>(value: NodeInteger, mut bit_range: I) -> NodeInteger {
        match bit_range.find(|i| value & (1 << i) != 0) {
            Some(i) => 1 << i,
            None => 0,
        }
    }

    fn highest_bit(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        let value = args.integer(context, 0)?;
        let result = first_active_bit(value, (0..NodeInteger::BITS).rev());
        super::op_assign(context, args, NodeValue::from(result))
    }

    fn lowest_bit(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        let value = args.integer(context, 0)?;
        let result = first_active_bit(value, 0..NodeInteger::BITS);
        super::op_assign(context, args, NodeValue::from(result))
    }

    fn count_bits(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        let result = args.integer(context, 0)?.count_ones();
        super::op_assign(context, args, NodeValue::from(result as NodeInteger))
    }
}

mod logical {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func_by_name("&&", self::and);
        context.register_func_by_name("||", self::or);
        context.register_func_by_name("^^", self::xor);
        context.register_func_by_name("!", self::not);
    }

    fn and(context: &mut Context, args: &NodeSlice) -> HandleResult {
        for node in args {
            if !node.boolean(context)? {
                return Ok(NodeValue::FALSE);
            }
        }

        Ok(NodeValue::TRUE)
    }

    fn or(context: &mut Context, args: &NodeSlice) -> HandleResult {
        for node in args {
            if node.boolean(context)? {
                return Ok(NodeValue::TRUE);
            }
        }

        Ok(NodeValue::FALSE)
    }

    fn xor(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        let result = args.boolean(context, 0)? ^ args.boolean(context, 1)?;
        Ok(NodeValue::from(result))
    }

    fn not(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        let result = !args.boolean(context, 0)?;
        Ok(NodeValue::from(result))
    }
}

mod comparison {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func_by_name("==", self::equal);
        context.register_func_by_name("!=", self::not_equal);
        context.register_func_by_name(">", self::greater_than);
        context.register_func_by_name(">=", self::greater_equal);
        context.register_func_by_name("<", self::less_than);
        context.register_func_by_name("<=", self::less_equal);
    }

    fn equal(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        Ok(NodeValue::from(
            args.evaluate(context, 0)? == args.evaluate(context, 1)?,
        ))
    }

    fn not_equal(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        Ok(NodeValue::from(
            args.evaluate(context, 0)? != args.evaluate(context, 1)?,
        ))
    }

    fn greater_than(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        Ok(NodeValue::from(args.evaluate(context, 0)? > args.evaluate(context, 1)?))
    }

    fn greater_equal(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        Ok(NodeValue::from(
            args.evaluate(context, 0)? >= args.evaluate(context, 1)?,
        ))
    }

    fn less_than(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        Ok(NodeValue::from(args.evaluate(context, 0)? < args.evaluate(context, 1)?))
    }

    fn less_equal(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        Ok(NodeValue::from(
            args.evaluate(context, 0)? <= args.evaluate(context, 1)?,
        ))
    }
}
