// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::core::*;
use crate::{arson_assert, arson_assert_len, evaluate_node};

pub fn register_funcs(context: &mut Context) {
    unary::register_funcs(context);
    binary::register_funcs(context);
    bitwise::register_funcs(context);
    logical::register_funcs(context);
    comparison::register_funcs(context);
}

fn op_assign(context: &mut Context, args: &NodeSlice, result: NodeValue) -> HandleResult {
    evaluate_node! {
        args.unevaluated(0)?;
        RawNodeValue::Variable(value) => context.set_variable(value.symbol.clone(), result.clone()),
        RawNodeValue::Property(_value) => todo!("op_assign property access"),
    };
    Ok(result)
}

mod unary {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func_by_name("++", self::increment);
        context.register_func_by_name("--", self::decrement);
        // context.register_func_by_name("+", self::promote); // No need for a promotion operator
        // context.register_func_by_name("-", self::negate); // "-" registered by binary module
        context.register_func_by_name("~", self::not);

        context.register_func_by_name("abs", self::abs);
    }

    fn increment(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);

        // Forward to arithmetic operator
        let add_args = [args.get(0)?.clone(), Node::from(1)];
        let result = binary::add(context, NodeSlice::new(&add_args))?;
        op_assign(context, args, result)
    }

    fn decrement(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);

        // Forward to arithmetic operator
        let subtract_args = [args.get(0)?.clone(), Node::from(-1)];
        let result = binary::subtract(context, NodeSlice::new(&subtract_args))?;
        op_assign(context, args, result)
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

    fn abs(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        evaluate_node! {
            args.evaluate(context, 0)?;
            NodeValue::Integer(value) => Ok(NodeValue::from(value.saturating_abs())),
            NodeValue::Float(value) => Ok(NodeValue::from(value.abs())),
        }
    }
}

mod binary {
    use std::ops::{Div, Rem};

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
        context.register_func_by_name("div_rem", self::divide_remainder);
    }

    pub(super) fn add(context: &mut Context, args: &NodeSlice) -> HandleResult {
        fn add_integer(context: &mut Context, args: &NodeSlice, left: NodeInteger) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => add_integer(context, args.slice(1..)?, left.overflowing_add(right).0),
                NodeValue::Float(right) => add_float(context, args.slice(1..)?, left as NodeFloat + right),
                unhandled => Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
            }
        }

        fn add_float(context: &mut Context, args: &NodeSlice, left: NodeFloat) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => add_float(context, args.slice(1..)?, left + right as NodeFloat),
                NodeValue::Float(right) => add_float(context, args.slice(1..)?, left + right),
                unhandled => Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
            }
        }

        evaluate_node! {
            args.evaluate(context, 0)?;
            NodeValue::Integer(value) => add_integer(context, args.slice(1..)?, value),
            NodeValue::Float(value) => add_float(context, args.slice(1..)?, value),
        }
    }

    pub(super) fn subtract(context: &mut Context, args: &NodeSlice) -> HandleResult {
        if args.len() == 1 {
            return unary::negate(context, args);
        }

        fn subtract_integer(context: &mut Context, args: &NodeSlice, left: NodeInteger) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => subtract_integer(context, args.slice(1..)?, left.overflowing_sub(right).0),
                NodeValue::Float(right) => subtract_float(context, args.slice(1..)?, left as NodeFloat - right),
                unhandled => Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
            }
        }

        fn subtract_float(context: &mut Context, args: &NodeSlice, left: NodeFloat) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => subtract_float(context, args.slice(1..)?, left - right as NodeFloat),
                NodeValue::Float(right) => subtract_float(context, args.slice(1..)?, left - right),
                unhandled => Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
            }
        }

        evaluate_node! {
            args.evaluate(context, 0)?;
            NodeValue::Integer(value) => subtract_integer(context, args.slice(1..)?, value),
            NodeValue::Float(value) => subtract_float(context, args.slice(1..)?, value),
        }
    }

    fn multiply(context: &mut Context, args: &NodeSlice) -> HandleResult {
        fn multiply_integer(context: &mut Context, args: &NodeSlice, left: NodeInteger) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => multiply_integer(context, args.slice(1..)?, left.overflowing_mul(right).0),
                NodeValue::Float(right) => multiply_float(context, args.slice(1..)?, left as NodeFloat * right),
                unhandled => Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
            }
        }

        fn multiply_float(context: &mut Context, args: &NodeSlice, left: NodeFloat) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => multiply_float(context, args.slice(1..)?, left * right as NodeFloat),
                NodeValue::Float(right) => multiply_float(context, args.slice(1..)?, left * right),
                unhandled => Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
            }
        }

        evaluate_node! {
            args.evaluate(context, 0)?;
            NodeValue::Integer(value) => multiply_integer(context, args.slice(1..)?, value),
            NodeValue::Float(value) => multiply_float(context, args.slice(1..)?, value),
        }
    }

    fn divide(context: &mut Context, args: &NodeSlice) -> HandleResult {
        fn divide_integer(context: &mut Context, args: &NodeSlice, left: NodeInteger) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => {
                    arson_assert!(right != 0, "attempted to divide by zero");
                    divide_integer(context, args.slice(1..)?, left.overflowing_div(right).0)
                },
                NodeValue::Float(right) => divide_float(context, args.slice(1..)?, left as NodeFloat / right),
                unhandled => Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
            }
        }

        fn divide_float(context: &mut Context, args: &NodeSlice, left: NodeFloat) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => divide_float(context, args.slice(1..)?, left / right as NodeFloat),
                NodeValue::Float(right) => divide_float(context, args.slice(1..)?, left / right),
                unhandled => Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
            }
        }

        evaluate_node! {
            args.evaluate(context, 0)?;
            NodeValue::Integer(value) => divide_integer(context, args.slice(1..)?, value),
            NodeValue::Float(value) => divide_float(context, args.slice(1..)?, value),
        }
    }

    fn modulo(context: &mut Context, args: &NodeSlice) -> HandleResult {
        fn divide_integer(context: &mut Context, args: &NodeSlice, left: NodeInteger) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => {
                    arson_assert!(right != 0, "attempted to modulo by zero");
                    divide_integer(context, args.slice(1..)?, left.overflowing_rem(right).0)
                },
                NodeValue::Float(right) => divide_float(context, args.slice(1..)?, left as NodeFloat % right),
                unhandled => Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
            }
        }

        fn divide_float(context: &mut Context, args: &NodeSlice, left: NodeFloat) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => divide_float(context, args.slice(1..)?, left % right as NodeFloat),
                NodeValue::Float(right) => divide_float(context, args.slice(1..)?, left % right),
                unhandled => Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
            }
        }

        evaluate_node! {
            args.evaluate(context, 0)?;
            NodeValue::Integer(value) => divide_integer(context, args.slice(1..)?, value),
            NodeValue::Float(value) => divide_float(context, args.slice(1..)?, value),
        }
    }

    fn add_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::add(context, args)?;
        op_assign(context, args, result)
    }

    fn subtract_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::subtract(context, args)?;
        op_assign(context, args, result)
    }

    fn multiply_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::multiply(context, args)?;
        op_assign(context, args, result)
    }

    fn divide_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::divide(context, args)?;
        op_assign(context, args, result)
    }

    fn modulo_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::modulo(context, args)?;
        op_assign(context, args, result)
    }

    fn divide_remainder(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);

        fn div_rem<T: Copy + Div + Rem>(left: T, right: T) -> HandleResult
        where
            node::NodeValue: From<<T as Div>::Output> + From<<T as Rem>::Output>,
        {
            let quotient = left / right;
            let remainder = left % right;
            Ok(NodeValue::from(NodeArray::from_iter([
                NodeValue::from(quotient),
                NodeValue::from(remainder),
            ])))
        }

        let left = args.evaluate(context, 0)?;
        let right = args.evaluate(context, 1)?;

        match (left, right) {
            (NodeValue::Integer(left), NodeValue::Integer(right)) => div_rem(left, right),
            (NodeValue::Float(left), NodeValue::Float(right)) => div_rem(left, right),
            (NodeValue::Integer(left), NodeValue::Float(right)) => div_rem(left as f64, right),
            (NodeValue::Float(left), NodeValue::Integer(right)) => div_rem(left, right as f64),
            (left, right) => Err(Error::bad_operand(left.get_type(), right.get_type())),
        }
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
        op_assign(context, args, result)
    }

    fn or_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::or(context, args)?;
        op_assign(context, args, result)
    }

    fn xor_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::xor(context, args)?;
        op_assign(context, args, result)
    }

    fn mask_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        let result = args.integer(context, 0)? & !args.integer(context, 1)?;
        op_assign(context, args, NodeValue::from(result))
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
        let result = first_active_bit(value, (0..i32::BITS).rev());
        op_assign(context, args, NodeValue::from(result))
    }

    fn lowest_bit(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        let value = args.integer(context, 0)?;
        let result = first_active_bit(value, 0..i32::BITS);
        op_assign(context, args, NodeValue::from(result))
    }

    fn count_bits(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        let result = args.integer(context, 0)?.count_ones();
        op_assign(context, args, NodeValue::from(result as i64))
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
