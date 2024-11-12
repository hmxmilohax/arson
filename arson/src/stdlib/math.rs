// SPDX-License-Identifier: LGPL-3.0-or-later

use std::ops::{Div, Rem};

use crate::core::*;
use crate::{arson_assert_len, evaluate_node};

use crate::builtin::{self, number_chain, op_assign};

pub fn register_funcs(context: &mut Context) {
    basic::register_funcs(context);
    limit::register_funcs(context);
    exponential::register_funcs(context);
    trigonometry::register_funcs(context);
}

pub mod basic {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func_by_name("abs", self::abs);
        context.register_func_by_name("mod", builtin::operators::binary::modulo);
        context.register_func_by_name("div_rem", self::divide_remainder);

        context.register_func_by_name("mask_eq", self::mask_assign);
        context.register_func_by_name("highest_bit", self::highest_bit);
        context.register_func_by_name("lowest_bit", self::lowest_bit);
        context.register_func_by_name("count_bits", self::count_bits);
    }

    pub fn abs(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        evaluate_node! {
            args.evaluate(context, 0)?;
            NodeValue::Integer(value) => Ok(value.saturating_abs().into()),
            NodeValue::Float(value) => Ok(value.abs().into()),
        }
    }

    pub fn divide_remainder(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);

        pub fn div_rem<T: Copy + Div + Rem>(left: T, right: T) -> HandleResult
        where
            NodeValue: From<<T as Div>::Output> + From<<T as Rem>::Output>,
        {
            let quotient = left / right;
            let remainder = left % right;
            Ok(NodeArray::from_iter([NodeValue::from(quotient), NodeValue::from(remainder)]).into())
        }

        let left = args.evaluate(context, 0)?;
        let right = args.evaluate(context, 1)?;

        match (left, right) {
            (NodeValue::Integer(left), NodeValue::Integer(right)) => div_rem(left, right),
            (NodeValue::Float(left), NodeValue::Float(right)) => div_rem(left, right),
            (NodeValue::Integer(left), NodeValue::Float(right)) => div_rem(left as NodeFloat, right),
            (NodeValue::Float(left), NodeValue::Integer(right)) => div_rem(left, right as NodeFloat),
            (left, right) => Err(Error::bad_operand(left.get_type(), right.get_type())),
        }
    }

    pub fn mask_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
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

    pub fn highest_bit(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        let value = args.integer(context, 0)?;
        let result = first_active_bit(value, (0..NodeInteger::BITS).rev());
        super::op_assign(context, args, NodeValue::from(result))
    }

    pub fn lowest_bit(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        let value = args.integer(context, 0)?;
        let result = first_active_bit(value, 0..NodeInteger::BITS);
        super::op_assign(context, args, NodeValue::from(result))
    }

    pub fn count_bits(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        let result = args.integer(context, 0)?.count_ones();
        super::op_assign(context, args, NodeValue::from(result as NodeInteger))
    }
}

pub mod limit {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func_by_name("min", self::min);
        context.register_func_by_name("max", self::max);
        context.register_func_by_name("clamp", self::clamp);
        context.register_func_by_name("min_eq", self::min_assign);
        context.register_func_by_name("max_eq", self::max_assign);
        context.register_func_by_name("clamp_eq", self::clamp_assign);

        context.register_func_by_name("ceil", self::ceiling);
        context.register_func_by_name("floor", self::floor);
        context.register_func_by_name("trunc", self::truncate);
        context.register_func_by_name("round", self::round);
    }

    pub fn min(context: &mut Context, args: &NodeSlice) -> HandleResult {
        number_chain(
            context,
            args,
            |left, right| Ok(left.min(right)),
            |left, right| Ok(left.min(right)),
        )
    }

    pub fn max(context: &mut Context, args: &NodeSlice) -> HandleResult {
        number_chain(
            context,
            args,
            |left, right| Ok(left.max(right)),
            |left, right| Ok(left.max(right)),
        )
    }

    pub fn clamp(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 3);

        let min = evaluate_node! {
            args.evaluate(context, 0)?;
            NodeValue::Integer(min) => min,
            NodeValue::Float(min) => {
                let max = args.float(context, 1)?;
                let value = args.float(context, 2)?;
                return Ok(value.clamp(min, max).into());
            },
        };
        let max = evaluate_node! {
            args.evaluate(context, 1)?;
            NodeValue::Integer(max) => max,
            NodeValue::Float(max) => {
                let value = args.float(context, 2)?;
                return Ok(value.clamp(min as NodeFloat, max).into());
            },
        };
        let value = evaluate_node! {
            args.evaluate(context, 1)?;
            NodeValue::Integer(value) => value,
            NodeValue::Float(value) => {
                return Ok(value.clamp(min as NodeFloat, max as NodeFloat).into());
            },
        };

        Ok(value.clamp(min, max).into())
    }

    pub fn min_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::min(context, args)?;
        op_assign(context, args, result)
    }

    pub fn max_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::max(context, args)?;
        op_assign(context, args, result)
    }

    pub fn clamp_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        let result = self::clamp(context, args)?;
        op_assign(context, args, result)
    }

    pub fn ceiling(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.ceil().into())
    }

    pub fn floor(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.floor().into())
    }

    pub fn truncate(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.trunc().into())
    }

    pub fn round(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.round().into())
    }
}

pub mod exponential {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func_by_name("pow", self::power);
        context.register_func_by_name("sqrt", self::square_root);
        context.register_func_by_name("cbrt", self::cube_root);
        context.register_func_by_name("hypot", self::hypotenuse);

        context.register_func_by_name("exp", self::power_of_e);
        context.register_func_by_name("exp2", self::power_of_2);
        context.register_func_by_name("expm1", self::power_of_e_minus_one);
        context.register_func_by_name("log", self::logarithm_natural);
        context.register_func_by_name("log10", self::logarithm_base_10);
        context.register_func_by_name("log2", self::logarithm_base_2);
    }

    pub fn power(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        evaluate_node! {
            args.evaluate(context, 0)?;
            NodeValue::Integer(left) => Ok(left.pow(args.integer(context, 1)? as u32).into()),
            NodeValue::Float(left) => match args.evaluate(context, 1)? {
                NodeValue::Integer(right) => Ok(left.powi(right as i32).into()),
                NodeValue::Float(right) => Ok(left.powf(right).into()),
                unhandled => Err(Error::bad_operand(NodeType::Float, unhandled.get_type())),
            }
        }
    }

    pub fn square_root(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.sqrt().into())
    }

    pub fn cube_root(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.cbrt().into())
    }

    pub fn hypotenuse(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        let x = args.float(context, 0)?;
        let y = args.float(context, 1)?;
        Ok(x.hypot(y).into())
    }

    pub fn power_of_e(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.exp().into())
    }

    pub fn power_of_2(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.exp2().into())
    }

    pub fn power_of_e_minus_one(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.exp_m1().into())
    }

    pub fn logarithm_natural(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        let x = args.float(context, 0)?;
        let y = args.float(context, 1)?;
        Ok(x.log(y).into())
    }

    pub fn logarithm_base_10(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.log10().into())
    }

    pub fn logarithm_base_2(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.log2().into())
    }
}

pub mod trigonometry {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func_by_name("sin", self::sine);
        context.register_func_by_name("cos", self::cosine);
        context.register_func_by_name("tan", self::tangent);
        context.register_func_by_name("asin", self::arc_sine);
        context.register_func_by_name("acos", self::arc_cosine);
        context.register_func_by_name("atan", self::arc_tangent);
        context.register_func_by_name("atan2", self::arc_tangent_quadrant);

        context.register_func_by_name("sinh", self::sine_hyperbolic);
        context.register_func_by_name("cosh", self::cosine_hyperbolic);
        context.register_func_by_name("tanh", self::tangent_hyperbolic);
        context.register_func_by_name("asinh", self::arc_sine_hyperbolic);
        context.register_func_by_name("acosh", self::arc_cosine_hyperbolic);
        context.register_func_by_name("atanh", self::arc_tangent_hyperbolic);
    }

    pub fn sine(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.sin().into())
    }

    pub fn cosine(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.cos().into())
    }

    pub fn tangent(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.tan().into())
    }

    pub fn arc_sine(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.asin().into())
    }

    pub fn arc_cosine(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.acos().into())
    }

    pub fn arc_tangent(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.atan().into())
    }

    pub fn arc_tangent_quadrant(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 2);
        let x = args.float(context, 0)?;
        let y = args.float(context, 1)?;
        Ok(x.atan2(y).into())
    }

    pub fn sine_hyperbolic(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.sinh().into())
    }

    pub fn cosine_hyperbolic(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.cosh().into())
    }

    pub fn tangent_hyperbolic(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.tanh().into())
    }

    pub fn arc_sine_hyperbolic(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.asinh().into())
    }

    pub fn arc_cosine_hyperbolic(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.acosh().into())
    }

    pub fn arc_tangent_hyperbolic(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.atanh().into())
    }
}
