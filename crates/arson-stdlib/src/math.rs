// SPDX-License-Identifier: LGPL-3.0-or-later

use std::ops::{Div, Rem};

use arson_core::prelude::*;
use arson_core::{FloatValue, Integer, IntegerValue, Number};

pub fn register_funcs<S>(context: &mut Context<S>) {
    context.add_macro("PI", arson_array![std::f64::consts::PI]);
    context.add_macro("TAU", arson_array![std::f64::consts::TAU]);
    context.add_macro("EULER", arson_array![std::f64::consts::E]);

    basic::register_funcs(context);
    limit::register_funcs(context);
    exponential::register_funcs(context);
    trigonometry::register_funcs(context);
}

pub mod basic {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("abs", self::abs);
        context.register_func_alias("mod", "%");
        context.register_func("div_rem", self::divide_remainder);

        context.register_func("mask_eq", self::mask_assign);
        context.register_func("highest_bit", self::highest_bit);
        context.register_func("lowest_bit", self::lowest_bit);
        context.register_func("count_bits", self::count_bits);
    }

    pub fn abs<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        match args.number(context, 0)? {
            Number::Integer(value) => Ok(value.0.saturating_abs().into()),
            Number::Float(value) => Ok(value.abs().into()),
        }
    }

    pub fn divide_remainder<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);

        pub fn div_rem<T: Copy + Div + Rem>(left: T, right: T) -> ExecuteResult
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

    pub fn mask_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let result = args.integer(context, 0)? & !args.integer(context, 1)?;
        args.set_variable(context, 0, result)?;
        Ok(result.into())
    }

    fn first_active_bit<I: Iterator<Item = u32>>(value: IntegerValue, mut bit_range: I) -> IntegerValue {
        match bit_range.find(|i| value & (1 << i) != 0) {
            Some(i) => 1 << i,
            None => 0,
        }
    }

    pub fn highest_bit<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let value = args.integer(context, 0)?;
        let result = first_active_bit(value.0, (0..IntegerValue::BITS).rev());
        Ok(result.into())
    }

    pub fn lowest_bit<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let value = args.integer(context, 0)?;
        let result = first_active_bit(value.0, 0..IntegerValue::BITS);
        Ok(result.into())
    }

    pub fn count_bits<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let result = args.integer(context, 0)?.0.count_ones() as IntegerValue;
        Ok(result.into())
    }
}

pub mod limit {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("min", self::min);
        context.register_func("max", self::max);
        context.register_func("clamp", self::clamp);
        context.register_func("min_eq", self::min_assign);
        context.register_func("max_eq", self::max_assign);
        context.register_func("clamp_eq", self::clamp_assign);

        context.register_func("ceil", self::ceiling);
        context.register_func("floor", self::floor);
        context.register_func("trunc", self::truncate);
        context.register_func("round", self::round);
    }

    pub fn min<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        args.number_chain(
            context,
            |left, right| Ok(left.min(right)),
            |left, right| Ok(left.min(right)),
        )
    }

    pub fn max<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        args.number_chain(
            context,
            |left, right| Ok(left.max(right)),
            |left, right| Ok(left.max(right)),
        )
    }

    pub fn clamp<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 3);

        let min = args.number(context, 0)?;
        let max = args.number(context, 1)?;
        let value = args.number(context, 2)?;

        _clamp(min, max, value)
    }

    fn _clamp(min: Number, max: Number, value: Number) -> ExecuteResult {
        fn _integer_clamp(min: Integer, max: Integer, value: Integer) -> ExecuteResult {
            arson_assert!(min <= max, "Invalid clamp range: min ({min}) is greater than max ({max})");
            Ok(value.clamp(min, max).into())
        }

        fn _float_clamp(min: FloatValue, max: FloatValue, value: FloatValue) -> ExecuteResult {
            arson_assert!(min <= max, "Invalid clamp range: min ({min}) is greater than max ({max})");
            arson_assert!(!min.is_nan(), "Min cannot be NaN");
            arson_assert!(!max.is_nan(), "Max cannot be NaN");
            Ok(value.clamp(min, max).into())
        }

        let Number::Integer(min) = min else {
            return _float_clamp(min.float(), max.float(), value.float());
        };
        let Number::Integer(max) = max else {
            return _float_clamp(min.0 as FloatValue, max.float(), value.float());
        };
        let Number::Integer(value) = value else {
            return _float_clamp(min.0 as FloatValue, max.0 as FloatValue, value.float());
        };

        _integer_clamp(min, max, value)
    }

    pub fn min_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::min(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn max_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let result = self::max(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn clamp_assign<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 3);

        let value = args.number(context, 0)?;
        let min = args.number(context, 1)?;
        let max = args.number(context, 2)?;

        let result = _clamp(min, max, value)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    pub fn ceiling<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.ceil().into())
    }

    pub fn floor<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.floor().into())
    }

    pub fn truncate<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.trunc().into())
    }

    pub fn round<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.round().into())
    }
}

pub mod exponential {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("pow", self::power);
        context.register_func("sqrt", self::square_root);
        context.register_func("cbrt", self::cube_root);
        context.register_func("hypot", self::hypotenuse);

        context.register_func("exp", self::power_of_e);
        context.register_func("exp2", self::power_of_2);
        context.register_func("expm1", self::power_of_e_minus_one);
        context.register_func("log", self::logarithm_natural);
        context.register_func("log10", self::logarithm_base_10);
        context.register_func("log2", self::logarithm_base_2);
    }

    pub fn power<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        match args.number(context, 0)? {
            Number::Integer(left) => Ok(left.0.pow(args.integer(context, 1)?.0 as u32).into()),
            Number::Float(left) => match args.number(context, 1)? {
                Number::Integer(right) => Ok(left.powi(right.0 as i32).into()),
                Number::Float(right) => Ok(left.powf(right).into()),
            },
        }
    }

    pub fn square_root<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.sqrt().into())
    }

    pub fn cube_root<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.cbrt().into())
    }

    pub fn hypotenuse<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let x = args.float(context, 0)?;
        let y = args.float(context, 1)?;
        Ok(x.hypot(y).into())
    }

    pub fn power_of_e<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.exp().into())
    }

    pub fn power_of_2<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.exp2().into())
    }

    pub fn power_of_e_minus_one<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.exp_m1().into())
    }

    pub fn logarithm_natural<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let x = args.float(context, 0)?;
        let y = args.float(context, 1)?;
        Ok(x.log(y).into())
    }

    pub fn logarithm_base_10<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.log10().into())
    }

    pub fn logarithm_base_2<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.log2().into())
    }
}

pub mod trigonometry {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("sin", self::sine);
        context.register_func("cos", self::cosine);
        context.register_func("tan", self::tangent);
        context.register_func("asin", self::arc_sine);
        context.register_func("acos", self::arc_cosine);
        context.register_func("atan", self::arc_tangent);
        context.register_func("atan2", self::arc_tangent_quadrant);

        context.register_func("sinh", self::sine_hyperbolic);
        context.register_func("cosh", self::cosine_hyperbolic);
        context.register_func("tanh", self::tangent_hyperbolic);
        context.register_func("asinh", self::arc_sine_hyperbolic);
        context.register_func("acosh", self::arc_cosine_hyperbolic);
        context.register_func("atanh", self::arc_tangent_hyperbolic);
    }

    pub fn sine<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.sin().into())
    }

    pub fn cosine<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.cos().into())
    }

    pub fn tangent<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.tan().into())
    }

    pub fn arc_sine<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.asin().into())
    }

    pub fn arc_cosine<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.acos().into())
    }

    pub fn arc_tangent<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.atan().into())
    }

    pub fn arc_tangent_quadrant<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let x = args.float(context, 0)?;
        let y = args.float(context, 1)?;
        Ok(x.atan2(y).into())
    }

    pub fn sine_hyperbolic<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.sinh().into())
    }

    pub fn cosine_hyperbolic<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.cosh().into())
    }

    pub fn tangent_hyperbolic<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.tanh().into())
    }

    pub fn arc_sine_hyperbolic<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.asinh().into())
    }

    pub fn arc_cosine_hyperbolic<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.acosh().into())
    }

    pub fn arc_tangent_hyperbolic<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.atanh().into())
    }
}
