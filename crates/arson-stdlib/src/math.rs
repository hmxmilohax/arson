// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_core::prelude::*;
use arson_core::Number;

pub fn register_funcs<S>(context: &mut Context<S>) {
    context.add_macro("PI", arson_array![std::f64::consts::PI]);
    context.add_macro("TAU", arson_array![std::f64::consts::TAU]);
    context.add_macro("EULER", arson_array![std::f64::consts::E]);

    exponential::register_funcs(context);
    trigonometry::register_funcs(context);
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
