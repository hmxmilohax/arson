// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::prelude::*;
use crate::{NodeFloat, NodeInteger, NodeIntegerValue, NodeNumber};

pub fn register_funcs(context: &mut Context) {
    context.add_required_macro("TRUE", arson_array![true]);
    context.add_required_macro("FALSE", arson_array![false]);

    bits::register_funcs(context);
    sign::register_funcs(context);
    limit::register_funcs(context);
    round::register_funcs(context);
    convert::register_funcs(context);
}

mod bits {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("highest_bit", self::highest_bit);
        context.register_func("lowest_bit", self::lowest_bit);
        context.register_func("count_bits", self::count_bits);
    }

    fn first_active_bit<I: Iterator<Item = u32>>(
        value: NodeIntegerValue,
        mut bit_range: I,
    ) -> NodeIntegerValue {
        match bit_range.find(|i| value & (1 << i) != 0) {
            Some(i) => 1 << i,
            None => 0,
        }
    }

    fn highest_bit(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let value = args.integer(context, 0)?;
        let result = first_active_bit(value.0, (0..NodeIntegerValue::BITS).rev());
        Ok(result.into())
    }

    fn lowest_bit(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let value = args.integer(context, 0)?;
        let result = first_active_bit(value.0, 0..NodeIntegerValue::BITS);
        Ok(result.into())
    }

    fn count_bits(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let result = args.integer(context, 0)?.0.count_ones() as NodeIntegerValue;
        Ok(result.into())
    }
}

mod sign {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("abs", self::abs);
        context.register_func("sign", self::sign);
    }

    fn abs(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        match args.number(context, 0)? {
            NodeNumber::Integer(value) => Ok(value.0.saturating_abs().into()),
            NodeNumber::Float(value) => Ok(value.abs().into()),
        }
    }

    fn sign(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        match args.number(context, 0)? {
            NodeNumber::Integer(value) => Ok(value.0.signum().into()),
            NodeNumber::Float(value) => Ok(value.signum().into()),
        }
    }
}

mod limit {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("min", self::min);
        context.register_func("max", self::max);
        context.register_func("clamp", self::clamp);

        context.register_func("min_eq", self::min_assign);
        context.register_func("max_eq", self::max_assign);
        context.register_func("clamp_eq", self::clamp_assign);
    }

    fn min(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        args.number_chain(
            context,
            |left, right| Ok(left.min(right)),
            |left, right| Ok(left.min(right)),
        )
    }

    fn max(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        args.number_chain(
            context,
            |left, right| Ok(left.max(right)),
            |left, right| Ok(left.max(right)),
        )
    }

    fn clamp(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        fn integer_clamp(min: NodeInteger, max: NodeInteger, value: NodeInteger) -> ExecuteResult {
            arson_assert!(min <= max, "Invalid clamp range: min ({min}) is greater than max ({max})");
            Ok(value.clamp(min, max).into())
        }

        fn float_clamp(min: NodeFloat, max: NodeFloat, value: NodeFloat) -> ExecuteResult {
            use std::cmp::Ordering;

            // Manual handling of `min <= max` to make the `None` case explicitly defined,
            // per Clippy recommendation. The behavior would be identical otherwise,
            // but this allows us to set a specific error message for this case.
            match min.partial_cmp(&max) {
                Some(Ordering::Less) => (/* continue on */),
                Some(Ordering::Equal) => (/* continue on */),
                Some(Ordering::Greater) => {
                    arson_fail!("Invalid clamp range: min ({min}) is greater than max ({max})")
                },
                None => arson_fail!("Invalid clamp range: min ({min}) and max ({max}) are not comparable"),
            }

            arson_assert!(!min.is_nan(), "Min cannot be NaN");
            arson_assert!(!max.is_nan(), "Max cannot be NaN");

            Ok(value.clamp(min, max).into())
        }

        arson_assert_len!(args, 3);

        let value = args.number(context, 0)?;
        let min = args.number(context, 1)?;
        let max = args.number(context, 2)?;

        let NodeNumber::Integer(min) = min else {
            return float_clamp(min.float(), max.float(), value.float());
        };
        let NodeNumber::Integer(max) = max else {
            return float_clamp(min.0 as NodeFloat, max.float(), value.float());
        };
        let NodeNumber::Integer(value) = value else {
            return float_clamp(min.0 as NodeFloat, max.0 as NodeFloat, value.float());
        };

        integer_clamp(min, max, value)
    }

    fn min_assign(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        let result = self::min(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn max_assign(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        let result = self::max(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }

    fn clamp_assign(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 3);

        let result = clamp(context, args)?;
        args.set_variable(context, 0, result.clone())?;
        Ok(result)
    }
}

mod round {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("ceil", self::ceiling);
        context.register_func("floor", self::floor);
        context.register_func("trunc", self::truncate);
        context.register_func("round", self::round);
    }

    fn ceiling(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.ceil().into())
    }

    fn floor(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.floor().into())
    }

    fn truncate(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.trunc().into())
    }

    fn round(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        Ok(args.float(context, 0)?.round().into())
    }
}

mod convert {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("int", self::int);
        context.register_func("float", self::float);
    }

    fn int(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        match args.evaluate(context, 0)? {
            NodeValue::Integer(value) => Ok(value.into()),
            NodeValue::Float(value) => Ok((value as NodeIntegerValue).into()),
            NodeValue::String(value) => Ok(value.parse::<NodeIntegerValue>()?.into()),
            NodeValue::Symbol(value) => Ok(value.name().parse::<NodeIntegerValue>()?.into()),
            // NodeValue::Object(value) => Ok(value.as_ptr() as usize as IntegerValue),
            value => arson_fail!("value of type {:?} is not convertible to an integer", value.get_kind()),
        }
    }

    fn float(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        match args.evaluate(context, 0)? {
            NodeValue::Integer(value) => Ok((value.0 as NodeFloat).into()),
            NodeValue::Float(value) => Ok(value.into()),
            NodeValue::String(value) => Ok(value.parse::<NodeFloat>()?.into()),
            NodeValue::Symbol(value) => Ok(value.name().parse::<NodeFloat>()?.into()),
            value => arson_fail!("value of type {:?} is not convertible to a float", value.get_kind()),
        }
    }
}
