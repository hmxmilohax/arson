// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::{Context, ExecuteResult, FloatValue, Integer, NodeSlice, Number};

// Number utilities
impl NodeSlice {
    pub fn number_chain(
        &self,
        context: &mut Context,
        f_int: impl Fn(Integer, Integer) -> crate::Result<Integer>,
        f_float: impl Fn(FloatValue, FloatValue) -> crate::Result<FloatValue>,
    ) -> ExecuteResult {
        fn integer_chain(
            context: &mut Context,
            args: &NodeSlice,
            left: Integer,
            f_int: impl Fn(Integer, Integer) -> crate::Result<Integer>,
            f_float: impl Fn(FloatValue, FloatValue) -> crate::Result<FloatValue>,
        ) -> ExecuteResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(left.into());
            };

            match node.number(context)? {
                Number::Integer(right) => {
                    integer_chain(context, args.slice(1..)?, f_int(left, right)?, f_int, f_float)
                },
                Number::Float(right) => {
                    float_chain(context, args.slice(1..)?, f_float(left.0 as FloatValue, right)?, f_float)
                },
            }
        }

        fn float_chain(
            context: &mut Context,
            args: &NodeSlice,
            left: FloatValue,
            f_float: impl Fn(FloatValue, FloatValue) -> crate::Result<FloatValue>,
        ) -> ExecuteResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(left.into());
            };

            match node.number(context)? {
                Number::Integer(right) => {
                    float_chain(context, args.slice(1..)?, f_float(left, right.0 as FloatValue)?, f_float)
                },
                Number::Float(right) => {
                    float_chain(context, args.slice(1..)?, f_float(left, right)?, f_float)
                },
            }
        }

        match self.number(context, 0)? {
            Number::Integer(value) => integer_chain(context, self.slice(1..)?, value, f_int, f_float),
            Number::Float(value) => float_chain(context, self.slice(1..)?, value, f_float),
        }
    }
}
