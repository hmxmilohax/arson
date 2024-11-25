// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::*;

#[allow(
    clippy::module_inception,
    reason = "inner module is re-exported and not publicly accessible"
)]
mod context;
mod flow;
mod operators;

pub use context::*;

// TODO: need a better module to put this in
pub(crate) fn number_chain(
    context: &mut Context,
    args: &NodeSlice,
    f_int: impl Fn(Integer, Integer) -> crate::Result<Integer>,
    f_float: impl Fn(Float, Float) -> crate::Result<Float>,
) -> HandleResult {
    fn integer_chain(
        context: &mut Context,
        args: &NodeSlice,
        left: Integer,
        f_int: impl Fn(Integer, Integer) -> crate::Result<Integer>,
        f_float: impl Fn(Float, Float) -> crate::Result<Float>,
    ) -> HandleResult {
        let Some(node) = args.get_opt(0) else {
            return Ok(left.into());
        };

        match node.number(context)? {
            Number::Integer(right) => integer_chain(context, args.slice(1..)?, f_int(left, right)?, f_int, f_float),
            Number::Float(right) => float_chain(context, args.slice(1..)?, f_float(left as Float, right)?, f_float),
        }
    }

    fn float_chain<FF: Fn(Float, Float) -> crate::Result<Float>>(
        context: &mut Context,
        args: &NodeSlice,
        left: Float,
        f_float: FF,
    ) -> HandleResult {
        let Some(node) = args.get_opt(0) else {
            return Ok(left.into());
        };

        match node.number(context)? {
            Number::Integer(right) => float_chain(context, args.slice(1..)?, f_float(left, right as Float)?, f_float),
            Number::Float(right) => float_chain(context, args.slice(1..)?, f_float(left, right)?, f_float),
        }
    }

    match args.number(context, 0)? {
        Number::Integer(value) => integer_chain(context, args.slice(1..)?, value, f_int, f_float),
        Number::Float(value) => float_chain(context, args.slice(1..)?, value, f_float),
    }
}
