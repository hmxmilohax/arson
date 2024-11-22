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
    f_int: impl Fn(NodeInteger, NodeInteger) -> crate::Result<NodeInteger>,
    f_float: impl Fn(NodeFloat, NodeFloat) -> crate::Result<NodeFloat>,
) -> HandleResult {
    fn integer_chain(
        context: &mut Context,
        args: &NodeSlice,
        left: NodeInteger,
        f_int: impl Fn(NodeInteger, NodeInteger) -> crate::Result<NodeInteger>,
        f_float: impl Fn(NodeFloat, NodeFloat) -> crate::Result<NodeFloat>,
    ) -> HandleResult {
        let Some(node) = args.get_opt(0) else {
            return Ok(left.into());
        };

        match node.evaluate(context)? {
            NodeValue::Integer(right) => integer_chain(context, args.slice(1..)?, f_int(left, right)?, f_int, f_float),
            NodeValue::Float(right) => {
                float_chain(context, args.slice(1..)?, f_float(left as NodeFloat, right)?, f_float)
            },
            unhandled => Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
        }
    }

    fn float_chain<FF: Fn(NodeFloat, NodeFloat) -> crate::Result<NodeFloat>>(
        context: &mut Context,
        args: &NodeSlice,
        left: NodeFloat,
        f_float: FF,
    ) -> HandleResult {
        let Some(node) = args.get_opt(0) else {
            return Ok(left.into());
        };

        match node.evaluate(context)? {
            NodeValue::Integer(right) => {
                float_chain(context, args.slice(1..)?, f_float(left, right as NodeFloat)?, f_float)
            },
            NodeValue::Float(right) => float_chain(context, args.slice(1..)?, f_float(left, right)?, f_float),
            unhandled => Err(Error::bad_operand(NodeType::Float, unhandled.get_type())),
        }
    }

    evaluate_node! {
        args.evaluate(context, 0)?;
        NodeValue::Integer(value) => integer_chain(context, args.slice(1..)?, value, f_int, f_float),
        NodeValue::Float(value) => float_chain(context, args.slice(1..)?, value, f_float),
    }
}
