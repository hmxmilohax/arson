// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::{arson_fail, evaluate_node, Error, NodeFloat, NodeInteger, NodeType, RawNodeValue};

use super::{Context, HandleResult, Node, NodeSlice, NodeValue};

pub mod flow;
pub mod operators;

pub fn register_funcs(context: &mut Context) {
    flow::register_funcs(context);
    operators::register_funcs(context);
}

pub fn set_variable(context: &mut Context, arg: &Node, result: NodeValue) -> HandleResult {
    match arg.unevaluated() {
        RawNodeValue::Variable(value) => context.set_variable(value.symbol.clone(), result.clone()),
        RawNodeValue::Property(_value) => todo!("op_assign property access"),
        unhandled => arson_fail!("Cannot set non-variable {:?}", unhandled.get_type()),
    };
    Ok(result)
}

pub fn op_assign(context: &mut Context, args: &NodeSlice, result: NodeValue) -> HandleResult {
    set_variable(context, args.get(0)?, result)
}

pub fn number_chain<
    IF: Fn(NodeInteger, NodeInteger) -> crate::Result<NodeInteger>,
    FF: Fn(NodeFloat, NodeFloat) -> crate::Result<NodeFloat>,
>(
    context: &mut Context,
    args: &NodeSlice,
    f_int: IF,
    f_float: FF,
) -> HandleResult {
    fn integer_chain<
        IF: Fn(NodeInteger, NodeInteger) -> crate::Result<NodeInteger>,
        FF: Fn(NodeFloat, NodeFloat) -> crate::Result<NodeFloat>,
    >(
        context: &mut Context,
        args: &NodeSlice,
        left: NodeInteger,
        f_int: IF,
        f_float: FF,
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
