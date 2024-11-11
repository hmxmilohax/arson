// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::core::*;
use crate::{arson_assert, evaluate_node, evaluate_node_opt};

pub fn register_funcs(context: &mut Context) {
    context.register_func_by_name("+", add);
    context.register_func_by_name("+=", add_assign);
    context.register_func_by_name("-", subtract);
    context.register_func_by_name("-=", subtract_assign);
    context.register_func_by_name("*", multiply);
    context.register_func_by_name("*=", multiply_assign);
    context.register_func_by_name("/", divide);
    context.register_func_by_name("/=", divide_assign);
    context.register_func_by_name("%", modulo);
    context.register_func_by_name("%=", modulo_assign);
}

pub fn add(context: &mut Context, args: &NodeSlice) -> HandleResult {
    fn add_integer(context: &mut Context, args: &NodeSlice, left: NodeInteger) -> HandleResult {
        evaluate_node_opt! {
            Some(node @ args.get_opt(0)) => node.evaluate(context)?;
            None => Ok(NodeValue::from(left));

            NodeValue::Integer(right) => add_integer(context, args.slice(1..)?, left.overflowing_add(right).0),
            NodeValue::Float(right) => add_float(context, args.slice(1..)?, left as NodeFloat + right),
        }
    }

    fn add_float(context: &mut Context, args: &NodeSlice, left: NodeFloat) -> HandleResult {
        evaluate_node_opt! {
            Some(node @ args.get_opt(0)) => node.evaluate(context)?;
            None => Ok(NodeValue::from(left));

            NodeValue::Integer(right) => add_float(context, args.slice(1..)?, left + right as NodeFloat),
            NodeValue::Float(right) => add_float(context, args.slice(1..)?, left + right),
        }
    }

    evaluate_node! {
        args.evaluate(context, 0)?;
        NodeValue::Integer(value) => add_integer(context, args.slice(1..)?, value),
        NodeValue::Float(value) => add_float(context, args.slice(1..)?, value),
    }
}

pub fn subtract(context: &mut Context, args: &NodeSlice) -> HandleResult {
    fn subtract_integer(context: &mut Context, args: &NodeSlice, left: NodeInteger) -> HandleResult {
        evaluate_node_opt! {
            Some(node @ args.get_opt(0)) => node.evaluate(context)?;
            None => Ok(NodeValue::from(left));

            NodeValue::Integer(right) => subtract_integer(context, args.slice(1..)?, left.overflowing_sub(right).0),
            NodeValue::Float(right) => subtract_float(context, args.slice(1..)?, left as NodeFloat - right),
        }
    }

    fn subtract_float(context: &mut Context, args: &NodeSlice, left: NodeFloat) -> HandleResult {
        evaluate_node_opt! {
            Some(node @ args.get_opt(0)) => node.evaluate(context)?;
            None => Ok(NodeValue::from(left));

            NodeValue::Integer(right) => subtract_float(context, args.slice(1..)?, left - right as NodeFloat),
            NodeValue::Float(right) => subtract_float(context, args.slice(1..)?, left - right),
        }
    }

    evaluate_node! {
        args.evaluate(context, 0)?;
        NodeValue::Integer(value) => subtract_integer(context, args.slice(1..)?, value),
        NodeValue::Float(value) => subtract_float(context, args.slice(1..)?, value),
    }
}

pub fn multiply(context: &mut Context, args: &NodeSlice) -> HandleResult {
    fn multiply_integer(context: &mut Context, args: &NodeSlice, left: NodeInteger) -> HandleResult {
        evaluate_node_opt! {
            Some(node @ args.get_opt(0)) => node.evaluate(context)?;
            None => Ok(NodeValue::from(left));

            NodeValue::Integer(right) => multiply_integer(context, args.slice(1..)?, left.overflowing_mul(right).0),
            NodeValue::Float(right) => multiply_float(context, args.slice(1..)?, left as NodeFloat * right),
        }
    }

    fn multiply_float(context: &mut Context, args: &NodeSlice, left: NodeFloat) -> HandleResult {
        evaluate_node_opt! {
            Some(node @ args.get_opt(0)) => node.evaluate(context)?;
            None => Ok(NodeValue::from(left));

            NodeValue::Integer(right) => multiply_float(context, args.slice(1..)?, left * right as NodeFloat),
            NodeValue::Float(right) => multiply_float(context, args.slice(1..)?, left * right),
        }
    }

    evaluate_node! {
        args.evaluate(context, 0)?;
        NodeValue::Integer(value) => multiply_integer(context, args.slice(1..)?, value),
        NodeValue::Float(value) => multiply_float(context, args.slice(1..)?, value),
    }
}

pub fn divide(context: &mut Context, args: &NodeSlice) -> HandleResult {
    fn divide_integer(context: &mut Context, args: &NodeSlice, left: NodeInteger) -> HandleResult {
        evaluate_node_opt! {
            Some(node @ args.get_opt(0)) => node.evaluate(context)?;
            None => Ok(NodeValue::from(left));

            NodeValue::Integer(right) => {
                arson_assert!(right != 0, "attempted to divide by zero");
                divide_integer(context, args.slice(1..)?, left.overflowing_div(right).0)
            },
            NodeValue::Float(right) => divide_float(context, args.slice(1..)?, left as NodeFloat / right),
        }
    }

    fn divide_float(context: &mut Context, args: &NodeSlice, left: NodeFloat) -> HandleResult {
        evaluate_node_opt! {
            Some(node @ args.get_opt(0)) => node.evaluate(context)?;
            None => Ok(NodeValue::from(left));

            NodeValue::Integer(right) => divide_float(context, args.slice(1..)?, left / right as NodeFloat),
            NodeValue::Float(right) => divide_float(context, args.slice(1..)?, left / right),
        }
    }

    evaluate_node! {
        args.evaluate(context, 0)?;
        NodeValue::Integer(value) => divide_integer(context, args.slice(1..)?, value),
        NodeValue::Float(value) => divide_float(context, args.slice(1..)?, value),
    }
}

pub fn modulo(context: &mut Context, args: &NodeSlice) -> HandleResult {
    fn divide_integer(context: &mut Context, args: &NodeSlice, left: NodeInteger) -> HandleResult {
        evaluate_node_opt! {
            Some(node @ args.get_opt(0)) => node.evaluate(context)?;
            None => Ok(NodeValue::from(left));

            NodeValue::Integer(right) => {
                arson_assert!(right != 0, "attempted to modulo by zero");
                divide_integer(context, args.slice(1..)?, left.overflowing_rem(right).0)
            },
            NodeValue::Float(right) => divide_float(context, args.slice(1..)?, left as NodeFloat % right),
        }
    }

    fn divide_float(context: &mut Context, args: &NodeSlice, left: NodeFloat) -> HandleResult {
        evaluate_node_opt! {
            Some(node @ args.get_opt(0)) => node.evaluate(context)?;
            None => Ok(NodeValue::from(left));

            NodeValue::Integer(right) => divide_float(context, args.slice(1..)?, left % right as NodeFloat),
            NodeValue::Float(right) => divide_float(context, args.slice(1..)?, left % right),
        }
    }

    evaluate_node! {
        args.evaluate(context, 0)?;
        NodeValue::Integer(value) => divide_integer(context, args.slice(1..)?, value),
        NodeValue::Float(value) => divide_float(context, args.slice(1..)?, value),
    }
}

fn op_assign(context: &mut Context, args: &NodeSlice, f: HandleFn) -> HandleResult {
    let result = f(context, args)?;
    evaluate_node! {
        args.unevaluated(0)?;
        RawNodeValue::Variable(value) => context.set_variable(value.symbol.clone(), result.clone()),
        RawNodeValue::Property(_value) => todo!("op_assign property access"),
    };
    Ok(result)
}

pub fn add_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
    self::op_assign(context, args, self::add)
}

pub fn subtract_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
    self::op_assign(context, args, self::subtract)
}

pub fn multiply_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
    self::op_assign(context, args, self::multiply)
}

pub fn divide_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
    self::op_assign(context, args, self::divide)
}

pub fn modulo_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
    self::op_assign(context, args, self::modulo)
}
