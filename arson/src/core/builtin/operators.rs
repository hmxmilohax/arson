// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::core::*;
use crate::{arson_assert, evaluate_node};

pub fn register_funcs(context: &mut Context) {
    arithmetic::register_funcs(context);
    comparison::register_funcs(context);
}

mod arithmetic {
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
    }

    fn add(context: &mut Context, args: &NodeSlice) -> HandleResult {
        fn add_integer(context: &mut Context, args: &NodeSlice, left: NodeInteger) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => add_integer(context, args.slice(1..)?, left.overflowing_add(right).0),
                NodeValue::Float(right) => add_float(context, args.slice(1..)?, left as NodeFloat + right),
                unhandled => return Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
            }
        }

        fn add_float(context: &mut Context, args: &NodeSlice, left: NodeFloat) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => add_float(context, args.slice(1..)?, left + right as NodeFloat),
                NodeValue::Float(right) => add_float(context, args.slice(1..)?, left + right),
                unhandled => return Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
            }
        }

        evaluate_node! {
            args.evaluate(context, 0)?;
            NodeValue::Integer(value) => add_integer(context, args.slice(1..)?, value),
            NodeValue::Float(value) => add_float(context, args.slice(1..)?, value),
        }
    }

    fn subtract(context: &mut Context, args: &NodeSlice) -> HandleResult {
        if args.len() == 1 {
            // Unary negation
            return evaluate_node! {
                args.evaluate(context, 0)?;
                NodeValue::Integer(value) => Ok(NodeValue::from(value.overflowing_neg().0)),
                NodeValue::Float(value) => Ok(NodeValue::from(-value)),
            };
        }

        fn subtract_integer(context: &mut Context, args: &NodeSlice, left: NodeInteger) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => subtract_integer(context, args.slice(1..)?, left.overflowing_sub(right).0),
                NodeValue::Float(right) => subtract_float(context, args.slice(1..)?, left as NodeFloat - right),
                unhandled => return Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
            }
        }

        fn subtract_float(context: &mut Context, args: &NodeSlice, left: NodeFloat) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => subtract_float(context, args.slice(1..)?, left - right as NodeFloat),
                NodeValue::Float(right) => subtract_float(context, args.slice(1..)?, left - right),
                unhandled => return Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
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
                unhandled => return Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
            }
        }

        fn multiply_float(context: &mut Context, args: &NodeSlice, left: NodeFloat) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => multiply_float(context, args.slice(1..)?, left * right as NodeFloat),
                NodeValue::Float(right) => multiply_float(context, args.slice(1..)?, left * right),
                unhandled => return Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
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
                unhandled => return Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
            }
        }

        fn divide_float(context: &mut Context, args: &NodeSlice, left: NodeFloat) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => divide_float(context, args.slice(1..)?, left / right as NodeFloat),
                NodeValue::Float(right) => divide_float(context, args.slice(1..)?, left / right),
                unhandled => return Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
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
                unhandled => return Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
            }
        }

        fn divide_float(context: &mut Context, args: &NodeSlice, left: NodeFloat) -> HandleResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(NodeValue::from(left));
            };

            match node.evaluate(context)? {
                NodeValue::Integer(right) => divide_float(context, args.slice(1..)?, left % right as NodeFloat),
                NodeValue::Float(right) => divide_float(context, args.slice(1..)?, left % right),
                unhandled => return Err(Error::bad_operand(NodeType::Integer, unhandled.get_type())),
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

    fn add_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        self::op_assign(context, args, self::add)
    }

    fn subtract_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        self::op_assign(context, args, self::subtract)
    }

    fn multiply_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        self::op_assign(context, args, self::multiply)
    }

    fn divide_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        self::op_assign(context, args, self::divide)
    }

    fn modulo_assign(context: &mut Context, args: &NodeSlice) -> HandleResult {
        self::op_assign(context, args, self::modulo)
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
        arson_assert!(args.len() == 2);
        Ok(NodeValue::from(
            args.evaluate(context, 0)? == args.evaluate(context, 1)?,
        ))
    }

    fn not_equal(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert!(args.len() == 2);
        Ok(NodeValue::from(
            args.evaluate(context, 0)? != args.evaluate(context, 1)?,
        ))
    }

    fn greater_than(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert!(args.len() == 2);
        Ok(NodeValue::from(args.evaluate(context, 0)? > args.evaluate(context, 1)?))
    }

    fn greater_equal(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert!(args.len() == 2);
        Ok(NodeValue::from(
            args.evaluate(context, 0)? >= args.evaluate(context, 1)?,
        ))
    }

    fn less_than(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert!(args.len() == 2);
        Ok(NodeValue::from(args.evaluate(context, 0)? < args.evaluate(context, 1)?))
    }

    fn less_equal(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert!(args.len() == 2);
        Ok(NodeValue::from(
            args.evaluate(context, 0)? <= args.evaluate(context, 1)?,
        ))
    }
}
