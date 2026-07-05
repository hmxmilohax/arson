// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::{Context, ExecuteResult, NodeFloat, NodeInteger, NodeNumber, NodeSlice};

// Number utilities
impl NodeSlice {
    pub fn number_chain(
        &self,
        context: &mut Context,
        f_int: impl Fn(NodeInteger, NodeInteger) -> crate::Result<NodeInteger>,
        f_float: impl Fn(NodeFloat, NodeFloat) -> crate::Result<NodeFloat>,
    ) -> ExecuteResult {
        fn integer_chain(
            context: &mut Context,
            args: &NodeSlice,
            left: NodeInteger,
            f_int: impl Fn(NodeInteger, NodeInteger) -> crate::Result<NodeInteger>,
            f_float: impl Fn(NodeFloat, NodeFloat) -> crate::Result<NodeFloat>,
        ) -> ExecuteResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(left.into());
            };

            match node.number(context)? {
                NodeNumber::Integer(right) => {
                    integer_chain(context, args.slice(1..)?, f_int(left, right)?, f_int, f_float)
                },
                NodeNumber::Float(right) => {
                    float_chain(context, args.slice(1..)?, f_float(left.0 as NodeFloat, right)?, f_float)
                },
            }
        }

        fn float_chain(
            context: &mut Context,
            args: &NodeSlice,
            left: NodeFloat,
            f_float: impl Fn(NodeFloat, NodeFloat) -> crate::Result<NodeFloat>,
        ) -> ExecuteResult {
            let Some(node) = args.get_opt(0) else {
                return Ok(left.into());
            };

            match node.number(context)? {
                NodeNumber::Integer(right) => {
                    float_chain(context, args.slice(1..)?, f_float(left, right.0 as NodeFloat)?, f_float)
                },
                NodeNumber::Float(right) => {
                    float_chain(context, args.slice(1..)?, f_float(left, right)?, f_float)
                },
            }
        }

        match self.number(context, 0)? {
            NodeNumber::Integer(value) => integer_chain(context, self.slice(1..)?, value, f_int, f_float),
            NodeNumber::Float(value) => float_chain(context, self.slice(1..)?, value, f_float),
        }
    }
}
