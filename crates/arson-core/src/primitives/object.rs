// SPDX-License-Identifier: LGPL-3.0-or-later

// use crate::Context;

// use super::{ExecuteResult, NodeSlice};

// Object handling will need to be done differently than how the original implementation does,
// due to both lifetime safety issues and due to Context being generic
// pub trait Object: std::fmt::Debug {
//     fn handle(&mut self, context: &mut Context, args: NodeSlice) -> ExecuteResult;
// }
