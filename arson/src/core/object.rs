// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::Context;

use super::{HandleResult, NodeSlice};

pub trait Object: std::fmt::Debug {
    fn handle(&mut self, context: &mut Context, args: NodeSlice) -> HandleResult;
}
