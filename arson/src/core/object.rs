// SPDX-License-Identifier: LGPL-3.0-or-later

use super::{Context, HandleResult, NodeSlice};

pub trait Object: std::fmt::Debug {
    fn handle(&mut self, context: &mut Context, args: NodeSlice) -> HandleResult;
}
