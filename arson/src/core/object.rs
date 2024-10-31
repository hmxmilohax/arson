// SPDX-License-Identifier: LGPL-3.0-or-later

use super::{Context, HandleResult, NodeCommand};

pub trait Object: std::fmt::Debug {
    fn handle(&mut self, context: &mut Context, args: &NodeCommand) -> HandleResult;
}
