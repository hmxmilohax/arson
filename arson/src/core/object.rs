// SPDX-License-Identifier: LGPL-3.0-or-later

use super::{HandleResult, NodeCommand};

pub trait Object: std::fmt::Debug {
    fn handle(&mut self, args: &NodeCommand) -> HandleResult;
}
