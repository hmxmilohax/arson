// SPDX-License-Identifier: LGPL-3.0-or-later

use std::rc::Rc;

mod table;

pub(crate) use table::*;

pub type NodeString = Rc<str>;

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum StringError {
    #[error("index {0} is not a UTF-8 character boundary")]
    NotCharBoundary(usize),
}
