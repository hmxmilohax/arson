// SPDX-License-Identifier: LGPL-3.0-or-later

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::{ArrayError, NodeArray};

#[derive(PartialEq, PartialOrd, Clone)]
pub struct ArrayRef {
    inner: Rc<RefCell<NodeArray>>,
}

pub type ArrayBorrow<'a> = std::cell::Ref<'a, NodeArray>;
pub type ArrayBorrowMut<'a> = std::cell::RefMut<'a, NodeArray>;

impl ArrayRef {
    pub fn new(array: NodeArray) -> Self {
        Self { inner: Rc::new(RefCell::new(array)) }
    }

    pub fn borrow(&self) -> crate::Result<ArrayBorrow<'_>> {
        self.inner.try_borrow().map_err(|e| ArrayError::BadBorrow(e).into())
    }

    pub fn borrow_mut(&self) -> crate::Result<ArrayBorrowMut<'_>> {
        self.inner.try_borrow_mut().map_err(|e| ArrayError::BadMutBorrow(e).into())
    }

    pub fn total_cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Unlike other places in the code, there's no good way of handling or propogating
        // borrow errors without destroying the intent of the method, so this is
        // one of the few places where a potential panic is deliberately left in.
        self.inner.borrow().total_cmp(&other.inner.borrow())
    }
}

impl std::fmt::Debug for ArrayRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner.try_borrow() {
            Ok(inner) => inner.fmt(f),
            Err(_) => f.debug_tuple("NodeArray").field(&"<borrowed>").finish(),
        }
    }
}

impl std::fmt::Display for ArrayRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner.try_borrow() {
            Ok(inner) => inner.fmt(f),
            Err(_) => f.write_str("(<borrowed>)"),
        }
    }
}
