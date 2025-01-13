// SPDX-License-Identifier: LGPL-3.0-or-later

use std::rc::Rc;

use crate::{Context, ExecuteResult, NodeSlice};

// TODO once stabilized: https://github.com/rust-lang/rust/issues/41517
// trait HandleFnInner = Fn(&mut Context, &NodeSlice) -> ExecuteResult;

/// A function which is callable from script.
#[derive(Clone)]
#[allow(
    clippy::type_complexity,
    reason = "no benefit due to inability to use trait object types as generics bounds"
)]
pub struct HandleFn(Rc<dyn Fn(&mut Context, &NodeSlice) -> ExecuteResult>);

impl HandleFn {
    pub fn new(f: impl Fn(&mut Context, &NodeSlice) -> ExecuteResult + 'static) -> Self {
        Self(Rc::new(f))
    }

    pub fn call(&self, context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        self.0(context, args)
    }

    pub fn total_cmp(&self, other: &Self) -> std::cmp::Ordering {
        let left = Rc::as_ptr(&self.0) as *const ();
        let right = Rc::as_ptr(&other.0) as *const ();
        left.cmp(&right)
    }
}

impl<F: Fn(&mut Context, &NodeSlice) -> ExecuteResult + 'static> From<F> for HandleFn {
    fn from(value: F) -> Self {
        Self::new(value)
    }
}

impl PartialEq for HandleFn {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl std::fmt::Debug for HandleFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("HandleFn").finish_non_exhaustive()
    }
}

impl std::fmt::Display for HandleFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<function {:X}>", Rc::as_ptr(&self.0) as *const () as usize)
    }
}
