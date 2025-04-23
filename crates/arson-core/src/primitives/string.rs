// SPDX-License-Identifier: LGPL-3.0-or-later

use std::cell::Cell;

use crate::{Context, NodeSlice};

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum StringError {
    #[error("index {0} is not a UTF-8 character boundary")]
    NotCharBoundary(usize),
}

/// Concatenates a slice of nodes into a string, without a separator between each.
pub struct ConcatSlice<'a> {
    context: Cell<Option<&'a mut Context>>,
    args: &'a NodeSlice,
}

impl<'a> ConcatSlice<'a> {
    pub fn new(context: &'a mut Context, args: &'a NodeSlice) -> Self {
        Self { context: Cell::new(Some(context)), args }
    }
}

impl std::fmt::Display for ConcatSlice<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.context.take() {
            Some(context) => {
                for arg in self.args {
                    match arg.evaluate(context) {
                        Ok(value) => value.fmt(f)?,
                        Err(err) => write!(f, "<error: {err}>")?,
                    }
                }
                self.context.set(Some(context));
            },
            None => {
                for arg in self.args {
                    arg.unevaluated().fmt(f)?;
                }
            },
        }

        Ok(())
    }
}
