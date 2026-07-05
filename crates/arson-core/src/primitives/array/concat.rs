// SPDX-License-Identifier: LGPL-3.0-or-later

use std::cell::Cell;

use crate::{Context, NodeSlice};

/// Concatenates a slice of nodes into a string, without any separator.
pub struct ArrayConcat<'a> {
    values: &'a NodeSlice,
    context: Cell<Option<&'a mut Context>>,
}

impl<'a> ArrayConcat<'a> {
    pub fn new(values: &'a NodeSlice) -> Self {
        Self { values, context: Cell::new(None) }
    }

    pub fn new_evaluated(values: &'a NodeSlice, context: &'a mut Context) -> Self {
        Self { values, context: Cell::new(Some(context)) }
    }
}

impl std::fmt::Display for ArrayConcat<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.context.take() {
            Some(context) => {
                for arg in self.values {
                    match arg.evaluate(context) {
                        Ok(value) => value.fmt(f)?,
                        Err(err) => write!(f, "<error: {err}>")?,
                    }
                }
                self.context.set(Some(context));
            },
            None => {
                for arg in self.values {
                    arg.unevaluated().fmt(f)?;
                }
            },
        }

        Ok(())
    }
}
