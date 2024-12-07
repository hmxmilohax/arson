// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::Context;

use super::{Node, Symbol};

#[derive(Debug, Clone)]
pub struct Variable {
    symbol: Symbol,
}

impl Variable {
    pub fn new<S>(name: &str, context: &mut Context<S>) -> Self {
        let symbol = context.add_symbol(name);
        Self { symbol }
    }

    pub fn symbol(&self) -> &Symbol {
        &self.symbol
    }

    pub fn get<S>(&self, context: &mut Context<S>) -> Node {
        context.get_variable(&self.symbol)
    }

    pub fn set<S, T: Into<Node>>(&self, context: &mut Context<S>, value: T) {
        context.set_variable(&self.symbol, value)
    }
}

#[derive(Debug)]
struct VariableEntry {
    variable: Variable,
    value: Node,
}

#[derive(Debug, Default)]
pub struct VariableStack {
    stack: Vec<VariableEntry>,
}

impl VariableStack {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn save<S>(&mut self, context: &mut Context<S>, variable: &Variable) {
        self.stack.push(VariableEntry {
            variable: variable.clone(),
            value: variable.get(context),
        })
    }

    // Would have liked if there were a way to do this implicitly,
    // but the borrow checker said no, so this will have to do...
    pub fn restore<S>(&mut self, context: &mut Context<S>) {
        for entry in self.stack.iter() {
            entry.variable.set(context, entry.value.clone());
        }

        self.stack.clear();
    }
}

impl Drop for VariableStack {
    fn drop(&mut self) {
        // Ensure no unbalanced stacks are left behind
        assert_eq!(self.stack.len(), 0);
    }
}
