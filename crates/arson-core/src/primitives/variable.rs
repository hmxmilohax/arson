// SPDX-License-Identifier: LGPL-3.0-or-later

use std::rc::Rc;

use super::{Node, NodeSlice, NodeValue, Symbol};
use crate::{arson_assert_len, Context};

#[derive(Clone)]
pub struct Variable {
    symbol: Symbol,
}

impl Variable {
    pub fn new(name: &str, context: &mut Context) -> Self {
        let symbol = context.add_symbol(name);
        Self { symbol }
    }

    pub const fn symbol(&self) -> &Symbol {
        &self.symbol
    }

    pub const fn name(&self) -> &Rc<String> {
        self.symbol.name()
    }

    pub fn get(&self, context: &mut Context) -> Node {
        context.get_variable(&self.symbol)
    }

    pub fn set(&self, context: &mut Context, value: impl Into<Node>) {
        context.set_variable(&self.symbol, value)
    }
}

impl std::fmt::Debug for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.name())
    }
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.name())
    }
}

#[derive(Debug)]
struct VariableEntry {
    variable: Variable,
    value: Node,
}

pub struct VariableStack<'ctx> {
    context: &'ctx mut Context,
    stack: Vec<VariableEntry>,
}

impl<'ctx> VariableStack<'ctx> {
    pub fn new(context: &'ctx mut Context) -> Self {
        Self { context, stack: Vec::new() }
    }

    pub fn context(&mut self) -> &mut Context {
        self.context
    }

    pub fn push(&mut self, variable: &Variable) {
        self.stack.push(VariableEntry {
            variable: variable.clone(),
            value: variable.get(self.context),
        })
    }

    pub fn pop(&mut self) -> Option<Variable> {
        let entry = self.stack.pop()?;
        entry.variable.set(self.context, entry.value);
        Some(entry.variable)
    }

    pub fn push_args(&mut self, script: &mut &NodeSlice, args: &NodeSlice) -> crate::Result {
        if let NodeValue::Array(parameters) = script.unevaluated(0)? {
            *script = script.slice(1..)?;

            let parameters = parameters.borrow()?;
            arson_assert_len!(parameters, args.len(), "script parameter list has the wrong size");

            for i in 0..parameters.len() {
                let variable = parameters.variable(i)?;
                self.push(variable);

                let value = args.evaluate(self.context, i)?;
                variable.set(self.context, value);
            }
        }

        Ok(())
    }

    pub fn push_initializers(&mut self, args: &mut &NodeSlice) -> crate::Result {
        while let NodeValue::Array(var_decl) = args.unevaluated(0)? {
            *args = args.slice(1..)?;

            let var_decl = var_decl.borrow()?;
            let variable = var_decl.variable(0)?;
            self.push(variable);

            let initializer = var_decl.slice(1..)?;
            if let Some(value) = initializer.get_opt(0) {
                arson_assert_len!(initializer, 1, "too many values in initializer for {variable}");
                let value = value.evaluate(self.context)?;
                variable.set(self.context, value);
            }
        }

        Ok(())
    }
}

impl Drop for VariableStack<'_> {
    fn drop(&mut self) {
        for entry in self.stack.iter() {
            entry.variable.set(self.context, entry.value.clone());
        }

        self.stack.clear();
    }
}
