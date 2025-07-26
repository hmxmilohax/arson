// SPDX-License-Identifier: LGPL-3.0-or-later

use std::rc::Rc;

use super::{Node, NodeSlice, NodeValue, Symbol};
use crate::{arson_assert_len, Context};

#[derive(Clone)]
pub struct Variable {
    symbol: Symbol,
}

impl Variable {
    pub fn new(name: &str, context: &mut Context) -> crate::Result<Self> {
        context.add_symbol(name).map(|symbol| Self { symbol })
    }

    pub fn new_required(name: &str, context: &mut Context) -> Self {
        let symbol = context.add_required_symbol(name);
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

    pub fn get_opt(&self, context: &mut Context) -> Option<Node> {
        context.get_variable_opt(&self.symbol)
    }

    pub fn set(&self, context: &mut Context, value: impl Into<Node>) -> Option<Node> {
        context.set_variable(&self.symbol, value)
    }

    pub fn save(&self, context: &mut Context) -> VariableSave {
        VariableSave::new(context, self)
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

#[derive(Debug, Clone)]
pub struct VariableSave {
    pub variable: Variable,
    pub value: Node,
}

impl VariableSave {
    pub fn new(context: &mut Context, variable: &Variable) -> Self {
        let value = variable.get(context);
        Self { variable: variable.clone(), value }
    }

    pub fn restore(&self, context: &mut Context) {
        self.variable.set(context, self.value.clone());
    }
}

pub struct VariableStack<'ctx> {
    context: &'ctx mut Context,
    stack: Vec<VariableSave>,
}

impl<'ctx> VariableStack<'ctx> {
    pub fn new(context: &'ctx mut Context) -> Self {
        Self { context, stack: Vec::new() }
    }

    pub fn context(&mut self) -> &mut Context {
        self.context
    }

    pub fn push(&mut self, variable: &Variable, value: impl Into<Node>) {
        let old = variable.save(self.context);
        self.stack.push(old);
        variable.set(self.context, value);
    }

    pub fn pop(&mut self) -> Option<Variable> {
        let entry = self.stack.pop()?;
        entry.restore(self.context);
        Some(entry.variable)
    }

    pub fn clear(&mut self) {
        while self.pop().is_some() {}
    }

    pub fn push_args(&mut self, script: &mut &NodeSlice, args: &NodeSlice) -> crate::Result {
        if let NodeValue::Array(parameters) = script.unevaluated(0)? {
            *script = script.slice(1..)?;

            let parameters = parameters.borrow()?;
            arson_assert_len!(args, parameters.len(), "script parameter list has the wrong size");

            for i in 0..parameters.len() {
                let variable = parameters.variable(i)?;
                let value = args.evaluate(self.context, i)?;
                self.push(variable, value);
            }
        }

        Ok(())
    }

    pub fn push_initializers(&mut self, args: &mut &NodeSlice) -> crate::Result {
        while let NodeValue::Array(var_decl) = args.unevaluated(0)? {
            *args = args.slice(1..)?;

            let var_decl = var_decl.borrow()?;
            let variable = var_decl.variable(0)?;

            let initializer = var_decl.slice(1..)?;
            if let Some(value) = initializer.get_opt(0) {
                arson_assert_len!(initializer, 1, "too many values in initializer for {variable}");
                let value = value.evaluate(self.context)?;
                self.push(variable, value);
            } else {
                self.push(variable, Node::UNHANDLED);
            }
        }

        Ok(())
    }

    pub fn push_saved(&mut self, saved: &[VariableSave]) {
        for saved in saved {
            self.push(&saved.variable, saved.value.clone());
        }
    }
}

impl Drop for VariableStack<'_> {
    fn drop(&mut self) {
        self.clear();
    }
}
