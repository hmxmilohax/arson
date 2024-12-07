// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::primitives::*;
use crate::{arson_array, arson_assert_len};

/// A function which is callable from script.
pub type HandleFn<State> = fn(context: &mut Context<State>, args: &NodeSlice) -> ExecuteResult;

/// The result of a script execution.
pub type ExecuteResult = crate::Result<Node>;

#[derive(thiserror::Error, Debug)]
pub enum ExecutionError {
    #[error("No function registered for name '{0}'")]
    FunctionNotFound(Symbol),

    #[error("{0}")]
    Failure(String),
}

pub struct Context<State> {
    // State type is exposed via Deref/DerefMut
    // for more seamless integration
    state: State,

    symbol_table: SymbolTable,

    macros: SymbolMap<NodeArray>,
    variables: SymbolMap<Node>,
    functions: SymbolMap<HandleFn<State>>,
}

impl<State> Context<State> {
    pub fn new(state: State) -> Self {
        let mut context = Self {
            state,

            symbol_table: SymbolTable::new(),

            macros: SymbolMap::new(),
            variables: SymbolMap::new(),
            functions: SymbolMap::new(),
        };

        crate::builtin::register_funcs(&mut context);

        context
    }

    pub fn add_symbol(&mut self, name: &str) -> Symbol {
        self.symbol_table.add(name)
    }

    pub fn remove_symbol(&mut self, name: &Symbol) {
        self.symbol_table.remove(name);
    }

    pub fn get_symbol(&mut self, name: &str) -> Option<Symbol> {
        self.symbol_table.get(name)
    }

    pub fn add_macro(&mut self, name: &Symbol, array: NodeArray) {
        self.macros.insert(name.clone(), array);
    }

    pub fn add_macro_define(&mut self, name: &Symbol) {
        self.add_macro(name, arson_array![1]);
    }

    pub fn remove_macro(&mut self, name: &Symbol) {
        self.macros.remove(name);
    }

    pub fn get_macro(&mut self, name: &Symbol) -> Option<&NodeArray> {
        self.macros.get(name)
    }

    pub fn get_variable(&mut self, name: &Symbol) -> Node {
        match self.variables.get(name) {
            Some(value) => value.clone(),
            None => {
                let value = Node::from(0);
                self.variables.insert(name.clone(), value.clone());
                value
            },
        }
    }

    pub fn set_variable<T: Into<Node>>(&mut self, name: &Symbol, value: T) {
        self.variables.insert(name.clone(), value.into());
    }

    pub fn register_func(&mut self, name: &str, func: HandleFn<State>) -> bool {
        let name = self.symbol_table.add(name);
        self.functions.insert(name.clone(), func).is_none()
    }

    pub fn register_func_alias(&mut self, alias: &str, actual: &str) -> bool {
        let actual = self.symbol_table.add(actual);
        match self.functions.get(&actual) {
            Some(func) => self.register_func(alias, *func),
            None => false,
        }
    }

    pub fn execute(&mut self, command: &NodeCommand) -> ExecuteResult {
        let result = match command.evaluate(self, 0)? {
            NodeValue::Symbol(symbol) => match self.functions.get(&symbol) {
                // TODO: cache function/object lookups
                Some(func) => func(self, command.slice(1..)?)?,
                None => return Err(ExecutionError::FunctionNotFound(symbol).into()),
            },
            _ => Node::UNHANDLED,
        };

        if let NodeValue::Unhandled = result.unevaluated() {
            todo!("default handler")
        }

        Ok(result)
    }

    pub fn execute_block(&mut self, script: &NodeSlice) -> ExecuteResult {
        for node in script.slice(..script.len() - 1)? {
            node.command()?.execute(self)?;
        }

        script.evaluate(self, script.len() - 1).map(|v| v.into())
    }

    pub fn execute_args(&mut self, mut script: &NodeSlice, args: &NodeSlice) -> ExecuteResult {
        let mut saved_variables = VariableStack::new();
        if let NodeValue::Array(parameters) = script.unevaluated(0)? {
            script = script.slice(1..)?;

            let parameters = parameters.borrow()?;
            arson_assert_len!(parameters, args.len(), "script parameter list has the wrong size");

            for i in 0..parameters.len() {
                let variable = parameters.variable(i)?;
                saved_variables.save(self, variable);

                let value = args.evaluate(self, i)?;
                variable.set(self, value);
            }
        }

        let result = self.execute_block(script)?;
        saved_variables.restore(self);
        Ok(result)
    }
}

impl<State: Default> Default for Context<State> {
    fn default() -> Self {
        Self::new(Default::default())
    }
}

impl<State> std::ops::Deref for Context<State> {
    type Target = State;

    fn deref(&self) -> &Self::Target {
        &self.state
    }
}

impl<State> std::ops::DerefMut for Context<State> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.state
    }
}
