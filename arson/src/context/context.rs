// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::core::*;
use crate::fs::{AbsolutePath, FileSystem, FileSystemDriver, VirtualPath};
use crate::parse::{self, Expression, LoadOptions};
use crate::{arson_array, arson_assert_len, LoadError};

#[derive(thiserror::Error, Debug)]
pub enum ExecutionError {
    #[error("No function registered for name '{0}'")]
    FunctionNotFound(Symbol),

    #[error("{0}")]
    Failure(String),
}

pub struct Context {
    symbol_table: SymbolTable,
    macros: SymbolMap<NodeArray>,
    variables: SymbolMap<Node>,
    functions: SymbolMap<HandleFn>,
    file_system: FileSystem,
}

impl Context {
    pub fn new() -> Self {
        let context = Self {
            symbol_table: SymbolTable::new(),
            macros: SymbolMap::new(),
            variables: SymbolMap::new(),
            functions: SymbolMap::new(),
            file_system: FileSystem::new_empty(),
        };

        Self::initialize(context)
    }

    pub fn with_file_driver<T: FileSystemDriver + 'static>(driver: T) -> Self {
        let context = Self {
            symbol_table: SymbolTable::new(),
            macros: SymbolMap::new(),
            variables: SymbolMap::new(),
            functions: SymbolMap::new(),
            file_system: FileSystem::new(driver),
        };

        Self::initialize(context)
    }

    #[inline]
    fn initialize(mut context: Self) -> Self {
        super::flow::register_funcs(&mut context);
        super::operators::register_funcs(&mut context);

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

    pub fn register_func(&mut self, name: &str, func: HandleFn) -> bool {
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

    pub fn file_system(&self) -> &FileSystem {
        &self.file_system
    }

    pub fn cwd(&self) -> &AbsolutePath {
        self.file_system.cwd()
    }

    pub fn set_cwd<P: AsRef<VirtualPath>>(&mut self, path: P) -> AbsolutePath {
        self.file_system.set_cwd(path)
    }

    pub fn load_path<P: AsRef<VirtualPath>>(&mut self, options: LoadOptions, path: P) -> Result<NodeArray, LoadError> {
        parse::load_path(self, options, path)
    }

    pub fn load_text(&mut self, options: LoadOptions, text: &str) -> Result<NodeArray, LoadError> {
        parse::load_text(self, options, text)
    }

    pub fn load_ast<'src>(
        &mut self,
        options: LoadOptions,
        ast: impl Iterator<Item = Expression<'src>>,
    ) -> Result<NodeArray, LoadError> {
        parse::load_ast(self, options, ast)
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

            arson_assert_len!(parameters, args.len(), "script parameter list has the wrong size");

            for i in 0..parameters.len() {
                let variable = parameters.variable(i)?;
                saved_variables.save(self, &variable);

                let value = args.evaluate(self, i)?;
                variable.set(self, value);
            }
        }

        let result = self.execute_block(script)?;
        saved_variables.restore(self);
        Ok(result)
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}
