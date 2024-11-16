// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::fs::{FileSystem, VirtualPath};
use crate::parse::loader::{self, LoadOptions};
use crate::{builtin, LoadError};

use super::{Error, HandleFn, NodeArray, NodeCommand, NodeValue, Symbol, SymbolMap, SymbolTable};

pub struct Context {
    symbol_table: SymbolTable,
    macros: SymbolMap<NodeArray>,
    variables: SymbolMap<NodeValue>,
    functions: SymbolMap<HandleFn>,
    file_system: Box<dyn FileSystem>,
}

impl Context {
    pub fn new(file_system: Box<dyn FileSystem>) -> Self {
        let mut context = Self {
            symbol_table: SymbolTable::new(),
            macros: SymbolMap::new(),
            variables: SymbolMap::new(),
            functions: SymbolMap::new(),
            file_system,
        };

        builtin::register_funcs(&mut context);

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

    pub fn add_macro(&mut self, name: Symbol, array: NodeArray) {
        self.macros.insert(name, array);
    }

    pub fn remove_macro(&mut self, name: &Symbol) {
        self.macros.remove(name);
    }

    pub fn get_macro(&mut self, name: &Symbol) -> Option<&NodeArray> {
        self.macros.get(name)
    }

    pub fn get_variable(&mut self, name: &Symbol) -> NodeValue {
        match self.variables.get(name) {
            Some(value) => value.clone(),
            None => {
                let value = NodeValue::from(0);
                self.variables.insert(name.clone(), value.clone());
                value
            },
        }
    }

    pub fn set_variable(&mut self, name: &Symbol, value: NodeValue) {
        self.variables.insert(name.clone(), value);
    }

    pub fn register_func_by_name(&mut self, name: &str, func: HandleFn) -> bool {
        let symbol = self.symbol_table.add(name);
        self.register_func(&symbol, func)
    }

    pub fn register_func(&mut self, name: &Symbol, func: HandleFn) -> bool {
        self.functions.insert(name.clone(), func).is_none()
    }

    pub fn file_system(&self) -> &dyn FileSystem {
        self.file_system.as_ref()
    }

    pub fn file_system_mut(&mut self) -> &mut dyn FileSystem {
        self.file_system.as_mut()
    }

    pub fn load_path(&mut self, options: LoadOptions, path: &VirtualPath) -> Result<NodeArray, LoadError> {
        loader::load_path(self, options, path)
    }

    pub fn load_text(&mut self, options: LoadOptions, text: &str) -> Result<NodeArray, LoadError> {
        loader::load_text(self, options, text)
    }

    pub fn execute(&mut self, command: &NodeCommand) -> crate::Result<NodeValue> {
        let result = match command.evaluate(self, 0)? {
            NodeValue::Symbol(symbol) => match self.functions.get(&symbol) {
                Some(func) => func(self, command.slice(1..)?)?,
                None => return Err(Error::EntryNotFound(symbol.clone())),
            },
            NodeValue::Object(_obj) => todo!("obj.handle(self, command)?"),
            NodeValue::Function(func) => func(self, command.slice(1..)?)?,

            _ => NodeValue::Unhandled,
        };

        if let NodeValue::Unhandled = result {
            todo!("default handler")
        }

        Ok(result)
    }
}
