// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::{parse::loader, LoadError};

use super::{Error, HandleFn, Node, NodeArray, NodeCommand, NodeValue, Symbol, SymbolMap, SymbolTable};

pub struct Context {
    symbol_table: SymbolTable,
    macros: SymbolMap<NodeArray>,
    fn_map: SymbolMap<HandleFn>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            macros: SymbolMap::new(),
            fn_map: SymbolMap::new(),
        }
    }

    pub fn add_symbol(&mut self, name: &str) -> Symbol {
        self.symbol_table.add(name)
    }

    pub fn remove_symbol(&mut self, name: &Symbol) -> bool {
        self.symbol_table.remove_by_symbol(name)
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

    pub fn register_func_by_name(&mut self, name: &str, func: HandleFn) -> crate::Result<()> {
        let symbol = self.symbol_table.add(name);
        self.register_func(symbol, func)
    }

    pub fn register_func(&mut self, name: Symbol, func: HandleFn) -> crate::Result<()> {
        if self.fn_map.contains_key(&name) {
            return Err(Error::DuplicateEntry(name));
        }

        self.fn_map.insert(name, func);
        Ok(())
    }

    pub fn load_text(&mut self, text: &str) -> Result<NodeArray, LoadError> {
        loader::load_text(self, text)
    }

    pub fn execute(&mut self, command: &NodeCommand) -> crate::Result<Node> {
        let result = match command.evaluate(self, 0)? {
            NodeValue::Symbol(symbol) => match self.fn_map.get(&symbol) {
                Some(func) => func(self, command)?,
                None => return Err(Error::EntryNotFound(symbol.clone())),
            },
            NodeValue::Object(_obj) => todo!("obj.handle(self, command)?"),
            NodeValue::Function(func) => func(self, command)?,

            _ => Node::unhandled(),
        };

        if result.is_unhandled() {
            todo!("default handler")
        }

        Ok(result)
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}
