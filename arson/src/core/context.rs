// SPDX-License-Identifier: LGPL-3.0-or-later

use super::{Error, HandleFn, NodeArray, Symbol, SymbolMap, SymbolTable};

pub struct Context {
    symbol_table: SymbolTable,
    macros: SymbolMap<NodeArray>,
    fn_map: SymbolMap<HandleFn>,
}

impl Context {
    pub fn new() -> Self {
        let context = Self {
            symbol_table: SymbolTable::new(),
            macros: SymbolMap::new(),
            fn_map: SymbolMap::new(),
        };

        context
    }

    pub fn add_symbol(&mut self, name: &str) -> Symbol {
        self.symbol_table.add(name)
    }

    pub fn remove_symbol(&mut self, name: &Symbol) -> bool {
        self.symbol_table.remove_by_symbol(name)
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

    pub fn add_macro(&mut self, name: Symbol, array: NodeArray) {
        self.macros.insert(name, array);
    }

    pub fn remove_macro(&mut self, name: &Symbol) {
        self.macros.remove(name);
    }

    pub fn get_macro(&mut self, name: &Symbol) -> Option<&NodeArray> {
        self.macros.get(name)
    }
}