// SPDX-License-Identifier: LGPL-3.0-or-later

use super::{Error, HandleFn, Symbol, SymbolMap, SymbolTable};

pub struct Context {
    symbol_table: SymbolTable,
    fn_map: SymbolMap<HandleFn>,
}

impl Context {
    pub fn new() -> Self {
        let context = Self {
            symbol_table: SymbolTable::new(),
            fn_map: SymbolMap::new(),
        };

        context
    }

    pub fn register_func_by_name(&mut self, name: &str, func: HandleFn) -> Result<(), Error> {
        let symbol = self.symbol_table.add(name);
        self.register_func(symbol, func)
    }

    pub fn register_func(&mut self, name: Symbol, func: HandleFn) -> Result<(), Error> {
        if self.fn_map.contains_key(&name) {
            return Err(Error::DuplicateEntry(name));
        }

        self.fn_map.insert(name, func);
        Ok(())
    }
}
