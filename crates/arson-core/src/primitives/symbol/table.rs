// SPDX-License-Identifier: LGPL-3.0-or-later

use std::rc::Rc;

use crate::{arson_fail, StringTable, Symbol};

pub(crate) struct SymbolTable {
    table: StringTable,
}

impl SymbolTable {
    /// Constructs a new [`SymbolTable`].
    pub fn new() -> Self {
        Self { table: StringTable::new() }
    }

    /// Adds a symbol by name to the table.
    /// The created symbol is also returned for convenience.
    ///
    /// If a symbol for this name already exists, that symbol is returned.
    ///
    /// # Errors
    ///
    /// Fails if the name is longer than [`Symbol::MAX_LENGTH`].
    pub fn add(&mut self, name: &str) -> crate::Result<Symbol> {
        if name.len() > Symbol::MAX_LENGTH {
            arson_fail!(
                "symbol '{}...' exceeds length safety limit",
                name.chars().take(Symbol::MAX_LENGTH.min(50)).collect::<String>()
            );
        }

        let sym = self.table.add(name);
        Ok(Symbol::new(sym))
    }

    /// Adds a symbol by name to the table, panicking if an error occurs.
    ///
    /// See [`Self::add`] for additional details.
    ///
    /// # Panics
    ///
    /// Panics when [`Self::add`] returns an error.
    pub fn add_required(&mut self, name: &str) -> Symbol {
        self.add(name).expect("failed to add symbol")
    }

    /// Returns the corresponding symbol for a given name, if one exists.
    pub fn get(&self, name: &str) -> Option<Symbol> {
        self.table.get(name).map(Symbol::new)
    }

    /// Removes the given symbol from the table.
    pub fn remove(&mut self, symbol: Symbol) {
        // Only remove the symbol if there are no other active references to it
        // There will be one reference in the symbol table, and another in the symbol being passed in
        if Rc::strong_count(&symbol.name) > 2 {
            return;
        }

        let Some(in_table) = self.table.get(symbol.name.as_ref()) else {
            return;
        };

        // Don't remove if the symbol did not come from this table
        if Rc::ptr_eq(&symbol.name, &in_table) {
            self.table.remove(symbol.name);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_required() {
        let mut table = SymbolTable::new();

        let sym = table.add_required("asdf");
        let sym2 = table.get("asdf").expect("The symbol should be added");
        assert_eq!(sym, sym2);
    }

    #[test]
    #[should_panic = "symbol '00000000000000000000000000000000000000000000000000...' exceeds length safety limit"]
    fn add_required_fail() {
        let mut table = SymbolTable::new();

        let length = Symbol::MAX_LENGTH + 1;
        let mut sym = String::with_capacity(length);
        for _i in 0..length {
            sym += "0";
        }

        table.add_required(&sym);
    }
}
