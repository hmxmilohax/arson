// SPDX-License-Identifier: LGPL-3.0-or-later

use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

/// Maps symbols by hash to an associated value. Alias for [`HashMap<ScriptSymbol, T>`].
pub type SymbolMap<T> = HashMap<Symbol, T>;

/// A unique identifier for a scripting element, such as a type or method.
#[derive(Debug, Clone)]
pub struct Symbol {
    name: Rc<String>,
}

impl Symbol {
    /// Returns the underlying name of this symbol.
    pub fn name(&self) -> &String {
        &self.name
    }
}

// Comparisons are done by pointer only for efficiency, as symbols are guaranteed to
// be unique by the symbol table they are created from.

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.name, &other.name)
    }
}

impl Eq for Symbol {}

impl PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Symbol {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Rc::as_ptr(&self.name).cmp(&Rc::as_ptr(&other.name))
    }
}

impl Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.name).hash(state);
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

/// Private helper method to create a new Symbol from a string.
#[inline]
fn new_symbol(str: &str) -> Symbol {
    Symbol { name: Rc::from(String::from(str)) }
}

pub struct SymbolTable {
    table: HashMap<String, Symbol>,
    empty_sym: Symbol,
}

impl SymbolTable {
    /// Constructs a new [`SymbolTable`].
    pub fn new() -> Self {
        let mut table = HashMap::new();

        // Make empty symbol and insert it into the table
        let empty_sym = new_symbol("");
        table.insert(empty_sym.name().clone(), empty_sym.clone());

        Self { table, empty_sym }
    }

    /// Retrieves the empty symbol for this table
    pub fn empty_sym(&self) -> Symbol {
        self.empty_sym.clone()
    }

    /// Adds a symbol by name to the table.
    /// The created symbol is also returned for convenience.
    pub fn add(&mut self, name: &str) -> Symbol {
        if let Some(existing) = self.get(name) {
            return existing;
        }

        let sym = new_symbol(name);
        self.table.insert(sym.name().clone(), sym.clone());
        sym.clone()
    }

    /// Returns the corresponding symbol for a given name, if one exists.
    pub fn get(&self, name: &str) -> Option<Symbol> {
        self.table.get(name).cloned()
    }

    /// Removes the given symbol from the table.
    pub fn remove(&mut self, symbol: &Symbol) {
        self.table.remove(&*symbol.name);
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod symbol {
        use super::*;

        #[test]
        fn eq() {
            let sym1 = new_symbol("asdf");
            let sym2 = sym1.clone();

            assert_eq!(sym1, sym2)
        }

        #[test]
        fn ref_equality_only() {
            let sym1 = new_symbol("asdf");
            let sym2 = new_symbol("jkl");
            let sym3 = new_symbol("jkl");

            // None of these symbols share the same box for their name
            // and so won't ever be equal
            assert_ne!(sym1, sym2);
            assert_ne!(sym1, sym3);
            assert_ne!(sym2, sym3);
        }
    }

    mod table {
        use super::*;

        #[test]
        fn add() {
            let mut table = SymbolTable::new();
            let symbol = table.add("asdf");

            assert!(table.table.contains_key("asdf"));
            assert!(table.table.contains_key(&*symbol.name));
        }

        #[test]
        fn get() {
            let mut table = SymbolTable::new();
            let symbol = table.add("asdf");
            let symbol2 = table.get("asdf").expect("The symbol should be added");

            assert_eq!(symbol, symbol2);
        }

        #[test]
        fn remove() {
            let mut table = SymbolTable::new();
            table.add("asdf");

            let symbol = table.get("asdf").expect("The symbol should be added");
            table.remove(&symbol);
            assert!(table.get("asdf") == None);
        }
    }
}
