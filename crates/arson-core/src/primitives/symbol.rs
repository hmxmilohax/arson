// SPDX-License-Identifier: LGPL-3.0-or-later

use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

use crate::arson_fail;

/// Maps symbols by hash to an associated value. Alias for [`HashMap<ScriptSymbol, T>`].
pub type SymbolMap<T> = HashMap<Symbol, T>;

/// A unique identifier for a scripting element, such as a type or method.
#[derive(Clone)]
pub struct Symbol {
    name: Rc<String>,
}

impl Symbol {
    #[cfg(feature = "text-loading")]
    pub const MAX_LENGTH: usize = arson_parse::MAX_SYMBOL_LENGTH;
    #[cfg(not(feature = "text-loading"))]
    pub const MAX_LENGTH: usize = 50;

    /// Returns the underlying name of this symbol.
    pub const fn name(&self) -> &Rc<String> {
        &self.name
    }

    pub fn to_name(&self) -> String {
        self.name.as_ref().clone()
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

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}'", self.name)
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // placed into variable to keep formatting nicer
        let pattern = [' ', '\x0b', '\t', '\r', '\n', '\x0c', '(', ')', '[', ']', '{', '}', '\''];
        if self.name.contains(pattern) {
            write!(f, "'{}'", self.name)
        } else if self.name.is_empty() {
            write!(f, "''")
        } else {
            self.name.fmt(f)
        }
    }
}

/// Private helper method to create a new Symbol from a string.
#[inline]
fn new_symbol(str: &str) -> Symbol {
    Symbol { name: Rc::new(str.to_owned()) }
}

pub(crate) struct SymbolTable {
    table: HashMap<String, Symbol>,
}

impl SymbolTable {
    /// Constructs a new [`SymbolTable`].
    pub fn new() -> Self {
        Self { table: HashMap::new() }
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
        if let Some(existing) = self.get(name) {
            return Ok(existing);
        }

        if name.len() > Symbol::MAX_LENGTH {
            arson_fail!(
                "symbol '{}...' exceeds length safety limit",
                name.chars().take(Symbol::MAX_LENGTH).collect::<String>()
            );
        }

        let sym = new_symbol(name);
        self.table.insert(sym.name.as_ref().clone(), sym.clone());
        Ok(sym.clone())
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
        self.table.get(name).cloned()
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
        if Rc::ptr_eq(&symbol.name, &in_table.name) {
            self.table.remove(&*symbol.name);
        }
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

        #[test]
        fn display() {
            assert_eq!(new_symbol("").to_string(), "''");

            assert_eq!(new_symbol("asdf").to_string(), "asdf");
            assert_eq!(new_symbol("jkl;").to_string(), "jkl;");

            assert_eq!(new_symbol("with space").to_string(), "'with space'");
            assert_eq!(new_symbol(" ").to_string(), "' '");

            assert_eq!(new_symbol("\x0b").to_string(), "'\x0b'");
            assert_eq!(new_symbol("\t").to_string(), "'\t'");
            assert_eq!(new_symbol("\r").to_string(), "'\r'");
            assert_eq!(new_symbol("\n").to_string(), "'\n'");
            assert_eq!(new_symbol("\x0c").to_string(), "'\x0c'");

            assert_eq!(new_symbol("(").to_string(), "'('");
            assert_eq!(new_symbol(")").to_string(), "')'");
            assert_eq!(new_symbol("[").to_string(), "'['");
            assert_eq!(new_symbol("]").to_string(), "']'");
            assert_eq!(new_symbol("{").to_string(), "'{'");
            assert_eq!(new_symbol("}").to_string(), "'}'");
            assert_eq!(new_symbol("'").to_string(), "'''");
        }
    }

    mod table {
        use super::*;

        #[test]
        fn add() {
            let mut table = SymbolTable::new();
            let symbol = table.add_required("asdf");

            assert!(table.table.contains_key("asdf"));
            assert!(table.table.contains_key(&*symbol.name));
        }

        #[test]
        fn get() {
            let mut table = SymbolTable::new();
            let symbol = table.add_required("asdf");
            let symbol2 = table.get("asdf").expect("The symbol should be added");

            assert_eq!(symbol, symbol2);
        }

        #[test]
        fn remove() {
            let mut table = SymbolTable::new();
            let sym_asdf = table.add_required("asdf");

            let sym_asdf_2 = table.get("asdf").expect("The symbol should be added");

            table.remove(sym_asdf);
            assert!(
                table.get("asdf").is_some(),
                "Symbols won't get removed from the table while instances of it exist"
            );

            table.remove(sym_asdf_2);
            assert!(
                table.get("asdf").is_none(),
                "Symbols will get removed from the table when the last instance is removed"
            );
        }
    }
}
