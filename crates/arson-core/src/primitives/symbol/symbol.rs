// SPDX-License-Identifier: LGPL-3.0-or-later

use std::hash::Hash;
use std::rc::Rc;

use crate::NodeString;

/// A unique identifier for a scripting element, such as a type or method.
#[derive(Clone)]
pub struct Symbol {
    pub(super) name: NodeString,
}

impl Symbol {
    #[cfg(feature = "text-loading")]
    pub const MAX_LENGTH: usize = arson_parse::MAX_SYMBOL_LENGTH;
    #[cfg(not(feature = "text-loading"))]
    pub const MAX_LENGTH: usize = 512;

    pub(super) fn new(name: NodeString) -> Self {
        Self { name }
    }

    /// Returns the underlying name of this symbol.
    pub const fn name(&self) -> &NodeString {
        &self.name
    }

    pub fn to_name(&self) -> String {
        self.name.as_ref().to_owned()
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
        let left = Rc::as_ptr(&self.name) as *const ();
        let right = Rc::as_ptr(&other.name) as *const ();
        left.cmp(&right)
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

#[cfg(test)]
mod tests {
    use super::*;

    fn new_symbol(name: &str) -> Symbol {
        Symbol { name: Rc::from(name) }
    }

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
