// SPDX-License-Identifier: LGPL-3.0-or-later

use std::collections::HashMap;
use std::rc::Rc;

use crate::NodeString;

/// A table of interned strings.
pub(crate) struct StringTable {
    table: HashMap<String, NodeString>,
}

impl StringTable {
    /// Constructs a new [`StringTable`].
    pub fn new() -> Self {
        let mut table = HashMap::new();

        // Default interning

        // The empty string
        table.insert(String::new(), NodeString::from(""));

        #[inline]
        fn insert_chars(table: &mut HashMap<String, NodeString>, chars: impl IntoIterator<Item = char>) {
            for c in chars {
                table.insert(c.to_string(), NodeString::from(c.to_string()));
            }
        }

        // Printable ASCII characters in numerical order
        insert_chars(&mut table, [
            ' ', '!', '\"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/',
        ]);
        insert_chars(&mut table, '0'..='9');
        insert_chars(&mut table, [':', ';', '<', '=', '>', '?', '@']);
        insert_chars(&mut table, 'A'..='Z');
        insert_chars(&mut table, ['[', '\\', ']', '^', '_', '`']);
        insert_chars(&mut table, 'a'..='z');
        insert_chars(&mut table, ['{', '|', '}', '~']);

        Self { table }
    }

    /// Adds a new string to the table.
    /// If it was already added, the existing copy is returned.
    pub fn add(&mut self, str: &str) -> NodeString {
        if let Some(existing) = self.get(str) {
            return existing;
        }

        let owned_str = str.to_owned();
        let str_ref = NodeString::from(owned_str.as_ref());
        self.table.insert(owned_str, str_ref.clone());
        str_ref
    }

    /// Returns the interned version of the given string, if it exists.
    pub fn get(&self, str: &str) -> Option<NodeString> {
        self.table.get(str).cloned()
    }

    /// Removes the given string from the table.
    pub fn remove(&mut self, str: NodeString) {
        // Only remove if there are no other active references to it
        // There will be one reference in the table, and another in the string being passed in
        if Rc::strong_count(&str) > 2 {
            return;
        }

        let Some(in_table) = self.table.get(str.as_ref()) else {
            return;
        };

        // Don't remove if the string did not come from this table
        if Rc::ptr_eq(&str, in_table) {
            self.table.remove(str.as_ref());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add() {
        let mut table = StringTable::new();

        let str = table.add("asdf");
        assert!(table.table.contains_key("asdf"));
        assert!(table.table.contains_key(str.as_ref()));
    }

    #[test]
    fn get() {
        let mut table = StringTable::new();

        let str = table.add("asdf");
        let str2 = table.get("asdf").expect("The string should be added");
        assert_eq!(str, str2);
    }

    #[test]
    fn remove() {
        let mut table = StringTable::new();

        let sym_asdf = table.add("asdf");
        let sym_asdf_2 = table.get("asdf").expect("The string should be added");

        table.remove(sym_asdf);
        assert!(
            table.get("asdf").is_some(),
            "Strings won't get removed from the table while instances of it exist"
        );

        table.remove(sym_asdf_2);
        assert!(
            table.get("asdf").is_none(),
            "Strings will get removed from the table when the last instance is removed"
        );
    }
}
