// SPDX-License-Identifier: LGPL-3.0-or-later

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// Interned strings
thread_local! {
    static STRING_POOL: RefCell<HashMap<String, Rc<String>>> = {
        let mut pool = HashMap::new();

        // By default, the empty string and all printable ASCII characters characters
        // are interned, the latter being done for the sake of `str_elem`.

        // The empty string
        pool.insert(String::new(), Rc::new(String::new()));

        #[inline]
        fn insert_chars(pool: &mut HashMap<String, Rc<String>>, chars: impl IntoIterator<Item = char>) {
            for c in chars {
                pool.insert(c.to_string(), Rc::new(c.to_string()));
            }
        }

        // Printable ASCII characters in numerical order
        insert_chars(&mut pool, [' ', '!', '\"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/']);
        insert_chars(&mut pool, '0'..='9');
        insert_chars(&mut pool, [':', ';', '<', '=', '>', '?', '@']);
        insert_chars(&mut pool, 'A'..='Z');
        insert_chars(&mut pool, ['[', '\\', ']', '^', '_', '`']);
        insert_chars(&mut pool, 'a'..='z');
        insert_chars(&mut pool, ['{', '|', '}', '~']);

        RefCell::new(pool)
    };
}

/// Creates and returns an internalized copy of a string.
///
/// Interned strings with lengths of 10 characters or less are used to de-duplicate strings
/// when creating [`Node`] instances that contain strings. By default only
pub fn intern_string(value: &str) -> Rc<String> {
    STRING_POOL.with_borrow_mut(|pool| match pool.get(value).cloned() {
        Some(interned) => interned,
        None => {
            let interned = Rc::new(value.to_owned());
            pool.insert(value.to_owned(), interned.clone());
            interned
        },
    })
}

pub fn is_string_interned(value: &str) -> bool {
    STRING_POOL.with_borrow(|pool| pool.contains_key(value))
}

pub(super) fn get_interned_string(value: &str) -> Option<Rc<String>> {
    // Skip for long strings
    if value.len() > 10 {
        return None;
    }

    STRING_POOL.with_borrow_mut(|pool| pool.get(value).cloned())
}
