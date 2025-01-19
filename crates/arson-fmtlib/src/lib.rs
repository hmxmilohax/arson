// SPDX-License-Identifier: LGPL-3.0-or-later

//! A formatter library for DTA files.

#![warn(missing_docs)]

use std::collections::HashMap;
use std::sync::LazyLock;

pub mod expr;
pub mod token;

/// The indentation to use when formatting.
#[derive(Debug, Clone, Copy)]
pub enum Indentation {
    /// Use tabs when formatting.
    /// The inner size value is how many characters a tab should be considered to be.
    Tabs(usize),

    /// Use spaces when formatting.
    Spaces(usize),
}

/// Options for formatting.
#[derive(Debug, Clone)]
pub struct Options {
    /// The indentation style to use.
    pub indentation: Indentation,
    /// The maximum width of arrays in the output.
    pub max_array_width: usize,
    /// The maximum width of lines in the output.
    pub max_line_width: usize,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            indentation: Indentation::Spaces(3),
            max_array_width: 60,
            max_line_width: 90,
        }
    }
}

pub(crate) static COMMAND_SAME_LINE_ARGS: LazyLock<HashMap<&str, usize>> = LazyLock::new(|| {
    HashMap::from_iter([
        ("foreach", 2),     // {foreach $var $array {...} ...}
        ("foreach_int", 3), // {foreach_int $var 0 5 {...} ...}
        ("func", 1),        // {func name ($arg1 ...) {...} ...}
        ("if", 1),          // {if {condition} {...} ...}
        ("if_else", 1),     // {if_else {condition} {...} {...}}
        ("set", 1),         // {set $var {...}}
        ("switch", 1),      // {switch $var (case_1 ...) (case_2 ...) ...}
        ("unless", 1),      // {unless {condition} {...} ...}
        ("with", 1),        // {with $object {...} ...}
        ("while", 1),       // {while {condition} {...} ...}
    ])
});
