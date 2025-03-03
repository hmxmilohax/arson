// SPDX-License-Identifier: LGPL-3.0-or-later

use std::collections::HashMap;
use std::str::FromStr;
use std::sync::LazyLock;

use arson_parse::{BlockCommentToken, Expression, ExpressionValue, Token, TokenValue};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum CommentDirective {
    FormattingOn,
    FormattingOff,
}

/// Commands which are known to execute blocks and should not be compacted into a single line.
///
/// The value stored by each key is the number of arguments to
/// keep on the same line as the command name itself.
pub static BLOCK_COMMANDS: LazyLock<HashMap<&str, usize>> = LazyLock::new(|| {
    HashMap::from_iter([
        // Control flow
        ("if", 1),      // {if {condition} {...} ...}
        ("if_else", 1), // {if_else {condition} {...} {...}}
        ("unless", 1),  // {unless {condition} {...} ...}
        ("switch", 1),  // {switch $var (case_1 ...) (case_2 ...) ...}
        // Variable scoping
        ("do", 0),   // {do (...) {...} ...}
        ("with", 1), // {with $object {...} ...}
        // Loops
        ("foreach", 2),     // {foreach $var $array {...} ...}
        ("foreach_int", 3), // {foreach_int $var 0 5 {...} ...}
        ("while", 1),       // {while {condition} {...} ...}
        // Functions
        ("func", 1),    // {func name ($arg1 ...) {...} ...}
        ("closure", 1), // {closure ($var1 ...) ($arg1 ...) {...} ...}
        // Etc.
        ("time", 1), // {time $var1 ... {...} ...}
    ])
});

/// Non-block commands which should have arguments kept on the same line as the command name
/// when not written as a single line.
pub static COMMAND_SAME_LINE_ARGS: LazyLock<HashMap<&str, usize>> = LazyLock::new(|| {
    HashMap::from_iter([
        // Modify-assign operators
        ("+=", 1), // {+= $value {...}}
        ("-=", 1), // {-= $value {...}}
        ("*=", 1), // {*= $value {...}}
        ("/=", 1), // {/= $value {...}}
        ("%=", 1), // {%= $value {...}}
        ("&=", 1), // {&= $value {...}}
        ("|=", 1), // {|= $value {...}}
        ("^=", 1), // {^= $value {...}}
        // Modify-assign functions
        ("clamp_eq", 1), // {clamp_eq $value {...}}
        ("max_eq", 1),   // {max_eq $value {...}}
        ("min_eq", 1),   // {min_eq $value {...}}
        ("mask_eq", 1),  // {mask_eq $value {...}}
        // Variable storage
        ("set", 1),     // {set $var {...}}
        ("set_var", 1), // {set_var var {...}}
        // Object creation
        ("new", 1), // {new ClassName (...)}
    ])
});

pub fn pack_args_on_same_line(name: &str) -> Option<usize> {
    BLOCK_COMMANDS
        .get(name)
        .or_else(|| COMMAND_SAME_LINE_ARGS.get(name))
        .copied()
}

impl TryFrom<&Token<'_>> for CommentDirective {
    type Error = ();

    fn try_from(value: &Token<'_>) -> Result<Self, Self::Error> {
        match &value.value {
            TokenValue::Comment(text) => text.parse::<CommentDirective>(),
            TokenValue::BlockComment(BlockCommentToken { body, .. }) => body.text.parse::<CommentDirective>(),
            _ => Err(()),
        }
    }
}

impl TryFrom<&Expression<'_>> for CommentDirective {
    type Error = ();

    fn try_from(value: &Expression<'_>) -> Result<Self, Self::Error> {
        match &value.value {
            ExpressionValue::Comment(text) => text.parse::<CommentDirective>(),
            ExpressionValue::BlockComment(BlockCommentToken { body, .. }) => {
                body.text.parse::<CommentDirective>()
            },
            _ => Err(()),
        }
    }
}

impl FromStr for CommentDirective {
    type Err = ();

    fn from_str(comment: &str) -> Result<Self, Self::Err> {
        const PREFIX: &str = "arson-fmt";

        let comment = comment.trim_start();
        let Some(directive) = comment.strip_prefix(PREFIX) else {
            return Err(());
        };

        let directive = match directive.split_once(':') {
            Some((directive, _comment)) => directive.trim(),
            None => directive.trim(),
        };

        let result = match directive {
            "on" => Self::FormattingOn,
            "off" => Self::FormattingOff,
            _ => return Err(()),
        };

        Ok(result)
    }
}
