// SPDX-License-Identifier: LGPL-3.0-or-later

use std::collections::{HashMap, HashSet};
use std::str::FromStr;
use std::sync::LazyLock;

use arson_parse::{BlockCommentToken, Expression, ExpressionValue, Token, TokenValue};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum CommentDirective {
    FormattingOn,
    FormattingOff,
}

pub static BLOCK_COMMANDS: LazyLock<HashSet<&str>> = LazyLock::new(|| {
    HashSet::from_iter([
        "do",
        "foreach",
        "foreach_int",
        "func",
        "if",
        "if_else",
        "switch",
        "unless",
        "with",
        "while",
    ])
});

pub static COMMAND_SAME_LINE_ARGS: LazyLock<HashMap<&str, usize>> = LazyLock::new(|| {
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

impl TryFrom<&Token<'_>> for CommentDirective {
    type Error = ();

    fn try_from(value: &Token<'_>) -> Result<Self, Self::Error> {
        match value.value {
            TokenValue::Comment(text)
            | TokenValue::BlockComment(BlockCommentToken { body: (text, _), .. }) => {
                text.parse::<CommentDirective>()
            },
            _ => Err(()),
        }
    }
}

impl TryFrom<&Expression<'_>> for CommentDirective {
    type Error = ();

    fn try_from(value: &Expression<'_>) -> Result<Self, Self::Error> {
        match value.value {
            ExpressionValue::Comment(text)
            | ExpressionValue::BlockComment(BlockCommentToken { body: (text, _), .. }) => {
                text.parse::<CommentDirective>()
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
