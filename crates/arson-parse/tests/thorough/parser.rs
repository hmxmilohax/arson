// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_parse::{ArrayKind, Diagnostic, DiagnosticKind, IntegerValue, TokenKind};
use logos::Span;

#[test]
fn thorough_errors() {
    let text = include_str!("../test_files/thorough_errors.dta").replace("\r\n", "\n");

    fn incorrect_symbol(location: Span) -> (DiagnosticKind, Span) {
        (
            DiagnosticKind::IncorrectToken {
                expected: TokenKind::Symbol,
                actual: TokenKind::Integer,
            },
            location,
        )
    }

    #[rustfmt::skip] // spacing matches the test file
    let errors = vec![
        (3, vec![(DiagnosticKind::UnmatchedBrace(ArrayKind::Array), 0..1)]),
        (4, vec![(DiagnosticKind::UnmatchedBrace(ArrayKind::Command), 0..1)]),
        (5, vec![(DiagnosticKind::UnmatchedBrace(ArrayKind::Property), 0..1)]),

        (7, vec![(
            DiagnosticKind::IntegerParseError(
                IntegerValue::from_str_radix("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16).unwrap_err(),
            ),
            9..43,
        )]),

        (9, vec![(DiagnosticKind::BadDirective, 15..19)]),

        (12, vec![incorrect_symbol(12..13)]),
        (13, vec![incorrect_symbol(11..12)]),
        (14, vec![incorrect_symbol(13..14)]),
        (15, vec![incorrect_symbol(17..18)]),
        (16, vec![incorrect_symbol(11..12)]),
        (17, vec![(
            DiagnosticKind::IncorrectToken {
                expected: TokenKind::CommandOpen,
                actual: TokenKind::Symbol,
            },
            13..19,
        )]),

        (21, vec![incorrect_symbol(11..12)]),
        (22, vec![incorrect_symbol(12..13)]),

        (26, vec![(DiagnosticKind::UnbalancedConditional, 4..48)]),
        (28, vec![(DiagnosticKind::UnmatchedBrace(ArrayKind::Array), 4..5)]),
        (29, vec![(DiagnosticKind::UnbalancedConditional, 4..40)]),
        (31, vec![(DiagnosticKind::UnmatchedBrace(ArrayKind::Array), 4..5)]),
        (34, vec![(DiagnosticKind::UnbalancedConditional, 4..50)]),
        (36, vec![(DiagnosticKind::UnmatchedBrace(ArrayKind::Array), 4..5)]),

        (41, vec![(DiagnosticKind::UnexpectedConditional, 4..10)]),
        (42, vec![(DiagnosticKind::UnexpectedConditional, 4..9)]),

        (45, vec![(DiagnosticKind::UnmatchedConditional, 4..11)]),
        (46, vec![(DiagnosticKind::UnmatchedConditional, 19..24)]),

        (48, vec![(DiagnosticKind::UnclosedBlockComment, 0..25)]),
    ];

    // Apply proper locations
    let errors = {
        // Collect lines by index and their locations in the original text
        let line_locations = {
            let mut lines = vec![];
            let mut location = 0;
            for line in text.lines() {
                lines.push(location);
                location += line.len() + 1; // + 1 for \n
            }

            lines
        };

        let mut adjusted_errors = vec![];
        for (line_number, token_line) in errors {
            let location = line_locations[line_number - 1];
            for token in token_line {
                let new_start = location + token.1.start;
                let new_end = location + token.1.end;
                adjusted_errors.push((token.0, new_start..new_end))
            }
        }

        adjusted_errors.push((DiagnosticKind::UnexpectedEof, text.len()..text.len()));
        adjusted_errors
    };

    let expected = Vec::from_iter(errors.into_iter().map(|(k, l)| Diagnostic::new(k, l)));
    let errors = match arson_parse::parse_text(&text) {
        Ok(ast) => {
            panic!("Expected parsing errors, got success instead: {ast:?}")
        },
        Err(errors) => errors,
    };
    assert_eq!(errors, expected, "Unexpected result for '{text}'");
}
