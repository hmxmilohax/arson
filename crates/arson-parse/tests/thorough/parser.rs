// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_parse::{
    ArrayKind,
    Diagnostic,
    DiagnosticKind,
    DirectiveArgumentDescription,
    IntegerValue,
    TokenKind,
};

#[test]
fn thorough_errors() {
    let text = include_str!("../test_files/thorough_errors.dta").replace("\r\n", "\n");

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

        (12, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::Symbol,
                expected_description: DirectiveArgumentDescription::MacroName,
                actual: TokenKind::Integer,
                expecting_location: 4..11
            },
            12..13,
        )]),
        (13, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::Symbol,
                expected_description: DirectiveArgumentDescription::MacroName,
                actual: TokenKind::Integer,
                expecting_location: 4..10
            },
            11..12,
        )]),
        (14, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::Symbol,
                expected_description: DirectiveArgumentDescription::FilePath,
                actual: TokenKind::Integer,
                expecting_location: 4..12
            },
            13..14,
        )]),
        (15, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::Symbol,
                expected_description: DirectiveArgumentDescription::FilePath,
                actual: TokenKind::Integer,
                expecting_location: 4..16
            },
            17..18,
        )]),
        (16, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::Symbol,
                expected_description: DirectiveArgumentDescription::FilePath,
                actual: TokenKind::Integer,
                expecting_location: 4..10
            },
            11..12,
        )]),

        (18, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::ArrayOpen,
                expected_description: DirectiveArgumentDescription::MacroBody,
                actual: TokenKind::Integer,
                expecting_location: 4..19,
            },
            20..21,
        )]),
        (19, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::CommandOpen,
                expected_description: DirectiveArgumentDescription::CommandBody,
                actual: TokenKind::Symbol,
                expecting_location: 4..12,
            },
            13..19,
        )]),

        (21, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::Symbol,
                expected_description: DirectiveArgumentDescription::MacroName,
                actual: TokenKind::Integer,
                expecting_location: 4..10
            },
            11..12,
        )]),
        (22, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::Symbol,
                expected_description: DirectiveArgumentDescription::MacroName,
                actual: TokenKind::Integer,
                expecting_location: 4..11
            },
            12..13,
        )]),

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
        for (line_number, error_line) in errors {
            let line_location = line_locations[line_number - 1];
            for (kind, location) in error_line {
                let new_start = line_location + location.start;
                let new_end = line_location + location.end;

                let kind = match kind {
                    DiagnosticKind::IncorrectDirectiveArgument {
                        expected,
                        expected_description,
                        actual,
                        expecting_location,
                    } => {
                        let new_start = line_location + expecting_location.start;
                        let new_end = line_location + expecting_location.end;
                        DiagnosticKind::IncorrectDirectiveArgument {
                            expected,
                            expected_description,
                            actual,
                            expecting_location: new_start..new_end,
                        }
                    },
                    _ => kind,
                };

                adjusted_errors.push((kind, new_start..new_end))
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
