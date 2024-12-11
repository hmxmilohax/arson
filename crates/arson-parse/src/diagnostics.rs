// SPDX-License-Identifier: LGPL-3.0-or-later

#[cfg(feature = "reporting")]
use codespan_reporting::diagnostic::{Diagnostic as CodespanDiagnostic, Label, Severity};
use logos::Span;

use crate::{ArrayKind, TokenKind};

#[derive(thiserror::Error, Debug, PartialEq)]
pub struct Diagnostic {
    #[source]
    kind: DiagnosticKind,
    location: Span,
}

impl Diagnostic {
    pub fn new(kind: DiagnosticKind, location: Span) -> Self {
        Self { kind, location }
    }

    pub fn kind(&self) -> &DiagnosticKind {
        &self.kind
    }

    pub fn location(&self) -> Span {
        self.location.clone()
    }

    pub(crate) fn sort_cmp(&self, other: &Self) -> std::cmp::Ordering {
        let ord = std::cmp::Ord::cmp(&self.location.start, &other.location.start);
        if !matches!(ord, std::cmp::Ordering::Equal) {
            return ord;
        }
        self.kind.sort_cmp(&other.kind)
    }
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.kind, f)
    }
}

#[derive(thiserror::Error, Debug, Clone, Default, PartialEq)]
pub enum DiagnosticKind {
    #[error("unexpected end of file")]
    UnexpectedEof,
    #[default]
    #[error("invalid token")]
    InvalidToken,

    #[error("internal error: failed to trim token delimiters")]
    TrimDelimiterError { trim_range: Span, actual_length: usize },
    #[error("integer parse error")]
    IntegerParseError(#[from] std::num::ParseIntError),
    #[error("float parse error")]
    FloatParseError(#[from] std::num::ParseFloatError),

    #[error("unrecognized parser directive")]
    BadDirective,
    #[error("directive is missing an argument")]
    MissingDirectiveArgument {
        missing: TokenKind,
        description: DirectiveArgumentDescription,
    },
    #[error("incorrect argument to directive")]
    IncorrectDirectiveArgument {
        expected: TokenKind,
        expected_description: DirectiveArgumentDescription,
        actual: TokenKind,
        expecting_location: Span,
    },

    #[error("unexpected conditional directive")]
    UnexpectedConditional,
    #[error("unmatched conditional directive")]
    UnmatchedConditional,
    #[error("unbalanced conditional block")]
    UnbalancedConditional,

    #[error("unmatched {0} delimiter")]
    UnmatchedBrace(ArrayKind),

    #[error("block comment was not closed")]
    UnclosedBlockComment,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum DirectiveArgumentDescription {
    MacroName,
    FilePath,
    MacroBody,
    CommandBody,
}

impl std::fmt::Display for DirectiveArgumentDescription {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DirectiveArgumentDescription::MacroName => f.write_str("macro name"),
            DirectiveArgumentDescription::FilePath => f.write_str("file path"),
            DirectiveArgumentDescription::MacroBody => f.write_str("macro body"),
            DirectiveArgumentDescription::CommandBody => f.write_str("command body"),
        }
    }
}

#[cfg(feature = "reporting")]
impl Diagnostic {
    // Manually formatted for consistency between match arms,
    // and because rustfmt does more harm than good here for both readability and maintenance
    #[rustfmt::skip]
    pub fn to_codespan<FileId: Clone>(&self, file_id: FileId) -> CodespanDiagnostic<FileId> {
        let diagnostic = CodespanDiagnostic::error()
            .with_message(self.to_string())
            .with_labels(vec![Label::primary(file_id.clone(), self.location.clone())]);

        // There are gaps in the error codes here to ease adding additional errors in the future.
        // TODO: These gaps should be removed once things are stabilized.
        match &self.kind {
            DiagnosticKind::UnexpectedEof => diagnostic
                .with_code("DTA0000"),
            DiagnosticKind::InvalidToken => diagnostic
                .with_code("DTA0001"),

            DiagnosticKind::TrimDelimiterError { trim_range, actual_length } => {
                CodespanDiagnostic { severity: Severity::Bug, ..diagnostic }
                    .with_code("DTA0005")
                    .with_notes(vec![format!(
                        "tried to extract ({trim_range:?}) from text with length ({actual_length})"
                    )])
            },
            DiagnosticKind::IntegerParseError(error) => diagnostic
                .with_code("DTA0006")
                .with_notes(vec![error.to_string()]),
            DiagnosticKind::FloatParseError(error) => diagnostic
                .with_code("DTA0007")
                .with_notes(vec![error.to_string()]),

            DiagnosticKind::BadDirective => diagnostic
                .with_code("DTA0010"),
            DiagnosticKind::MissingDirectiveArgument { missing, description: arg_description } => diagnostic
                .with_code("DTA0011")
                .with_labels(vec![
                    Label::secondary(file_id, self.location.end..self.location.end)
                        .with_message(format!("expected {missing} ({arg_description})"))
                ]),
            DiagnosticKind::IncorrectDirectiveArgument {
                expected,
                expected_description,
                actual,
                expecting_location,
            } => diagnostic
                .with_code("DTA0012")
                .with_labels(vec![Label::secondary(file_id, expecting_location.clone())
                .with_message(format!(
                    "directive here was expecting {expected} ({expected_description}), found {actual} instead"
                ))]),

            DiagnosticKind::UnexpectedConditional => diagnostic
                .with_code("DTA0015"),
            DiagnosticKind::UnmatchedConditional => diagnostic
                .with_code("DTA0016")
                .with_notes(vec!["#else or #endif required to close the conditional block".to_owned()]),
            DiagnosticKind::UnbalancedConditional => diagnostic
                .with_code("DTA0017")
                .with_notes(vec!["all arrays in conditionals must be self-contained".to_owned()]),

            DiagnosticKind::UnmatchedBrace(kind) => diagnostic
                .with_code("DTA0020")
                .with_notes(vec![format!("'{}' required to close the {kind}", kind.delimiters().1)]),

            DiagnosticKind::UnclosedBlockComment => diagnostic
                .with_code("DTA0025")
                .with_notes(vec!["'*/' required to close the comment".to_owned()]),
        }
    }
}

impl DiagnosticKind {
    pub(crate) fn sort_cmp(&self, other: &Self) -> std::cmp::Ordering {
        fn discriminant(value: &DiagnosticKind) -> u32 {
            match value {
                DiagnosticKind::UnexpectedEof => 0,
                DiagnosticKind::InvalidToken => 1,

                DiagnosticKind::TrimDelimiterError { .. } => 5,
                DiagnosticKind::IntegerParseError(_) => 6,
                DiagnosticKind::FloatParseError(_) => 7,

                DiagnosticKind::BadDirective => 10,
                DiagnosticKind::MissingDirectiveArgument { .. } => 11,
                DiagnosticKind::IncorrectDirectiveArgument { .. } => 12,

                DiagnosticKind::UnexpectedConditional => 15,
                DiagnosticKind::UnmatchedConditional => 16,
                DiagnosticKind::UnbalancedConditional => 17,

                DiagnosticKind::UnmatchedBrace(_) => 20,

                DiagnosticKind::UnclosedBlockComment => 25,
            }
        }

        discriminant(self).cmp(&discriminant(other))
    }
}
