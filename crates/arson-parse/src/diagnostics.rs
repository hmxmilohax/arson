// SPDX-License-Identifier: LGPL-3.0-or-later

#[cfg(feature = "reporting")]
use codespan_reporting::diagnostic::{Diagnostic as CodespanDiagnostic, Label};
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
    #[error("expected token of type {expected}, found {actual} instead")]
    IncorrectToken { expected: TokenKind, actual: TokenKind },

    #[error("internal error: failed to trim token delimiters")]
    TrimDelimiterError { trim_range: Span, actual_length: usize },
    #[error("integer parse error")]
    IntegerParseError(#[from] std::num::ParseIntError),
    #[error("float parse error")]
    FloatParseError(#[from] std::num::ParseFloatError),

    #[error("unrecognized parser directive")]
    BadDirective,
    #[error("directive is missing its argument; expected {0}")]
    IncompleteDirective(TokenKind),

    #[error("unexpected conditional directive")]
    UnexpectedConditional,
    #[error("unmatched conditional directive")]
    UnmatchedConditional,
    #[error("unbalanced conditional block")]
    UnbalancedConditional,

    #[error("block comment was not closed")]
    UnclosedBlockComment,

    #[error("unmatched {0} delimiter")]
    UnmatchedBrace(ArrayKind),
}

#[cfg(feature = "reporting")]
impl Diagnostic {
    pub fn to_codespan<FileId>(&self, file_id: FileId) -> CodespanDiagnostic<FileId> {
        let location = self.location.clone();
        let description = self.to_string();

        // There are gaps in the error codes here to ease adding additional errors in the future.
        // TODO: These gaps should be removed once things are stabilized.
        match &self.kind {
            DiagnosticKind::UnexpectedEof => CodespanDiagnostic::error()
                .with_code("DTA0000")
                .with_message(description)
                .with_labels(vec![Label::primary(file_id, location)]),
            DiagnosticKind::InvalidToken => CodespanDiagnostic::error()
                .with_code("DTA0001")
                .with_message(description)
                .with_labels(vec![Label::primary(file_id, location)]),
            DiagnosticKind::IncorrectToken { .. } => CodespanDiagnostic::error()
                .with_code("DTA0002")
                .with_message(description)
                .with_labels(vec![Label::primary(file_id, location.clone())]),

            DiagnosticKind::TrimDelimiterError { trim_range, actual_length } => CodespanDiagnostic::bug()
                .with_code("DTA0005")
                .with_message(description)
                .with_notes(vec![format!(
                    "tried to extract ({trim_range:?}) from text with length ({actual_length})"
                )])
                .with_labels(vec![Label::primary(file_id, location)]),
            DiagnosticKind::IntegerParseError(error) => CodespanDiagnostic::error()
                .with_code("DTA0006")
                .with_message(description)
                .with_notes(vec![error.to_string()])
                .with_labels(vec![Label::primary(file_id, location)]),
            DiagnosticKind::FloatParseError(error) => CodespanDiagnostic::error()
                .with_code("DTA0007")
                .with_message(description)
                .with_notes(vec![error.to_string()])
                .with_labels(vec![Label::primary(file_id, location)]),

            DiagnosticKind::BadDirective => CodespanDiagnostic::error()
                .with_code("DTA0010")
                .with_message(description)
                .with_labels(vec![Label::primary(file_id, location)]),
            DiagnosticKind::IncompleteDirective(_) => CodespanDiagnostic::error()
                .with_code("DTA0011")
                .with_message(description)
                .with_labels(vec![Label::primary(file_id, location)]),

            DiagnosticKind::UnexpectedConditional => CodespanDiagnostic::error()
                .with_code("DTA0015")
                .with_message(description)
                .with_labels(vec![Label::primary(file_id, location)]),
            DiagnosticKind::UnmatchedConditional => CodespanDiagnostic::error()
                .with_code("DTA0016")
                .with_message(description)
                .with_notes(vec!["#else or #endif required to close the conditional block".to_owned()])
                .with_labels(vec![Label::primary(file_id, location)]),
            DiagnosticKind::UnbalancedConditional => CodespanDiagnostic::error()
                .with_code("DTA0017")
                .with_message(description)
                .with_notes(vec!["all arrays in conditionals must be self-contained".to_owned()])
                .with_labels(vec![Label::primary(file_id, location)]),

            DiagnosticKind::UnclosedBlockComment => CodespanDiagnostic::error()
                .with_code("DTA0020")
                .with_message(description)
                .with_notes(vec!["*/ required to close the comment".to_owned()])
                .with_labels(vec![Label::primary(file_id, location)]),

            DiagnosticKind::UnmatchedBrace(_) => CodespanDiagnostic::error()
                .with_code("DTA0025")
                .with_message(description)
                .with_labels(vec![Label::primary(file_id, location)]),
        }
    }
}

impl DiagnosticKind {
    pub(crate) fn sort_cmp(&self, other: &Self) -> std::cmp::Ordering {
        fn discriminant(value: &DiagnosticKind) -> u32 {
            match value {
                DiagnosticKind::UnexpectedEof => 0,
                DiagnosticKind::InvalidToken => 1,
                DiagnosticKind::IncorrectToken { .. } => 2,

                DiagnosticKind::TrimDelimiterError { .. } => 5,
                DiagnosticKind::IntegerParseError(_) => 6,
                DiagnosticKind::FloatParseError(_) => 7,

                DiagnosticKind::BadDirective => 10,
                DiagnosticKind::IncompleteDirective(_) => 11,

                DiagnosticKind::UnexpectedConditional => 15,
                DiagnosticKind::UnmatchedConditional => 16,
                DiagnosticKind::UnbalancedConditional => 17,

                DiagnosticKind::UnclosedBlockComment => 20,

                DiagnosticKind::UnmatchedBrace(_) => 25,
            }
        }

        discriminant(self).cmp(&discriminant(other))
    }
}
