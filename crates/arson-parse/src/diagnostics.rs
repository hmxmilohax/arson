// SPDX-License-Identifier: LGPL-3.0-or-later

#[cfg(feature = "reporting")]
use codespan_reporting::diagnostic::{Diagnostic as CodespanDiagnostic, Label};
use logos::Span;

use crate::{ArrayKind, TokenKind};

#[derive(thiserror::Error, Debug, PartialEq)]
pub struct Diagnostic {
    #[source]
    pub(crate) kind: DiagnosticKind,
    pub(crate) location: Span,
}

impl Diagnostic {
    pub(crate) fn new(kind: DiagnosticKind, location: Span) -> Self {
        Self { kind, location }
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
                .with_code("DTA0003")
                .with_message(description)
                .with_notes(vec![format!(
                    "tried to extract ({trim_range:?}) from text with length ({actual_length})"
                )])
                .with_labels(vec![Label::primary(file_id, location)]),
            DiagnosticKind::IntegerParseError(error) => CodespanDiagnostic::error()
                .with_code("DTA0004")
                .with_message(description)
                .with_notes(vec![error.to_string()])
                .with_labels(vec![Label::primary(file_id, location)]),
            DiagnosticKind::FloatParseError(error) => CodespanDiagnostic::error()
                .with_code("DTA0005")
                .with_message(description)
                .with_notes(vec![error.to_string()])
                .with_labels(vec![Label::primary(file_id, location)]),

            DiagnosticKind::BadDirective => CodespanDiagnostic::error()
                .with_code("DTA0006")
                .with_message(description)
                .with_labels(vec![Label::primary(file_id, location)]),
            DiagnosticKind::UnexpectedConditional => CodespanDiagnostic::error()
                .with_code("DTA0007")
                .with_message(description)
                .with_labels(vec![Label::primary(file_id, location)]),
            DiagnosticKind::UnmatchedConditional => CodespanDiagnostic::error()
                .with_code("DTA0008")
                .with_message(description)
                .with_notes(vec!["#else or #endif required to close the conditional block".to_owned()])
                .with_labels(vec![Label::primary(file_id, location)]),
            DiagnosticKind::UnbalancedConditional => CodespanDiagnostic::error()
                .with_code("DTA0009")
                .with_message(description)
                .with_notes(vec!["all arrays in conditionals must be self-contained".to_owned()])
                .with_labels(vec![Label::primary(file_id, location)]),

            DiagnosticKind::UnclosedBlockComment => CodespanDiagnostic::error()
                .with_code("DTA0010")
                .with_message(description)
                .with_notes(vec!["*/ required to close the comment".to_owned()])
                .with_labels(vec![Label::primary(file_id, location)]),

            DiagnosticKind::UnmatchedBrace(_) => CodespanDiagnostic::error()
                .with_code("DTA0011")
                .with_message(description)
                .with_labels(vec![Label::primary(file_id, location)]),
        }
    }
}
