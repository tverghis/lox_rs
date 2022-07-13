use std::{fmt::Display, ops::Range};

use super::Span;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LexerErrorKind {
    UnrecognizedToken,
    UnterminatedString,
    Utf8Error,
}

impl Display for LexerErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            LexerErrorKind::UnrecognizedToken => "unrecognized token",
            LexerErrorKind::UnterminatedString => "unterminated string",
            LexerErrorKind::Utf8Error => "not valid UTF-8",
        };

        write!(f, "{}", name)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LexerError {
    span: Span,
    pub kind: LexerErrorKind,
}

impl LexerError {
    pub fn new(span: Span, kind: LexerErrorKind) -> Self {
        Self { span, kind }
    }

    pub fn unrecognized_token(span: Span) -> Self {
        Self {
            span,
            kind: LexerErrorKind::UnrecognizedToken,
        }
    }

    pub fn source_range(&self) -> Range<usize> {
        self.span.into()
    }
}
