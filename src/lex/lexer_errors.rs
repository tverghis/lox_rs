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

#[derive(Debug, PartialEq)]
pub struct LexerErrors {
    errors: Vec<LexerError>,
}

impl LexerErrors {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self { errors: vec![] }
    }

    pub fn push(&mut self, error: LexerError) {
        // If there are no errors, then just add it to the list.
        if !self.has_errors() {
            self.errors.push(error);
            return;
        }

        let last_error = self.errors.iter_mut().last().unwrap();

        // If the new error is of a different kind, then also just add it to the list.
        if last_error.kind != error.kind {
            self.errors.push(error);
            return;
        }

        // Try to merge the new error into `last_error`'s `Span`, failing which
        // just push the new error onto the list.
        match last_error.span.try_merge(&error.span) {
            Ok(merged) => last_error.span = merged,
            Err(_) => self.errors.push(error),
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

impl IntoIterator for LexerErrors {
    type Item = LexerError;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.errors.into_iter()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_has_errors_none() {
        let errors = LexerErrors::new();

        assert_eq!(errors.has_errors(), false);
    }

    #[test]
    fn test_has_errors_with_errors() {
        let mut errors = LexerErrors::new();
        errors.push(LexerError::unrecognized_token(Span::new(1, 0, 0)));
        errors.push(LexerError::unrecognized_token(Span::new(1, 0, 0)));

        assert_eq!(errors.has_errors(), true);
    }

    #[test]
    fn test_push_adjacent_error() {
        let mut errors = LexerErrors::new();
        errors.push(LexerError::unrecognized_token(Span::new(1, 0, 1)));
        errors.push(LexerError::unrecognized_token(Span::new(1, 1, 4)));

        assert_eq!(errors.has_errors(), true);
        assert_eq!(
            errors.errors,
            vec![LexerError::unrecognized_token(Span::new(1, 0, 4))]
        )
    }

    #[test]
    fn test_push_error_with_gap() {
        let mut errors = LexerErrors::new();
        errors.push(LexerError::unrecognized_token(Span::new(1, 0, 1)));
        errors.push(LexerError::unrecognized_token(Span::new(1, 3, 5)));

        assert_eq!(errors.has_errors(), true);
        assert_eq!(
            errors.errors,
            vec![
                LexerError::unrecognized_token(Span::new(1, 0, 1)),
                LexerError::unrecognized_token(Span::new(1, 3, 5)),
            ]
        )
    }
}
