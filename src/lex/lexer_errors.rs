use super::Span;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LexerErrorKind {
    UnrecognizedToken,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LexerError {
    span: Span,
    kind: LexerErrorKind,
}

impl LexerError {
    pub fn unrecognized_token(span: Span) -> Self {
        Self {
            span,
            kind: LexerErrorKind::UnrecognizedToken,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LexerErrors<'a> {
    source: &'a [u8],
    errors: Vec<LexerError>,
}

impl<'a> LexerErrors<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        Self {
            source,
            errors: vec![],
        }
    }

    pub fn push(&mut self, error: LexerError) {
        self.errors.push(error);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_has_errors_none() {
        let errors = LexerErrors::new(&[]);

        assert_eq!(errors.has_errors(), false);
    }

    #[test]
    fn test_has_errors_with_errors() {
        let mut errors = LexerErrors::new(&[]);
        errors.push(LexerError::unrecognized_token(Span::new(1, 0, 0)));
        errors.push(LexerError::unrecognized_token(Span::new(1, 0, 0)));

        assert_eq!(errors.has_errors(), true);
    }
}
