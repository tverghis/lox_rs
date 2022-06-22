use std::{cmp::Ordering, ops::Range};

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    span: Span,
    pub kind: TokenKind<'a>,
}

impl<'a> Token<'a> {
    pub fn new(span: Span, kind: TokenKind<'a>) -> Token<'a> {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    line: usize,
    start: usize,
    end: usize,
}

#[derive(Debug)]
pub struct UnmergeableSpansError;

impl Span {
    pub fn new(line: usize, start: usize, end: usize) -> Self {
        Span { line, start, end }
    }

    pub fn is_adjacent_to(&self, other: &Span) -> bool {
        if self.line != other.line {
            return false;
        }

        (other.start == self.end) || (self.start == other.end)
    }

    pub fn try_merge(&self, other: &Span) -> Result<Span, UnmergeableSpansError> {
        if !self.is_adjacent_to(other) {
            return Err(UnmergeableSpansError);
        }

        match self.start.cmp(&other.start) {
            Ordering::Less => Ok(Span::new(self.line, self.start, other.end)),
            Ordering::Greater => Ok(Span::new(self.line, other.start, self.end)),
            Ordering::Equal => Ok(Span::new(self.line, self.start, self.end.max(other.end))),
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind<'a> {
    // Single-character tokens
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Asterisk,

    // Single- or double-character tokens
    Exclamation,
    ExclamationEqual,
    Equal,
    EqualEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,

    // Literals
    Identifier(&'a str),
    QuotedString(&'a str),
    Number(f64),

    Keyword(KeywordKind),

    Comment(&'a str),

    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeywordKind {
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

#[cfg(test)]
mod span_tests {
    use super::*;

    #[test]
    fn test_adjacent_true() {
        let first_span = Span::new(1, 1, 2);
        let second_span = Span::new(1, 2, 4);

        assert_eq!(first_span.is_adjacent_to(&second_span), true);
        assert_eq!(second_span.is_adjacent_to(&first_span), true);
    }

    #[test]
    fn test_adjacent_false_same_line() {
        let first_span = Span::new(1, 1, 2);
        let second_span = Span::new(1, 5, 6);

        assert_eq!(first_span.is_adjacent_to(&second_span), false);
        assert_eq!(second_span.is_adjacent_to(&first_span), false);
    }

    #[test]
    fn test_adjacent_false_different_lines() {
        let first_span = Span::new(1, 1, 2);
        let second_span = Span::new(4, 2, 4);

        assert_eq!(first_span.is_adjacent_to(&second_span), false);
        assert_eq!(second_span.is_adjacent_to(&first_span), false);
    }

    #[test]
    fn test_merge_adjacent() -> Result<(), UnmergeableSpansError> {
        let first_span = Span::new(1, 1, 2);
        let second_span = Span::new(1, 2, 8);

        let merged_span = first_span.try_merge(&second_span)?;

        assert_eq!(merged_span.line, 1);
        assert_eq!(merged_span.start, 1);
        assert_eq!(merged_span.end, 8);

        Ok(())
    }

    #[test]
    fn test_merge_reversed() -> Result<(), UnmergeableSpansError> {
        let first_span = Span::new(1, 3, 8);
        let second_span = Span::new(1, 1, 3);

        let merged_span = first_span.try_merge(&second_span)?;

        assert_eq!(merged_span.line, 1);
        assert_eq!(merged_span.start, 1);
        assert_eq!(merged_span.end, 8);

        Ok(())
    }

    #[test]
    #[should_panic]
    fn test_merge_not_adjacent() {
        let first_span = Span::new(1, 1, 2);
        let second_span = Span::new(1, 5, 8);

        // Should be an error
        first_span.try_merge(&second_span).unwrap();
    }
}
