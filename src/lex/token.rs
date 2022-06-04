#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    span: Span,
    kind: TokenKind,
}

impl Token {
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    line: usize,
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(line: usize, start: usize, end: usize) -> Self {
        Span { line, start, end }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
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
    Identifier(String),
    QuotedString(String),
    Number(f64),

    // Keywords
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

    Eof,

    Unknown,
}

#[derive(Debug)]
pub struct UnrecognizedTokenError;

// Try to parse a single-character token from a u8
impl TryFrom<u8> for TokenKind {
    type Error = UnrecognizedTokenError;

    fn try_from(c: u8) -> Result<Self, Self::Error> {
        if c == b'(' {
            Ok(TokenKind::LParen)
        } else if c == b')' {
            Ok(TokenKind::RParen)
        } else if c == b'{' {
            Ok(TokenKind::LBrace)
        } else if c == b'}' {
            Ok(TokenKind::RBrace)
        } else if c == b',' {
            Ok(TokenKind::Comma)
        } else if c == b'.' {
            Ok(TokenKind::Dot)
        } else if c == b'-' {
            Ok(TokenKind::Minus)
        } else if c == b'+' {
            Ok(TokenKind::Plus)
        } else if c == b';' {
            Ok(TokenKind::Semicolon)
        } else if c == b'*' {
            Ok(TokenKind::Asterisk)
        } else {
            Err(UnrecognizedTokenError)
        }
    }
}

#[cfg(test)]
mod token_kind_tests {
    use super::TokenKind;

    #[test]
    fn try_from_u8() -> Result<(), <TokenKind as TryFrom<u8>>::Error> {
        assert_eq!(TokenKind::try_from(b'(')?, TokenKind::LParen);
        assert_eq!(TokenKind::try_from(b')')?, TokenKind::RParen);
        assert_eq!(TokenKind::try_from(b'{')?, TokenKind::LBrace);
        assert_eq!(TokenKind::try_from(b'}')?, TokenKind::RBrace);
        assert_eq!(TokenKind::try_from(b',')?, TokenKind::Comma);
        assert_eq!(TokenKind::try_from(b'.')?, TokenKind::Dot);
        assert_eq!(TokenKind::try_from(b'-')?, TokenKind::Minus);
        assert_eq!(TokenKind::try_from(b'+')?, TokenKind::Plus);
        assert_eq!(TokenKind::try_from(b';')?, TokenKind::Semicolon);
        assert_eq!(TokenKind::try_from(b'*')?, TokenKind::Asterisk);

        Ok(())
    }
}
