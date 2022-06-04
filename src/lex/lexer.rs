use super::{Span, Token, TokenKind};

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a [u8],
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        Lexer { source }
    }

    pub fn lex(&self) -> (LexedTokens<'_>, LexerErrors<'_>) {
        let mut tokens = vec![];
        let mut errors = vec![];

        let mut line = 1;

        for (index, &c) in self.source.iter().enumerate() {
            let kind = TokenKind::try_from(c);

            if c == b'\n' {
                line += 1;
                continue;
            }

            if let Ok(kind) = kind {
                tokens.push(Token::new(Span::new(line, index, index + 1), kind));
                continue;
            }

            // The token did not match anything we expected, so add it to the list of errors
            errors.push(Token::new(
                Span::new(line, index, index + 1),
                TokenKind::Unknown,
            ));
        }

        tokens.push(Token::new(
            Span::new(line, self.source.len(), self.source.len()),
            TokenKind::Eof,
        ));

        (
            LexedTokens::with_tokens(self.source, tokens),
            LexerErrors::with_errors(self.source, errors),
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct LexedTokens<'a> {
    source: &'a [u8],
    tokens: Vec<Token>,
}

impl<'a> LexedTokens<'a> {
    fn with_tokens(source: &'a [u8], tokens: Vec<Token>) -> Self {
        Self { source, tokens }
    }
}

#[derive(Debug, PartialEq)]
pub struct LexerErrors<'a> {
    source: &'a [u8],
    errors: Vec<Token>,
}

impl<'a> LexerErrors<'a> {
    fn with_errors(source: &'a [u8], errors: Vec<Token>) -> Self {
        Self { source, errors }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn empty_source() {
        let source = [];
        assert_eq!(
            Lexer::new(&source).lex(),
            (
                LexedTokens::with_tokens(
                    &source,
                    vec![Token::new(Span::new(1, 0, 0), TokenKind::Eof)]
                ),
                LexerErrors::with_errors(&source, vec![])
            )
        );
    }

    #[test]
    fn single_token_source() {
        let source = [b'('];
        assert_eq!(
            Lexer::new(&source).lex(),
            (
                LexedTokens::with_tokens(
                    &source,
                    vec![
                        Token::new(Span::new(1, 0, 1), TokenKind::LParen),
                        Token::new(Span::new(1, 1, 1), TokenKind::Eof)
                    ],
                ),
                LexerErrors::with_errors(&source, vec![])
            )
        );
    }

    #[test]
    fn multiple_token_source() {
        let source = b"()..()";
        assert_eq!(
            Lexer::new(source).lex(),
            (
                LexedTokens::with_tokens(
                    source,
                    vec![
                        Token::new(Span::new(1, 0, 1), TokenKind::LParen),
                        Token::new(Span::new(1, 1, 2), TokenKind::RParen),
                        Token::new(Span::new(1, 2, 3), TokenKind::Dot),
                        Token::new(Span::new(1, 3, 4), TokenKind::Dot),
                        Token::new(Span::new(1, 4, 5), TokenKind::LParen),
                        Token::new(Span::new(1, 5, 6), TokenKind::RParen),
                        Token::new(Span::new(1, 6, 6), TokenKind::Eof)
                    ],
                ),
                LexerErrors::with_errors(source, vec![])
            )
        );
    }

    #[test]
    fn multi_line_source() {
        let source = b"{\n(\n)}\n";
        assert_eq!(
            Lexer::new(source).lex(),
            (
                LexedTokens::with_tokens(
                    source,
                    vec![
                        Token::new(Span::new(1, 0, 1), TokenKind::LBrace),
                        Token::new(Span::new(2, 2, 3), TokenKind::LParen),
                        Token::new(Span::new(3, 4, 5), TokenKind::RParen),
                        Token::new(Span::new(3, 5, 6), TokenKind::RBrace),
                        Token::new(Span::new(4, 7, 7), TokenKind::Eof)
                    ],
                ),
                LexerErrors::with_errors(source, vec![])
            )
        );
    }

    #[test]
    fn only_unknown_tokens() {
        let source = b"^^";
        assert_eq!(
            Lexer::new(source).lex(),
            (
                LexedTokens::with_tokens(
                    source,
                    vec![Token::new(Span::new(1, 2, 2), TokenKind::Eof)],
                ),
                LexerErrors::with_errors(
                    source,
                    vec![
                        Token::new(Span::new(1, 0, 1), TokenKind::Unknown),
                        Token::new(Span::new(1, 1, 2), TokenKind::Unknown),
                    ]
                )
            )
        );
    }

    #[test]
    fn some_unknown_tokens() {
        let source = b"^()@";
        assert_eq!(
            Lexer::new(source).lex(),
            (
                LexedTokens::with_tokens(
                    source,
                    vec![
                        Token::new(Span::new(1, 1, 2), TokenKind::LParen),
                        Token::new(Span::new(1, 2, 3), TokenKind::RParen),
                        Token::new(Span::new(1, 4, 4), TokenKind::Eof)
                    ],
                ),
                LexerErrors::with_errors(
                    source,
                    vec![
                        Token::new(Span::new(1, 0, 1), TokenKind::Unknown),
                        Token::new(Span::new(1, 3, 4), TokenKind::Unknown),
                    ]
                )
            )
        );
    }
}
