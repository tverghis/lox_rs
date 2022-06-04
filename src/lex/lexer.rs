use super::{Span, Token, TokenKind};

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a [u8],
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        Lexer { source }
    }

    pub fn lex(&self) -> LexedTokens<'_> {
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

        LexedTokens {
            source: self.source,
            tokens,
            errors,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LexedTokens<'a> {
    source: &'a [u8],
    tokens: Vec<Token>,
    errors: Vec<Token>,
}

impl<'a> LexedTokens<'a> {
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
            LexedTokens {
                source: &source,
                tokens: vec![Token::new(Span::new(1, 0, 0), TokenKind::Eof)],
                errors: vec![]
            }
        );
    }

    #[test]
    fn single_token_source() {
        let source = [b'('];
        assert_eq!(
            Lexer::new(&source).lex(),
            LexedTokens {
                source: &source,
                tokens: vec![
                    Token::new(Span::new(1, 0, 1), TokenKind::LParen),
                    Token::new(Span::new(1, 1, 1), TokenKind::Eof)
                ],
                errors: vec![]
            }
        );
    }

    #[test]
    fn multiple_token_source() {
        let source = b"()..()";
        assert_eq!(
            Lexer::new(source).lex(),
            LexedTokens {
                source,
                tokens: vec![
                    Token::new(Span::new(1, 0, 1), TokenKind::LParen),
                    Token::new(Span::new(1, 1, 2), TokenKind::RParen),
                    Token::new(Span::new(1, 2, 3), TokenKind::Dot),
                    Token::new(Span::new(1, 3, 4), TokenKind::Dot),
                    Token::new(Span::new(1, 4, 5), TokenKind::LParen),
                    Token::new(Span::new(1, 5, 6), TokenKind::RParen),
                    Token::new(Span::new(1, 6, 6), TokenKind::Eof)
                ],
                errors: vec![]
            }
        );
    }

    #[test]
    fn multi_line_source() {
        let source = b"{\n(\n)}\n";
        assert_eq!(
            Lexer::new(source).lex(),
            LexedTokens {
                source,
                tokens: vec![
                    Token::new(Span::new(1, 0, 1), TokenKind::LBrace),
                    Token::new(Span::new(2, 2, 3), TokenKind::LParen),
                    Token::new(Span::new(3, 4, 5), TokenKind::RParen),
                    Token::new(Span::new(3, 5, 6), TokenKind::RBrace),
                    Token::new(Span::new(4, 7, 7), TokenKind::Eof)
                ],
                errors: vec![]
            }
        );
    }

    #[test]
    fn only_unknown_tokens() {
        let source = b"^^";
        assert_eq!(
            Lexer::new(source).lex(),
            LexedTokens {
                source,
                tokens: vec![Token::new(Span::new(1, 2, 2), TokenKind::Eof)],
                errors: vec![
                    Token::new(Span::new(1, 0, 1), TokenKind::Unknown),
                    Token::new(Span::new(1, 1, 2), TokenKind::Unknown),
                ]
            }
        );
    }

    #[test]
    fn some_unknown_tokens() {
        let source = b"^()@";
        assert_eq!(
            Lexer::new(source).lex(),
            LexedTokens {
                source,
                tokens: vec![
                    Token::new(Span::new(1, 1, 2), TokenKind::LParen),
                    Token::new(Span::new(1, 2, 3), TokenKind::RParen),
                    Token::new(Span::new(1, 4, 4), TokenKind::Eof)
                ],
                errors: vec![
                    Token::new(Span::new(1, 0, 1), TokenKind::Unknown),
                    Token::new(Span::new(1, 3, 4), TokenKind::Unknown)
                ]
            }
        );
    }
}

#[cfg(test)]
mod tokens_tests {
    use super::*;

    #[test]
    fn has_errors() {
        assert_eq!(
            LexedTokens {
                source: &[],
                tokens: vec![],
                errors: vec![]
            }
            .has_errors(),
            false
        );

        assert_eq!(
            LexedTokens {
                source: &[],
                tokens: vec![],
                errors: vec![Token::new(Span::new(1, 0, 0), TokenKind::Unknown),]
            }
            .has_errors(),
            true
        );
    }
}
