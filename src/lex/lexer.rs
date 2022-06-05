use super::{LexerError, LexerErrors, Span, Token, TokenKind};

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
        let mut errors = LexerErrors::new(self.source);

        let mut line = 1;

        for (index, &c) in self.source.iter().enumerate() {
            let kind = TokenKind::try_from(c);

            if c == b'\n' {
                line += 1;
                continue;
            }

            if c.is_ascii_whitespace() {
                continue;
            }

            if let Ok(kind) = kind {
                tokens.push(Token::new(Span::new(line, index, index + 1), kind));
                continue;
            }

            // The token did not match anything we expected, so add it to the list of errors
            errors.push(LexerError::unrecognized_token(Span::new(
                line,
                index,
                index + 1,
            )));
        }

        tokens.push(Token::new(
            Span::new(line, self.source.len(), self.source.len()),
            TokenKind::Eof,
        ));

        (LexedTokens::with_tokens(self.source, tokens), errors)
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

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn empty_source() {
        let source = [];
        let lexer = Lexer::new(&source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            LexedTokens::with_tokens(
                &source,
                vec![Token::new(Span::new(1, 0, 0), TokenKind::Eof)]
            ),
        );
        assert!(!errors.has_errors());
    }

    #[test]
    fn single_token_source() {
        let source = [b'('];
        let lexer = Lexer::new(&source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            LexedTokens::with_tokens(
                &source,
                vec![
                    Token::new(Span::new(1, 0, 1), TokenKind::LParen),
                    Token::new(Span::new(1, 1, 1), TokenKind::Eof)
                ],
            ),
        );
        assert!(!errors.has_errors());
    }

    #[test]
    fn multiple_token_source() {
        let source = b"()..()";
        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
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
        );
        assert!(!errors.has_errors());
    }

    #[test]
    fn multi_line_source() {
        let source = b"{\n(\n)}\n";
        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
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
        );
        assert!(!errors.has_errors());
    }

    #[test]
    fn only_unknown_tokens() {
        let source = b"^^";
        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            LexedTokens::with_tokens(source, vec![Token::new(Span::new(1, 2, 2), TokenKind::Eof)],),
        );
        assert!(errors.has_errors());
    }

    #[test]
    fn some_unknown_tokens() {
        let source = b"^()@";
        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            LexedTokens::with_tokens(
                source,
                vec![
                    Token::new(Span::new(1, 1, 2), TokenKind::LParen),
                    Token::new(Span::new(1, 2, 3), TokenKind::RParen),
                    Token::new(Span::new(1, 4, 4), TokenKind::Eof)
                ],
            ),
        );
        assert!(errors.has_errors());
    }
}
