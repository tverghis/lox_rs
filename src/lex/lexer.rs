use super::{LexerError, LexerErrors, Span, Token, TokenKind};

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a [u8],
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        Lexer { source }
    }

    pub fn lex(&self) -> (Vec<Token>, LexerErrors) {
        let mut tokens = vec![];
        let mut errors = LexerErrors::new();

        let mut line = 1;
        let mut index = 0;

        while index < self.source.len() {
            let c = self.source[index];

            if c == b'\n' {
                line += 1;
            } else if c == b'(' {
                tokens.push(Token::new(
                    Span::new(line, index, index + 1),
                    TokenKind::LParen,
                ));
            } else if c == b')' {
                tokens.push(Token::new(
                    Span::new(line, index, index + 1),
                    TokenKind::RParen,
                ));
            } else if c == b'{' {
                tokens.push(Token::new(
                    Span::new(line, index, index + 1),
                    TokenKind::LBrace,
                ));
            } else if c == b'}' {
                tokens.push(Token::new(
                    Span::new(line, index, index + 1),
                    TokenKind::RBrace,
                ));
            } else if c == b',' {
                tokens.push(Token::new(
                    Span::new(line, index, index + 1),
                    TokenKind::Comma,
                ));
            } else if c == b'.' {
                tokens.push(Token::new(
                    Span::new(line, index, index + 1),
                    TokenKind::Dot,
                ));
            } else if c == b'-' {
                tokens.push(Token::new(
                    Span::new(line, index, index + 1),
                    TokenKind::Minus,
                ));
            } else if c == b'+' {
                tokens.push(Token::new(
                    Span::new(line, index, index + 1),
                    TokenKind::Plus,
                ));
            } else if c == b';' {
                tokens.push(Token::new(
                    Span::new(line, index, index + 1),
                    TokenKind::Semicolon,
                ));
            } else if c == b'*' {
                tokens.push(Token::new(
                    Span::new(line, index, index + 1),
                    TokenKind::Asterisk,
                ));
            } else if c == b'!' {
                let start = index;
                let next = start + 1;

                if (next < self.source.len()) && (self.source[next] == b'=') {
                    index += 1;

                    tokens.push(Token::new(
                        Span::new(line, start, next + 1),
                        TokenKind::ExclamationEqual,
                    ));
                } else {
                    tokens.push(Token::new(
                        Span::new(line, index, index + 1),
                        TokenKind::Exclamation,
                    ));
                }
            } else if c == b'=' {
                let start = index;
                let next = start + 1;

                if (next < self.source.len()) && (self.source[next] == b'=') {
                    index += 1;

                    tokens.push(Token::new(
                        Span::new(line, start, next + 1),
                        TokenKind::EqualEqual,
                    ));
                } else {
                    tokens.push(Token::new(
                        Span::new(line, index, index + 1),
                        TokenKind::Equal,
                    ));
                }
            } else if c == b'<' {
                let start = index;
                let next = start + 1;

                if (next < self.source.len()) && (self.source[next] == b'=') {
                    index += 1;

                    tokens.push(Token::new(
                        Span::new(line, start, next + 1),
                        TokenKind::LessThanEqual,
                    ));
                } else {
                    tokens.push(Token::new(
                        Span::new(line, index, index + 1),
                        TokenKind::LessThan,
                    ));
                }
            } else if c == b'>' {
                let start = index;
                let next = start + 1;

                if (next < self.source.len()) && (self.source[next] == b'=') {
                    index += 1;

                    tokens.push(Token::new(
                        Span::new(line, start, next + 1),
                        TokenKind::GreaterThanEqual,
                    ));
                } else {
                    tokens.push(Token::new(
                        Span::new(line, index, index + 1),
                        TokenKind::GreaterThan,
                    ));
                }
            } else if c == b'/' {
                if (index < self.source.len() - 1) && (self.source[index + 1] == b'/') {
                    index += 1;
                    while (index < self.source.len()) && (self.source[index] != b'\n') {
                        index += 1;
                    }
                } else {
                    tokens.push(Token::new(
                        Span::new(line, index, index + 1),
                        TokenKind::Slash,
                    ));
                }
            } else if !c.is_ascii_whitespace() {
                // The token did not match anything we expected, so add it to the list of errors
                errors.push(LexerError::unrecognized_token(Span::new(
                    line,
                    index,
                    index + 1,
                )));
            }

            index += 1;
        }

        tokens.push(Token::new(
            Span::new(line, self.source.len(), self.source.len()),
            TokenKind::Eof,
        ));

        (tokens, errors)
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

        assert_eq!(tokens, vec![Token::new(Span::new(1, 0, 0), TokenKind::Eof)]);
        assert!(!errors.has_errors());
    }

    #[test]
    fn single_token_source() {
        let source = b"(";
        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(1, 0, 1), TokenKind::LParen),
                Token::new(Span::new(1, 1, 1), TokenKind::Eof)
            ],
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
            vec![
                Token::new(Span::new(1, 0, 1), TokenKind::LParen),
                Token::new(Span::new(1, 1, 2), TokenKind::RParen),
                Token::new(Span::new(1, 2, 3), TokenKind::Dot),
                Token::new(Span::new(1, 3, 4), TokenKind::Dot),
                Token::new(Span::new(1, 4, 5), TokenKind::LParen),
                Token::new(Span::new(1, 5, 6), TokenKind::RParen),
                Token::new(Span::new(1, 6, 6), TokenKind::Eof)
            ],
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
            vec![
                Token::new(Span::new(1, 0, 1), TokenKind::LBrace),
                Token::new(Span::new(2, 2, 3), TokenKind::LParen),
                Token::new(Span::new(3, 4, 5), TokenKind::RParen),
                Token::new(Span::new(3, 5, 6), TokenKind::RBrace),
                Token::new(Span::new(4, 7, 7), TokenKind::Eof)
            ],
        );
        assert!(!errors.has_errors());
    }

    #[test]
    fn only_unknown_tokens() {
        let source = b"^^";
        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(tokens, vec![Token::new(Span::new(1, 2, 2), TokenKind::Eof)],);
        assert!(errors.has_errors());
    }

    #[test]
    fn some_unknown_tokens() {
        let source = b"^()@";
        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(1, 1, 2), TokenKind::LParen),
                Token::new(Span::new(1, 2, 3), TokenKind::RParen),
                Token::new(Span::new(1, 4, 4), TokenKind::Eof)
            ],
        );
        assert!(errors.has_errors());
    }
}
