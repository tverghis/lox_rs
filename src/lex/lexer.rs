use std::{fmt::Debug, ops::Range};

use super::{KeywordKind, LexerError, LexerErrorKind, Span, Token, TokenKind};

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a [u8],
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        Lexer { source }
    }
}

impl<'a> IntoIterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexerError>;

    type IntoIter = LexerIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        LexerIter::new(self.source)
    }
}

pub struct LexerIter<'a> {
    source: &'a [u8],
    index: usize,
    line: usize,
}

impl<'a> Debug for LexerIter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_at_end() {
            return Ok(());
        }

        write!(
            f,
            "Char: {:?}, Line: {}, Index: {}",
            char::from(self.source[self.index]),
            self.line,
            self.index
        )
    }
}

impl<'a> LexerIter<'a> {
    fn new(source: &'a [u8]) -> Self {
        Self {
            source,
            index: 0,
            line: 0,
        }
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn advance_line(&mut self) {
        self.line += 1;
    }

    fn retreat(&mut self) {
        self.index -= 1;
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.source.len()
    }

    fn peek(&self) -> Option<u8> {
        if self.is_at_end() {
            return None;
        }

        Some(self.source[self.index])
    }

    fn peek_next(&self) -> Option<u8> {
        if self.index >= self.source.len() - 1 {
            return None;
        }

        Some(self.source[self.index + 1])
    }

    fn take_single_char_token(&self) -> Option<Result<Token<'a>, LexerError>> {
        macro_rules! match_single_char_token {
            ($($char:literal => $tk:expr),*) => {
                match self.peek() {
                    $(
                        Some($char) => Some(Token::new(Span::new(self.line, self.index, self.index + 1), $tk)),
                    )*
                    _ => None,
                }
            };
        }

        match_single_char_token!(
            b'(' => TokenKind::LParen,
            b')' => TokenKind::RParen,
            b'{' => TokenKind::LBrace,
            b'}' => TokenKind::RBrace,
            b',' => TokenKind::Comma,
            b'.' => TokenKind::Dot,
            b'-' => TokenKind::Minus,
            b'+' => TokenKind::Plus,
            b';' => TokenKind::Semicolon,
            b'*' => TokenKind::Asterisk
        )
        .map(|t| Ok(t))
    }

    fn take_two_char_token(&mut self) -> Option<Result<Token<'a>, LexerError>> {
        macro_rules! match_two_char_token {
            ($($c1:literal, $c2:literal => $t_comb:expr, $t_single:expr),*) => {
                match self.peek() {
                    $(
                        Some($c1) => {
                            let start = self.index;
                            let next = start + 1;

                            let token = if (next < self.source.len()) && (self.source[next] == $c2) {
                                self.advance();
                                Token::new(Span::new(self.line, start, next + 1), $t_comb)
                            } else {
                                Token::new(Span::new(self.line, self.index, self.index + 1), $t_single)
                            };

                            Some(token)
                        }
                    )*
                    _ => None,
                }
            };

        }

        match_two_char_token!(
            b'!', b'=' => TokenKind::ExclamationEqual, TokenKind::Exclamation,
            b'=', b'=' => TokenKind::EqualEqual, TokenKind::Equal,
            b'<', b'=' => TokenKind::LessThanEqual, TokenKind::LessThan,
            b'>', b'=' => TokenKind::GreaterThanEqual, TokenKind::GreaterThan
        )
        .map(|t| Ok(t))
    }

    fn take_string_literal(&mut self) -> Option<Result<Token<'a>, LexerError>> {
        if self.peek() != Some(b'"') {
            return None;
        }

        self.advance();
        let start_line = self.line;
        let start = self.index;

        while !self.is_at_end() && self.peek() != Some(b'"') {
            if self.peek() == Some(b'\n') {
                self.advance_line();
            }

            self.advance();
        }

        let res = if self.index == self.source.len() {
            // If we reached the end, the string was unterminated.
            Err(LexerError::new(
                Span::new(self.line, start - 1, self.index), // start the span at the opening `"`
                LexerErrorKind::UnterminatedString,
            ))
        } else {
            // Otherwise, we found a closing `"`.
            let span = Span::new(start_line, start, self.index);

            parse_str_or_utf8_error(self.source, span)
                .map(|s| Token::new(span, TokenKind::QuotedString(s)))
        };

        Some(res)
    }

    fn take_number_literal(&mut self) -> Option<Result<Token<'a>, LexerError>> {
        if !matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
            return None;
        }

        let start = self.index;

        while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
            self.advance();
        }

        if self.peek() == Some(b'.') && matches!(self.peek_next(), Some(c) if c.is_ascii_digit()) {
            self.advance(); // consume the decimal

            while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
                self.advance();
            }
        }

        let span = Span::new(self.line, start, self.index);

        let number = parse_str_or_utf8_error(self.source, span)
            .map(|s| {
                Token::new(
                    span,
                    TokenKind::Number(s.parse().unwrap()), // unwrap is ok -- string is all ASCII digits
                )
            })
            .unwrap(); // unwrap is ok - always valid UTF-8 since all bytes are ASCII digits

        self.retreat();

        Some(Ok(number))
    }

    fn take_identifier_or_keyword(&mut self) -> Option<Result<Token<'a>, LexerError>> {
        if !matches!(self.peek(), Some(c) if c.is_ascii_alphabetic()) {
            return None;
        }

        let start = self.index;

        while matches!(self.peek(), Some(c) if c.is_ascii_alphanumeric() || c == b'_') {
            self.advance();
        }

        let span = Span::new(self.line, start, self.index);
        let ident = parse_str_or_utf8_error(self.source, span).unwrap();

        let token = match ident {
            "and" => Token::new(span, TokenKind::Keyword(KeywordKind::And)),
            "class" => Token::new(span, TokenKind::Keyword(KeywordKind::Class)),
            "else" => Token::new(span, TokenKind::Keyword(KeywordKind::Else)),
            "false" => Token::new(span, TokenKind::Keyword(KeywordKind::False)),
            "for" => Token::new(span, TokenKind::Keyword(KeywordKind::For)),
            "fun" => Token::new(span, TokenKind::Keyword(KeywordKind::Fun)),
            "if" => Token::new(span, TokenKind::Keyword(KeywordKind::If)),
            "nil" => Token::new(span, TokenKind::Keyword(KeywordKind::Nil)),
            "or" => Token::new(span, TokenKind::Keyword(KeywordKind::Or)),
            "print" => Token::new(span, TokenKind::Keyword(KeywordKind::Print)),
            "return" => Token::new(span, TokenKind::Keyword(KeywordKind::Return)),
            "super" => Token::new(span, TokenKind::Keyword(KeywordKind::Super)),
            "this" => Token::new(span, TokenKind::Keyword(KeywordKind::This)),
            "true" => Token::new(span, TokenKind::Keyword(KeywordKind::True)),
            "var" => Token::new(span, TokenKind::Keyword(KeywordKind::Var)),
            "while" => Token::new(span, TokenKind::Keyword(KeywordKind::While)),
            _ => Token::new(span, TokenKind::Identifier(ident)),
        };

        self.retreat();

        Some(Ok(token))
    }

    fn take_slash_or_consume_comment(&mut self) -> Option<Result<Token<'a>, LexerError>> {
        if self.peek() != Some(b'/') {
            return None;
        }

        if self.peek_next() != Some(b'/') {
            return Some(Ok(Token::new(
                Span::new(self.line, self.index, self.index + 1),
                TokenKind::Slash,
            )));
        }

        let start = self.index;

        while self.peek() != Some(b'\n') && !self.is_at_end() {
            self.advance();
        }

        let span = Span::new(self.line, start, self.index);

        let comment = parse_str_or_utf8_error(self.source, span)
            .map(|s| Token::new(span, TokenKind::Comment(s)));

        if self.peek() == Some(b'\n') {
            self.consume_whitespace();
            self.retreat();
        }

        Some(comment)
    }

    fn consume_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_ascii_whitespace() {
                if c == b'\n' {
                    self.advance_line();
                }
                self.advance();
            } else {
                break;
            }
        }
    }
}

impl<'a> Iterator for LexerIter<'a> {
    type Item = Result<Token<'a>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.consume_whitespace();

        if self.is_at_end() {
            return None;
        }

        let item = self
            .take_single_char_token()
            .or_else(|| self.take_two_char_token())
            .or_else(|| self.take_string_literal())
            .or_else(|| self.take_number_literal())
            .or_else(|| self.take_identifier_or_keyword())
            .or_else(|| self.take_slash_or_consume_comment())
            .or_else(|| {
                Some(Err(LexerError::new(
                    Span::new(self.line, self.index, self.index + 1),
                    LexerErrorKind::UnrecognizedToken,
                )))
            });

        self.advance();

        item
    }
}

fn parse_str_or_utf8_error(source: &[u8], span: Span) -> Result<&str, LexerError> {
    std::str::from_utf8(&source[Range::from(span)])
        .map_err(|_| LexerError::new(span, LexerErrorKind::Utf8Error))
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn empty_source() {
        let source = [];
        let tokens: Vec<_> = Lexer::new(&source).into_iter().collect();

        assert_eq!(tokens, vec![]);
    }

    #[test]
    fn single_token_source() {
        let source = b"(";
        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens,
            vec![Token::new(Span::new(0, 0, 1), TokenKind::LParen),],
        );
    }

    #[test]
    fn multiple_token_source() {
        let source = b"()..()";
        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(0, 0, 1), TokenKind::LParen),
                Token::new(Span::new(0, 1, 2), TokenKind::RParen),
                Token::new(Span::new(0, 2, 3), TokenKind::Dot),
                Token::new(Span::new(0, 3, 4), TokenKind::Dot),
                Token::new(Span::new(0, 4, 5), TokenKind::LParen),
                Token::new(Span::new(0, 5, 6), TokenKind::RParen),
            ],
        );
    }

    #[test]
    fn multi_line_source() {
        let source = b"{\n(\n)}\n";
        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(0, 0, 1), TokenKind::LBrace),
                Token::new(Span::new(1, 2, 3), TokenKind::LParen),
                Token::new(Span::new(2, 4, 5), TokenKind::RParen),
                Token::new(Span::new(2, 5, 6), TokenKind::RBrace),
            ],
        );
    }

    #[test]
    fn only_unknown_tokens() {
        let source = b"^^";
        let tokens: Vec<_> = Lexer::new(source).into_iter().collect();

        assert_eq!(
            tokens,
            vec![
                Err(LexerError::new(
                    Span::new(0, 0, 1),
                    LexerErrorKind::UnrecognizedToken
                )),
                Err(LexerError::new(
                    Span::new(0, 1, 2),
                    LexerErrorKind::UnrecognizedToken
                )),
            ]
        );
    }

    #[test]
    fn some_unknown_tokens() {
        let source = b"^()@";
        let tokens: Vec<_> = Lexer::new(source).into_iter().collect();

        assert_eq!(
            tokens,
            vec![
                Err(LexerError::new(
                    Span::new(0, 0, 1),
                    LexerErrorKind::UnrecognizedToken
                )),
                Ok(Token::new(Span::new(0, 1, 2), TokenKind::LParen)),
                Ok(Token::new(Span::new(0, 2, 3), TokenKind::RParen)),
                Err(LexerError::new(
                    Span::new(0, 3, 4),
                    LexerErrorKind::UnrecognizedToken
                )),
            ],
        );
    }

    #[test]
    fn operators_grouping_comments() {
        let source = r#"// this is a comment
(( )) {} // grouping stuff
!*+-/=<> <= == // operators
"#
        .as_bytes();

        dbg!(source);

        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        let expected_tokens = vec![
            Token::new(
                Span::new(0, 0, 20),
                TokenKind::Comment("// this is a comment"),
            ),
            Token::new(Span::new(1, 21, 22), TokenKind::LParen),
            Token::new(Span::new(1, 22, 23), TokenKind::LParen),
            Token::new(Span::new(1, 24, 25), TokenKind::RParen),
            Token::new(Span::new(1, 25, 26), TokenKind::RParen),
            Token::new(Span::new(1, 27, 28), TokenKind::LBrace),
            Token::new(Span::new(1, 28, 29), TokenKind::RBrace),
            Token::new(
                Span::new(1, 30, 47),
                TokenKind::Comment("// grouping stuff"),
            ),
            Token::new(Span::new(2, 48, 49), TokenKind::Exclamation),
            Token::new(Span::new(2, 49, 50), TokenKind::Asterisk),
            Token::new(Span::new(2, 50, 51), TokenKind::Plus),
            Token::new(Span::new(2, 51, 52), TokenKind::Minus),
            Token::new(Span::new(2, 52, 53), TokenKind::Slash),
            Token::new(Span::new(2, 53, 54), TokenKind::Equal),
            Token::new(Span::new(2, 54, 55), TokenKind::LessThan),
            Token::new(Span::new(2, 55, 56), TokenKind::GreaterThan),
            Token::new(Span::new(2, 57, 59), TokenKind::LessThanEqual),
            Token::new(Span::new(2, 60, 62), TokenKind::EqualEqual),
            Token::new(Span::new(2, 63, 75), TokenKind::Comment("// operators")),
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn emtpy_string() {
        let source = r#""""#.as_bytes();

        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens,
            vec![Token::new(
                Span::new(0, 1, 1),
                TokenKind::QuotedString("".into())
            ),]
        );
    }

    #[test]
    fn simple_string() {
        let source = r#""hello world""#.as_bytes();

        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens,
            vec![Token::new(
                Span::new(0, 1, 12),
                TokenKind::QuotedString("hello world")
            ),]
        );
    }

    #[test]
    fn multi_line_string() {
        let source = r#""hello
world" >= // comment"#
            .as_bytes();

        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(0, 1, 12), TokenKind::QuotedString("hello\nworld")),
                Token::new(Span::new(1, 14, 16), TokenKind::GreaterThanEqual),
                Token::new(Span::new(1, 17, 27), TokenKind::Comment("// comment")),
            ]
        );
    }

    #[test]
    fn unterminated_string() {
        let source = r#"<= "hello //cmt"#.as_bytes();

        let tokens: Vec<_> = Lexer::new(source).into_iter().collect();

        assert_eq!(
            tokens,
            vec![
                Ok(Token::new(Span::new(0, 0, 2), TokenKind::LessThanEqual)),
                Err(LexerError::new(
                    Span::new(0, 3, 15),
                    LexerErrorKind::UnterminatedString
                ))
            ]
        );
    }

    #[test]
    fn invalid_utf8_string() {
        // "hello" "helxo" where x = invalid utf-8
        let source = [
            34, 104, 101, 108, 108, 111, 34, 32, 34, 104, 101, 0xFF, 108, 111, 34,
        ];

        let tokens: Vec<_> = Lexer::new(&source).into_iter().collect();

        assert_eq!(
            tokens,
            vec![
                Ok(Token::new(
                    Span::new(0, 1, 6),
                    TokenKind::QuotedString("hello")
                )),
                Err(LexerError::new(
                    Span::new(0, 9, 14),
                    LexerErrorKind::Utf8Error
                ))
            ]
        );
    }

    #[test]
    fn simple_number() {
        let source = "1234567890".as_bytes();

        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens,
            vec![Token::new(
                Span::new(0, 0, 10),
                TokenKind::Number(1234567890_f64)
            ),]
        );
    }

    #[test]
    fn floating_point_number() {
        let source = "12340.56789".as_bytes();

        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens,
            vec![Token::new(
                Span::new(0, 0, 11),
                TokenKind::Number(12340.56789)
            ),]
        );
    }

    #[test]
    fn integer_dot_other() {
        let source = "12340.hello".as_bytes();

        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(0, 0, 5), TokenKind::Number(12340_f64)),
                Token::new(Span::new(0, 5, 6), TokenKind::Dot),
                Token::new(Span::new(0, 6, 11), TokenKind::Identifier("hello")),
            ]
        );
    }

    #[test]
    fn float_dot_other() {
        let source = "123.40.hello".as_bytes();

        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(0, 0, 6), TokenKind::Number(123.4)),
                Token::new(Span::new(0, 6, 7), TokenKind::Dot),
                Token::new(Span::new(0, 7, 12), TokenKind::Identifier("hello")),
            ]
        );
    }

    #[test]
    fn integer_dot_nothing() {
        let source = "123.".as_bytes();

        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(0, 0, 3), TokenKind::Number(123_f64)),
                Token::new(Span::new(0, 3, 4), TokenKind::Dot),
            ]
        );
    }

    #[test]
    fn number_before_and_after() {
        let source = r#""123"456({789
})"#
        .as_bytes();

        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(0, 1, 4), TokenKind::QuotedString("123")),
                Token::new(Span::new(0, 5, 8), TokenKind::Number(456_f64)),
                Token::new(Span::new(0, 8, 9), TokenKind::LParen),
                Token::new(Span::new(0, 9, 10), TokenKind::LBrace),
                Token::new(Span::new(0, 10, 13), TokenKind::Number(789_f64)),
                Token::new(Span::new(1, 14, 15), TokenKind::RBrace),
                Token::new(Span::new(1, 15, 16), TokenKind::RParen),
            ]
        );
    }

    #[test]
    fn identifer_alpha_only() {
        let source = "hello".as_bytes();

        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens,
            vec![Token::new(
                Span::new(0, 0, 5),
                TokenKind::Identifier("hello")
            )]
        );
    }

    #[test]
    fn identifier_complex() {
        let source = "hello123_oops_456bye".as_bytes();

        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens,
            vec![Token::new(
                Span::new(0, 0, 20),
                TokenKind::Identifier("hello123_oops_456bye")
            )]
        );
    }

    #[test]
    fn identifier_after() {
        let source = "hello.foo123()".as_bytes();

        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(0, 0, 5), TokenKind::Identifier("hello")),
                Token::new(Span::new(0, 5, 6), TokenKind::Dot),
                Token::new(Span::new(0, 6, 12), TokenKind::Identifier("foo123")),
                Token::new(Span::new(0, 12, 13), TokenKind::LParen),
                Token::new(Span::new(0, 13, 14), TokenKind::RParen),
            ]
        );
    }

    #[test]
    fn identifer_starting_with_keyword() {
        let source = "orchid".as_bytes();

        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens,
            vec![Token::new(
                Span::new(0, 0, 6),
                TokenKind::Identifier("orchid")
            ),]
        );
    }

    #[test]
    fn keywords() {
        let source =
            "and class else false for fun if nil or print return super this true var while"
                .as_bytes();

        let tokens: Vec<_> = Lexer::new(source).into_iter().flatten().collect();

        assert_eq!(
            tokens.into_iter().map(|t| t.kind).collect::<Vec<_>>(),
            vec![
                TokenKind::Keyword(KeywordKind::And),
                TokenKind::Keyword(KeywordKind::Class),
                TokenKind::Keyword(KeywordKind::Else),
                TokenKind::Keyword(KeywordKind::False),
                TokenKind::Keyword(KeywordKind::For),
                TokenKind::Keyword(KeywordKind::Fun),
                TokenKind::Keyword(KeywordKind::If),
                TokenKind::Keyword(KeywordKind::Nil),
                TokenKind::Keyword(KeywordKind::Or),
                TokenKind::Keyword(KeywordKind::Print),
                TokenKind::Keyword(KeywordKind::Return),
                TokenKind::Keyword(KeywordKind::Super),
                TokenKind::Keyword(KeywordKind::This),
                TokenKind::Keyword(KeywordKind::True),
                TokenKind::Keyword(KeywordKind::Var),
                TokenKind::Keyword(KeywordKind::While),
            ]
        );
    }

    #[test]
    fn parse_str_ok() {
        let source = "hello".as_bytes();
        let span = Span::new(0, 0, 5);

        assert_eq!(parse_str_or_utf8_error(source, span), Ok("hello"));
    }

    #[test]
    fn parse_str_utf8_error() {
        let source = [104, 101, 0xFF, 108, 111];
        let span = Span::new(0, 0, 5);

        assert_eq!(
            parse_str_or_utf8_error(&source, span),
            Err(LexerError::new(span, LexerErrorKind::Utf8Error))
        );
    }
}
