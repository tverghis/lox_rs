use super::{LexerError, LexerErrorKind, LexerErrors, Span, Token, TokenKind};

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

            if c == b'(' {
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
            } else if c == b'"' {
                index += 1;
                let start_line = line;
                let start = index;

                while (index < self.source.len()) && (self.source[index] != b'"') {
                    if self.source[index] == b'\n' {
                        line += 1;
                    }

                    index += 1;
                }

                if index == self.source.len() {
                    // If we reached the end, the string was unterminated.
                    errors.push(LexerError::new(
                        Span::new(line, start - 1, index), // start the span at the opening `"`
                        LexerErrorKind::UnterminatedString,
                    ));
                } else {
                    // Otherwise, we found a closing `"`.
                    let span = Span::new(start_line, start, index);

                    match std::str::from_utf8(&self.source[start..index]) {
                        Ok(s) => tokens.push(Token::new(span, TokenKind::QuotedString(s))),
                        Err(_) => errors.push(LexerError::new(span, LexerErrorKind::Utf8Error)),
                    }
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
            } else if c.is_ascii_digit() {
                let start = index;

                while index < self.source.len() && self.source[index].is_ascii_digit() {
                    index += 1;
                }

                if (index + 1 < self.source.len())
                    && self.source[index] == b'.'
                    && self.source[index + 1].is_ascii_digit()
                {
                    index += 1; // consume the decimal point

                    while index < self.source.len() && self.source[index].is_ascii_digit() {
                        index += 1;
                    }
                }

                // FIXME: Can we avoid  allocating a `String` here for every numeric token?
                // `from_utf8_lossy` and `unwrap` are safe to use here, because we guarantee that
                // the bytes are ASCII digits.
                let number = String::from_utf8_lossy(&self.source[start..index])
                    .parse()
                    .unwrap();

                tokens.push(Token::new(
                    Span::new(line, start, index),
                    TokenKind::Number(number),
                ));

                // When we get here, `index` is going to point to the next byte in the source.
                // This is too far, because we will increment `index` again at the end of the loop.
                // Set `index` back by 1 to ensure that we don't skip reading the byte right after a number.
                index -= 1;
            } else if c.is_ascii_alphabetic() {
                let start = index;

                while index < self.source.len()
                    && (self.source[index].is_ascii_alphanumeric() || self.source[index] == b'_')
                {
                    index += 1;
                }

                let span = Span::new(line, start, index);

                match std::str::from_utf8(&self.source[start..index]) {
                    Ok(s) => match s {
                        "and" => tokens.push(Token::new(span, TokenKind::And)),
                        "class" => tokens.push(Token::new(span, TokenKind::Class)),
                        "else" => tokens.push(Token::new(span, TokenKind::Else)),
                        "false" => tokens.push(Token::new(span, TokenKind::False)),
                        "for" => tokens.push(Token::new(span, TokenKind::For)),
                        "fun" => tokens.push(Token::new(span, TokenKind::Fun)),
                        "if" => tokens.push(Token::new(span, TokenKind::If)),
                        "nil" => tokens.push(Token::new(span, TokenKind::Nil)),
                        "or" => tokens.push(Token::new(span, TokenKind::Or)),
                        "print" => tokens.push(Token::new(span, TokenKind::Print)),
                        "return" => tokens.push(Token::new(span, TokenKind::Return)),
                        "super" => tokens.push(Token::new(span, TokenKind::Super)),
                        "this" => tokens.push(Token::new(span, TokenKind::This)),
                        "true" => tokens.push(Token::new(span, TokenKind::True)),
                        "var" => tokens.push(Token::new(span, TokenKind::Var)),
                        "while" => tokens.push(Token::new(span, TokenKind::While)),
                        _ => tokens.push(Token::new(span, TokenKind::Identifier(s))),
                    },
                    Err(_) => errors.push(LexerError::new(span, LexerErrorKind::Utf8Error)),
                }

                index -= 1;
            } else if !c.is_ascii_whitespace() {
                // The token did not match anything we expected, so add it to the list of errors
                errors.push(LexerError::unrecognized_token(Span::new(
                    line,
                    index,
                    index + 1,
                )));
            }

            if (index < self.source.len()) && (self.source[index] == b'\n') {
                line += 1;
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

    #[test]
    fn operators_grouping_comments() {
        let source = r#"// this is a comment
(( )) {} // grouping stuff
!*+-/=<> <= == // operators
"#
        .as_bytes();

        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        let expected_tokens = vec![
            Token::new(Span::new(2, 21, 22), TokenKind::LParen),
            Token::new(Span::new(2, 22, 23), TokenKind::LParen),
            Token::new(Span::new(2, 24, 25), TokenKind::RParen),
            Token::new(Span::new(2, 25, 26), TokenKind::RParen),
            Token::new(Span::new(2, 27, 28), TokenKind::LBrace),
            Token::new(Span::new(2, 28, 29), TokenKind::RBrace),
            Token::new(Span::new(3, 48, 49), TokenKind::Exclamation),
            Token::new(Span::new(3, 49, 50), TokenKind::Asterisk),
            Token::new(Span::new(3, 50, 51), TokenKind::Plus),
            Token::new(Span::new(3, 51, 52), TokenKind::Minus),
            Token::new(Span::new(3, 52, 53), TokenKind::Slash),
            Token::new(Span::new(3, 53, 54), TokenKind::Equal),
            Token::new(Span::new(3, 54, 55), TokenKind::LessThan),
            Token::new(Span::new(3, 55, 56), TokenKind::GreaterThan),
            Token::new(Span::new(3, 57, 59), TokenKind::LessThanEqual),
            Token::new(Span::new(3, 60, 62), TokenKind::EqualEqual),
            Token::new(Span::new(4, 76, 76), TokenKind::Eof),
        ];

        assert_eq!(tokens, expected_tokens);
        assert_eq!(errors.has_errors(), false);
    }

    #[test]
    fn emtpy_string() {
        let source = r#""""#.as_bytes();

        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(1, 1, 1), TokenKind::QuotedString("".into())),
                Token::new(Span::new(1, 2, 2), TokenKind::Eof)
            ]
        );
        assert_eq!(errors.has_errors(), false);
    }

    #[test]
    fn simple_string() {
        let source = r#""hello world""#.as_bytes();

        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(1, 1, 12), TokenKind::QuotedString("hello world")),
                Token::new(Span::new(1, 13, 13), TokenKind::Eof)
            ]
        );
        assert_eq!(errors.has_errors(), false);
    }

    #[test]
    fn multi_line_string() {
        let source = r#""hello
world" >= // comment"#
            .as_bytes();

        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(1, 1, 12), TokenKind::QuotedString("hello\nworld")),
                Token::new(Span::new(2, 14, 16), TokenKind::GreaterThanEqual),
                Token::new(Span::new(2, 27, 27), TokenKind::Eof)
            ]
        );
        assert_eq!(errors.has_errors(), false);
    }

    #[test]
    fn unterminated_string() {
        let source = r#"<= "hello //cmt"#.as_bytes();

        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(1, 0, 2), TokenKind::LessThanEqual),
                Token::new(Span::new(1, 15, 15), TokenKind::Eof)
            ]
        );
        assert_eq!(
            errors.into_iter().next().unwrap(),
            LexerError::new(Span::new(1, 3, 15), LexerErrorKind::UnterminatedString)
        )
    }

    #[test]
    fn invalid_utf8_string() {
        // "hello" "helxo" where x = invalid utf-8
        let source = [
            34, 104, 101, 108, 108, 111, 34, 32, 34, 104, 101, 0xFF, 108, 111, 34,
        ];

        let lexer = Lexer::new(&source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(1, 1, 6), TokenKind::QuotedString("hello")),
                Token::new(Span::new(1, 15, 15), TokenKind::Eof)
            ]
        );
        assert_eq!(
            errors.into_iter().next().unwrap(),
            LexerError::new(Span::new(1, 9, 14), LexerErrorKind::Utf8Error)
        )
    }

    #[test]
    fn simple_number() {
        let source = "1234567890".as_bytes();

        let lexer = Lexer::new(&source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(1, 0, 10), TokenKind::Number(1234567890_f64)),
                Token::new(Span::new(1, 10, 10), TokenKind::Eof)
            ]
        );
        assert_eq!(errors.has_errors(), false);
    }

    #[test]
    fn floating_point_number() {
        let source = "12340.56789".as_bytes();

        let lexer = Lexer::new(&source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(1, 0, 11), TokenKind::Number(12340.56789)),
                Token::new(Span::new(1, 11, 11), TokenKind::Eof)
            ]
        );
        assert_eq!(errors.has_errors(), false);
    }

    #[test]
    fn integer_dot_other() {
        let source = "12340.hello".as_bytes();

        let lexer = Lexer::new(&source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(1, 0, 5), TokenKind::Number(12340_f64)),
                Token::new(Span::new(1, 5, 6), TokenKind::Dot),
                Token::new(Span::new(1, 6, 11), TokenKind::Identifier("hello")),
                Token::new(Span::new(1, 11, 11), TokenKind::Eof)
            ]
        );
        assert_eq!(errors.has_errors(), false);
    }

    #[test]
    fn float_dot_other() {
        let source = "123.40.hello".as_bytes();

        let lexer = Lexer::new(&source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(1, 0, 6), TokenKind::Number(123.4)),
                Token::new(Span::new(1, 6, 7), TokenKind::Dot),
                Token::new(Span::new(1, 7, 12), TokenKind::Identifier("hello")),
                Token::new(Span::new(1, 12, 12), TokenKind::Eof)
            ]
        );
        assert_eq!(errors.has_errors(), false);
    }

    #[test]
    fn integer_dot_nothing() {
        let source = "123.".as_bytes();

        let lexer = Lexer::new(&source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(1, 0, 3), TokenKind::Number(123_f64)),
                Token::new(Span::new(1, 3, 4), TokenKind::Dot),
                Token::new(Span::new(1, 4, 4), TokenKind::Eof)
            ]
        );
        assert_eq!(errors.has_errors(), false);
    }

    #[test]
    fn number_before_and_after() {
        let source = r#""123"456({789
})"#
        .as_bytes();

        let lexer = Lexer::new(&source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(1, 1, 4), TokenKind::QuotedString("123")),
                Token::new(Span::new(1, 5, 8), TokenKind::Number(456_f64)),
                Token::new(Span::new(1, 8, 9), TokenKind::LParen),
                Token::new(Span::new(1, 9, 10), TokenKind::LBrace),
                Token::new(Span::new(1, 10, 13), TokenKind::Number(789_f64)),
                Token::new(Span::new(2, 14, 15), TokenKind::RBrace),
                Token::new(Span::new(2, 15, 16), TokenKind::RParen),
                Token::new(Span::new(2, 16, 16), TokenKind::Eof)
            ]
        );
        assert_eq!(errors.has_errors(), false);
    }

    #[test]
    fn identifer_alpha_only() {
        let source = "hello".as_bytes();

        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(1, 0, 5), TokenKind::Identifier("hello")),
                Token::new(Span::new(1, 5, 5), TokenKind::Eof)
            ]
        );
        assert_eq!(errors.has_errors(), false);
    }

    #[test]
    fn identifier_complex() {
        let source = "hello123_oops_456bye".as_bytes();

        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(
                    Span::new(1, 0, 20),
                    TokenKind::Identifier("hello123_oops_456bye")
                ),
                Token::new(Span::new(1, 20, 20), TokenKind::Eof)
            ]
        );
        assert_eq!(errors.has_errors(), false);
    }

    #[test]
    fn identifier_after() {
        let source = "hello.foo123()".as_bytes();

        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(1, 0, 5), TokenKind::Identifier("hello")),
                Token::new(Span::new(1, 5, 6), TokenKind::Dot),
                Token::new(Span::new(1, 6, 12), TokenKind::Identifier("foo123")),
                Token::new(Span::new(1, 12, 13), TokenKind::LParen),
                Token::new(Span::new(1, 13, 14), TokenKind::RParen),
                Token::new(Span::new(1, 14, 14), TokenKind::Eof)
            ]
        );
        assert_eq!(errors.has_errors(), false);
    }

    #[test]
    fn identifer_starting_with_keyword() {
        let source = "orchid".as_bytes();

        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens,
            vec![
                Token::new(Span::new(1, 0, 6), TokenKind::Identifier("orchid")),
                Token::new(Span::new(1, 6, 6), TokenKind::Eof)
            ]
        );
        assert_eq!(errors.has_errors(), false);
    }

    #[test]
    fn keywords() {
        let source =
            "and class else false for fun if nil or print return super this true var while"
                .as_bytes();

        let lexer = Lexer::new(source);
        let (tokens, errors) = lexer.lex();

        assert_eq!(
            tokens.into_iter().map(|t| t.kind).collect::<Vec<_>>(),
            vec![
                TokenKind::And,
                TokenKind::Class,
                TokenKind::Else,
                TokenKind::False,
                TokenKind::For,
                TokenKind::Fun,
                TokenKind::If,
                TokenKind::Nil,
                TokenKind::Or,
                TokenKind::Print,
                TokenKind::Return,
                TokenKind::Super,
                TokenKind::This,
                TokenKind::True,
                TokenKind::Var,
                TokenKind::While,
                TokenKind::Eof
            ]
        );
        assert_eq!(errors.has_errors(), false);
    }
}
