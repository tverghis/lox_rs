#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Span {
    start: usize,
    end: usize,
}
