#[derive(Debug)]
pub enum LoxError {
    IOError(std::io::Error),
}

impl From<std::io::Error> for LoxError {
    fn from(e: std::io::Error) -> Self {
        LoxError::IOError(e)
    }
}
