use std::fmt;

pub type ParserErrors = Vec<ParserError>;

#[derive(Debug, Clone)]
pub struct ParserError(String);

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl ParserError {
    pub fn new(msg: String) -> Self {
        ParserError(msg)
    }
}
