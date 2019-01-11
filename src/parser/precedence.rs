use crate::token::Token;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,      // == or !=
    LessGreater, // > or <
    Sum,         // + or -
    Product,     // * or /
    Prefix,      // -X or !X
    Call,        // Function call
    Index,       // myArray[i]
}

pub fn token_to_precedence(token: &Token) -> Precedence {
    match token {
        Token::Asterisk | Token::Slash => Precedence::Product,
        Token::Plus | Token::Minus => Precedence::Sum,
        Token::Lt | Token::Gt => Precedence::LessGreater,
        Token::Eq | Token::NotEq => Precedence::Equals,
        Token::LParen => Precedence::Call,
        Token::LBracket => Precedence::Index,
        _ => Precedence::Lowest,
    }
}
