use crate::token::Token;

/// A struct representing a lexer.  It keeps track of the input string as Vec of chars.  
/// `position` points to the input that corresponds to `ch` and `read_position` points to next input used for peeking.
#[derive(Debug, Default)]
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer {
            input: input.chars().collect::<Vec<char>>(),
            ..Default::default()
        };

        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        let token: Token;

        self.skip_whitespace();

        match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    token = Token::Eq;
                } else {
                    token = Token::Assign;
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    token = Token::NotEq;
                } else {
                    token = Token::Bang;
                }
            }
            '+' => token = Token::Plus,
            '-' => token = Token::Minus,
            '*' => token = Token::Asterisk,
            '/' => token = Token::Slash,
            '<' => token = Token::Lt,
            '>' => token = Token::Gt,
            ',' => token = Token::Comma,
            ';' => token = Token::Semicolon,
            ':' => token = Token::Colon,
            '(' => token = Token::LParen,
            ')' => token = Token::RParen,
            '{' => token = Token::LBrace,
            '}' => token = Token::RBrace,
            '[' => token = Token::LBracket,
            ']' => token = Token::RBracket,
            '\0' => token = Token::EOF,
            '"' => {
                let string_literal = self.read_string();
                token = Token::String(string_literal);
            }
            c => {
                if c.is_alphabetic() || c == '_' {
                    let id = self.read_identifier();
                    return match id.as_str() {
                        "let" => Token::Let,
                        "fn" => Token::Fn,
                        "true" => Token::Boolean(true),
                        "false" => Token::Boolean(false),
                        "if" => Token::If,
                        "else" => Token::Else,
                        "return" => Token::Return,
                        _ => Token::Ident(id),
                    };
                } else if c.is_digit(10) {
                    let n = self.read_number();
                    return Token::Integer(n);
                } else {
                    token = Token::Illegal;
                }
            }
        }

        self.read_char();
        token
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> String {
        let start = self.position;

        while self.ch.is_alphabetic() || self.ch == '_' {
            self.read_char();
        }

        let end = self.position;

        self.input[start..end].iter().collect()
    }

    fn read_number(&mut self) -> i32 {
        let start = self.position;

        while self.ch.is_digit(10) {
            self.read_char();
        }

        let end = self.position;

        self.input[start..end]
            .iter()
            .collect::<String>()
            .parse::<i32>()
            .expect("Unexpected character in sequence of numbers")
    }

    fn read_string(&mut self) -> String {
        let start = self.position + 1;

        loop {
            self.read_char();
            if self.ch == '"' || self.ch == '\0' {
                break;
            }
        }

        let end = self.position;

        self.input[start..end].iter().collect::<String>()
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_tokenize_delimiters() {
        let input = "(){}[],;:";
        let mut tokens = Lexer::new(input);

        let expected = vec![
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::LBracket,
            Token::RBracket,
            Token::Comma,
            Token::Semicolon,
            Token::Colon,
            Token::EOF,
        ];

        for token_expected in expected.iter() {
            let token = tokens.next_token();
            assert_eq!(&token, token_expected);
        }
    }

    #[test]
    fn test_tokenize_operators() {
        let input = "+ = == ! != - / * < >";
        let mut tokens = Lexer::new(input);

        let expected = vec![
            Token::Plus,
            Token::Assign,
            Token::Eq,
            Token::Bang,
            Token::NotEq,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Lt,
            Token::Gt,
            Token::EOF,
        ];

        for token_expected in expected.iter() {
            let token = tokens.next_token();
            assert_eq!(&token, token_expected);
        }
    }

    #[test]
    fn test_tokenize_keywords() {
        let input = "if else true false return";
        let mut tokens = Lexer::new(input);

        let expected = vec![
            Token::If,
            Token::Else,
            Token::Boolean(true),
            Token::Boolean(false),
            Token::Return,
            Token::EOF,
        ];

        for token_expected in expected.iter() {
            let token = tokens.next_token();
            assert_eq!(&token, token_expected);
        }
    }

    #[test]
    fn test_tokenize_program() {
        let input = "let five = 5; \
                     let ten = 10; \
                     let add = fn(x, y) { x + y;}; \
                     let result = add(five, ten);";

        let mut tokens = Lexer::new(input);

        let expected = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Integer(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Integer(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Fn,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::EOF,
        ];

        for token_expected in expected.iter() {
            let token = tokens.next_token();
            assert_eq!(&token, token_expected);
        }
    }

    #[test]
    fn test_tokenize_string() {
        let input = "\"foobar\"";
        let mut tokens = Lexer::new(input);

        let expected = vec![Token::String("foobar".to_string())];

        for token_expected in expected.iter() {
            let token = tokens.next_token();
            assert_eq!(&token, token_expected);
        }
    }
}
