pub mod error;
pub mod precedence;

use crate::parser::{error::*, precedence::*};
use crate::{ast::*, lexer::*, token::Token};

#[derive(Debug)]
struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>,
}

pub fn parse(input: &str) -> Result<Node, ParserErrors> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program()?;

    Ok(Node::Program(program))
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();
        let errors = Vec::new();

        Parser {
            lexer,
            current_token,
            peek_token,
            errors,
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn current_token_is(&self, token: &Token) -> bool {
        self.current_token == *token
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        self.peek_token == *token
    }

    fn expect_peek_token(&mut self, token: &Token) -> Result<(), ParserError> {
        if self.peek_token_is(&token) {
            self.next_token();
            Ok(())
        } else {
            Err(ParserError::new(format!(
                "expected next token to be {}, but got {} instead",
                token, self.peek_token
            )))
        }
    }

    fn next_token_precedence(&self) -> Precedence {
        token_to_precedence(&self.peek_token)
    }

    fn error_no_identifier(&self, token: &Token) -> ParserError {
        ParserError::new(format!("Expected an identifier but got {}", token.clone()))
    }

    fn parse_program(&mut self) -> Result<Vec<Statement>, ParserErrors> {
        let mut program = Vec::new();

        while !self.current_token_is(&Token::EOF) {
            match self.parse_statement() {
                Ok(stmt) => program.push(stmt),
                Err(e) => self.errors.push(e),
            }
            self.next_token();
        }

        if !self.errors.is_empty() {
            Err(self.errors.clone())
        } else {
            Ok(program)
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let ident = match &self.peek_token {
            Token::Ident(ref id) => id.clone(),
            t => {
                return Err(self.error_no_identifier(t));
            }
        };
        // Consume identifier
        self.next_token();

        self.expect_peek_token(&Token::Assign)?;
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Let(ident, expr))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        // Consume `return`
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Return(expr))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Expr(expr))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParserError> {
        // Consume LBrace
        self.next_token();

        let mut block_statement = Vec::new();

        while !self.current_token_is(&Token::RBrace) && !self.current_token_is(&Token::EOF) {
            if let Ok(stmt) = self.parse_statement() {
                block_statement.push(stmt);
            }

            self.next_token();
        }

        Ok(block_statement)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError> {
        let mut left_expr = match self.current_token {
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            Token::Ident(ref id) => Ok(Expression::Ident(id.clone())),
            Token::Integer(i) => Ok(Expression::Lit(Literal::Integer(i))),
            Token::String(ref s) => Ok(Expression::Lit(Literal::String(s.clone()))),
            Token::Boolean(b) => Ok(Expression::Lit(Literal::Boolean(b))),
            Token::LParen => {
                self.next_token();
                let expr = self.parse_expression(Precedence::Lowest);
                self.expect_peek_token(&Token::RParen)?;
                expr
            }
            Token::If => self.parse_if_expression(),
            Token::Fn => self.parse_fn_expression(),
            Token::LBracket => self.parse_array_literal(),
            Token::LBrace => self.parse_hash_literal(),
            _ => {
                return Err(ParserError::new(format!(
                    "No prefix parse function for {} is found",
                    self.current_token
                )));
            }
        };

        while !self.peek_token_is(&Token::Semicolon) && precedence < self.next_token_precedence() {
            match self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Eq
                | Token::NotEq
                | Token::Lt
                | Token::Gt => {
                    self.next_token();
                    let expr = left_expr.unwrap();
                    left_expr = self.parse_infix_expression(expr);
                }
                Token::LBracket => {
                    self.next_token();
                    let expr = left_expr.unwrap();
                    left_expr = self.parse_index_expression(expr);
                }
                Token::LParen => {
                    self.next_token();
                    let expr = left_expr.unwrap();
                    left_expr = self.parse_fn_call_expression(expr);
                }
                _ => return left_expr,
            }
        }

        left_expr
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        let prefix = self.current_token.clone();
        self.next_token();

        let expr = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::Prefix(prefix, Box::new(expr)))
    }

    fn parse_infix_expression(&mut self, left_expr: Expression) -> Result<Expression, ParserError> {
        let infix_op = self.current_token.clone();

        let precedence = token_to_precedence(&self.current_token);
        self.next_token();

        let right_expr = self.parse_expression(precedence)?;

        Ok(Expression::Infix(
            infix_op,
            Box::new(left_expr),
            Box::new(right_expr),
        ))
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
        self.expect_peek_token(&Token::LParen)?;
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek_token(&Token::RParen)?;
        self.expect_peek_token(&Token::LBrace)?;

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token_is(&Token::Else) {
            self.next_token();

            self.expect_peek_token(&Token::LBrace)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expression::If(
            Box::new(condition),
            consequence,
            alternative,
        ))
    }

    fn parse_fn_expression(&mut self) -> Result<Expression, ParserError> {
        self.expect_peek_token(&Token::LParen)?;

        let parameters = self.parse_fn_parameters()?;

        self.expect_peek_token(&Token::LBrace)?;

        let body = self.parse_block_statement()?;

        Ok(Expression::Fn(parameters, body))
    }

    fn parse_fn_parameters(&mut self) -> Result<Vec<String>, ParserError> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(&Token::RParen) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        match &self.current_token {
            Token::Ident(ref id) => identifiers.push(id.clone()),
            token => return Err(self.error_no_identifier(token)),
        }

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            match &self.current_token {
                Token::Ident(ref id) => identifiers.push(id.clone()),
                token => return Err(self.error_no_identifier(token)),
            }
        }

        self.expect_peek_token(&Token::RParen)?;

        Ok(identifiers)
    }

    fn parse_fn_call_expression(&mut self, expr: Expression) -> Result<Expression, ParserError> {
        let arguments = self.parse_expression_list(&Token::RParen)?;
        Ok(Expression::FnCall(Box::new(expr), arguments))
    }

    fn parse_array_literal(&mut self) -> Result<Expression, ParserError> {
        let array = self.parse_expression_list(&Token::RBracket)?;
        Ok(Expression::Lit(Literal::Array(array)))
    }

    fn parse_hash_literal(&mut self) -> Result<Expression, ParserError> {
        let mut map = Vec::new();

        while !self.peek_token_is(&Token::RBrace) {
            self.next_token();
            let key = self.parse_expression(Precedence::Lowest)?;

            self.expect_peek_token(&Token::Colon)?;

            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;

            map.push((key, value));

            if !self.peek_token_is(&Token::RBrace) {
                self.expect_peek_token(&Token::Comma)?;
            }
        }

        self.expect_peek_token(&Token::RBrace)?;

        Ok(Expression::Lit(Literal::Hash(map)))
    }

    fn parse_expression_list(&mut self, end: &Token) -> Result<Vec<Expression>, ParserError> {
        let mut list = Vec::new();

        if self.peek_token_is(end) {
            self.next_token();
            return Ok(list);
        }

        self.next_token();

        list.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek_token(end)?;

        Ok(list)
    }

    fn parse_index_expression(&mut self, left_expr: Expression) -> Result<Expression, ParserError> {
        self.next_token();

        let index_expr = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek_token(&Token::RBracket)?;

        Ok(Expression::Index(Box::new(left_expr), Box::new(index_expr)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn apply_test(test_case: &[(&str, &str)]) {
        for (input, expected) in test_case {
            match parse(input) {
                Ok(node) => assert_eq!(expected, &format!("{}", node)),
                Err(e) => panic!("Parsing Error: {:#?}", e),
            }
        }
    }

    #[test]
    fn test_let_statement() {
        let test_case = [
            ("let x = 5;", "let x = 5;"),
            ("let y = true;", "let y = true;"),
            ("let foobar = y;", "let foobar = y;"),
        ];

        apply_test(&test_case);
    }

    #[test]
    fn test_return_statement() {
        let test_case = [
            ("return 5;", "return 5;"),
            ("return true;", "return true;"),
            ("return foobar;", "return foobar;"),
        ];

        apply_test(&test_case);
    }

    #[test]
    fn test_identifier_expression() {
        let test_case = [("foobar;", "foobar")];

        apply_test(&test_case);
    }

    #[test]
    fn test_integer_literal_expression() {
        let test_case = [("5;", "5")];

        apply_test(&test_case);
    }

    #[test]
    fn test_parse_prefix_expression() {
        let test_case = [
            ("!5;", "(!5)"),
            ("-15;", "(-15)"),
            ("!foobar;", "(!foobar)"),
            ("-foobar;", "(-foobar)"),
            ("!true;", "(!true)"),
            ("!false;", "(!false)"),
        ];

        apply_test(&test_case);
    }

    #[test]
    fn test_parse_infix_expression() {
        let test_case = [
            ("5 + 5;", "(5 + 5)"),
            ("5 - 5;", "(5 - 5)"),
            ("5 * 5;", "(5 * 5)"),
            ("5 / 5;", "(5 / 5)"),
            ("5 > 5;", "(5 > 5)"),
            ("5 < 5;", "(5 < 5)"),
            ("5 == 5;", "(5 == 5)"),
            ("5 != 5;", "(5 != 5)"),
            ("foobar + barfoo;", "(foobar + barfoo)"),
            ("foobar - barfoo;", "(foobar - barfoo)"),
            ("foobar * barfoo;", "(foobar * barfoo)"),
            ("foobar / barfoo;", "(foobar / barfoo)"),
            ("foobar > barfoo;", "(foobar > barfoo)"),
            ("foobar < barfoo;", "(foobar < barfoo)"),
            ("foobar == barfoo;", "(foobar == barfoo)"),
            ("foobar != barfoo;", "(foobar != barfoo)"),
            ("true == true", "(true == true)"),
            ("true != false", "(true != false)"),
            ("false == false", "(false == false)"),
        ];

        apply_test(&test_case);
    }

    #[test]
    fn test_operator_precedence() {
        let test_case = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        apply_test(&test_case);
    }

    #[test]
    fn test_boolean_literal_expression() {
        let test_case = [("true;", "true"), ("false;", "false")];
        apply_test(&test_case);
    }

    #[test]
    fn test_if_expression() {
        let test_case = [("if (x < y) { x }", "if (x < y) { x }")];
        apply_test(&test_case);
    }

    #[test]
    fn test_if_else_expression() {
        let test_case = [("if (x < y) { x } else { y }", "if (x < y) { x } else { y }")];
        apply_test(&test_case);
    }

    #[test]
    fn test_function_expression() {
        let test_case = [
            ("fn() {};", "fn() {...}"),
            ("fn(x) {};", "fn(x) {...}"),
            ("fn(x, y, z) {};", "fn(x, y, z) {...}"),
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_fn_call_expression() {
        let test_case = [("add(1, 2 * 3, 4 + 5);", "add(1, (2 * 3), (4 + 5))")];
        apply_test(&test_case);
    }

    #[test]
    fn test_string_literal_expression() {
        let test_case = [(r#""hello world";"#, r#""hello world""#)];
        apply_test(&test_case);
    }

    #[test]
    fn test_array_literal_expression() {
        let test_case = [("[]", "[]"), ("[1, 2 * 2, 3 + 3]", "[1, (2 * 2), (3 + 3)]")];
        apply_test(&test_case);
    }

    #[test]
    fn test_index_expression() {
        let test_case = [("myArray[1 + 1]", "(myArray[(1 + 1)])")];
        apply_test(&test_case);
    }

    #[test]
    fn test_hash_literal_expression() {
        let test_case = [
            (
                r#"{"one": 1, "two": 2, "three": 3}"#,
                r#"{"one": 1, "two": 2, "three": 3}"#,
            ),
            (r#"{}"#, r#"{}"#),
            (
                r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#,
                r#"{"one": (0 + 1), "two": (10 - 8), "three": (15 / 5)}"#,
            ),
        ];
        apply_test(&test_case);
    }
}
