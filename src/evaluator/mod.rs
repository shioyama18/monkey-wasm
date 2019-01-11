pub mod builtin;
pub mod environment;
pub mod error;
pub mod object;

use self::builtin::*;
use self::environment::*;
use self::error::*;
use self::object::*;
use crate::ast::*;
use crate::token::*;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type EvaluatorResult = Result<Rc<Object>, EvaluatorError>;

fn is_truthy(obj: &Object) -> bool {
    match *obj {
        Object::Null => false,
        Object::Boolean(false) => false,
        _ => true,
    }
}

pub fn eval(node: Node, env: &Env) -> EvaluatorResult {
    match node {
        Node::Program(program) => eval_program(&program, env),
        Node::Stmt(stmt) => eval_statement(&stmt, env),
        Node::Expr(expr) => eval_expression(&expr, env),
    }
}

fn eval_program(program: &[Statement], env: &Env) -> EvaluatorResult {
    let mut result = Rc::new(Object::Empty);

    for stmt in program {
        let val = eval_statement(stmt, &Rc::clone(env))?;

        match *val {
            Object::ReturnValue(_) => return Ok(val),
            _ => result = val,
        }
    }

    Ok(result)
}

fn eval_block_statement(stmts: &[Statement], env: &Env) -> EvaluatorResult {
    let mut result = Rc::new(Object::Null);

    for stmt in stmts {
        let val = eval_statement(stmt, &Rc::clone(env))?;

        match *val {
            Object::ReturnValue(_) => return Ok(val),
            _ => result = val,
        }
    }

    Ok(result)
}

fn eval_statement(stmt: &Statement, env: &Env) -> EvaluatorResult {
    match stmt {
        Statement::Let(id, expr) => {
            let val = eval_expression(expr, &Rc::clone(env))?;
            let obj = Rc::clone(&val);
            env.borrow_mut().set(id.clone(), obj);
            Ok(val)
        }
        Statement::Expr(expr) => eval_expression(expr, env),
        Statement::Return(expr) => {
            let val = eval_expression(expr, env)?;
            Ok(Rc::new(Object::ReturnValue(val)))
        }
    }
}

fn eval_expression(expr: &Expression, env: &Env) -> EvaluatorResult {
    match expr {
        Expression::Ident(id) => eval_identifier(&id, env),
        Expression::Lit(c) => eval_literal(c, env),
        Expression::Prefix(op, expr) => {
            let right = eval_expression(expr, env)?;
            eval_prefix_expression(op, &right)
        }
        Expression::Infix(op, left, right) => {
            let left = eval_expression(left, &Rc::clone(env))?;
            let right = eval_expression(right, &Rc::clone(env))?;
            eval_infix_expression(op, &left, &right)
        }
        Expression::If(condition, consequence, alternative) => {
            let condition = eval_expression(condition, &Rc::clone(env))?;

            if is_truthy(&condition) {
                eval_block_statement(consequence, env)
            } else {
                match alternative {
                    Some(alt) => eval_block_statement(alt, env),
                    None => Ok(Rc::new(Object::Null)),
                }
            }
        }
        Expression::Fn(params, body) => Ok(Rc::new(Object::Function(
            params.clone(),
            body.clone(),
            Rc::clone(&env),
        ))),
        Expression::FnCall(func, args) => {
            let func = eval_expression(func, &Rc::clone(env))?;
            let args = eval_expressions(args, env)?;
            apply_function(&func, &args)
        }
        Expression::Index(left, index) => {
            let left = eval_expression(left, &Rc::clone(env))?;
            let index = eval_expression(index, env)?;
            eval_index_expression(&left, &index)
        }
    }
}

fn eval_expressions(
    expressions: &[Expression],
    env: &Env,
) -> Result<Vec<Rc<Object>>, EvaluatorError> {
    let mut list = Vec::new();

    for expr in expressions {
        let val = eval_expression(expr, &Rc::clone(env))?;
        list.push(val);
    }

    Ok(list)
}

fn eval_identifier(id: &str, env: &Env) -> EvaluatorResult {
    match env.borrow().get(id) {
        Some(obj) => Ok(obj.clone()),
        None => match Builtin::lookup(id) {
            Some(obj) => Ok(Rc::new(obj)),
            None => Err(EvaluatorError::new(format!("identifier not found: {}", id))),
        },
    }
}

fn eval_literal(lit: &Literal, env: &Env) -> EvaluatorResult {
    match lit {
        Literal::Integer(i) => Ok(Rc::new(Object::Integer(*i))),
        Literal::Boolean(b) => Ok(Rc::new(Object::Boolean(*b))),
        Literal::String(s) => Ok(Rc::new(Object::String(s.clone()))),
        Literal::Array(a) => {
            let list = eval_expressions(&a, &Rc::clone(env))?;
            Ok(Rc::new(Object::Array(list)))
        }
        Literal::Hash(map) => {
            let map = eval_hash_literal(&map, &Rc::clone(env))?;
            Ok(Rc::new(Object::Hash(map)))
        }
    }
}

fn eval_prefix_expression(op: &Token, expr: &Rc<Object>) -> EvaluatorResult {
    match op {
        Token::Bang => eval_bang_operator(expr),
        Token::Minus => eval_minus_prefix_operator(expr),
        _ => Err(EvaluatorError::new(format!(
            "unknown operator: {}{}",
            op, expr
        ))),
    }
}

fn eval_bang_operator(expr: &Rc<Object>) -> EvaluatorResult {
    match **expr {
        Object::Boolean(b) => Ok(Rc::new(Object::Boolean(!b))),
        Object::Null => Ok(Rc::new(Object::Boolean(true))),
        _ => Ok(Rc::new(Object::Boolean(false))),
    }
}

fn eval_minus_prefix_operator(expr: &Rc<Object>) -> EvaluatorResult {
    match **expr {
        Object::Integer(i) => Ok(Rc::new(Object::Integer(-i))),
        _ => Err(EvaluatorError::new(format!("unknown operator: -{}", expr))),
    }
}

fn eval_infix_expression(op: &Token, left: &Rc<Object>, right: &Rc<Object>) -> EvaluatorResult {
    match (&**left, &**right) {
        (Object::Integer(left_val), Object::Integer(right_val)) => {
            eval_integer_infix_expression(op, *left_val, *right_val)
        }
        (Object::Boolean(left_val), Object::Boolean(right_val)) => {
            eval_boolean_infix_expression(op, *left_val, *right_val)
        }
        (Object::String(left_val), Object::String(right_val)) => {
            eval_string_infix_expression(op, left_val.to_string(), right_val)
        }
        _ => Err(EvaluatorError::new(format!(
            "type mismatch: {} {} {}",
            left, op, right
        ))),
    }
}

fn eval_integer_infix_expression(op: &Token, left_val: i32, right_val: i32) -> EvaluatorResult {
    let result = match op {
        Token::Plus => Object::Integer(left_val + right_val),
        Token::Minus => Object::Integer(left_val - right_val),
        Token::Asterisk => Object::Integer(left_val * right_val),
        Token::Slash => Object::Integer(left_val / right_val),
        Token::Lt => Object::Boolean(left_val < right_val),
        Token::Gt => Object::Boolean(left_val > right_val),
        Token::Eq => Object::Boolean(left_val == right_val),
        Token::NotEq => Object::Boolean(left_val != right_val),
        op => {
            return Err(EvaluatorError::new(format!(
                "unknown operator: {} {} {}",
                left_val, op, right_val
            )))
        }
    };

    Ok(Rc::new(result))
}

fn eval_boolean_infix_expression(op: &Token, left_val: bool, right_val: bool) -> EvaluatorResult {
    let result = match op {
        Token::Eq => Object::Boolean(left_val == right_val),
        Token::NotEq => Object::Boolean(left_val != right_val),
        op => {
            return Err(EvaluatorError::new(format!(
                "unknown operator: {} {} {}",
                left_val, op, right_val
            )))
        }
    };

    Ok(Rc::new(result))
}

fn eval_string_infix_expression(op: &Token, left_val: String, right_val: &str) -> EvaluatorResult {
    let result = match op {
        Token::Eq => Object::Boolean(left_val == right_val),
        Token::NotEq => Object::Boolean(left_val != right_val),
        Token::Plus => Object::String(left_val + right_val),
        op => {
            return Err(EvaluatorError::new(format!(
                "unknown operator: {} {} {}",
                left_val, op, right_val
            )))
        }
    };

    Ok(Rc::new(result))
}

fn apply_function(function: &Rc<Object>, args: &[Rc<Object>]) -> EvaluatorResult {
    match &**function {
        Object::Function(params, body, env) => {
            let mut env = Environment::new_enclosed_environment(&Rc::clone(env));

            if params.len() != args.len() {
                return Err(EvaluatorError::new(format!(
                    "invalid number of arguments: exected={}, got={}",
                    params.len(),
                    args.len()
                )));
            }

            params.iter().enumerate().for_each(|(i, param)| {
                env.set(param.clone(), args[i].clone());
            });

            let evaluated = eval_block_statement(&body, &Rc::new(RefCell::new(env)))?;
            unwrap_return_value(evaluated)
        }
        Object::Builtin(b) => b.apply(args),
        f => Err(EvaluatorError::new(format!("not a function: {}", f))),
    }
}

fn unwrap_return_value(obj: Rc<Object>) -> EvaluatorResult {
    if let Object::ReturnValue(val) = &*obj {
        Ok(Rc::clone(&val))
    } else {
        Ok(obj)
    }
}

fn eval_index_expression(left: &Rc<Object>, index: &Rc<Object>) -> EvaluatorResult {
    match (&**left, &**index) {
        (Object::Array(arr), Object::Integer(idx)) => {
            if *idx < 0 {
                Ok(Rc::new(Object::Null))
            } else {
                match arr.get(*idx as usize) {
                    Some(obj) => Ok(Rc::clone(obj)),
                    None => Ok(Rc::new(Object::Null)),
                }
            }
        }
        (Object::Hash(map), key) => {
            if !key.is_hashable() {
                return Err(EvaluatorError::new(format!(
                    "unusable as hash key: {}",
                    key
                )));
            }

            match map.get(key) {
                Some(val) => Ok(Rc::clone(val)),
                None => Ok(Rc::new(Object::Null)),
            }
        }
        _ => Err(EvaluatorError::new(format!(
            "index operator not supported: {}",
            left
        ))),
    }
}

fn eval_hash_literal(
    map: &[(Expression, Expression)],
    env: &Env,
) -> Result<HashMap<Rc<Object>, Rc<Object>>, EvaluatorError> {
    let mut hash_map = HashMap::new();

    for (k, v) in map {
        let k = eval_expression(k, env)?;

        if !k.is_hashable() {
            return Err(EvaluatorError::new(format!("unusable as hash key: {}", k)));
        }

        let v = eval_expression(v, env)?;
        hash_map.insert(k, v);
    }

    Ok(hash_map)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::*;

    fn apply_test(test_case: &[(&str, &str)]) {
        let env: Env = Rc::new(RefCell::new(Default::default()));

        for (input, expected) in test_case {
            match parse(input) {
                Ok(node) => match eval(node, &Rc::clone(&env)) {
                    Ok(evaluated) => assert_eq!(expected, &format!("{}", evaluated)),
                    Err(err) => assert_eq!(expected, &format!("{}", err)),
                },
                Err(err) => panic!("Parser Error: {:?}", err),
            }
        }
    }

    #[test]
    fn test_integer_expressions() {
        let test_case = [
            ("1", "1"),
            ("-10", "-10"),
            ("5 + 5 + 5 + 5 - 10", "10"),
            ("2 * 2 * 2 * 2 * 2", "32"),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", "50"),
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_boolean_expressions() {
        let test_case = [
            ("true", "true"),
            ("false", "false"),
            ("1 < 2", "true"),
            ("1 > 2", "false"),
            ("1 < 1", "false"),
            ("1 > 1", "false"),
            ("1 == 1", "true"),
            ("1 != 1", "false"),
            ("1 == 2", "false"),
            ("1 != 2", "true"),
            ("true == true", "true"),
            ("false == false", "true"),
            ("true == false", "false"),
            ("true != false", "true"),
            ("false != true", "true"),
            ("(1 < 2) == true", "true"),
            ("(1 < 2) == false", "false"),
            ("(1 > 2) == true", "false"),
            ("(1 > 2) == false", "true"),
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_bang_operators() {
        let test_case = [
            ("!true", "false"),
            ("!false", "true"),
            ("!5", "false"),
            ("!!true", "true"),
            ("!!false", "false"),
            ("!!5", "true"),
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_if_else_expressions() {
        let test_case = [
            ("if (true) { 10 }", "10"),
            ("if (false) { 10 }", "null"),
            ("if (1) { 10 }", "10"),
            ("if (1 < 2) { 10 }", "10"),
            ("if (1 > 2) { 10 }", "null"),
            ("if (1 > 2) { 10 } else { 20 }", "20"),
            ("if (1 < 2) { 10 } else { 20 }", "10"),
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_return_statements() {
        let test_case = [
            ("return 10;", "10"),
            ("return 10; 9;", "10"),
            ("return 2 * 5; 9;", "10"),
            ("9; return 2 * 5; 9;", "10"),
            ("if (10 > 1) { return 10; }", "10"),
            (
                "if (10 > 1) { \
                 if (10 > 1) { \
                 return 10; \
                 } \
                 return 1; \
                 }",
                "10",
            ),
            (
                "let f = fn(x) { \
                 return x; \
                 x + 10; \
                 }; \
                 f(10);",
                "10",
            ),
            (
                "let f = fn(x) { \
                 let result = x + 10; \
                 return result; \
                 return 10; \
                 }; \
                 f(10);",
                "20",
            ),
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_error_handling() {
        let test_case = [
            ("5 + true;", "type mismatch: 5 + true"),
            ("5 + true; 5;", "type mismatch: 5 + true"),
            ("-true", "unknown operator: -true"),
            ("true + false;", "unknown operator: true + false"),
            (
                "true + false + true + false;",
                "unknown operator: true + false",
            ),
            ("5; true + false; 5", "unknown operator: true + false"),
            (r#""Hello" - "World""#, "unknown operator: Hello - World"),
            (
                "if (10 > 1) { true + false; )",
                "unknown operator: true + false",
            ),
            (
                r#"{"name": "Monkey"}[fn(x) { x }];"#,
                "unusable as hash key: fn(x) {...}",
            ),
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_let_statements() {
        let test_case = [
            ("let a = 5; a;", "5"),
            ("let a = 5 * 5; a;", "25"),
            ("let a = 5; let b = a; b;", "5"),
            ("let a = 5; let b = a; let c = a + b + 5; c;", "15"),
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_function_object() {
        let test_case = [("fn(x) { x + 2; };", "fn(x) {...}")];
        apply_test(&test_case);
    }

    #[test]
    fn test_function_application() {
        let test_case = [
            ("let identity = fn(x) { x; }; identity(5);", "5"),
            ("let identity = fn(x) { return x; }; identity(5);", "5"),
            ("let double = fn(x) { x * 2; }; double(5);", "10"),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", "10"),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                "20",
            ),
            ("fn(x) { x; }(5)", "5"),
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_enclosing_environment() {
        let test_case = [(
            "let first = 10; \
             let second = 10; \
             let third = 10; \
             let ourFunction = fn(first) { \
             let second = 20; \
             first + second + third; \
             }; \
             ourFunction(20) + first + second;",
            "70",
        )];
        apply_test(&test_case);
    }

    #[test]
    fn test_closure() {
        let test_case = [(
            "let newAdder = fn(x) { \
             fn(y) { x + y }; \
             }; \
             let addTwo = newAdder(2); \
             addTwo(2);",
            "4",
        )];
        apply_test(&test_case);
    }

    #[test]
    fn test_string_expressions() {
        let test_case = [(r#""Hello World!""#, "Hello World!")];
        apply_test(&test_case);
    }

    #[test]
    fn test_string_concatenation() {
        let test_case = [(r#""Hello" + " " + "World!""#, "Hello World!")];
        apply_test(&test_case);
    }

    #[test]
    fn test_builtin_functions() {
        let test_case = [
            (r#"len("")"#, "0"),
            (r#"len("four")"#, "4"),
            (r#"len("hello world")"#, "11"),
            ("len(1)", "argument to `len` not supported, got 1"),
            (
                r#"len("one", "two")"#,
                "invalid number of arguments: expected=1, got=2",
            ),
            ("len([1, 2, 3])", "3"),
            ("len([])", "0"),
            // (r#"puts("hello", "world!")"#, "null"),
            ("first([1, 2, 3])", "1"),
            ("first([])", "null"),
            ("first(1)", "argument to `first` must be ARRAY, got 1"),
            ("last([1, 2, 3])", "3"),
            ("last([])", "null"),
            ("last(1)", "argument to `last` must be ARRAY, got 1"),
            ("rest([1, 2, 3])", "[2, 3]"),
            ("rest([])", "null"),
            ("push([], 1)", "[1]"),
            ("push(1, 1)", "argument to `push` must be ARRAY, got 1"),
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_array_literals() {
        let test_case = [("[1, 2 * 2, 3 + 3]", "[1, 4, 6]")];
        apply_test(&test_case);
    }

    #[test]
    fn test_array_index_expressions() {
        let test_case = [
            ("[1, 2, 3][0]", "1"),
            ("[1, 2, 3][1]", "2"),
            ("[1, 2, 3][2]", "3"),
            ("let i = 0; [1][i];", "1"),
            ("[1, 2, 3][1 + 1];", "3"),
            ("let myArray = [1, 2, 3]; myArray[2];", "3"),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                "6",
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                "2",
            ),
            ("[1, 2, 3][3]", "null"),
            ("[1, 2, 3][-1]", "null"),
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"let two = "two"
                       {
                         "one": 10 - 9,
                         two: 1 + 1,
                         "thr" + "ee": 6 / 2,
                         4: 4,
                         true: 5,
                         false: 6
                       }"#;

        let env: Env = Rc::new(RefCell::new(Default::default()));
        let program = parse(input).unwrap();
        let evaluated = eval(program, &env).unwrap();

        let expected = Rc::new(Object::Hash(
            vec![
                (
                    Rc::new(Object::String("one".to_string())),
                    Rc::new(Object::Integer(1)),
                ),
                (
                    Rc::new(Object::String("two".to_string())),
                    Rc::new(Object::Integer(2)),
                ),
                (
                    Rc::new(Object::String("three".to_string())),
                    Rc::new(Object::Integer(3)),
                ),
                (Rc::new(Object::Integer(4)), Rc::new(Object::Integer(4))),
                (Rc::new(Object::Boolean(true)), Rc::new(Object::Integer(5))),
                (Rc::new(Object::Boolean(false)), Rc::new(Object::Integer(6))),
            ]
            .into_iter()
            .collect::<HashMap<Rc<Object>, Rc<Object>>>(),
        ));

        assert_eq!(evaluated, expected);
    }

    #[test]
    fn test_hash_index_expressions() {
        let test_case = [
            (r#"{"foo": 5}["foo"]"#, "5"),
            (r#"{"foo": 5}["bar"]"#, "null"),
            (r#"let key = "foo"; {"foo": 5}[key]"#, "5"),
            (r#"{}["foo"]"#, "null"),
            (r#"{5: 5}[5]"#, "5"),
            (r#"{true: 5}[true]"#, "5"),
            (r#"{false: 5}[false]"#, "5"),
        ];
        apply_test(&test_case);
    }
}
