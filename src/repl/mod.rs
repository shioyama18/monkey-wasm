use crate::evaluator::environment::*;
use crate::evaluator::*;
use crate::parser::*;

use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

pub fn start() {
    let env: Rc<RefCell<Environment>> = Rc::new(RefCell::new(Default::default()));

    loop {
        print!(">> ");
        std::io::stdout().flush().expect("Flush to stdout failed.");

        let mut input = String::new();
        let _ = std::io::stdin().read_line(&mut input);

        if input == "exit\n" {
            break;
        }

        match parse(&input) {
            Ok(node) => match eval(node, &Rc::clone(&env)) {
                Ok(evaluated) => {
                    if !evaluated.is_empty() {
                        println!("{}", evaluated)
                    }
                }
                Err(err) => eprintln!("{}", err),
            },
            Err(errors) => {
                for e in errors {
                    eprintln!("{}", e);
                }
            }
        }
    }
}
