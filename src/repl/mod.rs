use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::evaluator::environment::*;
use crate::evaluator::*;
use crate::parser::*;

use std::cell::RefCell;
use std::rc::Rc;

pub fn start() {
    let mut rl = Editor::<()>::new();
    let env: Rc<RefCell<Environment>> = Rc::new(RefCell::new(Default::default()));

    println!("Welcome to the Monkey programming language!");
    println!("Feel free to type in commands");

    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                rl.add_history_entry(<String as AsRef<str>>::as_ref(&line));

                if line.trim() == "exit" {
                    break;
                }

                match parse(&line) {
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
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
