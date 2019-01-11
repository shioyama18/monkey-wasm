use wasm_bindgen::prelude::*;

use crate::evaluator::environment::*;
use crate::evaluator::*;
use crate::parser::*;

use std::cell::RefCell;
use std::rc::Rc;

#[wasm_bindgen]
pub fn monkey_eval(input: &str) -> String {
    let env: Rc<RefCell<Environment>> = Rc::new(RefCell::new(Default::default()));

    match parse(&input) {
        Ok(node) => match eval(node, &Rc::clone(&env)) {
            Ok(evaluated) => evaluated.to_string(),
            Err(err) => err.to_string(),
        },
        Err(errors) => errors
            .into_iter()
            .map(|e| format!("{}\n", e))
            .collect::<String>(),
    }
}
