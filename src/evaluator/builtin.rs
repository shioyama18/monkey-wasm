use crate::evaluator::error::*;
use crate::evaluator::object::*;

use std::fmt;
use std::rc::Rc;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Builtin {
    Len,
    First,
    Last,
    Rest,
    Push,
    Puts,
}

impl Builtin {
    pub fn lookup(name: &str) -> Option<Object> {
        match name {
            "len" => Some(Object::Builtin(Builtin::Len)),
            "first" => Some(Object::Builtin(Builtin::First)),
            "last" => Some(Object::Builtin(Builtin::Last)),
            "rest" => Some(Object::Builtin(Builtin::Rest)),
            "push" => Some(Object::Builtin(Builtin::Push)),
            "puts" => Some(Object::Builtin(Builtin::Puts)),
            _ => None,
        }
    }

    pub fn apply(&self, args: &[Rc<Object>]) -> Result<Rc<Object>, EvaluatorError> {
        match self {
            Builtin::Len => {
                check_argument_count(1, args.len())?;

                match &*args[0] {
                    Object::String(s) => Ok(Rc::new(Object::Integer(s.len() as i32))),
                    Object::Array(a) => Ok(Rc::new(Object::Integer(a.len() as i32))),
                    obj => Err(EvaluatorError::new(format!(
                        "argument to `len` not supported, got {}",
                        obj
                    ))),
                }
            }
            Builtin::First => {
                check_argument_count(1, args.len())?;

                match &*args[0] {
                    Object::Array(a) => match a.first() {
                        Some(obj) => Ok(Rc::clone(obj)),
                        None => Ok(Rc::new(Object::Null)),
                    },
                    obj => Err(EvaluatorError::new(format!(
                        "argument to `first` must be ARRAY, got {}",
                        obj
                    ))),
                }
            }
            Builtin::Last => {
                check_argument_count(1, args.len())?;

                match &*args[0] {
                    Object::Array(a) => match a.last() {
                        Some(obj) => Ok(Rc::clone(obj)),
                        None => Ok(Rc::new(Object::Null)),
                    },
                    obj => Err(EvaluatorError::new(format!(
                        "argument to `last` must be ARRAY, got {}",
                        obj
                    ))),
                }
            }
            Builtin::Rest => {
                check_argument_count(1, args.len())?;

                match &*args[0] {
                    Object::Array(a) => {
                        let length = a.len();
                        if length > 0 {
                            let new_array = a[1..length].to_vec();
                            Ok(Rc::new(Object::Array(new_array)))
                        } else {
                            Ok(Rc::new(Object::Null))
                        }
                    }
                    obj => Err(EvaluatorError::new(format!(
                        "argument to `rest` must be ARRAY, got {}",
                        obj
                    ))),
                }
            }
            Builtin::Push => {
                check_argument_count(2, args.len())?;

                let array = args.first().unwrap();
                let obj = Rc::clone(args.last().unwrap());

                match &**array {
                    Object::Array(a) => {
                        let mut new_array = a.clone();
                        new_array.push(obj);
                        Ok(Rc::new(Object::Array(new_array)))
                    }
                    obj => Err(EvaluatorError::new(format!(
                        "argument to `push` must be ARRAY, got {}",
                        obj
                    ))),
                }
            }
            Builtin::Puts => {
                args.iter().for_each(|obj| println!("{}", obj));
                Ok(Rc::new(Object::Null))
            }
        }
    }
}

fn check_argument_count(expected: usize, actual: usize) -> Result<(), EvaluatorError> {
    if expected != actual {
        Err(EvaluatorError::new(format!(
            "invalid number of arguments: expected={}, got={}",
            expected, actual
        )))
    } else {
        Ok(())
    }
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Builtin::Len => write!(f, "len"),
            Builtin::First => write!(f, "first"),
            Builtin::Last => write!(f, "last"),
            Builtin::Rest => write!(f, "rest"),
            Builtin::Push => write!(f, "push"),
            Builtin::Puts => write!(f, "puts"),
        }
    }
}
