use crate::ast::*;
use crate::evaluator::builtin::Builtin;
use crate::evaluator::environment::Env;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    String(String),
    Array(Vec<Rc<Object>>),
    Hash(HashMap<Rc<Object>, Rc<Object>>),
    Null,
    ReturnValue(Rc<Object>),
    Function(Vec<String>, BlockStatement, Env),
    Builtin(Builtin),
    Empty,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::String(s) => write!(f, "{}", s),
            Object::Array(a) => write!(
                f,
                "[{}]",
                a.iter()
                    .map(|obj| obj.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Object::Hash(map) => write!(
                f,
                "{{{}}}",
                map.iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(obj) => write!(f, "{}", obj),
            Object::Function(params, _body, _env) => {
                let params = params.join(",");
                write!(f, "fn({}) {{...}}", params,)
            }
            Object::Builtin(b) => write!(f, "Builtin Function: {}", b),
            Object::Empty => Ok(()),
        }
    }
}

impl Object {
    pub fn is_int(&self) -> bool {
        if let Object::Integer(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_empty(&self) -> bool {
        if let Object::Empty = self {
            true
        } else {
            false
        }
    }

    pub fn is_hashable(&self) -> bool {
        match self {
            Object::Integer(_) | Object::Boolean(_) | Object::String(_) => true,
            _ => false,
        }
    }
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Integer(i) => i.hash(state),
            Object::Boolean(b) => b.hash(state),
            Object::String(s) => s.hash(state),
            _ => "".hash(state),
        }
    }
}
