use crate::{eval::eval, MalEnv, Result};
use std::{
    cell::RefCell,
    fmt::{Display, Formatter},
    rc::Rc,
};

#[derive(Debug, Clone)]
pub enum MalType {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(Rc<str>),
    Keyword(&'static str),
    Symbol(MalSymbol),
    HashKey(Rc<str>),
    List(MalList),
    Vector(MalVec),
    HashMap(MalHashMap),
    Func(Box<MalFunc>),
    WithMeta(Box<MalType>, Box<MalType>),
    Quoted(Box<MalType>),
    QuasiQuoted(Box<MalType>),
    Unquote(Box<MalType>),
    SpliceUnquote(Box<MalType>),
    Atom(Rc<RefCell<MalType>>),
}

impl PartialEq for MalType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::Str(l0), Self::Str(r0)) => l0 == r0,
            (Self::Keyword(l0), Self::Keyword(r0)) => l0 == r0,
            (Self::Symbol(l0), Self::Symbol(r0)) => l0 == r0,
            (Self::HashKey(l0), Self::HashKey(r0)) => l0 == r0,
            (Self::List(l0), Self::List(r0)) => l0 == r0,
            (Self::Vector(l0), Self::Vector(r0)) => l0 == r0,
            (Self::Vector(MalVec(v)), Self::List(MalList(l)))
            | (Self::List(MalList(l)), Self::Vector(MalVec(v))) => l == v,
            (Self::HashMap(l0), Self::HashMap(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Eq for MalType {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MalSymbol {
    ident: Rc<str>,
}

impl Display for MalSymbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl MalSymbol {
    pub fn new(ident: &str) -> Self {
        Self {
            ident: ident.into(),
        }
    }

    pub fn strcmp(&self, o: &str) -> bool {
        self.ident.as_ref() == o
    }
}

impl Into<MalSymbol> for &str {
    fn into(self) -> MalSymbol {
        MalSymbol { ident: self.into() }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MalList(pub Vec<MalType>);
impl MalList {
    pub fn new(types: Vec<MalType>) -> Self {
        Self(types)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MalVec(pub Vec<MalType>);
impl MalVec {
    pub fn new(types: Vec<MalType>) -> Self {
        Self(types)
    }
}

// MalType -> MalType, MalAtom => HashKey | Literal::Str
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MalHashMap(pub Vec<(MalType, MalType)>);

impl MalHashMap {
    pub fn new(v: Vec<(MalType, MalType)>) -> Self {
        Self(v)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MalFunc {
    args: Vec<MalSymbol>,
    body: MalType,
    closure: Option<fn(Vec<MalType>) -> Result<MalType>>,
    env: Option<Rc<MalEnv>>,
    is_macro: bool,
}

impl MalFunc {
    pub fn from_closure(closure: fn(Vec<MalType>) -> Result<MalType>) -> Self {
        Self {
            args: Vec::new(),
            body: MalType::Nil,
            closure: Some(closure),
            env: None,
            is_macro: false,
        }
    }

    pub fn from_binds(args: Vec<MalSymbol>, body: MalType, env: &Rc<MalEnv>) -> Self {
        Self {
            args,
            body,
            closure: None,
            env: Some(env.clone()),
            is_macro: false,
        }
    }

    pub fn call(&self, exprs: Vec<MalType>) -> Result<MalType> {
        if let Some(f) = self.closure {
            (f)(exprs)
        } else {
            let env = MalEnv::from_binds(self.args.clone(), exprs, &self.env.clone().unwrap());
            return eval(self.body.clone(), env);
        }
    }

    pub fn set_macro(&mut self) {
        self.is_macro = true;
    }

    pub fn is_macro(&self) -> bool {
        self.is_macro
    }
}
