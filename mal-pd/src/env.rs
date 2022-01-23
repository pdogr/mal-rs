use crate::{
    ast::{MalList, MalSymbol, MalType},
    print, Result,
};
use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MalEnv {
    map: RefCell<HashMap<MalSymbol, MalType>>,
    pub outer: Option<Rc<MalEnv>>,
}

impl MalEnv {
    pub fn new() -> Rc<Self> {
        Rc::new(Self {
            map: RefCell::new(HashMap::new()),
            outer: None,
        })
    }

    pub fn insert(&self, ms: MalSymbol, mt: MalType) {
        self.map.borrow_mut().insert(ms, mt);
    }

    pub fn from_binds(binds: Vec<MalSymbol>, exprs: Vec<MalType>, outer: &Rc<MalEnv>) -> Rc<Self> {
        let env = MalEnv::detach(outer);
        for (i, ms) in binds.iter().enumerate() {
            if ms.strcmp("&") {
                env.insert(
                    binds[i + 1].clone(),
                    MalType::List(MalList::new(exprs[i..].to_vec())),
                );
                break;
            } else {
                env.insert(ms.clone(), exprs[i].clone());
            }
        }
        env
    }

    pub fn detach(outer: &Rc<MalEnv>) -> Rc<Self> {
        Rc::new(Self {
            map: RefCell::new(HashMap::new()),
            outer: Some(outer.clone()),
        })
    }

    pub fn borrow(&self) -> Ref<'_, HashMap<MalSymbol, MalType>> {
        Ref::map(self.map.borrow(), |m| m)
    }

    pub fn has(&self, ms: &MalSymbol) -> bool {
        self.map.borrow().contains_key(ms)
    }

    pub fn find(env: &Rc<MalEnv>, ms: &MalSymbol) -> Option<Rc<MalEnv>> {
        if env.has(ms) {
            return Some(env.clone());
        }
        if env.outer.is_some() {
            return MalEnv::find(env.outer.as_ref().unwrap(), ms);
        }
        None
    }

    pub fn set(&self, k: &MalType, v: MalType) -> Result<()> {
        if let MalType::Symbol(s) = k {
            self.map.borrow_mut().insert(s.clone(), v);
            return Ok(());
        }
        Err(format!("unable to set on non symbol, found {}", print(k, true)).into())
    }
}
