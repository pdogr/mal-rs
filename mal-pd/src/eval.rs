use crate::{
    ast::{MalFunc, MalHashMap, MalList, MalSymbol, MalType, MalVec},
    env::MalEnv,
    Result,
};
use std::rc::Rc;

pub fn not_unquote(l: Vec<MalType>) -> Result<MalType> {
    let mut l = l.clone();
    l.reverse();
    let mut acc = Vec::new();
    for elt in l.into_iter() {
        match &elt {
            MalType::List(MalList(elt_l)) if !elt_l.is_empty() => match &elt_l[0] {
                MalType::Symbol(ms) if ms.strcmp("splice-unquote") => {
                    acc = vec![
                        MalType::Symbol(MalSymbol::new("concat")),
                        elt_l[1].clone(),
                        MalType::List(MalList(acc)),
                    ]
                }
                _ => {
                    acc = vec![
                        MalType::Symbol(MalSymbol::new("cons")),
                        quasiquote(elt)?,
                        MalType::List(MalList(acc)),
                    ]
                }
            },
            _ => {
                acc = vec![
                    MalType::Symbol(MalSymbol::new("cons")),
                    quasiquote(elt)?,
                    MalType::List(MalList(acc)),
                ]
            }
        }
    }
    Ok(MalType::List(MalList(acc)))
}

pub fn quasiquote(mt: MalType) -> Result<MalType> {
    match &mt {
        MalType::List(MalList(l)) => {
            if l.is_empty() {
                return Ok(mt);
            }
            match &l[0] {
                MalType::Symbol(ms) if ms.strcmp("unquote") => Ok(l[1].clone()),
                _ => not_unquote(l.to_vec()),
            }
        }
        MalType::Symbol(_) | MalType::HashMap(_) => Ok(MalType::List(MalList(vec![
            MalType::Symbol(MalSymbol::new("quote")),
            mt,
        ]))),
        MalType::Vector(MalVec(l)) => Ok(MalType::List(MalList(vec![
            MalType::Symbol(MalSymbol::new("vec")),
            not_unquote(l.to_vec())?,
        ]))),
        _ => Ok(mt),
    }
}

pub fn is_macro_call(ast: MalType, env: &Rc<MalEnv>) -> Option<MalType> {
    match &ast {
        MalType::List(MalList(l)) if !l.is_empty() => match &l[0] {
            MalType::Symbol(s) => {
                if let Some(env) = MalEnv::find(env, &s) {
                    if let MalType::Func(f) = env.as_ref().borrow().get(&s).unwrap() {
                        if f.is_macro() {
                            return Some(MalType::Func(f.clone()));
                        }
                    }
                }
                return None;
            }
            _ => return None,
        },
        _ => return None,
    }
}

pub fn macroexpand(mut ast: MalType, env: &Rc<MalEnv>) -> Result<(bool, MalType)> {
    let mut expanded: bool = false;
    while let Some(MalType::Func(f)) = is_macro_call(ast.clone(), env) {
        if let MalType::List(MalList(l)) = ast {
            match f.call(l[1..].to_vec()) {
                Ok(nast) => {
                    expanded = true;
                    ast = nast;
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
    }
    Ok((expanded, ast))
}

pub fn eval_ast(mt: MalType, env: &Rc<MalEnv>) -> Result<MalType> {
    if let MalType::Symbol(s) = mt {
        if let Some(env) = MalEnv::find(env, &s) {
            return Ok(env.as_ref().borrow().get(&s).unwrap().clone());
        }
        return Err(format!("symbol {} not found", s).into());
    } else if let MalType::List(l) = mt {
        let mut v: Vec<MalType> = Vec::new();
        for item in l.0 {
            let res = eval(item, env.clone())?;
            v.push(res);
        }
        return Ok(MalType::List(MalList::new(v)));
    } else if let MalType::Vector(l) = mt {
        let mut v: Vec<MalType> = Vec::new();
        for item in l.0 {
            let res = eval(item, env.clone())?;
            v.push(res);
        }
        return Ok(MalType::Vector(MalVec::new(v)));
    } else if let MalType::HashMap(h) = mt {
        let mut hm: Vec<(MalType, MalType)> = Vec::new();
        for (k, v) in h.0 {
            let v = eval(v, env.clone())?;
            hm.push((k, v))
        }
        return Ok(MalType::HashMap(MalHashMap::new(hm)));
    }
    Ok(mt)
}

pub fn eval(mut mt: MalType, mut env: Rc<MalEnv>) -> Result<MalType> {
    loop {
        if let MalType::List(MalList(l)) = &mt {
            if l.len() == 0 {
                return Ok(mt);
            }
            match macroexpand(mt.clone(), &env) {
                Ok((true, ast)) => match ast {
                    MalType::List(_) => {
                        mt = ast;
                        continue;
                    }
                    _ => {
                        return eval_ast(ast, &env);
                    }
                },
                Err(e) => return Err(e),
                _ => {}
            };

            match &l[0] {
                MalType::Symbol(ms) if ms.strcmp("def!") => {
                    let k = &l[1];
                    let v = eval(l[2].clone(), env.clone())?;
                    env.set(k, v.clone())?;
                    return Ok(v);
                }
                MalType::Symbol(ms) if ms.strcmp("let*") => {
                    let new_env = MalEnv::detach(&env);
                    let (a1, a2) = (l[1].clone(), l[2].clone());
                    match a1 {
                        MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => {
                            if l.len() & 1 != 0 {
                                return Err("even amount of bindings expected".to_string().into());
                            }
                            for kv in l.chunks(2) {
                                let (k, v) = (&kv[0], &kv[1]);
                                let v = eval(v.clone(), new_env.clone())?;
                                new_env.set(k, v)?;
                            }
                        }
                        _ => {
                            return Err("Expected list or vector".to_string().into());
                        }
                    }
                    env = new_env;
                    mt = a2;
                    continue;
                }
                MalType::Symbol(ms) if ms.strcmp("quote") => {
                    return Ok(l[1].clone());
                }
                MalType::Symbol(ms) if ms.strcmp("quasiquote") => {
                    mt = quasiquote(l[1].clone())?;
                    continue;
                }
                MalType::Symbol(ms) if ms.strcmp("quasiquoteexpand") => {
                    return quasiquote(l[1].clone());
                }
                MalType::Symbol(ms) if ms.strcmp("do") => {
                    for mt in l[1..l.len() - 1].iter() {
                        eval(mt.clone(), env.clone())?;
                    }
                    mt = l[l.len() - 1].clone();
                    continue;
                }
                MalType::Symbol(ms) if ms.strcmp("if") => {
                    let cond = eval(l[1].clone(), env.clone())?;
                    if l.len() < 3 {
                        return Err(format!("if expects three arguments").into());
                    }
                    let a = match cond {
                        MalType::Nil | MalType::Bool(false) => {
                            if l.len() >= 4 {
                                l[3].clone()
                            } else {
                                MalType::Nil
                            }
                        }
                        _ => l[2].clone(),
                    };
                    mt = a;
                    continue;
                }
                MalType::Symbol(ms) if ms.strcmp("fn*") => {
                    let a1 = l[1].clone();
                    let mut args = Vec::new();
                    match a1 {
                        MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => {
                            for mt in l.into_iter() {
                                if let MalType::Symbol(ms) = mt {
                                    args.push(ms);
                                } else {
                                    return Err("Expected symbol in args".into());
                                }
                            }
                        }
                        _ => {
                            return Err("Expected list or vector".to_string().into());
                        }
                    }
                    let body = l[2].clone();
                    return Ok(MalType::Func(Box::new(MalFunc::from_binds(
                        args, body, &env,
                    ))));
                }
                MalType::Symbol(ms) if ms.strcmp("eval") => {
                    mt = eval(l[1].clone(), env.clone())?;
                    if let Some(e) = &env.outer {
                        env = e.clone();
                    }
                    continue;
                }
                MalType::Symbol(ms) if ms.strcmp("defmacro!") => {
                    let k = &l[1];
                    let v = eval(l[2].clone(), env.clone())?;
                    match v {
                        MalType::Func(mut f) => {
                            f.set_macro();
                            let nv = MalType::Func(f);
                            env.set(k, nv.clone())?;
                            return Ok(nv);
                        }
                        _ => {
                            return Err("expected function in defmacro! definition"
                                .to_string()
                                .into());
                        }
                    };
                }
                MalType::Symbol(ms) if ms.strcmp("macroexpand") => {
                    let ast = &l[1];
                    return macroexpand(ast.clone(), &env).map(|(_, ast)| ast);
                }
                _ => {
                    if let MalType::List(MalList(l)) = eval_ast(mt, &env)? {
                        let f = l[0].clone();
                        if let MalType::Func(f) = f {
                            return f.call(l[1..].to_vec());
                        } else {
                            return Err("expected function as first argument".to_string().into());
                        }
                    }
                    return Err("expected function and args in a list".to_string().into());
                }
            };
        } else {
            return eval_ast(mt, &env);
        }
    }
}
