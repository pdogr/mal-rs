#![feature(box_into_inner)]
extern crate rustyline;
use std::{cell::RefCell, rc::Rc};

pub use rustyline::{error::ReadlineError, Editor};
extern crate nom;

pub mod ast;
pub mod env;
pub mod eval;
pub mod lexer;
pub(crate) mod parser;
pub mod tokens;
pub(crate) mod utils;
use ast::*;
pub use env::MalEnv;
pub use eval::eval;
pub use lexer::MalLexer;
pub use nom::Finish;
pub use parser::parse_type as parse;
use tokens::*;
type Result<R> = std::result::Result<R, Box<dyn std::error::Error>>;

#[allow(unused_macros)]
macro_rules! binary_op {
    ($map: ident, $sym: literal, $op: tt) => {{
        $map.insert(
            MalSymbol::new($sym),
            MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
                let a1 = &args[0];
                let a2 = &args[1];
                if let (MalType::Int(i), MalType::Int(j)) = (a1, a2) {
                    return Ok(MalType::Int((i $op j)));
                }
                Err(format!("could not convert args to int64").into())
            }))),
        );
    }};
}

#[allow(unused_macros)]
macro_rules! binary_cmp {
    ($map: ident, $sym: literal, $op: tt) => {{
        $map.insert(
            MalSymbol::new($sym),
            MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
                let a1 = &args[0];
                let a2 = &args[1];
                if let (MalType::Int(i), MalType::Int(j)) = (a1, a2) {
                    return Ok(MalType::Bool((i $op j)));
                }
                Err(format!("could not convert args to int64").into())
            }))),
        );
    }};
}

pub fn print(mt: &MalType, print_readonly: bool) -> String {
    match mt {
        MalType::Str(s) => match print_readonly {
            true => format!("\"{}\"", utils::escape(s)),
            false => s.to_string(),
        },
        MalType::Nil => format!("nil"),
        MalType::Bool(b) => format!("{}", b),
        MalType::Int(i) => format!("{}", i),
        MalType::Float(fs) => format!("{}", fs),
        MalType::Keyword(k) => format!("{}", k),
        MalType::Symbol(s) => format!("{}", s),
        MalType::HashKey(k) => format!(":{}", k),
        MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => {
            let result = l
                .iter()
                .map(|mt| print(mt, print_readonly))
                .collect::<Vec<_>>()
                .join(" ");
            match mt {
                MalType::List(_) => format!("({})", result),
                MalType::Vector(_) => format!("[{}]", result),
                _ => unreachable!(),
            }
        }
        MalType::HashMap(MalHashMap(h)) => {
            let result = h
                .iter()
                .map(|(k, v)| format!("{} {}", print(k, print_readonly), print(v, print_readonly)))
                .collect::<Vec<_>>()
                .join(" ");
            format!("{{{}}}", result)
        }
        MalType::Quoted(q) => format!("({} {})", MalToken::Quote, print(q, print_readonly)),
        MalType::QuasiQuoted(q) => {
            format!("({} {})", MalToken::QuasiQuote, print(q, print_readonly))
        }
        MalType::Unquote(q) => format!("({} {})", MalToken::Unquote, print(q, print_readonly)),
        MalType::WithMeta(a, b) => format!(
            "({} {} {})",
            MalToken::WithMeta,
            print(b, print_readonly),
            print(a, print_readonly)
        ),
        MalType::SpliceUnquote(s) => {
            format!("({} {})", MalToken::SpliceUnquote, print(s, print_readonly))
        }
        MalType::Func(_func) => format!(""),
        MalType::Atom(a) => format!("(atom {})", print(&a.borrow(), print_readonly)),
    }
}

pub fn read_str(s: &str) -> Result<MalType> {
    let t: Vec<_> = MalLexer::lex(s)?
        .into_iter()
        .filter_map(|t| {
            if let MalToken::Sequence(s) = &t {
                if s.starts_with(";") {
                    return None;
                }
            }
            Some(t)
        })
        .collect();
    let t = MalTokens(t.as_slice());
    match parse(t).finish() {
        Ok((_, ast)) => Ok(ast),
        Err(e) => Err(format!("parsing error {:?}", e).into()),
    }
}

pub fn make_env() -> std::rc::Rc<MalEnv> {
    let env = MalEnv::new();
    binary_op!(env, "+", +);
    binary_op!(env, "-", -);
    binary_op!(env, "*", *);
    binary_op!(env, "/", /);
    env.insert(
        MalSymbol::new("pr-str"),
        MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
            let result = args
                .iter()
                .map(|arg| print(arg, true))
                .collect::<Vec<_>>()
                .join("");
            Ok(MalType::Str(result.into()))
        }))),
    );
    env.insert(
        MalSymbol::new("str"),
        MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
            let result = args
                .iter()
                .map(|arg| print(arg, false))
                .collect::<Vec<_>>()
                .join("");
            Ok(MalType::Str(result.into()))
        }))),
    );
    env.insert(
        MalSymbol::new("prn"),
        MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
            let result = args
                .iter()
                .map(|arg| print(arg, true))
                .collect::<Vec<_>>()
                .join(" ");
            println!("{}", result);
            Ok(MalType::Nil)
        }))),
    );
    env.insert(
        MalSymbol::new("println"),
        MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
            let result = args
                .iter()
                .map(|arg| print(arg, false))
                .collect::<Vec<_>>()
                .join(" ");
            println!("{}", result);
            Ok(MalType::Nil)
        }))),
    );
    env.insert(
        MalSymbol::new("list"),
        MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
            Ok(MalType::List(MalList::new(args)))
        }))),
    );
    env.insert(
        MalSymbol::new("list?"),
        MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
            if let MalType::List(_) = &args[0] {
                return Ok(MalType::Bool(true));
            }
            return Ok(MalType::Bool(false));
        }))),
    );
    env.insert(
        MalSymbol::new("empty?"),
        MalType::Func(Box::new(MalFunc::from_closure(
            |args: Vec<MalType>| match &args[0] {
                MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => {
                    if l.len() == 0 {
                        return Ok(MalType::Bool(true));
                    }
                    return Ok(MalType::Bool(false));
                }
                _ => Ok(MalType::Bool(false)),
            },
        ))),
    );
    env.insert(
        MalSymbol::new("count"),
        MalType::Func(Box::new(MalFunc::from_closure(
            |args: Vec<MalType>| match &args[0] {
                MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => {
                    Ok(MalType::Int(l.len() as i64))
                }
                _ => Ok(MalType::Int(0)),
            },
        ))),
    );
    env.insert(
        MalSymbol::new("="),
        MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
            Ok(MalType::Bool(args[0] == args[1]))
        }))),
    );
    binary_cmp!(env, "<", <);
    binary_cmp!(env, "<=", <=);
    binary_cmp!(env, ">", >);
    binary_cmp!(env, ">=", >=);
    env.insert(
        MalSymbol::new("read-string"),
        MalType::Func(Box::new(MalFunc::from_closure(
            |args: Vec<MalType>| match &args[0] {
                MalType::Str(s) => read_str(s),
                _ => Err(format!("expected string in read-string").into()),
            },
        ))),
    );
    env.insert(
        MalSymbol::new("slurp"),
        MalType::Func(Box::new(MalFunc::from_closure(
            |args: Vec<MalType>| match &args[0] {
                MalType::Str(s) => {
                    let contents = std::fs::read_to_string(&s.as_ref())?;
                    Ok(MalType::Str(contents.into()))
                }
                _ => Err(format!("expected filename in slurp").into()),
            },
        ))),
    );
    env.insert(
        MalSymbol::new("atom"),
        MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
            Ok(MalType::Atom(Rc::new(RefCell::new(args[0].clone()))))
        }))),
    );
    env.insert(
        MalSymbol::new("atom?"),
        MalType::Func(Box::new(MalFunc::from_closure(
            |args: Vec<MalType>| match &args[0] {
                MalType::Atom(_) => Ok(MalType::Bool(true)),
                _ => Ok(MalType::Bool(false)),
            },
        ))),
    );
    env.insert(
        MalSymbol::new("deref"),
        MalType::Func(Box::new(MalFunc::from_closure(
            |args: Vec<MalType>| match &args[0] {
                MalType::Atom(a) => Ok(a.borrow().clone()),
                _ => Err(format!("expected atom as argument of deref").into()),
            },
        ))),
    );
    env.insert(
        MalSymbol::new("reset!"),
        MalType::Func(Box::new(MalFunc::from_closure(
            |args: Vec<MalType>| match &args[0].clone() {
                MalType::Atom(a) => {
                    let val = args[1].clone();
                    a.replace(val.clone());
                    Ok(val)
                }
                _ => Err(format!("expected atom as 1st argument of reset!").into()),
            },
        ))),
    );
    env.insert(
        MalSymbol::new("swap!"),
        MalType::Func(Box::new(MalFunc::from_closure(
            |args: Vec<MalType>| match &args[0].clone() {
                MalType::Atom(a) => match args[1].clone() {
                    MalType::Func(f) => {
                        let mut func_args = vec![a.borrow().clone()];
                        func_args = func_args
                            .into_iter()
                            .chain(args[2..].to_vec().into_iter())
                            .collect();
                        let val = f.call(func_args)?;
                        a.replace(val.clone());
                        Ok(val)
                    }
                    _ => Err(format!("expected func as 2nd argument of swap").into()),
                },
                _ => Err(format!("expected atom as 1st argument of swap").into()),
            },
        ))),
    );
    env.insert(
        MalSymbol::new("cons"),
        MalType::Func(Box::new(MalFunc::from_closure(
            |args: Vec<MalType>| match &args[1] {
                MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => {
                    let mut vector = l.clone();
                    vector.insert(0, args[0].clone());
                    return Ok(MalType::List(MalList::new(vector)));
                }
                _ => Err(format!("expected list or vec as second argument").into()),
            },
        ))),
    );
    env.insert(
        MalSymbol::new("concat"),
        MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
            let l = args.iter().fold(Vec::new(), |mut acc, arg| match &arg {
                MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => {
                    acc.extend(l.to_vec());
                    acc
                }
                _ => acc,
            });
            Ok(MalType::List(MalList(l)))
        }))),
    );
    env.insert(
        MalSymbol::new("vec"),
        MalType::Func(Box::new(MalFunc::from_closure(
            |args: Vec<MalType>| match &args[0] {
                MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => {
                    Ok(MalType::Vector(MalVec(l.to_vec())))
                }
                _ => Err(format!("expected list or vec as arguments").into()),
            },
        ))),
    );
    env.insert(
        MalSymbol::new("nth"),
        MalType::Func(Box::new(MalFunc::from_closure(
            |args: Vec<MalType>| match &args[0] {
                MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => match &args[1] {
                    MalType::Int(ref i) => {
                        let idx = *i as usize;
                        if idx >= l.len() {
                            return Err(format!("nth: index out of range").into());
                        }
                        return Ok(l[idx].clone());
                    }
                    _ => Err(format!("expected index as second arg").into()),
                },
                _ => Err(format!("expected list or vec as arguments").into()),
            },
        ))),
    );
    env.insert(
        MalSymbol::new("first"),
        MalType::Func(Box::new(MalFunc::from_closure(
            |args: Vec<MalType>| match &args[0] {
                MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => {
                    if l.is_empty() {
                        return Ok(MalType::Nil);
                    }
                    return Ok(l[0].clone());
                }
                MalType::Nil => Ok(MalType::Nil),
                _ => Err(format!("expected list or vec as arguments").into()),
            },
        ))),
    );
    env.insert(
        MalSymbol::new("rest"),
        MalType::Func(Box::new(MalFunc::from_closure(
            |args: Vec<MalType>| match &args[0] {
                MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => {
                    if l.is_empty() {
                        return Ok(MalType::List(MalList::new(vec![])));
                    }
                    return Ok(MalType::List(MalList::new(l[1..].to_vec())));
                }
                MalType::Nil => Ok(MalType::List(MalList::new(vec![]))),
                _ => Err(format!("expected list or vec as arguments").into()),
            },
        ))),
    );

    return env;
}
