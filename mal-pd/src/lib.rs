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
            symbol!($sym),
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
            symbol!($sym),
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

#[allow(unused_macros)]
macro_rules! symbol {
    ($sym: literal) => {{
        MalSymbol::new($sym)
    }};
}

#[allow(unused_macros)]
macro_rules! mal_func {
    ($f: expr) => {{
        MalType::Func(Box::new(MalFunc::from_closure($f)))
    }};
}

#[allow(unused_macros)]
macro_rules! mal_str {
    ($s: expr) => {{
        MalType::Str($s)
    }};
}

#[allow(unused_macros)]
macro_rules! mal_list {
    ($s: expr) => {{
        MalType::List(MalList::new($s))
    }};
}

#[allow(unused_macros)]
macro_rules! mal_vec {
    ($s: expr) => {{
        MalType::Vector(MalVec::new($s))
    }};
}

#[allow(unused_macros)]
macro_rules! mal_bool {
    ($b: expr) => {{
        MalType::Bool($b)
    }};
}

#[allow(unused_macros)]
macro_rules! mal_int {
    ($i: expr) => {{
        MalType::Int($i)
    }};
}

#[allow(unused_macros)]
macro_rules! mal_atom {
    ($a: expr) => {{
        MalType::Atom(Rc::new(RefCell::new($a)))
    }};
}

#[inline]
fn gil(mt: &MalType) -> Result<&Vec<MalType>> {
    match mt {
        MalType::List(MalList(l)) => Ok(l),
        _ => Err(format!("expected list").into()),
    }
}

#[inline]
fn giv(mt: &MalType) -> Result<&Vec<MalType>> {
    match mt {
        MalType::Vector(MalVec(l)) => Ok(l),
        _ => Err(format!("expected vec").into()),
    }
}

#[inline]
fn gilv(mt: &MalType) -> Result<&Vec<MalType>> {
    match mt {
        MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => Ok(l),
        _ => Err(format!("expected list or vec").into()),
    }
}

#[inline]
fn gis(mt: &MalType) -> Result<&Rc<str>> {
    match mt {
        MalType::Str(s) => Ok(s),
        _ => Err(format!("expected str").into()),
    }
}

#[inline]
fn gia(mt: &MalType) -> Result<&Rc<RefCell<MalType>>> {
    match mt {
        MalType::Atom(a) => Ok(a),
        _ => Err(format!("expected atom").into()),
    }
}

#[inline]
fn gii(mt: &MalType) -> Result<i64> {
    match mt {
        MalType::Int(i) => Ok(*i),
        _ => Err(format!("expected atom").into()),
    }
}

#[inline]
fn gif(mt: &MalType) -> Result<&MalFunc> {
    match mt {
        MalType::Func(f) => Ok(f),
        _ => Err(format!("expected atom").into()),
    }
}

#[inline]
fn exists(v: &Vec<MalType>, i: usize) -> Result<()> {
    if i < v.len() {
        return Ok(());
    }
    Err(format!("need atleast {} arguments", i + 1).into())
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
        symbol!("pr-str"),
        mal_func!(|args| {
            let result = args
                .iter()
                .map(|arg| print(arg, true))
                .collect::<Vec<_>>()
                .join("");
            Ok(mal_str!(result.into()))
        }),
    );
    env.insert(
        symbol!("str"),
        mal_func!(|args| {
            let result = args
                .iter()
                .map(|arg| print(arg, false))
                .collect::<Vec<_>>()
                .join("");
            Ok(mal_str!(result.into()))
        }),
    );
    env.insert(
        symbol!("prn"),
        mal_func!(|args| {
            let result = args
                .iter()
                .map(|arg| print(arg, true))
                .collect::<Vec<_>>()
                .join(" ");
            println!("{}", result);
            Ok(MalType::Nil)
        }),
    );
    env.insert(
        symbol!("println"),
        mal_func!(|args| {
            let result = args
                .iter()
                .map(|arg| print(arg, false))
                .collect::<Vec<_>>()
                .join(" ");
            println!("{}", result);
            Ok(MalType::Nil)
        }),
    );
    env.insert(symbol!("list"), mal_func!(|args| Ok(mal_list!(args))));
    env.insert(
        symbol!("list?"),
        mal_func!(
            |args| gil(&args[0]).map_or_else(|_| Ok(mal_bool!(false)), |_| Ok(mal_bool!(true)))
        ),
    );
    env.insert(
        symbol!("empty?"),
        mal_func!(|args| gilv(&args[0]).map_or_else(
            |_| Ok(mal_bool!(false)),
            |l| {
                if l.len() == 0 {
                    return Ok(mal_bool!(true));
                }
                return Ok(mal_bool!(false));
            }
        )),
    );
    env.insert(
        symbol!("count"),
        mal_func!(|args| gilv(&args[0])
            .map_or_else(|_| Ok(mal_int!(0)), |l| Ok(mal_int!(l.len() as i64)))),
    );
    env.insert(
        symbol!("="),
        mal_func!(|args| Ok(mal_bool!(args[0] == args[1]))),
    );
    binary_cmp!(env, "<", <);
    binary_cmp!(env, "<=", <=);
    binary_cmp!(env, ">", >);
    binary_cmp!(env, ">=", >=);
    env.insert(
        symbol!("read-string"),
        mal_func!(|args| {
            let s = gis(&args[0])?;
            read_str(s)
        }),
    );
    env.insert(
        symbol!("slurp"),
        mal_func!(|args| {
            let s = gis(&args[0])?;
            let contents = std::fs::read_to_string(&s.as_ref())?;
            Ok(mal_str!(contents.into()))
        }),
    );
    env.insert(
        symbol!("atom"),
        mal_func!(|args| Ok(mal_atom!(args[0].clone()))),
    );
    env.insert(
        symbol!("atom?"),
        mal_func!(
            |args| gia(&args[0]).map_or_else(|_| Ok(mal_bool!(false)), |_| Ok(mal_bool!(true)))
        ),
    );
    env.insert(
        symbol!("deref"),
        mal_func!(|args| {
            let a = gia(&args[0])?;
            Ok(a.borrow().clone())
        }),
    );
    env.insert(
        symbol!("reset!"),
        mal_func!(|args| {
            exists(&args, 1)?;
            let a = gia(&args[0])?;
            let val = args[1].clone();
            a.replace(val.clone());
            Ok(val)
        }),
    );
    env.insert(
        symbol!("swap!"),
        mal_func!(|args| {
            exists(&args, 1)?;
            let a = gia(&args[0])?;
            let f = gif(&args[1])?;
            let mut func_args = vec![a.borrow().clone()];
            func_args = func_args
                .into_iter()
                .chain(args[2..].to_vec().into_iter())
                .collect();
            let val = f.call(func_args)?;
            a.replace(val.clone());
            Ok(val)
        }),
    );
    env.insert(
        symbol!("cons"),
        mal_func!(|args| {
            exists(&args, 1)?;
            let l = gilv(&args[1])?;
            let mut vector = l.clone();
            vector.insert(0, args[0].clone());
            Ok(mal_list!(vector))
        }),
    );
    env.insert(
        symbol!("concat"),
        mal_func!(|args| {
            let l = args.iter().fold(Vec::new(), |mut acc, arg| {
                let next = gilv(&arg).map_or_else(|_| None, |l| Some(l.to_vec()));
                if let Some(elt) = next {
                    acc.extend(elt)
                }
                acc
            });
            Ok(MalType::List(MalList(l)))
        }),
    );
    env.insert(
        symbol!("vec"),
        mal_func!(|args| {
            let l = gilv(&args[0])?;
            Ok(mal_vec!(l.to_vec()))
        }),
    );
    env.insert(
        symbol!("nth"),
        mal_func!(|args| {
            exists(&args, 1)?;
            let l = gilv(&args[0])?;
            let idx = gii(&args[1])? as usize;
            if idx >= l.len() {
                return Err(format!("nth: index out of range").into());
            }
            return Ok(l[idx].clone());
        }),
    );
    env.insert(
        symbol!("first"),
        mal_func!(|args| {
            match &args[0] {
                MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => {
                    if l.is_empty() {
                        return Ok(MalType::Nil);
                    }
                    return Ok(l[0].clone());
                }
                MalType::Nil => Ok(MalType::Nil),
                _ => Err(format!("expected list or vec as arguments").into()),
            }
        }),
    );
    env.insert(
        symbol!("rest"),
        mal_func!(|args| match &args[0] {
            MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => {
                if l.is_empty() {
                    return Ok(MalType::List(MalList::new(vec![])));
                }
                return Ok(MalType::List(MalList::new(l[1..].to_vec())));
            }
            MalType::Nil => Ok(MalType::List(MalList::new(vec![]))),
            _ => Err(format!("expected list or vec").into()),
        }),
    );

    return env;
}
