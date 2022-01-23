use crate::{ast::*, tokens::*};
use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::anychar,
    combinator::{complete, map, map_res, opt, recognize, value},
    multi::{many0, many1},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::borrow::Borrow;

#[allow(unused_macros)]
macro_rules! tag {
    ($i:ident::$b:ident($s: ident)) => {
        move |input: MalTokens<'a>| {
            let (_input, r) = take1(input)?;
            if let Some($i::$b($s)) = r.0.get(0) {
                return Ok((_input, $s.borrow()));
            }
            return Err(nom::Err::Error(nom::error::Error {
                input: _input,
                code: nom::error::ErrorKind::Tag,
            }));
        }
    };
    ($i:ident::$b:ident) => {
        tag(MalTokens(&[$i::$b]))
    };
}

#[allow(unused_macros)]
macro_rules! keyword {
    ($k: literal) => {
        complete(tag($k))
    };
}

fn take1<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalTokens<'a>> {
    take(1usize)(i)
}

fn parse_keyword<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalType> {
    map_res(tag!(MalToken::Sequence(s)), move |s: &str| {
        alt::<_, _, nom::error::Error<&str>, _>((
            value(MalType::Nil, keyword!("nil")),
            value(MalType::Bool(true), keyword!("true")),
            value(MalType::Bool(false), keyword!("false")),
        ))(s)
        .map(|(_, r)| r)
    })(i)
}

fn parse_symbol<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalSymbol> {
    map(tag!(MalToken::Sequence(s)), MalSymbol::new)(i)
}

fn parse_hashkey<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalType> {
    map_res(tag!(MalToken::Sequence(s)), move |s: &str| {
        preceded::<_, _, _, nom::error::Error<&str>, _, _>(tag(":"), recognize(many1(anychar)))(s)
            .map(|(_, r)| MalType::HashKey(r.into()))
    })(i)
}

fn parse_list<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalList> {
    map(opt(many0(parse_type)), |ws| {
        MalList::new(ws.unwrap_or_default())
    })(i)
}

fn parse_vector<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalVec> {
    map(opt(many0(parse_type)), |ws| {
        MalVec::new(ws.unwrap_or_default())
    })(i)
}

fn parse_hashmap<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalHashMap> {
    map_res(opt(many0(parse_type)), |ws: Option<Vec<MalType>>| {
        let ws = ws.unwrap_or_default();
        if ws.len() & 1 == 1 {
            Err(nom::Err::Error(nom::error::Error {
                input: ws,
                code: nom::error::ErrorKind::Count,
            }))
        } else {
            let mut vec = Vec::new();
            for kv in ws.chunks_exact(2) {
                let (k, v) = kv.split_at(1);
                let k = match k.get(0).unwrap() {
                    MalType::Str(_) | MalType::HashKey(_) => k.get(0).unwrap(),
                    _ => {
                        return Err(nom::Err::Error(nom::error::Error {
                            input: ws,
                            code: nom::error::ErrorKind::Tag,
                        }));
                    }
                };
                let v = v.get(0).unwrap();
                vec.push((k.clone(), v.clone()));
            }
            Ok(MalHashMap::new(vec))
        }
    })(i)
}

pub fn parse_type<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalType> {
    alt((
        map(
            delimited(
                tag!(MalToken::LeftParen),
                parse_list,
                tag!(MalToken::RightParen),
            ),
            MalType::List,
        ),
        map(
            delimited(
                tag!(MalToken::LeftSquare),
                parse_vector,
                tag!(MalToken::RightSquare),
            ),
            MalType::Vector,
        ),
        map(
            delimited(
                tag!(MalToken::LeftBrace),
                parse_hashmap,
                tag!(MalToken::RightBrace),
            ),
            MalType::HashMap,
        ),
        map(preceded(tag!(MalToken::Quote), parse_type), |q| {
            MalType::List(MalList::new(vec![
                MalType::Symbol(MalSymbol::new("quote")),
                q,
            ]))
        }),
        map(preceded(tag!(MalToken::QuasiQuote), parse_type), |q| {
            MalType::List(MalList::new(vec![
                MalType::Symbol(MalSymbol::new("quasiquote")),
                q,
            ]))
        }),
        map(preceded(tag!(MalToken::Unquote), parse_type), |q| {
            MalType::List(MalList::new(vec![
                MalType::Symbol(MalSymbol::new("unquote")),
                q,
            ]))
        }),
        map(preceded(tag!(MalToken::Deref), parse_type), |q| {
            MalType::List(MalList::new(vec![
                MalType::Symbol(MalSymbol::new("deref")),
                q,
            ]))
        }),
        map(
            preceded(tag!(MalToken::WithMeta), tuple((parse_type, parse_type))),
            |(a, b)| MalType::WithMeta(Box::new(a), Box::new(b)),
        ),
        map(preceded(tag!(MalToken::SpliceUnquote), parse_type), |q| {
            MalType::List(MalList::new(vec![
                MalType::Symbol(MalSymbol::new("splice-unquote")),
                q,
            ]))
        }),
        parse_hashkey,
        parse_keyword,
        map(tag!(MalToken::QuotedSequence(s)), |s: &str| {
            MalType::Str(s.into())
        }),
        map_res(tag!(MalToken::Sequence(s)), move |s: &str| {
            complete::<_, _, nom::error::Error<&str>, _>(alt((
                map(nom::character::complete::i64, MalType::Int),
                map(nom::number::complete::double, MalType::Float),
            )))(s)
            .map(|(_, r)| r)
        }),
        map(parse_symbol, MalType::Symbol),
    ))(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::MalLexer, print, tokens::MalTokens};
    use nom::*;

    #[allow(unused_macros)]
    macro_rules! check {
        ($fn: ident,$lit: literal,$expected: literal) => {{
            let t = MalLexer::lex($lit).unwrap_or_default();
            println!("{:?}", t);
            let t = MalTokens(t.as_slice());
            let (_, r) = $fn(t).finish().unwrap();
            assert_eq!($expected, format!("{}", print(&r, true)));
        }};
    }

    #[test]
    fn parse_int0() {
        check!(parse_type, "123", "123");
    }

    #[test]
    fn parse_str0() {
        check!(parse_type, "\"123\"", "\"123\"");
    }

    #[test]
    fn parse_sym0() {
        check!(parse_type, "abc", "abc");
    }

    #[test]
    fn parse_str1() {
        check!(parse_type, "\"\\\\\"", "\"\\\\\"");
    }

    #[test]
    fn parse_type0() {
        check!(parse_type, "doge", "doge");
    }

    #[test]
    fn parse_type1() {
        check!(parse_type, "12345", "12345");
    }

    #[test]
    fn parse0() {
        check!(parse_type, "( + 2 (  * 3 4  ) ) \n\n\n\n", "(+ 2 (* 3 4))");
    }

    #[test]
    fn parse1() {
        check!(parse_type, "(    \"1231\")", "(\"1231\")");
    }

    #[test]
    fn parse2() {
        check!(parse_type, "(   )", "()");
    }
    #[test]
    fn parse3() {
        check!(parse_type, "(nil)", "(nil)");
    }

    #[test]
    fn parse5() {
        check!(parse_type, "\"\"", "\"\"");
    }

    #[test]
    fn parse6() {
        check!(parse_type, "\"abc (with parens)\"", "\"abc (with parens)\"");
    }
}
