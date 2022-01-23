use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, is_not, tag},
    character::complete::{none_of, one_of},
    combinator::{map, opt, recognize, value},
    multi::many0,
    sequence::{delimited, preceded},
    Compare, CompareResult, IResult, InputIter, InputLength, InputTake, Slice,
};
use std::{
    borrow::Cow,
    fmt::Display,
    iter::Enumerate,
    ops::{Index, RangeFrom, RangeFull, RangeTo},
};
const MAL_SEP: &str = "\t \r\n,";
const MAL_DELIM: &str = "[]{}()\'\"`~^@\t \r\n,;";

#[allow(unused_macros)]
macro_rules! token_fn {
    ($fn: ident,$i: expr, $lit: literal) => {
        pub(crate) fn $fn<'a, 'b, I: 'b, E: nom::error::ParseError<I>>(
            i: I,
        ) -> nom::IResult<I, MalToken<'b>, E>
        where
            I: nom::InputTake + nom::Compare<&'static str>,
        {
            map(tag::<_, I, E>($lit), |_| $i)(i)
        }
    };
}

token_fn!(splice_unquote, MalToken::SpliceUnquote, "~@");
token_fn!(left_paren, MalToken::LeftParen, "(");
token_fn!(right_paren, MalToken::RightParen, ")");
token_fn!(left_brace, MalToken::LeftBrace, "{");
token_fn!(right_brace, MalToken::RightBrace, "}");
token_fn!(left_square, MalToken::LeftSquare, "[");
token_fn!(right_square, MalToken::RightSquare, "]");
token_fn!(quote, MalToken::Quote, "'");
token_fn!(quasi_quote, MalToken::QuasiQuote, "`");
token_fn!(unquote, MalToken::Unquote, "~");
token_fn!(with_meta, MalToken::WithMeta, "^");
token_fn!(deref, MalToken::Deref, "@");
token_fn!(comment_start, MalToken::CommentStart, ";");

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MalToken<'a> {
    LeftBrace,  // {
    RightBrace, // }
    LeftParen,  // (
    RightParen, // )
    LeftSquare,
    RightSquare,
    Quote,
    QuasiQuote,
    Unquote,
    Deref,
    SpliceUnquote,
    WithMeta,
    CommentStart,
    QuotedSequence(Cow<'a, str>), // Sequence starting with "
    Sequence(Cow<'a, str>),       // Seqeuence of characters not matching anyothers
}

impl<'a> Display for MalToken<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MalToken::LeftBrace => write!(f, "{{"),
            MalToken::RightBrace => write!(f, "}}"),
            MalToken::LeftParen => write!(f, "("),
            MalToken::RightParen => write!(f, ")"),
            MalToken::LeftSquare => write!(f, "["),
            MalToken::RightSquare => write!(f, "]"),
            MalToken::Quote => write!(f, "quote"),
            MalToken::QuasiQuote => write!(f, "quasiquote"),
            MalToken::Unquote => write!(f, "unquote"),
            MalToken::Deref => write!(f, "deref"),
            MalToken::SpliceUnquote => write!(f, "splice-unquote"),
            MalToken::WithMeta => write!(f, "with-meta"),
            MalToken::QuotedSequence(s) => write!(f, "\"{}\"", s),
            MalToken::Sequence(s) => write!(f, "{}", s),
            MalToken::CommentStart => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MalTokens<'a>(pub &'a [MalToken<'a>]);

impl<'a> Into<Vec<MalToken<'a>>> for MalTokens<'a> {
    fn into(self) -> Vec<MalToken<'a>> {
        self.0.to_vec()
    }
}

impl<'a> From<&'a [MalToken<'a>]> for MalTokens<'a> {
    fn from(v: &'a [MalToken<'a>]) -> Self {
        Self(v)
    }
}

impl<'a> From<&'a Vec<MalToken<'a>>> for MalTokens<'a> {
    fn from(v: &'a Vec<MalToken<'a>>) -> Self {
        Self(&v[..])
    }
}

impl<'a, Idx: Into<usize>> Index<Idx> for MalTokens<'a> {
    type Output = MalToken<'a>;

    fn index(&self, index: Idx) -> &Self::Output {
        &self.0[index.into()]
    }
}

impl<'a> InputLength for MalTokens<'a> {
    fn input_len(&self) -> usize {
        self.0.len()
    }
}

impl<'a> InputTake for MalTokens<'a> {
    fn take(&self, count: usize) -> Self {
        Self(&self.0[0..count])
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.0.split_at(count);
        (Self(suffix), Self(prefix))
    }
}

impl<'a> Slice<RangeTo<usize>> for MalTokens<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        Self(&self.0[range])
    }
}

impl<'a> Slice<RangeFrom<usize>> for MalTokens<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        Self(&self.0[range])
    }
}

impl<'a> Slice<RangeFull> for MalTokens<'a> {
    fn slice(&self, _: RangeFull) -> Self {
        Self(self.0)
    }
}

impl<'a> InputIter for MalTokens<'a> {
    type Item = &'a MalToken<'a>;
    type Iter = Enumerate<Self::IterElem>;
    type IterElem = std::slice::Iter<'a, MalToken<'a>>;

    fn iter_indices(&self) -> Self::Iter {
        self.0.iter().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.0.iter()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.0.iter().position(|b| predicate(b))
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        if self.0.len() >= count {
            Ok(count)
        } else {
            Err(nom::Needed::new(count - self.0.len()))
        }
    }
}

impl<'a, 'b> Compare<MalTokens<'b>> for MalTokens<'a> {
    #[inline(always)]
    fn compare(&self, t: MalTokens<'b>) -> nom::CompareResult {
        let pos = self.0.iter().zip(t.0.iter()).position(|(a, b)| a != b);
        match pos {
            Some(_) => CompareResult::Error,
            None => {
                if self.0.len() >= t.0.len() {
                    CompareResult::Ok
                } else {
                    CompareResult::Incomplete
                }
            }
        }
    }

    #[inline(always)]
    fn compare_no_case(&self, t: MalTokens<'b>) -> nom::CompareResult {
        self.compare(t)
    }
}

pub fn parse_token<'a>(i: &'a str) -> IResult<&'a str, MalToken<'a>>
where
{
    preceded(
        recognize(many0(one_of(MAL_SEP))),
        alt((
            splice_unquote,
            left_brace,
            right_brace,
            left_paren,
            right_paren,
            left_square,
            right_square,
            quote,
            quasi_quote,
            unquote,
            with_meta,
            deref,
            map(
                delimited(
                    tag("\""),
                    opt(escaped_transform(
                        none_of("\"\\"),
                        '\\',
                        alt((
                            value("\\", tag("\\")),
                            value("\"", tag("\"")),
                            value("\n", tag("n")),
                        )),
                    )),
                    tag("\""),
                ),
                |s: Option<String>| MalToken::QuotedSequence(Cow::Owned(s.unwrap_or_default())),
            ),
            preceded(
                comment_start,
                map(is_not("\n"), |s| {
                    MalToken::Sequence(Cow::Owned(format!(";{}", s)))
                }),
            ),
            map(is_not(MAL_DELIM), |s: &str| {
                MalToken::Sequence(Cow::Owned(s.to_string()))
            }),
        )),
    )(i)
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_empty_quoted() {
        let (_, t) = parse_token(r#""""#).unwrap();
        println!("{:?}", t);
        assert_eq!(t, MalToken::QuotedSequence(Cow::Borrowed("")));
    }
}
