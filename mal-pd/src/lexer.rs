use nom::{multi::many0, Finish};

use crate::tokens::{parse_token, MalToken};

pub struct MalLexer {}

impl MalLexer {
    pub fn lex<'a, T: Into<&'a str>>(
        i: T,
    ) -> Result<Vec<MalToken<'a>>, Box<dyn std::error::Error>> {
        let (_, tokens) = many0(parse_token)(i.into())
            .finish()
            .map_err(|e| format!("{:?}", e))?;
        Ok(tokens)
    }
}
