use crate::{
    common::{identifier::RawIdentifier, primitive::Const},
    stage1_lex::tokens::*,
};
use anyhow::{anyhow, Context, Result};
use regex::Regex;
use std::collections::VecDeque;
use std::io::{BufRead, Read};

pub struct Lexer<R> {
    matchers: TokenMatchers,

    r: R,

    /// Buffer of `char`s. Separate from `R`'s internal buffer (of e.g. `u8`s). Allocated once.
    chars_buf: String,

    /// Pending tokens.
    next_tokens: VecDeque<Token>,
}
impl<R: Read + BufRead> Lexer<R> {
    pub fn new(r: R) -> Result<Self> {
        Ok(Self {
            matchers: TokenMatchers::new()?,
            r,
            chars_buf: String::new(),
            next_tokens: VecDeque::new(),
        })
    }

    fn get_next(&mut self) -> Result<Option<Token>> {
        while self.next_tokens.is_empty() {
            /* Read until an arbitrary char. We choose to read until '\n'.
            Doing this guarantees, for now, b/c of the limited syntax we support, that each reading collects token(s) in whole.
            We don't want to read `std::io::Bytes`: for each byte, we'd have to check `Result`. */
            let read_len = self.r.read_line(&mut self.chars_buf)?;
            if read_len == 0 {
                return Ok(None);
            }

            self.buf_to_tokens()
                .with_context(|| anyhow!("within below slice:\n{}", &self.chars_buf))?;

            self.chars_buf.clear();
        }

        Ok(self.next_tokens.pop_front())
    }

    fn buf_to_tokens(&mut self) -> Result<()> {
        let mut sfx: &str = &self.chars_buf;

        loop {
            sfx = sfx.trim_start();
            if sfx.len() == 0 {
                break;
            }

            let (match_len, token) = self.sfx_to_token(sfx)?;
            sfx = &sfx[match_len..];
            self.next_tokens.push_back(token);
        }

        Ok(())
    }

    fn sfx_to_token(&self, sfx: &str) -> Result<(usize, Token)> {
        let byte0 = sfx.as_bytes()[0];
        if byte0.is_ascii_alphabetic() {
            self.alpha_sfx_to_token(sfx)
        } else if byte0 == b'.' || byte0.is_ascii_digit() {
            self.dot_or_digit_sfx_to_token(sfx)
        } else {
            self.nonalphanum_sfx_to_token(sfx)
        }
    }

    fn alpha_sfx_to_token(&self, sfx: &str) -> Result<(usize, Token)> {
        if let Some(mach) = self.matchers.wordlike.find(sfx) {
            #[rustfmt::skip]
            let token = match mach.as_str() {
                "return" => Keyword::Return.into(),
                "void"     => Type::Void.into(),
                "int"      => Type::Int.into(),
                "long"     => Type::Long.into(),
                "signed"   => Type::Signed.into(),
                "unsigned" => Type::Unsigned.into(),
                "double"   => Type::Double.into(),
                "static" => StorageClassSpecifier::Static.into(),
                "extern" => StorageClassSpecifier::Extern.into(),
                "if"     => Control::If.into(),
                "else"   => Control::Else.into(),
                "do"       => Loop::Do.into(),
                "while"    => Loop::While.into(),
                "for"      => Loop::For.into(),
                "break"    => Loop::Break.into(),
                "continue" => Loop::Continue.into(),
                s => RawIdentifier::new(String::from(s)).into(),
            };
            return Ok((mach.len(), token));
        }
        return Err(anyhow!("{sfx}"));
    }

    fn dot_or_digit_sfx_to_token(&self, sfx: &str) -> Result<(usize, Token)> {
        if let Some(caps) = self.matchers.numeric.captures(sfx) {
            let (whole, [head, head_dot, int_marker, float_expo]) = caps.extract();

            let konst = if (head != "") && (head_dot == "") && (float_expo == "") {
                Self::parse_integer(head, int_marker)?
            } else if (head.len() > head_dot.len()) && (int_marker == "") {
                let literal = &whole[..(whole.len() - 1)];
                literal.parse::<f64>().map(Const::Double)?
            } else {
                return Err(anyhow!("{whole}"));
            };

            let match_len = whole.len() - 1; // Un-count the tail-end `[^\w.]`.

            return Ok((match_len, konst.into()));
        }
        return Err(anyhow!("{sfx}"));
    }
    fn parse_integer(digits: &str, int_marker: &str) -> Result<Const> {
        let mut markers = Vec::with_capacity(2);
        markers.extend(
            int_marker
                .as_bytes()
                .iter()
                .take(2)
                .map(|b| b.to_ascii_lowercase()),
        );

        /* Note, each literal we parse is zero-or-positive.
        As a consequence, we're unable to parse a signed long literal valued -(1<<63). */
        let konst = match &markers[..] {
            b"ul" | b"lu" => digits.parse::<u64>().map(Const::ULong)?,
            b"l" => digits.parse::<i64>().map(Const::Long)?,
            b"u" => {
                let i_wide = digits.parse::<u64>()?;
                u32::try_from(i_wide)
                    .map(Const::UInt)
                    .unwrap_or_else(|_| Const::ULong(i_wide))
            }
            b"" => {
                let i_wide = digits.parse::<i64>()?;
                i32::try_from(i_wide)
                    .map(Const::Int)
                    .unwrap_or_else(|_| Const::Long(i_wide))
            }
            actual => return Err(anyhow!("{actual:?}")),
        };
        Ok(konst)
    }

    fn nonalphanum_sfx_to_token(&self, sfx: &str) -> Result<(usize, Token)> {
        let sfx_bytes = sfx.as_bytes();
        match sfx_bytes[0] {
            b'(' => Ok((1, Demarcator::ParenOpen.into())),
            b')' => Ok((1, Demarcator::ParenClose.into())),
            b'{' => Ok((1, Demarcator::CurlyOpen.into())),
            b'}' => Ok((1, Demarcator::CurlyClose.into())),
            b'[' => Ok((1, Demarcator::SquareOpen.into())),
            b']' => Ok((1, Demarcator::SquareClose.into())),
            b';' => Ok((1, Demarcator::Semicolon.into())),
            b',' => Ok((1, Demarcator::Comma.into())),
            b'*' => Ok((1, Operator::Star.into())),
            b'~' => Ok((1, Operator::Tilde.into())),
            b'?' => Ok((1, Operator::Question.into())),
            b':' => Ok((1, Operator::Colon.into())),
            byte0 => {
                let mut match_len = 1;
                while (match_len < sfx_bytes.len()) && (sfx_bytes[match_len] == byte0) {
                    match_len += 1;
                }
                while (match_len < sfx_bytes.len()) && (sfx_bytes[match_len] == b'=') {
                    match_len += 1;
                }
                let sfx_pfx = &sfx[..match_len];

                let token = match sfx_pfx {
                    "-" => Operator::Minus.into(),
                    "+" => Operator::Plus.into(),
                    "/" => Operator::Slash.into(),
                    "%" => Operator::Percent.into(),
                    "&" => Operator::Ampersand.into(),
                    "&&" => Operator::And.into(),
                    "||" => Operator::Or.into(),
                    "=" => Operator::Assign.into(),
                    "==" => Operator::Eq.into(),
                    "!" => Operator::Bang.into(),
                    "!=" => Operator::Neq.into(),
                    "<" => Operator::Lt.into(),
                    "<=" => Operator::Lte.into(),
                    ">" => Operator::Gt.into(),
                    ">=" => Operator::Gte.into(),
                    _ => return Err(anyhow!("{sfx}")),
                };
                return Ok((match_len, token));
            }
        }
    }
}
impl<R: Read + BufRead> Iterator for Lexer<R> {
    type Item = Result<Token>;
    fn next(&mut self) -> Option<Result<Token>> {
        self.get_next().transpose()
    }
}

struct TokenMatchers {
    wordlike: Regex,
    numeric: Regex,
}
impl TokenMatchers {
    fn new() -> Result<Self> {
        let wordlike = Regex::new(r"^[a-zA-Z_]\w*\b")?;
        let numeric = Regex::new(r"^(\d*(\.?)\d*)([lLuU]{0,2})((?:[eE][+-]?\d+)?)[^\w.]")?;
        Ok(Self { wordlike, numeric })
    }
}
