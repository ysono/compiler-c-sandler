//! Lexing; and parsing of some values.

use crate::{
    common::{identifier::RawIdentifier, primitive::Const},
    stage1_lex::tokens::*,
};
use anyhow::{Context, Result, anyhow};
use regex::Regex;
use std::{
    collections::VecDeque,
    io::{BufRead, Read},
};

pub struct Lexer<R> {
    matchers: TokenMatchers,

    r: R,

    /// Buffer of `char`s. Separate from `R`'s internal buffer (of e.g. `u8`s).
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
            let read_bytelen = self.r.read_line(&mut self.chars_buf)?;
            if read_bytelen == 0 {
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

            let (match_bytelen, token) = self.lex_sfx(sfx)?;
            sfx = &sfx[match_bytelen..];
            self.next_tokens.push_back(token);
        }

        Ok(())
    }

    fn lex_sfx(&self, sfx: &str) -> Result<(usize, Token)> {
        let byte0 = sfx.as_bytes()[0];
        if byte0.is_ascii_alphabetic() {
            self.lex_alphabetic(sfx).map_err(|()| anyhow!("{sfx}"))
        } else if byte0 == b'.' || byte0.is_ascii_digit() {
            self.lex_dot_or_digit(sfx)
        } else {
            self.lex_non_alphanum(sfx).map_err(|()| anyhow!("{sfx}"))
        }
    }

    fn lex_alphabetic(&self, sfx: &str) -> Result<(usize, Token), ()> {
        match self.matchers.wordlike.find(sfx) {
            Some(mach) => {
                #[rustfmt::skip]
                let token = match mach.as_str() {
                    "return" => Keyword::Return.into(),
                    "void"     => TypeSpecifier::Void.into(),
                    "char"     => TypeSpecifier::Char.into(),
                    "int"      => TypeSpecifier::Int.into(),
                    "long"     => TypeSpecifier::Long.into(),
                    "signed"   => TypeSpecifier::Signed.into(),
                    "unsigned" => TypeSpecifier::Unsigned.into(),
                    "double"   => TypeSpecifier::Double.into(),
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
                Ok((mach.len(), token))
            }
            None => Err(()),
        }
    }

    fn lex_dot_or_digit(&self, sfx: &str) -> Result<(usize, Token)> {
        match self.matchers.numeric.captures(sfx) {
            Some(caps) => {
                let (whole, [head, head_dot, int_marker, float_expo]) = caps.extract();

                let konst = if (head != "") && (head_dot == "") && (float_expo == "") {
                    Self::parse_integer_literal(head, int_marker)?
                } else if (head.len() > head_dot.len()) && (int_marker == "") {
                    let literal = &whole[..(whole.len() - 1)];
                    literal.parse::<f64>().map(Const::Double)?
                } else {
                    return Err(anyhow!("{whole}"));
                };

                let match_bytelen = whole.len() - 1; // Un-count the tail-end `[^\w.]`.

                Ok((match_bytelen, konst.into()))
            }
            None => Err(anyhow!("{sfx}")),
        }
    }
    fn parse_integer_literal(digits: &str, int_marker: &str) -> Result<Const> {
        let mut markers = Vec::with_capacity(2);
        markers.extend(
            int_marker
                .as_bytes()
                .iter()
                .take(2)
                .map(|b| b.to_ascii_lowercase()),
        );

        /* Note, each literal we parse is zero-or-positive.
        Nonetheless, we are able to represent the full range of i64 and u64 values.
        Eg the value -(1<<63) is representable by two AST nodes: negate( Const::ULong(9223372036854775808) ). */
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

    fn lex_non_alphanum(&self, sfx: &str) -> Result<(usize, Token), ()> {
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
            b'\'' => self.parse_character_literal(sfx),
            b'"' => self.parse_string_literal(sfx),
            _ => Self::lex_operator(sfx),
        }
    }

    fn parse_character_literal(&self, sfx: &str) -> Result<(usize, Token), ()> {
        match self.matchers.character.captures(sfx) {
            Some(caps) => {
                let (whole, [inner]) = caps.extract();

                let semantic_char = match inner.as_bytes()[..] {
                    [raw_char_0] => raw_char_0,
                    [_, raw_char_1] => Self::unescape_character(raw_char_1)?,
                    _ => unreachable!(),
                };

                /* In C, each single-character literal is interpreted as a <const> with type=`int`,
                _not_ any of {`char`, `signed char`, `unsigned char`}. */
                let konst = Const::Int(semantic_char as i32);

                Ok((whole.len(), Token::Const(konst)))
            }
            None => Err(()),
        }
    }
    fn parse_string_literal(&self, sfx: &str) -> Result<(usize, Token), ()> {
        match self.matchers.string.captures(sfx) {
            Some(caps) => {
                let (whole, [inner]) = caps.extract();

                let mut semantic_chars = vec![];
                let mut inner = inner.as_bytes().iter();
                while let Some(raw_char_0) = inner.next() {
                    let semantic_char = match *raw_char_0 {
                        b'\\' => {
                            let raw_char_1 = inner.next().unwrap();
                            Self::unescape_character(*raw_char_1)?
                        }
                        raw_char_0 => raw_char_0,
                    };
                    semantic_chars.push(semantic_char);
                }

                Ok((whole.len(), Token::String(semantic_chars)))
            }
            None => Err(()),
        }
    }
    fn unescape_character(raw_escaped_char: u8) -> Result<u8, ()> {
        let semantic_char = match raw_escaped_char {
            b'\'' | b'"' | b'?' | b'\\' => raw_escaped_char,
            b'a' => 7,
            b'b' => 8,
            b'f' => 12,
            b'n' => b'\n',
            b'r' => b'\r',
            b't' => b'\t',
            b'v' => 11,
            _ => Err(())?,
        };
        Ok(semantic_char)
    }

    fn lex_operator(sfx: &str) -> Result<(usize, Token), ()> {
        let sfx_bytes = sfx.as_bytes();
        let byte0 = sfx_bytes[0];

        let mut match_bytelen = 1;
        while (match_bytelen < sfx_bytes.len()) && (sfx_bytes[match_bytelen] == byte0) {
            match_bytelen += 1;
        }
        while (match_bytelen < sfx_bytes.len()) && (sfx_bytes[match_bytelen] == b'=') {
            match_bytelen += 1;
        }
        let sfx_pfx = &sfx[..match_bytelen];

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
            _ => return Err(()),
        };

        return Ok((match_bytelen, token));
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
    character: Regex,
    string: Regex,
}
impl TokenMatchers {
    fn new() -> Result<Self> {
        let wordlike = Regex::new(r"^[a-zA-Z_]\w*\b")?;
        let numeric = Regex::new(r"^(\d*(\.?)\d*)([lLuU]{0,2})((?:[eE][+-]?\d+)?)[^\w.]")?;
        let single_quotes = Regex::new(r#"^'([^'\\\n]|\\['"?\\abfnrtv])'"#)?;
        let double_quotes = Regex::new(r#"^"((?:[^"\\\n]|\\['"?\\abfnrtv])*)""#)?;
        Ok(Self {
            wordlike,
            numeric,
            character: single_quotes,
            string: double_quotes,
        })
    }
}
