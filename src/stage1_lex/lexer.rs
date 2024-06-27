use crate::{files::PreprocessedFilepath, stage1_lex::tokens::*, types_frontend::Const};
use anyhow::{anyhow, Context, Result};
use regex::Regex;
use std::collections::VecDeque;
use std::fs::File;
use std::io::{BufRead, BufReader, Seek};
use std::path::PathBuf;

pub struct Lexer {
    matchers: TokenMatchers,

    br: BufReader<File>,

    /// Buffer of `char`s. A second buffer aside from `BufReader`'s internal buffer of `u8`s. Allocated once per `Lexer` instance.
    chars_buf: String,

    /// Pending tokens.
    next_tokens: VecDeque<Token>,
}
impl<'a> TryFrom<&'a PreprocessedFilepath> for Lexer {
    type Error = anyhow::Error;
    fn try_from(pp_filepath: &'a PreprocessedFilepath) -> Result<Self> {
        let f = File::open(pp_filepath as &PathBuf)
            .with_context(|| format!("Failed to open {pp_filepath:?}"))?;
        let br = BufReader::new(f);
        Ok(Self {
            matchers: TokenMatchers::new()?,
            br,
            chars_buf: String::new(),
            next_tokens: VecDeque::new(),
        })
    }
}
impl Lexer {
    fn get_next(&mut self) -> Result<Option<Token>> {
        while self.next_tokens.is_empty() {
            /* Read until an arbitrary char. We choose to read until '\n'.
            Doing this guarantees, for now, b/c of the limited syntax we support, that each reading collects token(s) in whole.
            We don't want to read `std::io::Bytes`: for each byte, we'd have to check `Result`. */
            let br_pos_before = self.br.stream_position().ok();
            let read_len = self.br.read_line(&mut self.chars_buf)?;
            if read_len == 0 {
                return Ok(None);
            }

            self.buf_to_tokens().with_context(|| {
                let br_pos_after = self.br.stream_position().ok();
                format!(
                    "Between intra-file positions {:?} and {:?}, within below slice:\n{}",
                    br_pos_before, br_pos_after, &self.chars_buf
                )
            })?;

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
        let sfx_bytes = sfx.as_bytes();
        match sfx_bytes[0] {
            b'(' => Ok((1, Demarcator::ParenOpen.into())),
            b')' => Ok((1, Demarcator::ParenClose.into())),
            b'{' => Ok((1, Demarcator::BraceOpen.into())),
            b'}' => Ok((1, Demarcator::BraceClose.into())),
            b';' => Ok((1, Demarcator::Semicolon.into())),
            b',' => Ok((1, Demarcator::Comma.into())),
            b'~' => Ok((1, Operator::Tilde.into())),
            b'?' => Ok((1, Operator::Question.into())),
            b':' => Ok((1, Operator::Colon.into())),
            b if b.is_ascii_alphanumeric() == false => {
                let mut match_len = 1;
                while (match_len < sfx_bytes.len()) && (sfx_bytes[match_len] == b) {
                    match_len += 1;
                }
                while (match_len < sfx_bytes.len()) && (sfx_bytes[match_len] == b'=') {
                    match_len += 1;
                }
                let sfx_pfx = &sfx[..match_len];

                let token = match sfx_pfx {
                    "-" => Operator::Minus.into(),
                    "+" => Operator::Plus.into(),
                    "*" => Operator::Star.into(),
                    "/" => Operator::Slash.into(),
                    "%" => Operator::Percent.into(),
                    "&&" => Operator::And.into(),
                    "||" => Operator::Or.into(),
                    "=" => Operator::Assign.into(),
                    "==" => Operator::Eq.into(),
                    "!" => Operator::Not.into(),
                    "!=" => Operator::Neq.into(),
                    "<" => Operator::Lt.into(),
                    "<=" => Operator::Lte.into(),
                    ">" => Operator::Gt.into(),
                    ">=" => Operator::Gte.into(),
                    _ => return Err(anyhow!("{sfx}")),
                };
                return Ok((match_len, token));
            }
            b if b.is_ascii_digit() => {
                if let Some(caps) = self.matchers.decimals.captures(sfx) {
                    let (literal, [digits, type_marker]) = caps.extract();

                    let mut markers = Vec::with_capacity(2);
                    markers.extend(
                        type_marker
                            .as_bytes()
                            .iter()
                            .take(2)
                            .map(|b| b.to_ascii_lowercase()),
                    );

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

                    return Ok((literal.len(), konst.into()));
                }
                return Err(anyhow!("{sfx}"));
            }
            _ => {
                if let Some(mach) = self.matchers.wordlike.find(sfx) {
                    #[rustfmt::skip]
                    let token = match mach.as_str() {
                        "return" => Keyword::Return.into(),
                        "void"     => Type::Void.into(),
                        "int"      => Type::Int.into(),
                        "long"     => Type::Long.into(),
                        "signed"   => Type::Signed.into(),
                        "unsigned" => Type::Unsigned.into(),
                        "static" => StorageClassSpecifier::Static.into(),
                        "extern" => StorageClassSpecifier::Extern.into(),
                        "if"     => Control::If.into(),
                        "else"   => Control::Else.into(),
                        "do"       => Loop::Do.into(),
                        "while"    => Loop::While.into(),
                        "for"      => Loop::For.into(),
                        "break"    => Loop::Break.into(),
                        "continue" => Loop::Continue.into(),
                        s => Identifier(String::from(s)).into(),
                    };
                    return Ok((mach.len(), token));
                }
                return Err(anyhow!("{sfx}"));
            }
        }
    }
}
impl Iterator for Lexer {
    type Item = Result<Token>;
    fn next(&mut self) -> Option<Result<Token>> {
        self.get_next().transpose()
    }
}

struct TokenMatchers {
    decimals: Regex,
    wordlike: Regex,
}
impl TokenMatchers {
    fn new() -> Result<Self> {
        let decimals = Regex::new(r"^([0-9]+)([lLuU]{0,2})\b")?;
        let wordlike = Regex::new(r"^[a-zA-Z_]\w*\b")?;
        Ok(Self { decimals, wordlike })
    }
}
