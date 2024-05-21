use crate::files::PreprocessedFilepath;
use anyhow::{anyhow, Context, Result};
use std::collections::VecDeque;
use std::fs::File;
use std::io::{BufRead, BufReader, Seek};
use std::path::PathBuf;
use std::str::Chars;

pub struct Lexer {
    br: BufReader<File>,

    /// Buffer of `char`s. A second buffer aside from `BufReader`'s internal buffer of `u8`s. Allocated once per `Lexer` instance.
    chars_buf: String,

    /// Pending tokens.
    next_tokens: VecDeque<Token>,
}
impl<'a> TryFrom<&'a PreprocessedFilepath> for Lexer {
    type Error = anyhow::Error;
    fn try_from(pp_filepath: &'a PreprocessedFilepath) -> Result<Self> {
        let f = File::open(&pp_filepath as &PathBuf)
            .with_context(|| format!("Failed to open {pp_filepath:?}"))?;
        let br = BufReader::new(f);
        Ok(Self {
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
        let mut chars = self.chars_buf.chars();

        loop {
            match Self::char_iter_to_token(&mut chars)? {
                Some(token) => self.next_tokens.push_back(token),
                None => break,
            }
        }

        Ok(())
    }

    fn char_iter_to_token(chars: &mut Chars) -> Result<Option<Token>> {
        use token_matchers::*;

        loop {
            /* A syntactically kludgy way to peek. We don't want to `move` `Chars` into a new `Peekable` iterator, so that we can call `Chars::as_str()` later. */
            if let Some(ch) = chars.as_str().chars().next() {
                if ch.is_whitespace() {
                    chars.next();
                    continue;
                }
            }
            break;
        }

        let sfx = chars.as_str();
        if sfx.len() == 0 {
            return Ok(None);
        }

        let match_len;
        let token;
        match sfx.as_bytes()[0] {
            PAREN_OPEN => (match_len, token) = (1, Token::ParenOpen),
            PAREN_CLOSE => (match_len, token) = (1, Token::ParenClose),
            BRACE_OPEN => (match_len, token) = (1, Token::BraceOpen),
            BRACE_CLOSE => (match_len, token) = (1, Token::BraceClose),
            SEMICOLON => (match_len, token) = (1, Token::Semicolon),
            _ => {
                if let Some(mach) = KW_INT.find(sfx) {
                    match_len = mach.len();
                    token = Token::Keyword(Keyword::Int);
                } else if let Some(mach) = KW_VOID.find(sfx) {
                    match_len = mach.len();
                    token = Token::Keyword(Keyword::Void);
                } else if let Some(mach) = KW_RET.find(sfx) {
                    match_len = mach.len();
                    token = Token::Keyword(Keyword::Return);
                } else if let Some(mach) = CONST_INT.find(sfx) {
                    match_len = mach.len();
                    let literal = &sfx[..match_len];
                    let literal = literal.parse::<i64>().context("Const integer")?;
                    token = Token::Const(Const::Int(literal));
                } else if let Some(mach) = IDENT.find(sfx) {
                    match_len = mach.len();
                    let literal = &sfx[..match_len];
                    let literal = String::from(literal);
                    token = Token::Identifier(Identifier(literal));
                } else {
                    return Err(anyhow!("Unknown syntax at {sfx}"));
                }
            }
        };

        for _ in 0..match_len {
            chars.next();
        }
        return Ok(Some(token));
    }
}
impl Iterator for Lexer {
    type Item = Result<Token>;
    fn next(&mut self) -> Option<Result<Token>> {
        self.get_next().transpose()
    }
}

mod token_matchers {
    use lazy_static::lazy_static;
    use regex::Regex;

    lazy_static! {
        pub static ref IDENT: Regex = Regex::new(r"^[a-zA-Z_]\w*\b").unwrap();
        pub static ref CONST_INT: Regex = Regex::new(r"^[0-9]+\b").unwrap();
        pub static ref KW_INT: Regex = Regex::new(r"^int\b").unwrap();
        pub static ref KW_VOID: Regex = Regex::new(r"^void\b").unwrap();
        pub static ref KW_RET: Regex = Regex::new(r"^return\b").unwrap();
    }

    pub const PAREN_OPEN: u8 = '(' as u8;
    pub const PAREN_CLOSE: u8 = ')' as u8;
    pub const BRACE_OPEN: u8 = '{' as u8;
    pub const BRACE_CLOSE: u8 = '}' as u8;
    pub const SEMICOLON: u8 = ';' as u8;
}

#[derive(Debug)]
pub enum Token {
    Identifier(Identifier),
    Const(Const),
    Keyword(Keyword),
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Semicolon,
}
#[derive(Debug)]
pub struct Identifier(pub String);
#[derive(Debug)]
pub enum Const {
    Int(i64),
}
#[derive(Debug)]
pub enum Keyword {
    Int,
    Void,
    Return,
}
