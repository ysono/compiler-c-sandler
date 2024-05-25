use crate::files::PreprocessedFilepath;
use anyhow::{anyhow, Context, Result};
use derive_more::From;
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
        let f = File::open(pp_filepath as &PathBuf)
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

        let match_len: usize;
        let token: Token;
        match sfx.as_bytes()[0] {
            PAREN_OPEN => (match_len, token) = (1, Demarcator::ParenOpen.into()),
            PAREN_CLOSE => (match_len, token) = (1, Demarcator::ParenClose.into()),
            BRACE_OPEN => (match_len, token) = (1, Demarcator::BraceOpen.into()),
            BRACE_CLOSE => (match_len, token) = (1, Demarcator::BraceClose.into()),
            SEMICOLON => (match_len, token) = (1, Demarcator::Semicolon.into()),
            TILDE => (match_len, token) = (1, Operation::Complement.into()),
            _ => {
                if let Some(mach) = KW_INT.find(sfx) {
                    match_len = mach.len();
                    token = Keyword::Int.into();
                } else if let Some(mach) = KW_VOID.find(sfx) {
                    match_len = mach.len();
                    token = Keyword::Void.into();
                } else if let Some(mach) = KW_RET.find(sfx) {
                    match_len = mach.len();
                    token = Keyword::Return.into();
                } else if let Some(mach) = MINUS_SINGLE.find(sfx) {
                    if mach.len() == "-".len() {
                        match_len = mach.len();
                        token = Operation::Negate.into();
                    } else {
                        return Err(anyhow!("Unknown syntax at {sfx}"));
                    }
                } else if let Some(mach) = DECIMALS.find(sfx) {
                    match_len = mach.len();
                    let literal = &sfx[..match_len];
                    let literal = literal.parse::<i32>().context("Const integer")?;
                    token = Const::Int(literal).into();
                } else if let Some(mach) = IDENT.find(sfx) {
                    match_len = mach.len();
                    let literal = &sfx[..match_len];
                    let literal = String::from(literal);
                    token = Identifier(literal).into();
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

    /* Tokens whose existence doesn't depend on the subsequent char (eg `\b`) */
    /* Demarcators */
    pub const PAREN_OPEN: u8 = '(' as u8;
    pub const PAREN_CLOSE: u8 = ')' as u8;
    pub const BRACE_OPEN: u8 = '{' as u8;
    pub const BRACE_CLOSE: u8 = '}' as u8;
    pub const SEMICOLON: u8 = ';' as u8;
    /* Operations */
    pub const TILDE: u8 = '~' as u8;

    lazy_static! {
        /* Keywords */
        pub static ref KW_INT: Regex = Regex::new(r"^int\b").unwrap();
        pub static ref KW_VOID: Regex = Regex::new(r"^void\b").unwrap();
        pub static ref KW_RET: Regex = Regex::new(r"^return\b").unwrap();

        /* Operations */
        pub static ref MINUS_SINGLE: Regex = Regex::new(r"^--?").unwrap();

        /* Misc variable-length tokens */
        pub static ref DECIMALS: Regex = Regex::new(r"^[0-9]+\b").unwrap();
        pub static ref IDENT: Regex = Regex::new(r"^[a-zA-Z_]\w*\b").unwrap();
    }
}

#[derive(From, PartialEq, Eq, Debug)]
pub enum Token {
    Demarcator(Demarcator),
    Keyword(Keyword),
    Operation(Operation),
    Const(Const),
    Identifier(Identifier),
}
#[derive(PartialEq, Eq, Debug)]
pub enum Demarcator {
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Semicolon,
}
#[derive(PartialEq, Eq, Debug)]
pub enum Keyword {
    Int,
    Void,
    Return,
}
#[derive(PartialEq, Eq, Debug)]
pub enum Operation {
    Complement,
    Negate,
}
#[derive(PartialEq, Eq, Debug)]
pub enum Const {
    Int(i32),
}
#[derive(PartialEq, Eq, Debug)]
pub struct Identifier(pub String);
