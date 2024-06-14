use crate::{files::PreprocessedFilepath, stage1_lex::tokens::*};
use anyhow::{anyhow, Context, Result};
use std::collections::VecDeque;
use std::fs::File;
use std::io::{BufRead, BufReader, Seek};
use std::path::PathBuf;

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
        let mut sfx: &str = &self.chars_buf;

        while let Some(token) = Self::sfx_to_token(&mut sfx)? {
            self.next_tokens.push_back(token);
        }

        Ok(())
    }

    fn sfx_to_token(sfx: &mut &str) -> Result<Option<Token>> {
        use token_matchers::*;

        *sfx = sfx.trim_start();

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
            COMMA => (match_len, token) = (1, Demarcator::Comma.into()),
            TILDE => (match_len, token) = (1, Operator::Tilde.into()),
            STAR => (match_len, token) = (1, Operator::Star.into()),
            SLASH => (match_len, token) = (1, Operator::Slash.into()),
            PERCENT => (match_len, token) = (1, Operator::Percent.into()),
            QUESTION => (match_len, token) = (1, Operator::Question.into()),
            COLON => (match_len, token) = (1, Operator::Colon.into()),
            _ => {
                if sfx.starts_with(AND) {
                    (match_len, token) = (AND.len(), Operator::And.into());
                } else if sfx.starts_with(OR) {
                    (match_len, token) = (OR.len(), Operator::Or.into());
                } else if let Some(mach) = MINUSES.find(sfx) {
                    if mach.len() == "-".len() {
                        (match_len, token) = (mach.len(), Operator::Minus.into());
                    } else {
                        return Err(anyhow!("Unknown syntax at {sfx}"));
                    }
                } else if let Some(mach) = PLUSES.find(sfx) {
                    if mach.len() == "+".len() {
                        (match_len, token) = (mach.len(), Operator::Plus.into());
                    } else {
                        return Err(anyhow!("Unknown syntax at {sfx}"));
                    }
                } else if let Some(mach) = BANG_EQ.find(sfx) {
                    match_len = mach.len();
                    if mach.len() == "!=".len() {
                        token = Operator::Neq.into();
                    } else {
                        token = Operator::Not.into();
                    }
                } else if let Some(mach) = LT_EQ.find(sfx) {
                    match_len = mach.len();
                    if mach.len() == "<=".len() {
                        token = Operator::Lte.into();
                    } else {
                        token = Operator::Lt.into();
                    }
                } else if let Some(mach) = GT_EQ.find(sfx) {
                    match_len = mach.len();
                    if mach.len() == ">=".len() {
                        token = Operator::Gte.into();
                    } else {
                        token = Operator::Gt.into();
                    }
                } else if let Some(mach) = EQ_EQ.find(sfx) {
                    match_len = mach.len();
                    if mach.len() == "==".len() {
                        token = Operator::Eq.into();
                    } else {
                        token = Operator::Assign.into();
                    }
                } else if let Some(mach) = KW_RET.find(sfx) {
                    (match_len, token) = (mach.len(), Keyword::Return.into());
                } else if let Some(mach) = KW_INT.find(sfx) {
                    (match_len, token) = (mach.len(), Type::Int.into());
                } else if let Some(mach) = KW_VOID.find(sfx) {
                    (match_len, token) = (mach.len(), Type::Void.into());
                } else if let Some(mach) = KW_STATIC.find(sfx) {
                    (match_len, token) = (mach.len(), StorageClassSpecifier::Static.into());
                } else if let Some(mach) = KW_EXTERN.find(sfx) {
                    (match_len, token) = (mach.len(), StorageClassSpecifier::Extern.into());
                } else if let Some(mach) = KW_IF.find(sfx) {
                    (match_len, token) = (mach.len(), Control::If.into());
                } else if let Some(mach) = KW_ELSE.find(sfx) {
                    (match_len, token) = (mach.len(), Control::Else.into());
                } else if let Some(mach) = KW_DO.find(sfx) {
                    (match_len, token) = (mach.len(), Loop::Do.into());
                } else if let Some(mach) = KW_WHILE.find(sfx) {
                    (match_len, token) = (mach.len(), Loop::While.into());
                } else if let Some(mach) = KW_FOR.find(sfx) {
                    (match_len, token) = (mach.len(), Loop::For.into());
                } else if let Some(mach) = KW_BREAK.find(sfx) {
                    (match_len, token) = (mach.len(), Loop::Break.into());
                } else if let Some(mach) = KW_CONTINUE.find(sfx) {
                    (match_len, token) = (mach.len(), Loop::Continue.into());
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

        *sfx = &sfx[match_len..];
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

    /* Tokens whose existence _doesn't_ depend on the subsequent char (eg `\b`) */
    /* Demarcators */
    pub const PAREN_OPEN: u8 = b'(';
    pub const PAREN_CLOSE: u8 = b')';
    pub const BRACE_OPEN: u8 = b'{';
    pub const BRACE_CLOSE: u8 = b'}';
    pub const SEMICOLON: u8 = b';';
    pub const COMMA: u8 = b',';
    /* Operations */
    pub const TILDE: u8 = b'~';
    pub const STAR: u8 = b'*';
    pub const SLASH: u8 = b'/';
    pub const PERCENT: u8 = b'%';
    pub const AND: &str = "&&";
    pub const OR: &str = "||";
    /* Control */
    pub const QUESTION: u8 = b'?';
    pub const COLON: u8 = b':';

    lazy_static! {
        /* Tokens whose existence _does_ depend on the subsequent char (eg `\b`) */
        /* Operations */
        pub static ref MINUSES: Regex = Regex::new(r"^--?").unwrap();
        pub static ref PLUSES: Regex = Regex::new(r"^\+\+?").unwrap();
        pub static ref BANG_EQ: Regex = Regex::new(r"^\!=?").unwrap();
        pub static ref LT_EQ: Regex = Regex::new(r"^<=?").unwrap();
        pub static ref GT_EQ: Regex = Regex::new(r"^>=?").unwrap();
        pub static ref EQ_EQ: Regex = Regex::new(r"^==?").unwrap();
        /* Keywords */
        pub static ref KW_RET: Regex = Regex::new(r"^return\b").unwrap();
        /* Types */
        pub static ref KW_INT: Regex = Regex::new(r"^int\b").unwrap();
        pub static ref KW_VOID: Regex = Regex::new(r"^void\b").unwrap();
        /* Storage class specifiers */
        pub static ref KW_STATIC: Regex = Regex::new(r"^static\b").unwrap();
        pub static ref KW_EXTERN: Regex = Regex::new(r"^extern\b").unwrap();
        /* Control */
        pub static ref KW_IF: Regex = Regex::new(r"^if\b").unwrap();
        pub static ref KW_ELSE: Regex = Regex::new(r"^else\b").unwrap();
        /* Loop */
        pub static ref KW_DO: Regex = Regex::new(r"^do\b").unwrap();
        pub static ref KW_WHILE: Regex = Regex::new(r"^while\b").unwrap();
        pub static ref KW_FOR: Regex = Regex::new(r"^for\b").unwrap();
        pub static ref KW_BREAK: Regex = Regex::new(r"^break\b").unwrap();
        pub static ref KW_CONTINUE: Regex = Regex::new(r"^continue\b").unwrap();
        /* Misc variable-length tokens */
        pub static ref DECIMALS: Regex = Regex::new(r"^[0-9]+\b").unwrap();
        pub static ref IDENT: Regex = Regex::new(r"^[a-zA-Z_]\w*\b").unwrap();
    }
}
