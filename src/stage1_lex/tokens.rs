use crate::common::{identifier::RawIdentifier, primitive::Const};
use derive_more::From;

#[derive(From, PartialEq, Debug)]
pub enum Token {
    Demarcator(Demarcator),
    Keyword(Keyword),
    TypeSpecifier(TypeSpecifier),
    StorageClassSpecifier(StorageClassSpecifier),
    Operator(Operator),
    Control(Control),
    Loop(Loop),
    Const(Const),
    String(Vec<u8>),
    Identifier(RawIdentifier),
}
#[derive(PartialEq, Debug)]
pub enum Demarcator {
    ParenOpen,
    ParenClose,
    CurlyOpen,
    CurlyClose,
    SquareOpen,
    SquareClose,
    Semicolon,
    Comma,
}
#[derive(PartialEq, Debug)]
pub enum Keyword {
    Return,
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum TypeSpecifier {
    Void = 0,
    Char = 1,
    Int = 2,
    Long = 3,
    Signed = 4,
    Unsigned = 5,
    Double = 6,
}
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum StorageClassSpecifier {
    Static,
    Extern,
}
#[derive(PartialEq, Debug)]
pub enum Operator {
    /* arithmetic and misc basic */
    Tilde,
    Bang,
    Minus,
    Plus,
    Star,
    Slash,
    Percent,
    Ampersand,
    /* binary logic */
    And,
    Or,
    /* binary comparison */
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    /* assign */
    Assign,
    /* control */
    Question,
    Colon,
    /* sizeof */
    SizeOf,
}
#[derive(PartialEq, Debug)]
pub enum Control {
    If,
    Else,
}
#[derive(PartialEq, Debug)]
pub enum Loop {
    Do,
    While,
    For,
    Break,
    Continue,
}
