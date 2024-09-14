use crate::common::types_frontend::Const;
use derive_more::{Deref, From};

#[derive(From, PartialEq, Debug)]
pub enum Token {
    Demarcator(Demarcator),
    Keyword(Keyword),
    Type(Type),
    StorageClassSpecifier(StorageClassSpecifier),
    Operator(Operator),
    Control(Control),
    Loop(Loop),
    Const(Const),
    Identifier(Identifier),
}
#[derive(PartialEq, Debug)]
pub enum Demarcator {
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Semicolon,
    Comma,
}
#[derive(PartialEq, Debug)]
pub enum Keyword {
    Return,
}
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Type {
    Void = 0,
    Int = 1,
    Long = 2,
    Signed = 3,
    Unsigned = 4,
    Double = 5,
}
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum StorageClassSpecifier {
    Static,
    Extern,
}
#[derive(PartialEq, Debug)]
pub enum Operator {
    /* unary -> int */
    Tilde,
    /* unary -> bool */
    Not,
    /* unary or binary -> int */
    Minus,
    /* binary -> int */
    Plus,
    Star,
    Slash,
    Percent,
    /* binary -(logic)-> boolean */
    And,
    Or,
    /* binary -(compare)-> boolean */
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
#[derive(Deref, PartialEq, Eq, Hash, Debug)]
pub struct Identifier(pub(super) String);
