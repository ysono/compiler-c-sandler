use derive_more::{Deref, From};

#[derive(From, PartialEq, Eq, Debug)]
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
#[derive(PartialEq, Eq, Debug)]
pub enum Demarcator {
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Semicolon,
    Comma,
}
#[derive(PartialEq, Eq, Debug)]
pub enum Keyword {
    Return,
}
#[derive(PartialEq, Eq, Debug)]
pub enum Type {
    Int,
    Void,
}
#[derive(PartialEq, Eq, Debug)]
pub enum StorageClassSpecifier {
    Static,
    Extern,
}
#[derive(PartialEq, Eq, Debug)]
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
#[derive(PartialEq, Eq, Debug)]
pub enum Control {
    If,
    Else,
}
#[derive(PartialEq, Eq, Debug)]
pub enum Loop {
    Do,
    While,
    For,
    Break,
    Continue,
}
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Const {
    Int(i32),
}
#[derive(Deref, PartialEq, Eq, Hash, Debug)]
pub struct Identifier(pub(super) String);
