pub use self::expression::*;
pub use self::statement::*;
pub use crate::stage1_lex::tokens::{Const, Identifier};

#[derive(Debug)]
pub struct Program {
    pub func: Function,
}

#[derive(Debug)]
pub struct Function {
    pub ident: Identifier,
    pub body: Block,
}

#[derive(Debug)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}

#[derive(Debug)]
pub struct Declaration {
    pub ident: Identifier,
    pub init: Option<Expression>,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    If(If),
    Compound(Block),
    Break,
    Continue,
    While(CondBody),
    DoWhile(CondBody),
    For(For),
    Null,
}
mod statement {
    use super::*;

    #[derive(Debug)]
    pub struct If {
        pub condition: Expression,
        pub then: Box<Statement>,
        pub elze: Option<Box<Statement>>,
    }

    #[derive(Debug)]
    pub struct CondBody {
        pub condition: Expression,
        pub body: Box<Statement>,
    }

    #[derive(Debug)]
    pub struct For {
        pub init: ForInit,
        pub condition: Option<Expression>,
        pub post: Option<Expression>,
        pub body: Box<Statement>,
    }

    #[derive(Debug)]
    pub enum ForInit {
        Decl(Declaration),
        Exp(Expression),
        None,
    }
}

#[derive(Debug)]
pub enum Expression {
    Const(Const),
    Var(Identifier),
    Unary(Unary),
    Binary(Binary),
    Assignment(Assignment),
    Conditional(Conditional),
}
mod expression {
    use super::*;

    #[derive(Debug)]
    pub struct Unary {
        pub op: UnaryOperator,
        pub sub_exp: Box<Expression>,
    }

    #[derive(Debug)]
    pub struct Binary {
        pub op: BinaryOperator,
        pub lhs: Box<Expression>,
        pub rhs: Box<Expression>,
    }

    #[derive(Debug)]
    pub struct Assignment {
        pub lhs: Box<Expression>,
        pub rhs: Box<Expression>,
    }

    #[derive(Debug)]
    pub struct Conditional {
        pub condition: Box<Expression>,
        pub then: Box<Expression>,
        pub elze: Box<Expression>,
    }
}

#[derive(Debug)]
pub enum UnaryOperator {
    /* -> int */
    Complement,
    Negate,
    /* -> bool */
    Not,
}

#[derive(Debug)]
pub enum BinaryOperator {
    /* -> int */
    Sub,
    Add,
    Mul,
    Div,
    Rem,
    /* -(logic)-> bool */
    And,
    Or,
    /* -(compare)-> bool */
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}
