pub use self::declaration::*;
pub use self::expression::*;
pub use self::statement::*;
pub use crate::stage1_lex::tokens::{Const, Identifier, StorageClassSpecifier};
use crate::symbol_table::{FunType, VarType};

#[derive(Debug)]
pub struct Program {
    pub decls: Vec<Declaration>,
}

#[derive(Debug)]
pub enum Declaration {
    VarDecl(VariableDeclaration),
    FunDecl(FunctionDeclaration),
}
mod declaration {
    use super::*;

    #[derive(Debug)]
    pub struct VariableDeclaration {
        pub ident: Identifier,
        pub init: Option<Expression>,
        pub typ: VarType,
        pub storage_class: Option<StorageClassSpecifier>,
    }

    #[derive(Debug)]
    pub struct FunctionDeclaration {
        pub ident: Identifier,
        pub param_idents: Vec<Identifier>,
        pub body: Option<Block>,
        pub typ: FunType,
        pub storage_class: Option<StorageClassSpecifier>,
    }
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
        Decl(VariableDeclaration),
        Exp(Expression),
        None,
    }
}

#[derive(Debug)]
pub enum Expression {
    Const(Const),
    Var(Identifier),
    Cast(Cast),
    Unary(Unary),
    Binary(Binary),
    Assignment(Assignment),
    Conditional(Conditional),
    FunctionCall(FunctionCall),
}
mod expression {
    use super::*;

    #[derive(Debug)]
    pub struct Cast {
        pub typ: VarType,
        pub sub_exp: Box<Expression>,
    }

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

    #[derive(Debug)]
    pub struct FunctionCall {
        pub ident: Identifier,
        pub args: Vec<Expression>,
    }
}
