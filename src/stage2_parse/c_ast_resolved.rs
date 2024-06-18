pub use self::declaration::*;
pub use self::expression::*;
pub use self::statement::*;
pub use crate::{
    stage2_parse::c_ast::{BinaryOperator, Const, UnaryOperator},
    symbol_table::ResolvedIdentifier,
};
use derivative::Derivative;
use getset::Getters;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug)]
pub struct Program {
    pub funs: Vec<FunctionDeclOrDefn>,
}

pub(super) enum Declaration {
    VarDecl(VariableDeclaration),
    FunDecl(FunctionDeclaration),
    FunDefn,
}
#[derive(Debug)]
pub enum FunctionDeclOrDefn {
    FunDecl(FunctionDeclaration),
    FunDefn(FunctionDefinition),
}
#[derive(Debug)]
pub enum BlockScopeDeclaration {
    VarDecl(VariableDeclaration),
    FunDecl(FunctionDeclaration),
}
mod declaration {
    use super::*;

    #[derive(Debug)]
    pub struct VariableDeclaration {
        pub ident: Rc<ResolvedIdentifier>,
        pub init: Option<Expression>,
    }

    #[derive(Debug)]
    pub struct FunctionDeclaration {
        pub ident: Rc<ResolvedIdentifier>,
        pub params: Vec<Rc<ResolvedIdentifier>>,
    }

    #[derive(Debug)]
    pub struct FunctionDefinition {
        pub decl: FunctionDeclaration,
        pub body: Block,
    }
}

#[derive(Debug)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum BlockItem {
    Declaration(BlockScopeDeclaration),
    Statement(Statement),
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    If(If),
    Compound(Block),
    Break(Rc<LoopId>),
    Continue(Rc<LoopId>),
    While(Rc<LoopId>, CondBody),
    DoWhile(Rc<LoopId>, CondBody),
    For(Rc<LoopId>, For),
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
    Var(Rc<ResolvedIdentifier>),
    Unary(Unary),
    Binary(Binary),
    Assignment(Assignment),
    Conditional(Conditional),
    FunctionCall(FunctionCall),
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
        pub ident: Rc<ResolvedIdentifier>,
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
        pub ident: Rc<ResolvedIdentifier>,
        pub args: Vec<Expression>,
    }
}

#[derive(Derivative, Getters, Debug)]
#[derivative(PartialEq, Eq, Hash)]
#[getset(get = "pub")]
pub struct LoopId {
    id: usize,

    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    descr: &'static str,
}
impl LoopId {
    pub fn new(descr: &'static str) -> Self {
        static NEXT_ID: AtomicUsize = AtomicUsize::new(0);
        let curr_id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
        Self { id: curr_id, descr }
    }
}
