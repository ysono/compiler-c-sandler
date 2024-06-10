pub use self::expression::*;
pub use self::statement::*;
pub use crate::stage2_parse::c_ast::{BinaryOperator, Const, Identifier, UnaryOperator};
use derivative::Derivative;
use getset::Getters;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug)]
pub struct Program {
    pub func: FunctionDeclaration,
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub var: Rc<Variable>,
    pub init: Option<Expression>,
}
#[derive(Debug)]
pub struct FunctionDeclaration {
    pub ident: Identifier,
    pub body: Block,
}

#[derive(Debug)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum BlockItem {
    Declaration(VariableDeclaration),
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
    Var(Rc<Variable>),
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
        pub var: Rc<Variable>,
        pub rhs: Box<Expression>,
    }

    #[derive(Debug)]
    pub struct Conditional {
        pub condition: Box<Expression>,
        pub then: Box<Expression>,
        pub elze: Box<Expression>,
    }
}

#[derive(Derivative, Getters, Debug)]
#[derivative(PartialEq, Eq, Hash)]
#[getset(get = "pub")]
pub struct Variable {
    id: usize,

    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    orig_ident: Option<Rc<Identifier>>,
}
impl Variable {
    pub fn new_anon() -> Self {
        Self::new(None)
    }
    pub(super) fn new(orig_ident: Option<Rc<Identifier>>) -> Self {
        static NEXT_ID: AtomicUsize = AtomicUsize::new(0);
        let curr_id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
        Self {
            id: curr_id,
            orig_ident,
        }
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
