pub use self::declaration::*;
pub use self::expression::*;
pub use self::statement::*;
pub use crate::stage2_parse::c_ast::{BinaryOperator, Const, StorageClassSpecifier, UnaryOperator};
use crate::symbol_table::{FunType, ResolvedIdentifier, VarType};
use derivative::Derivative;
use getset::Getters;
use std::fmt::Debug;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

pub trait CAstVariant {
    type Expression: Debug;
}

#[derive(Debug)]
pub struct Program<T: CAstVariant> {
    pub decls: Vec<Declaration<T>>,
}

#[derive(Debug)]
pub enum Declaration<T: CAstVariant> {
    VarDecl(VariableDeclaration<T>),
    FunDecl(FunctionDeclaration),
    FunDefn(FunctionDefinition<T>),
}
#[derive(Debug)]
pub enum BlockScopeDeclaration<T: CAstVariant> {
    VarDecl(VariableDeclaration<T>),
    FunDecl(FunctionDeclaration),
}
mod declaration {
    use super::*;

    #[derive(Debug)]
    pub struct VariableDeclaration<T: CAstVariant> {
        pub ident: Rc<ResolvedIdentifier>,
        pub init: Option<T::Expression>,
        pub typ: VarType,
        pub storage_class: Option<StorageClassSpecifier>,
    }

    #[derive(Debug)]
    pub struct FunctionDeclaration {
        pub ident: Rc<ResolvedIdentifier>,
        pub param_idents: Vec<Rc<ResolvedIdentifier>>,
        pub typ: Rc<FunType>,
        pub storage_class: Option<StorageClassSpecifier>,
    }

    #[derive(Debug)]
    pub struct FunctionDefinition<T: CAstVariant> {
        pub decl: FunctionDeclaration,
        pub body: Block<T>,
    }
}

#[derive(Debug)]
pub struct Block<T: CAstVariant> {
    pub items: Vec<BlockItem<T>>,
}

#[derive(Debug)]
pub enum BlockItem<T: CAstVariant> {
    Declaration(BlockScopeDeclaration<T>),
    Statement(Statement<T>),
}

#[derive(Debug)]
pub enum Statement<T: CAstVariant> {
    Return(T::Expression),
    Expression(T::Expression),
    If(If<T>),
    Compound(Block<T>),
    Break(Rc<LoopId>),
    Continue(Rc<LoopId>),
    While(Rc<LoopId>, CondBody<T>),
    DoWhile(Rc<LoopId>, CondBody<T>),
    For(Rc<LoopId>, For<T>),
    Null,
}
mod statement {
    use super::*;

    #[derive(Debug)]
    pub struct If<T: CAstVariant> {
        pub condition: T::Expression,
        pub then: Box<Statement<T>>,
        pub elze: Option<Box<Statement<T>>>,
    }

    #[derive(Debug)]
    pub struct CondBody<T: CAstVariant> {
        pub condition: T::Expression,
        pub body: Box<Statement<T>>,
    }

    #[derive(Debug)]
    pub struct For<T: CAstVariant> {
        pub init: ForInit<T>,
        pub condition: Option<T::Expression>,
        pub post: Option<T::Expression>,
        pub body: Box<Statement<T>>,
    }

    #[derive(Debug)]
    pub enum ForInit<T: CAstVariant> {
        Decl(VariableDeclaration<T>),
        Exp(T::Expression),
        None,
    }
}

#[derive(Debug)]
pub struct TypedExpression<T: CAstVariant> {
    pub exp: Expression<T>,
    pub typ: VarType,
}
#[derive(Debug)]
pub enum Expression<T: CAstVariant> {
    Const(Const),
    Var(Rc<ResolvedIdentifier>),
    Cast(Cast<T>),
    Unary(Unary<T>),
    Binary(Binary<T>),
    Assignment(Assignment<T>),
    Conditional(Conditional<T>),
    FunctionCall(FunctionCall<T>),
}
mod expression {
    use super::*;

    #[derive(Debug)]
    pub struct Cast<T: CAstVariant> {
        pub typ: VarType,
        pub sub_exp: Box<T::Expression>,
    }

    #[derive(Debug)]
    pub struct Unary<T: CAstVariant> {
        pub op: UnaryOperator,
        pub sub_exp: Box<T::Expression>,
    }

    #[derive(Debug)]
    pub struct Binary<T: CAstVariant> {
        pub op: BinaryOperator,
        pub lhs: Box<T::Expression>,
        pub rhs: Box<T::Expression>,
    }

    #[derive(Debug)]
    pub struct Assignment<T: CAstVariant> {
        pub ident: Rc<ResolvedIdentifier>,
        pub rhs: Box<T::Expression>,
    }

    #[derive(Debug)]
    pub struct Conditional<T: CAstVariant> {
        pub condition: Box<T::Expression>,
        pub then: Box<T::Expression>,
        pub elze: Box<T::Expression>,
    }

    #[derive(Debug)]
    pub struct FunctionCall<T: CAstVariant> {
        pub ident: Rc<ResolvedIdentifier>,
        pub args: Vec<T::Expression>,
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
