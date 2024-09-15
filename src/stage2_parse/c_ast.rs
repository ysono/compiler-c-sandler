pub use self::{declaration::*, expression::*, statement::*};
pub use crate::stage1_lex::tokens::StorageClassSpecifier;
use crate::{
    common::{
        identifier::UniqueId,
        symbol_table_frontend::StaticVisibility,
        types_frontend::{Const, FunType, VarType},
    },
    ds_n_a::singleton::Singleton,
};
use derivative::Derivative;
use std::fmt::Debug;

pub trait CAstVariant {
    type FileScopeDeclaration: Debug;
    type BlockScopeDeclaration: Debug;
    type ForInitDeclaration: Debug;
    type Identifier: Debug;
    type LoopId: Debug;
    type Expression: Debug;
    type LvalueExpression: Debug;
}

#[derive(Debug)]
pub struct Program<T: CAstVariant> {
    pub decls: Vec<T::FileScopeDeclaration>,
}

#[derive(Debug)]
pub enum Declaration<T: CAstVariant> {
    Var(VariableDeclaration<T>),
    Fun(FunctionDeclaration<T>),
}
mod declaration {
    use super::*;

    #[derive(Debug)]
    pub struct VariableDeclaration<T: CAstVariant> {
        pub ident: T::Identifier,
        pub typ: Singleton<VarType>,
        pub storage_class: Option<StorageClassSpecifier>,
        pub init: Option<T::Expression>,
    }
    #[derive(Debug)]
    pub struct VariableDefinition<T: CAstVariant> {
        pub ident: T::Identifier,
        pub init: T::Expression,
    }

    #[derive(Debug)]
    pub struct FunctionDeclaration<T: CAstVariant> {
        pub ident: T::Identifier,
        pub typ: Singleton<FunType>,
        pub storage_class: Option<StorageClassSpecifier>,
        pub param_idents: Vec<T::Identifier>,
        pub body: Option<Block<T>>,
    }
    #[derive(Debug)]
    pub struct FunctionDefinition<T: CAstVariant> {
        pub ident: T::Identifier,
        pub visibility: StaticVisibility,
        pub param_idents: Vec<T::Identifier>,
        pub body: Block<T>,
    }
}

#[derive(Debug)]
pub struct Block<T: CAstVariant> {
    pub items: Vec<BlockItem<T>>,
}

#[derive(Debug)]
pub enum BlockItem<T: CAstVariant> {
    Declaration(T::BlockScopeDeclaration),
    Statement(Statement<T>),
}

#[derive(Debug)]
pub enum Statement<T: CAstVariant> {
    Return(T::Expression),
    Expression(T::Expression),
    If(If<T>),
    Compound(Block<T>),
    Break(T::LoopId),
    Continue(T::LoopId),
    While(T::LoopId, CondBody<T>),
    DoWhile(T::LoopId, CondBody<T>),
    For(T::LoopId, For<T>),
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
        Decl(T::ForInitDeclaration),
        Exp(T::Expression),
        None,
    }

    #[derive(Derivative, Debug)]
    #[derivative(PartialEq, Eq, Hash)]
    pub struct LoopId {
        pub id: UniqueId,

        #[derivative(PartialEq = "ignore", Hash = "ignore")]
        pub descr: &'static str,
    }
    impl LoopId {
        pub fn new(descr: &'static str) -> Self {
            let id = UniqueId::new();
            Self { id, descr }
        }
    }
}

#[derive(Debug)]
pub struct TypedExpression<Exp> {
    pub exp: Exp,
    pub typ: Singleton<VarType>,
}
#[derive(Debug)]
pub enum Expression<T: CAstVariant> {
    Const(Const),
    Var(T::Identifier),
    Cast(Cast<T>),
    Unary(Unary<T>),
    Binary(Binary<T>),
    Assignment(Assignment<T>),
    Conditional(Conditional<T>),
    FunctionCall(FunctionCall<T>),
    Dereference(Dereference<T>),
    AddrOf(AddrOf<T>),
}
#[derive(Debug)]
pub enum LvalueExpression<T: CAstVariant> {
    Var(T::Identifier),
    Dereference(Dereference<T>),
}
mod expression {
    use super::*;

    #[derive(Debug)]
    pub struct Cast<T: CAstVariant> {
        pub typ: Singleton<VarType>,
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
    pub enum UnaryOperator {
        /* -> int */
        Complement,
        Negate,
        /* -> bool */
        Not,
    }

    #[derive(Debug)]
    pub enum BinaryOperator {
        /* arithmetic */
        Sub,
        Add,
        Mul,
        Div,
        Rem,
        /* logic */
        And,
        Or,
        /* compare */
        Eq,
        Neq,
        Lt,
        Lte,
        Gt,
        Gte,
    }

    #[derive(Debug)]
    pub struct Assignment<T: CAstVariant> {
        pub lhs: Box<T::LvalueExpression>,
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
        pub ident: T::Identifier,
        pub args: Vec<T::Expression>,
    }

    #[derive(Debug)]
    pub struct Dereference<T: CAstVariant>(pub Box<T::Expression>);

    #[derive(Debug)]
    pub struct AddrOf<T: CAstVariant>(pub Box<T::LvalueExpression>);
}
