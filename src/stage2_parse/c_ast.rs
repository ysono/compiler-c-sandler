pub use self::{declaration::*, expression::*, statement::*, typed_expression::*};
pub use crate::stage1_lex::tokens::StorageClassSpecifier;
use crate::{
    common::{
        identifier::UniqueId,
        primitive::Const,
        symbol_table_frontend::StaticVisibility,
        types_frontend::{FunType, VarType},
    },
    ds_n_a::singleton::Singleton,
};
use derivative::Derivative;
use derive_more::From;
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
pub struct Program<C: CAstVariant> {
    pub decls: Vec<C::FileScopeDeclaration>,
}

#[derive(Debug)]
pub enum Declaration<C: CAstVariant> {
    Var(VariableDeclaration<C>),
    Fun(FunctionDeclaration<C>),
}
mod declaration {
    use super::*;

    #[derive(Debug)]
    pub struct VariableDeclaration<C: CAstVariant> {
        pub ident: C::Identifier,
        pub typ: Singleton<VarType>,
        pub storage_class: Option<StorageClassSpecifier>,
        pub init: Option<C::Expression>,
    }
    #[derive(Debug)]
    pub struct VariableDefinition<C: CAstVariant> {
        pub ident: C::Identifier,
        pub init: C::Expression,
    }

    #[derive(Debug)]
    pub struct FunctionDeclaration<C: CAstVariant> {
        pub ident: C::Identifier,
        pub typ: Singleton<FunType>,
        pub storage_class: Option<StorageClassSpecifier>,
        pub param_idents: Vec<C::Identifier>,
        pub body: Option<Block<C>>,
    }
    #[derive(Debug)]
    pub struct FunctionDefinition<C: CAstVariant> {
        pub ident: C::Identifier,
        pub typ: Singleton<FunType>,
        pub visibility: StaticVisibility,
        pub param_idents: Vec<C::Identifier>,
        pub body: Block<C>,
    }
}

#[derive(Debug)]
pub struct Block<C: CAstVariant> {
    pub items: Vec<BlockItem<C>>,
}

#[derive(Debug)]
pub enum BlockItem<C: CAstVariant> {
    Declaration(C::BlockScopeDeclaration),
    Statement(Statement<C>),
}

#[derive(Debug)]
pub enum Statement<C: CAstVariant> {
    Return(C::Expression),
    Expression(C::Expression),
    If(If<C>),
    Compound(Block<C>),
    Break(C::LoopId),
    Continue(C::LoopId),
    While(C::LoopId, CondBody<C>),
    DoWhile(C::LoopId, CondBody<C>),
    For(C::LoopId, For<C>),
    Null,
}
mod statement {
    use super::*;

    #[derive(Debug)]
    pub struct If<C: CAstVariant> {
        pub condition: C::Expression,
        pub then: Box<Statement<C>>,
        pub elze: Option<Box<Statement<C>>>,
    }

    #[derive(Debug)]
    pub struct CondBody<C: CAstVariant> {
        pub condition: C::Expression,
        pub body: Box<Statement<C>>,
    }

    #[derive(Debug)]
    pub struct For<C: CAstVariant> {
        pub init: ForInit<C>,
        pub condition: Option<C::Expression>,
        pub post: Option<C::Expression>,
        pub body: Box<Statement<C>>,
    }

    #[derive(Debug)]
    pub enum ForInit<C: CAstVariant> {
        Decl(C::ForInitDeclaration),
        Exp(C::Expression),
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

#[derive(From, Debug)]
pub enum Expression<C: CAstVariant> {
    R(RExp<C>),
    L(LExp<C>),
}
/// Rvalue expression.
#[derive(Debug)]
pub enum RExp<C: CAstVariant> {
    Const(Const),
    Cast(Cast<C>),
    Unary(Unary<C>),
    Binary(Binary<C>),
    Conditional(Conditional<C>),
    FunctionCall(FunctionCall<C>),
    Assignment(Assignment<C>),
    AddrOf(AddrOf<C>),
}
/// Lvalue expression.
#[derive(Debug)]
pub enum LExp<C: CAstVariant> {
    Var(C::Identifier),
    Dereference(Dereference<C>),
}
mod expression {
    use super::*;

    #[derive(Debug)]
    pub struct Cast<C: CAstVariant> {
        pub typ: Singleton<VarType>,
        pub sub_exp: Box<C::Expression>,
    }

    #[derive(Debug)]
    pub struct Unary<C: CAstVariant> {
        pub op: UnaryOperator,
        pub sub_exp: Box<C::Expression>,
    }

    #[derive(Debug)]
    pub struct Binary<C: CAstVariant> {
        pub op: BinaryOperator,
        pub lhs: Box<C::Expression>,
        pub rhs: Box<C::Expression>,
    }

    #[derive(Debug)]
    pub enum UnaryOperator {
        /* -> int */
        Complement,
        Negate,
        /* -> bool */
        Not,
    }

    #[derive(From, Debug)]
    pub enum BinaryOperator {
        Arith(ArithmeticBinaryOperator),
        Logic(LogicBinaryOperator),
        Cmp(ComparisonBinaryOperator),
    }
    #[derive(Debug)]
    pub enum ArithmeticBinaryOperator {
        Sub,
        Add,
        Mul,
        Div,
        Rem,
    }
    #[derive(Clone, Copy, Debug)]
    pub enum LogicBinaryOperator {
        And,
        Or,
    }
    #[derive(Debug)]
    pub enum ComparisonBinaryOperator {
        Eq,
        Neq,
        Lt,
        Lte,
        Gt,
        Gte,
    }

    #[derive(Debug)]
    pub struct Conditional<C: CAstVariant> {
        pub condition: Box<C::Expression>,
        pub then: Box<C::Expression>,
        pub elze: Box<C::Expression>,
    }

    #[derive(Debug)]
    pub struct FunctionCall<C: CAstVariant> {
        pub ident: C::Identifier,
        pub args: Vec<C::Expression>,
    }

    #[derive(Debug)]
    pub struct Assignment<C: CAstVariant> {
        pub lhs: Box<C::LvalueExpression>,
        pub rhs: Box<C::Expression>,
    }

    #[derive(Debug)]
    pub struct AddrOf<C: CAstVariant>(pub Box<C::LvalueExpression>);

    #[derive(Debug)]
    pub struct Dereference<C: CAstVariant>(pub Box<C::Expression>);
}

mod typed_expression {
    use super::super::phase3_typecheck::TypeCheckedCAst;
    use super::*;

    #[derive(Debug)]
    pub enum TypedExp {
        R(TypedRExp),
        L(TypedLExp),
    }
    impl TypedExp {
        pub fn typ(&self) -> &Singleton<VarType> {
            match self {
                Self::R(typed_rexp) => &typed_rexp.typ,
                Self::L(typed_lexp) => &typed_lexp.typ,
            }
        }
    }

    #[derive(Debug)]
    pub struct TypAndExp<Exp> {
        pub typ: Singleton<VarType>,
        pub exp: Exp,
    }
    pub type TypedRExp = TypAndExp<RExp<TypeCheckedCAst>>;
    pub type TypedLExp = TypAndExp<LExp<TypeCheckedCAst>>;
}
