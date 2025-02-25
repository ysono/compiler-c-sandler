pub use self::{declaration::*, expression::*, statement::*, typed_expression::*};
pub use crate::stage1_lex::tokens::StorageClassSpecifier;
use crate::{
    common::{
        primitive::Const,
        symbol_table_frontend::{InitializerItem, StaticVisibility},
        types_frontend::{ObjType, ParsedFunType, ScalarFunType, ScalarType, SubObjType},
    },
    ds_n_a::singleton::Singleton,
};
use derive_more::From;
use std::fmt::Debug;

pub trait CAstVariant {
    type FileScopeDeclaration: Debug;
    type BlockScopeDeclaration: Debug;
    type ForInitDeclaration: Debug;

    type Identifier: Debug;

    type LoopId: Debug;

    type Expression: Debug;
    type ScalarExpression: Debug;
    type LvalueExpression: Debug;
    type ScalarLvalueExpression: Debug;

    type BinaryOperator: Debug;
    type StringExpression: Debug;
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
        pub typ: Singleton<ObjType>,
        pub storage_class: Option<StorageClassSpecifier>,
        pub init: Option<VariableInitializer<C>>,
    }

    #[derive(Debug)]
    pub enum VariableInitializer<C: CAstVariant> {
        Single(C::Expression),
        Compound(Vec<VariableInitializer<C>>),
    }

    #[derive(Debug)]
    pub struct VariableDefinition<C: CAstVariant> {
        pub ident: C::Identifier,
        pub init: Vec<RuntimeInitializerItem>,
    }

    pub type RuntimeInitializerItem = InitializerItem<TypedExp<ScalarType>, ()>;

    #[derive(Debug)]
    pub struct FunctionDeclaration<C: CAstVariant> {
        pub ident: C::Identifier,
        pub typ: Singleton<ParsedFunType>,
        pub storage_class: Option<StorageClassSpecifier>,
        pub param_idents: Vec<C::Identifier>,
        pub body: Option<Block<C>>,
    }

    #[derive(Debug)]
    pub struct FunctionDefinition<C: CAstVariant> {
        pub ident: C::Identifier,
        pub typ: Singleton<ScalarFunType>,
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
    Return(C::ScalarExpression),
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
        pub condition: C::ScalarExpression,
        pub then: Box<Statement<C>>,
        pub elze: Option<Box<Statement<C>>>,
    }

    #[derive(Debug)]
    pub struct CondBody<C: CAstVariant> {
        pub condition: C::ScalarExpression,
        pub body: Box<Statement<C>>,
    }

    #[derive(Debug)]
    pub struct For<C: CAstVariant> {
        pub init: ForInit<C>,
        pub condition: Option<C::ScalarExpression>,
        pub post: Option<C::Expression>,
        pub body: Box<Statement<C>>,
    }

    #[derive(Debug)]
    pub enum ForInit<C: CAstVariant> {
        Decl(C::ForInitDeclaration),
        Exp(C::Expression),
        None,
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
    String(C::StringExpression),
    Var(C::Identifier),
    Dereference(Dereference<C>),
    Subscript(Subscript<C>),
}
mod expression {
    use super::*;

    #[derive(Debug)]
    pub struct Cast<C: CAstVariant> {
        pub typ: Singleton<ObjType>,
        pub sub_exp: Box<C::ScalarExpression>,
    }

    #[derive(Debug)]
    pub struct Unary<C: CAstVariant> {
        pub op: UnaryOperator,
        pub sub_exp: Box<C::ScalarExpression>,
    }

    #[derive(Debug)]
    pub struct Binary<C: CAstVariant> {
        pub op: C::BinaryOperator,
        pub lhs: Box<C::ScalarExpression>,
        pub rhs: Box<C::ScalarExpression>,
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
    #[derive(From, Debug)]
    pub enum TypeCheckedBinaryOperator {
        Arith(ArithmeticBinaryOperator),
        ArithPtr(PointerArithmeticBinaryOperator),
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
    #[allow(clippy::enum_variant_names)]
    #[derive(Clone, Copy, Debug)]
    pub enum PointerArithmeticBinaryOperator {
        PointerPlusInteger,
        PointerMinusInteger,
        PointerMinusPointer,
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
        pub condition: Box<C::ScalarExpression>,
        pub then: Box<C::ScalarExpression>,
        pub elze: Box<C::ScalarExpression>,
    }

    #[derive(Debug)]
    pub struct FunctionCall<C: CAstVariant> {
        pub ident: C::Identifier,
        pub args: Vec<C::ScalarExpression>,
    }

    #[derive(Debug)]
    pub struct Assignment<C: CAstVariant> {
        pub lhs: Box<C::ScalarLvalueExpression>,
        pub rhs: Box<C::ScalarExpression>,
    }

    #[derive(Debug)]
    pub struct AddrOf<C: CAstVariant>(pub Box<C::LvalueExpression>);

    #[derive(Debug)]
    pub struct Dereference<C: CAstVariant>(pub Box<C::ScalarExpression>);

    #[derive(Debug)]
    pub struct Subscript<C: CAstVariant> {
        pub exp1: Box<C::ScalarExpression>,
        pub exp2: Box<C::ScalarExpression>,
    }
}

mod typed_expression {
    use super::super::phase3_typecheck::TypeCheckedCAst;
    use super::*;

    #[derive(Debug)]
    pub enum TypedExp<LTyp> {
        R(TypedRExp),
        L(TypedLExp<LTyp>),
    }
    impl TypedExp<ScalarType> {
        pub fn typ(&self) -> &SubObjType<ScalarType> {
            match self {
                Self::R(typed_rexp) => &typed_rexp.typ,
                Self::L(typed_lexp) => &typed_lexp.typ,
            }
        }
    }

    #[derive(Debug)]
    pub struct TypAndExp<Typ, Exp> {
        pub typ: Typ,
        pub exp: Exp,
    }
    pub type TypedRExp = TypAndExp<SubObjType<ScalarType>, RExp<TypeCheckedCAst>>;
    pub type TypedLExp<LTyp> = TypAndExp<SubObjType<LTyp>, LExp<TypeCheckedCAst>>;
}
