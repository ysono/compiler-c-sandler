pub use self::{declaration::*, expression::*, statement::*, typed_expression::*};
pub use crate::stage1_lex::tokens::StorageClassSpecifier;
use crate::{
    common::{
        primitive::Const,
        symbol_table_frontend::{InitializerItem, StaticVisibility},
        types_frontend::{
            NonVoidType, ObjType, ParsedFunType, ParsedObjType, ScalarFunType, ScalarType,
            SubObjType,
        },
    },
    ds_n_a::singleton::Singleton,
};
use derive_more::From;
use std::fmt::Debug;

#[allow(non_camel_case_types)]
pub trait CAstVariant {
    /* Declarations */

    type FileScopeDeclaration: Debug;
    type BlockScopeDeclaration: Debug;
    type ForInitDeclaration: Debug;

    /* IDs */

    type SymbolId: Debug;
    type LoopId: Debug;

    /* Categories of Expressions */

    type Expression_AnyType: Debug;
    type Expression_ScalarType: Debug;
    type Expression_Lvalue_AnyType: Debug;
    type Expression_Lvalue_ScalarType: Debug;

    /* Specific Expressions ; Operands */

    type BinaryOperator: Debug;
    type StringExpression: Debug;
    type TypeOperand<Typ: Debug>: Debug;
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
        pub ident: C::SymbolId,
        pub typ: ParsedObjType,
        pub storage_class: Option<StorageClassSpecifier>,
        pub init: Option<VariableInitializer<C>>,
    }

    #[derive(Debug)]
    pub enum VariableInitializer<C: CAstVariant> {
        Single(C::Expression_AnyType),
        Compound(Vec<VariableInitializer<C>>),
    }

    #[derive(Debug)]
    pub struct VariableDefinition<C: CAstVariant> {
        pub ident: C::SymbolId,
        pub init: Vec<RuntimeInitializerItem>,
    }

    pub type RuntimeInitializerItem = InitializerItem<ScalarExp, ()>;

    #[derive(Debug)]
    pub struct FunctionDeclaration<C: CAstVariant> {
        pub ident: C::SymbolId,
        pub typ: Singleton<ParsedFunType>,
        pub storage_class: Option<StorageClassSpecifier>,
        pub param_idents: Vec<C::SymbolId>,
        pub body: Option<Block<C>>,
    }

    #[derive(Debug)]
    pub struct FunctionDefinition<C: CAstVariant> {
        pub ident: C::SymbolId,
        pub typ: Singleton<ScalarFunType>,
        pub visibility: StaticVisibility,
        pub param_idents: Vec<C::SymbolId>,
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
    Return(Option<C::Expression_ScalarType>),
    Expression(C::Expression_AnyType),
    If(If<C>),
    Compound(Block<C>),
    Break(C::LoopId),
    Continue(C::LoopId),
    While(CondBody<C>),
    DoWhile(CondBody<C>),
    For(For<C>),
    Null,
}
mod statement {
    use super::*;

    #[derive(Debug)]
    pub struct If<C: CAstVariant> {
        pub condition: C::Expression_ScalarType,
        pub then: Box<Statement<C>>,
        pub elze: Option<Box<Statement<C>>>,
    }

    #[derive(Debug)]
    pub struct CondBody<C: CAstVariant> {
        pub loop_id: C::LoopId,
        pub condition: C::Expression_ScalarType,
        pub body: Box<Statement<C>>,
    }

    #[derive(Debug)]
    pub struct For<C: CAstVariant> {
        pub loop_id: C::LoopId,
        pub init: ForInit<C>,
        pub condition: Option<C::Expression_ScalarType>,
        pub post: Option<C::Expression_AnyType>,
        pub body: Box<Statement<C>>,
    }

    #[derive(Debug)]
    pub enum ForInit<C: CAstVariant> {
        Decl(C::ForInitDeclaration),
        Exp(C::Expression_AnyType),
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
    SizeOfType(C::TypeOperand<NonVoidType>),
    SizeOfExp(SizeOfExp<C>),
}
/// Lvalue expression.
#[derive(Debug)]
pub enum LExp<C: CAstVariant> {
    String(C::StringExpression),
    Var(C::SymbolId),
    Dereference(Dereference<C>),
    Subscript(Subscript<C>),
}
mod expression {
    use super::*;

    #[derive(Debug)]
    pub struct Cast<C: CAstVariant> {
        pub typ: C::TypeOperand<Singleton<ObjType>>,
        pub sub_exp: Box<C::Expression_ScalarType>,
    }

    #[derive(Debug)]
    pub struct Unary<C: CAstVariant> {
        pub op: UnaryOperator,
        pub sub_exp: Box<C::Expression_ScalarType>,
    }

    #[derive(Debug)]
    pub struct Binary<C: CAstVariant> {
        pub op: C::BinaryOperator,
        pub lhs: Box<C::Expression_ScalarType>,
        pub rhs: Box<C::Expression_ScalarType>,
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
        pub condition: Box<C::Expression_ScalarType>,
        pub then: Box<C::Expression_ScalarType>,
        pub elze: Box<C::Expression_ScalarType>,
    }

    #[derive(Debug)]
    pub struct FunctionCall<C: CAstVariant> {
        pub ident: C::SymbolId,
        pub args: Vec<C::Expression_ScalarType>,
    }

    #[derive(Debug)]
    pub struct Assignment<C: CAstVariant> {
        pub lhs: Box<C::Expression_Lvalue_ScalarType>,
        pub rhs: Box<C::Expression_ScalarType>,
    }

    #[derive(Debug)]
    pub struct AddrOf<C: CAstVariant>(pub Box<C::Expression_Lvalue_AnyType>);

    #[derive(Debug)]
    pub struct SizeOfExp<C: CAstVariant> {
        pub sub_exp: Box<C::Expression_AnyType>,
    }

    #[derive(Debug)]
    pub struct Dereference<C: CAstVariant>(pub Box<C::Expression_ScalarType>);

    #[derive(Debug)]
    pub struct Subscript<C: CAstVariant> {
        pub exp1: Box<C::Expression_ScalarType>,
        pub exp2: Box<C::Expression_ScalarType>,
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
    pub type AnyExp = TypedExp<SubObjType<ObjType>>;
    pub type ScalarExp = TypedExp<SubObjType<ScalarType>>;
    impl ScalarExp {
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
    pub type TypedLExp<LTyp> = TypAndExp<LTyp, LExp<TypeCheckedCAst>>;
}
