pub use self::{instruction::*, operand::*, operator::*};
use crate::{
    common::{
        identifier::{JumpLabel, SymbolIdentifier},
        primitive::Const,
        symbol_table_frontend::StaticVisibility,
        types_backend::ByteLen,
        types_frontend::{ScalarType, SubObjType, TypecheckedFunType},
    },
    ds_n_a::{singleton::Singleton, witness::Witness},
};
use derive_more::From;
use std::rc::Rc;

#[derive(Debug)]
pub struct Program {
    pub funs: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub ident: Rc<SymbolIdentifier>,
    pub typ: Singleton<TypecheckedFunType>,
    pub visibility: StaticVisibility,
    pub param_idents: Vec<Rc<SymbolIdentifier>>,
    pub instrs: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Return(Option<Value>),
    SignExtend(SrcDst),
    ZeroExtend(SrcDst),
    Truncate(SrcDst),
    DoubleToSInteg(SrcDst),
    DoubleToUInteg(SrcDst),
    SIntegToDouble(SrcDst),
    UIntegToDouble(SrcDst),
    Unary(Unary),
    Binary(Binary),
    Copy(SrcDst),
    GetAddress(GetAddress),
    Load(Load),
    Store(Store),
    AddPtr(AddPtr),
    CopyToOffset(CopyToOffset),
    Jump(Rc<JumpLabel>),
    JumpIf(JumpIf),
    Label(Rc<JumpLabel>),
    FunCall(FunCall),
}
mod instruction {
    use super::*;

    #[derive(Debug)]
    pub struct SrcDst {
        pub src: Value,
        pub dst: Value,
    }

    #[derive(Debug)]
    pub struct Unary {
        pub op: UnaryOperator,
        pub src: Value,
        pub dst: Value,
    }

    #[derive(Debug)]
    pub struct Binary {
        pub op: BinaryOperator,
        pub lhs: Value,
        pub rhs: Value,
        pub dst: Value,
    }

    #[derive(Debug)]
    pub struct GetAddress {
        pub src_obj: Rc<SymbolIdentifier>,
        pub dst_addr: Value,
    }

    #[derive(Debug)]
    pub struct Load {
        pub src_addr: Value,
        pub dst: Value,
    }

    #[derive(Debug)]
    pub struct Store {
        pub src: Value,
        pub dst_addr: Value,
    }

    #[derive(Debug)]
    pub struct AddPtr {
        pub ptr: Value,
        pub idx: Value,
        pub scale: ByteLen,
        pub dst: Value,
    }

    #[derive(Debug)]
    pub struct CopyToOffset {
        pub src: Value,
        pub dst_obj: Rc<SymbolIdentifier>,
        pub offset: ByteLen,
    }

    #[derive(Debug)]
    pub struct JumpIf {
        pub condition: Value,
        pub jump_crit: JumpCriterion,
        pub lbl: Rc<JumpLabel>,
    }
    #[derive(Debug)]
    pub enum JumpCriterion {
        JumpIfZero,
        JumpIfNotZero,
    }

    #[derive(Debug)]
    pub struct FunCall {
        pub ident: Rc<SymbolIdentifier>,
        pub args: Vec<Value>,
        pub dst: Option<Value>,
    }
}

mod operator {
    use super::*;

    #[derive(From, Debug)]
    pub enum UnaryOperator {
        Numeric(NumericUnaryOperator),
        Comparison(ComparisonUnaryOperator),
    }
    #[derive(Clone, Copy, Debug)]
    pub enum NumericUnaryOperator {
        /* integer -> integer */
        Complement,
        /* integer -> integer or floating-pt -> floating-pt */
        Negate,
    }
    #[derive(Clone, Copy, Debug)]
    pub enum ComparisonUnaryOperator {
        /* integer or floating-pt -> bool */
        Not,
    }

    #[derive(From, Debug)]
    pub enum BinaryOperator {
        Arithmetic(ArithmeticBinaryOperator),
        DivRem(DivRemBinaryOperator),
        Comparison(ComparisonBinaryOperator),
    }
    #[derive(Clone, Copy, Debug)]
    pub enum ArithmeticBinaryOperator {
        Sub,
        Add,
        Mul,
    }
    #[derive(Clone, Copy, Debug)]
    pub enum DivRemBinaryOperator {
        Div,
        Rem,
    }
    #[derive(Clone, Copy, Debug)]
    pub enum ComparisonBinaryOperator {
        Eq,
        Neq,
        Lt,
        Lte,
        Gt,
        Gte,
    }
}

mod operand {
    use super::*;

    /// The "value" concept comprises a bit-sequence and a type; and is readonly.
    #[derive(Clone, Debug)]
    #[cfg_attr(test, derive(From))]
    pub enum Value {
        /// A value that's contained in a constant (ie somewhere outside the addressable memory).
        Constant(Const),

        /// A value that's contained in an abstract memory location.
        /// The memory location may be identified by either
        ///     a variable identifier that's declared in the C src code, or
        ///     an anonymous identifier that's dynamically generated during the Tacky stage.
        /// The memory location can have any storage duration: automatic or static.
        Variable(Rc<SymbolIdentifier>, Witness<SubObjType<ScalarType>>),
    }
    #[cfg(test)]
    impl PartialEq for Value {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (Self::Constant(l_konst), Self::Constant(r_konst)) => l_konst == r_konst,
                (Self::Variable(l_ident, _), Self::Variable(r_ident, _)) => l_ident == r_ident,
                _ => false,
            }
        }
    }

    /// The "object" concept comprises a memory location that contains a "value"; and is mutable.
    pub(in crate::stage3_tacky) enum Object<LTyp> {
        /// An object whose memory location is identified directly.
        Direct(Rc<SymbolIdentifier>, Witness<LTyp>),

        /// An object whose memory location is contained in a separate value.
        Pointee {
            addr: Value,

            /// The type of the target pointee object (not the type of the memory location value).
            typ: LTyp,
        },
    }
}
