pub use self::{instruction::*, operand::*, operator::*};
use crate::{
    common::{
        identifier::{JumpLabel, SymbolIdentifier},
        primitive::Const,
        symbol_table_frontend::StaticVisibility,
        types_frontend::{FunType, VarType},
    },
    ds_n_a::singleton::Singleton,
};
use derive_more::From;
use std::rc::Rc;

#[derive(Debug)]
pub struct Program {
    pub static_vars: Vec<StaticVariable>,
    pub funs: Vec<Function>,
}

#[derive(Debug)]
pub struct StaticVariable {
    pub ident: Rc<SymbolIdentifier>,
    pub visibility: StaticVisibility,
    pub typ: Singleton<VarType>,
    pub init: Const,
}

#[derive(Debug)]
pub struct Function {
    pub ident: Rc<SymbolIdentifier>,
    pub typ: Singleton<FunType>,
    pub visibility: StaticVisibility,
    pub param_idents: Vec<Rc<SymbolIdentifier>>,
    pub instrs: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Return(Value),
    SignExtend(SrcDst),
    Truncate(SrcDst),
    ZeroExtend(SrcDst),
    DoubleToInt(SrcDst),
    DoubleToUInt(SrcDst),
    IntToDouble(SrcDst),
    UIntToDouble(SrcDst),
    Unary(Unary),
    Binary(Binary),
    Copy(SrcDst),
    GetAddress(GetAddress),
    Load(Load),
    Store(Store),
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
        pub dst: Value,
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

    #[derive(From)]
    pub(in crate::stage3_tacky) enum ExpResult {
        Value(Value),
        Object(Object),
    }

    /// The "value" concept comprises a bit-sequence and a type; and is readonly.
    #[derive(Clone, Debug)]
    pub enum Value {
        /// A value that's contained in a constant (ie somewhere outside the addressable memory).
        Constant(Const),

        /// A value that's contained in an abstract memory location.
        /// The memory location may be identified by
        ///     an identifier declared in the C src code, or
        ///     an identifier that's dynamically generated during the Tacky stage.
        Variable(Rc<SymbolIdentifier>),
    }

    /// The "object" concept comprises a memory location that contains a "value"; and is mutable.
    pub(in crate::stage3_tacky) enum Object {
        /// An object whose memory location is identified directly.
        Direct(Rc<SymbolIdentifier>),

        /// An object whose memory location is contained in a separate value.
        Pointee {
            addr: Value,

            /// The type of the target pointee object (not the type of the memory location value).
            typ: Singleton<VarType>,
        },
    }
}
