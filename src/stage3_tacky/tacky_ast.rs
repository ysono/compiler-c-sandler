pub use self::{instruction::*, operator::*};
use crate::common::{
    identifier::{JumpLabel, SymbolIdentifier},
    symbol_table_frontend::StaticVisibility,
    types_frontend::{Const, VarType},
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
    pub typ: VarType,
    pub init: Const,
}

#[derive(Debug)]
pub struct Function {
    pub ident: Rc<SymbolIdentifier>,
    pub visibility: StaticVisibility,
    pub param_idents: Vec<Rc<SymbolIdentifier>>,
    pub instrs: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Return(ReadableValue),
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
    Jump(Rc<JumpLabel>),
    JumpIf(JumpIf),
    Label(Rc<JumpLabel>),
    FunCall(FunCall),
}
mod instruction {
    use super::*;

    #[derive(Debug)]
    pub struct SrcDst {
        pub src: ReadableValue,
        pub dst: Rc<SymbolIdentifier>,
    }

    #[derive(Debug)]
    pub struct Unary {
        pub op: UnaryOperator,
        pub src: ReadableValue,
        pub dst: Rc<SymbolIdentifier>,
    }

    #[derive(Debug)]
    pub struct Binary {
        pub op: BinaryOperator,
        pub lhs: ReadableValue,
        pub rhs: ReadableValue,
        pub dst: Rc<SymbolIdentifier>,
    }

    #[derive(Debug)]
    pub struct JumpIf {
        pub condition: ReadableValue,
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
        pub args: Vec<ReadableValue>,
        pub dst: Rc<SymbolIdentifier>,
    }
}

mod operator {
    use super::*;

    #[derive(Debug)]
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

#[derive(From, Debug)]
pub enum ReadableValue {
    Constant(Const),
    Variable(Rc<SymbolIdentifier>),
}
