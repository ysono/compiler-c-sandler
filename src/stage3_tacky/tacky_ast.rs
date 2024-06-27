pub use self::instruction::*;
use crate::{
    symbol_table_frontend::{ResolvedIdentifier, StaticVisibility},
    types_frontend::{Const, VarType},
};
use derive_more::From;
use getset::Getters;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug)]
pub struct Program {
    pub static_vars: Vec<StaticVariable>,
    pub funs: Vec<Function>,
}

#[derive(Debug)]
pub struct StaticVariable {
    pub ident: Rc<ResolvedIdentifier>,
    pub visibility: StaticVisibility,
    pub typ: VarType,
    pub init: Const,
}

#[derive(Debug)]
pub struct Function {
    pub ident: Rc<ResolvedIdentifier>,
    pub visibility: StaticVisibility,
    pub param_idents: Vec<Rc<ResolvedIdentifier>>,
    pub instrs: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Return(ReadableValue),
    SignExtend(SrcDst),
    Truncate(SrcDst),
    ZeroExtend(SrcDst),
    Unary(Unary),
    Binary(Binary),
    Copy(SrcDst),
    Jump(Rc<LabelIdentifier>),
    JumpIfZero(JumpIf),
    JumpIfNotZero(JumpIf),
    Label(Rc<LabelIdentifier>),
    FunCall(FunCall),
}
mod instruction {
    use super::*;

    #[derive(Debug)]
    pub struct SrcDst {
        pub src: ReadableValue,
        pub dst: Rc<ResolvedIdentifier>,
    }

    #[derive(Debug)]
    pub struct Unary {
        pub op: UnaryOperator,
        pub src: ReadableValue,
        pub dst: Rc<ResolvedIdentifier>,
    }

    #[derive(Debug)]
    pub struct Binary {
        pub op: BinaryOperator,
        pub src1: ReadableValue,
        pub src2: ReadableValue,
        pub dst: Rc<ResolvedIdentifier>,
    }

    #[derive(Debug)]
    pub struct JumpIf {
        pub condition: ReadableValue,
        pub tgt: Rc<LabelIdentifier>,
    }

    #[derive(Debug)]
    pub struct FunCall {
        pub ident: Rc<ResolvedIdentifier>,
        pub args: Vec<ReadableValue>,
        pub dst: Rc<ResolvedIdentifier>,
    }
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

#[derive(From, Debug)]
pub enum ReadableValue {
    Constant(Const),
    Variable(Rc<ResolvedIdentifier>),
}

#[derive(Getters, Debug)]
#[getset(get = "pub")]
pub struct LabelIdentifier {
    id: usize,
    name: String,
}
impl LabelIdentifier {
    pub(super) fn new(name: String) -> Self {
        static NEXT_ID: AtomicUsize = AtomicUsize::new(0);
        let curr_id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
        Self { id: curr_id, name }
    }
}
