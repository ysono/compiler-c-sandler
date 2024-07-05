pub use crate::stage3_tacky::tacky_ast::LabelIdentifier;
use crate::{
    symbol_table_frontend::{ResolvedIdentifier, StaticVisibility},
    types_backend::{Alignment, AssemblyType},
    types_frontend::Const,
};
use derive_more::{Deref, DerefMut, From};
use std::fmt::Debug;
use std::rc::Rc;

pub trait AsmAstVariant {
    type Instructions: Debug;
    type Operand: Debug;
}

#[derive(Debug)]
pub struct Program<T: AsmAstVariant> {
    pub static_vars: Vec<StaticVariable>,
    pub funs: Vec<Function<T>>,
}

#[derive(Debug)]
pub struct StaticVariable {
    pub ident: Rc<ResolvedIdentifier>,
    pub visibility: StaticVisibility,
    pub alignment: Alignment,
    pub init: Const,
}

#[derive(Debug)]
pub struct Function<T: AsmAstVariant> {
    pub ident: Rc<ResolvedIdentifier>,
    pub visibility: StaticVisibility,
    pub instrs: T::Instructions,
}

#[derive(Debug)]
pub enum Instruction<T: AsmAstVariant> {
    Mov {
        asm_type: AssemblyType,
        src: T::Operand,
        dst: T::Operand,
    },
    Movsx {
        src: T::Operand,
        dst: T::Operand,
    },
    MovZeroExtend {
        src: T::Operand,
        dst: T::Operand,
    },
    Unary(UnaryOperator, AssemblyType, T::Operand),
    Binary {
        op: BinaryOperator,
        asm_type: AssemblyType,
        arg: T::Operand, // Semantic RHS. Asm operand #1.
        tgt: T::Operand, // Semantic LHS, as well as output. Asm operand #2.
    },
    Cmp {
        asm_type: AssemblyType,
        arg: T::Operand, // Semantic RHS. Asm operand #1.
        tgt: T::Operand, // Semantic LHS, non-modified. Asm operand #2.
    },
    Idiv(AssemblyType, T::Operand),
    Div(AssemblyType, T::Operand),
    Cdq(AssemblyType),
    Jmp(Rc<LabelIdentifier>),
    JmpCC(ConditionCode, Rc<LabelIdentifier>),
    SetCC(ConditionCode, T::Operand),
    Label(Rc<LabelIdentifier>),
    Push(T::Operand),
    Call(Rc<ResolvedIdentifier>),
    Ret,
}

#[derive(Debug)]
pub enum UnaryOperator {
    BitwiseComplement,
    TwosComplement,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
}

#[derive(From, Clone, Debug)]
pub enum PreFinalOperand {
    ImmediateValue(i64),
    Register(Register),
    StackPosition(StackPosition),
    Pseudo(Rc<ResolvedIdentifier>),
}

#[derive(From, Clone, Debug)]
pub enum Operand {
    ImmediateValue(i64),
    Register(Register),
    StackPosition(StackPosition),
    Data(Rc<ResolvedIdentifier>),
}
impl Operand {
    pub fn is_on_mem(&self) -> bool {
        match self {
            Self::ImmediateValue(_) | Self::Register(_) => false,
            Self::StackPosition(_) | Self::Data(_) => true,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Register {
    AX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
    /* Above are caller-saved. Below are callee-saved. */
    SP,
}

/// Offset from RBP.
#[derive(Clone, Copy, Deref, DerefMut, Debug)]
pub struct StackPosition(pub(super) i64);

#[derive(Debug)]
pub enum ConditionCode {
    E,
    Ne,
    L,
    Le,
    G,
    Ge,
    A,
    Ae,
    B,
    Be,
}
