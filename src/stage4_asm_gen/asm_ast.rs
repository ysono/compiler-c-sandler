pub use crate::stage3_tacky::tacky_ast::{Const, LabelIdentifier};
use crate::{
    symbol_table::{ResolvedIdentifier, StaticVisibility},
    symbol_table_backend::{Alignment, AssemblyType},
};
use derive_more::{Deref, DerefMut, From};
use std::collections::VecDeque;
use std::rc::Rc;

#[derive(Debug)]
pub struct Program {
    pub funs: Vec<Function>,
    pub vars: Vec<StaticVariable>,
}

#[derive(Debug)]
pub struct Function {
    pub ident: Rc<ResolvedIdentifier>,
    pub visibility: StaticVisibility,
    pub instrs: VecDeque<Instruction<Operand>>,
}

#[derive(Debug)]
pub struct StaticVariable {
    pub ident: Rc<ResolvedIdentifier>,
    pub visibility: StaticVisibility,
    pub alignment: Alignment,
    pub init: Const,
}

#[derive(Debug)]
pub enum Instruction<Oprnd> {
    Mov {
        asm_type: AssemblyType,
        src: Oprnd,
        dst: Oprnd,
    },
    Movsx {
        src: Oprnd,
        dst: Oprnd,
    },
    Unary(UnaryOperator, AssemblyType, Oprnd),
    Binary {
        op: BinaryOperator,
        asm_type: AssemblyType,
        arg: Oprnd, // Semantic RHS. Asm operand #1.
        tgt: Oprnd, // Semantic LHS, as well as output. Asm operand #2.
    },
    Cmp {
        asm_type: AssemblyType,
        arg: Oprnd, // Semantic RHS. Asm operand #1.
        tgt: Oprnd, // Semantic LHS, non-modified. Asm operand #2.
    },
    Idiv(AssemblyType, Oprnd),
    Cdq(AssemblyType),
    Jmp(Rc<LabelIdentifier>),
    JmpCC(ConditionCode, Rc<LabelIdentifier>),
    SetCC(ConditionCode, Oprnd),
    Label(Rc<LabelIdentifier>),
    Push(Oprnd),
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
}
