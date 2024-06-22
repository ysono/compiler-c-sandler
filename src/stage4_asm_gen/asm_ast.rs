pub use crate::stage3_tacky::tacky_ast::{
    Const, LabelIdentifier, ResolvedIdentifier, StaticVariable, StaticVisibility,
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
pub enum Instruction<Oprnd> {
    Mov {
        src: Oprnd,
        dst: Oprnd,
    },
    Unary(UnaryOperator, Oprnd),
    Binary {
        op: BinaryOperator,
        arg: Oprnd, // Semantic RHS. Asm operand #1.
        tgt: Oprnd, // Semantic LHS, as well as output. Asm operand #2.
    },
    Cmp {
        arg: Oprnd, // Semantic RHS. Asm operand #1.
        tgt: Oprnd, // Semantic LHS, non-modified. Asm operand #2.
    },
    Idiv(Oprnd),
    Cdq,
    Jmp(Rc<LabelIdentifier>),
    JmpCC(ConditionCode, Rc<LabelIdentifier>),
    SetCC(ConditionCode, Oprnd),
    Label(Rc<LabelIdentifier>),
    AllocateStack(StackPosition),
    DeallocateStack(StackPosition),
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
    ImmediateValue(i32),
    Register(Register),
    StackPosition(StackPosition),
    Pseudo(Rc<ResolvedIdentifier>),
}

#[derive(From, Clone, Debug)]
pub enum Operand {
    ImmediateValue(i32),
    Register(Register),
    StackPosition(StackPosition),
    Data(Rc<ResolvedIdentifier>),
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
    /* These are all caller-saved. */
}

/// Offset from RBP.
#[derive(Clone, Copy, Deref, DerefMut, Debug)]
pub struct StackPosition(pub(super) isize);

#[derive(Debug)]
pub enum ConditionCode {
    E,
    Ne,
    L,
    Le,
    G,
    Ge,
}
