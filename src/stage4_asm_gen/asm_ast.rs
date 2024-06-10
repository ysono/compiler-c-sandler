pub use crate::stage3_tacky::tacky_ast::{LabelIdentifier, ResolvedIdentifier};
use derive_more::{Deref, From};
use std::rc::Rc;

#[derive(Debug)]
pub struct Program {
    pub func: Function,
}

#[derive(Debug)]
pub struct Function {
    pub ident: Rc<ResolvedIdentifier>,
    pub instructions: Vec<Instruction<Operand>>,
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
    PseudoRegister(Rc<ResolvedIdentifier>),
}

#[derive(From, Clone, Debug)]
pub enum Operand {
    ImmediateValue(i32),
    Register(Register),
    StackPosition(StackPosition),
}

#[derive(Clone, Copy, Debug)]
pub enum Register {
    AX,
    DX,
    R10,
    R11,
}

/// Abs offset from RBP. I.e. negation of at-runtime offset from RBP.
#[derive(Clone, Copy, Deref, Debug)]
pub struct StackPosition(pub(super) usize);

#[derive(Debug)]
pub enum ConditionCode {
    E,
    Ne,
    L,
    Le,
    G,
    Ge,
}
