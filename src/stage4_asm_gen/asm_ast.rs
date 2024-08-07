pub use crate::stage3_tacky::tacky_ast::LabelIdentifier;
use crate::{
    identifier::UniqueIdentifier,
    symbol_table_frontend::StaticVisibility,
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
    pub static_consts: Vec<StaticConstant>,
    pub static_vars: Vec<StaticVariable>,
    pub funs: Vec<Function<T>>,
}

#[derive(Debug)]
pub struct StaticConstant {
    pub ident: Rc<UniqueIdentifier>,
    pub alignment: Alignment,
    pub init: Const,
}

#[derive(Debug)]
pub struct StaticVariable {
    pub ident: Rc<UniqueIdentifier>,
    pub visibility: StaticVisibility,
    pub alignment: Alignment,
    pub init: Const,
}

#[derive(Debug)]
pub struct Function<T: AsmAstVariant> {
    pub ident: Rc<UniqueIdentifier>,
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
    Cvttsd2si {
        dst_asm_type: AssemblyType,
        src: T::Operand,
        dst: T::Operand,
    },
    Cvtsi2sd {
        src_asm_type: AssemblyType,
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
    Call(Rc<UniqueIdentifier>),
    Ret,
}

#[derive(Debug)]
pub enum UnaryOperator {
    BitwiseComplement,
    TwosComplement,
    Shr,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    DivDouble,
    And,
    Or,
    Xor,
}

#[derive(Clone, Debug)]
pub enum PreFinalOperand {
    ImmediateValue(u64),
    Register(Register),
    StackPosition(StackPosition),
    Data(Rc<UniqueIdentifier>),
    Pseudo(Rc<UniqueIdentifier>),
}

#[derive(From, Clone, Debug)]
pub enum Operand {
    /// The assmebler will translate each immediate value literal into bytes.
    /// The literal's type will be discarded in the object file.
    /// The byte length will be interpreted automatically, according to the instruction name suffix.
    /// The immediate value literal text can be in various formats: integer, 1e2, ....
    /// We arbitrarily choose to emit each immediate value as unsigned integer literal.
    ImmediateValue(u64),
    Register(Register),
    StackPosition(StackPosition),
    Data(Rc<UniqueIdentifier>),
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
    /* Elsewhere in code, non-SSE registers are aka "general purpose" (aka "gp") registers,
    where this nomenclature applies to registers other than R8 ~ R15. */

    /* non-SSE, caller-saved */
    AX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
    /* non-SSE, callee-saved */
    SP,
    /* SSE */
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM14,
    XMM15,
}

/// Offset from RBP.
#[derive(Clone, Copy, Deref, DerefMut, Debug)]
pub struct StackPosition(pub(super) i64);

#[derive(Debug)]
pub enum ConditionCode {
    E,
    Ne,
    G,
    Ge,
    L,
    Le,
    A,  // gt
    Ae, // gte
    B,  // lt
    Be, // lte
}
