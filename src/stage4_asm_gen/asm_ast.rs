pub use self::{operand::*, operator::*};
use crate::common::{
    identifier::{JumpLabel, SymbolIdentifier},
    primitive::Const,
    symbol_table_frontend::StaticVisibility,
    types_backend::{Alignment, AssemblyType},
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
    pub ident: Rc<SymbolIdentifier>,
    pub alignment: Alignment,
    pub init: Const,
}

#[derive(Debug)]
pub struct StaticVariable {
    pub ident: Rc<SymbolIdentifier>,
    pub visibility: StaticVisibility,
    pub alignment: Alignment,
    pub init: Const,
}

#[derive(Debug)]
pub struct Function<T: AsmAstVariant> {
    pub ident: Rc<SymbolIdentifier>,
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
    Lea {
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
        arg: T::Operand, // Semantic RHS. Asm operand #1 in AT&T syntax.
        tgt: T::Operand, // Semantic LHS, as well as output. Asm operand #2.
    },
    Cmp {
        asm_type: AssemblyType,
        arg: T::Operand, // Semantic RHS. Asm operand #1 in AT&T syntax.
        tgt: T::Operand, // Semantic LHS, non-modified. Asm operand #2.
    },
    Idiv(AssemblyType, T::Operand),
    Div(AssemblyType, T::Operand),
    Cdq(AssemblyType),
    Jmp(Rc<JumpLabel>),
    JmpCC(ConditionCode, Rc<JumpLabel>),
    SetCC(ConditionCode, T::Operand),
    Label(Rc<JumpLabel>),
    Push(T::Operand),
    Call(Rc<SymbolIdentifier>),
    Ret,
}

mod operator {
    use super::*;

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
}

mod operand {
    use super::*;

    #[derive(From, Clone, Debug)]
    pub enum PreFinalOperand {
        O(Operand),
        Pseudo(Rc<SymbolIdentifier>),
    }
    impl PreFinalOperand {
        pub fn is_on_mem(&self) -> bool {
            match self {
                Self::O(o) => o.is_on_mem(),
                Self::Pseudo(_) => true,
            }
        }
    }

    #[derive(Clone, Debug)]
    pub enum Operand {
        ImmediateValue(i64),
        Register(Register),
        MemoryAddress(Register, MemoryAddressOffset),
        ReadWriteData(Rc<SymbolIdentifier>),
        ReadonlyData(Rc<SymbolIdentifier>),
    }
    impl Operand {
        pub fn is_on_mem(&self) -> bool {
            match self {
                Self::ImmediateValue(_) | Self::Register(_) => false,
                Self::MemoryAddress(..) | Self::ReadWriteData(_) | Self::ReadonlyData(_) => true,
            }
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub enum Register {
        /* Elsewhere in code, non-SSE registers are aka "general purpose" ("gp") registers,
        including those other than R8 ~ R15. */

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
        BP,
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
    impl Register {
        pub fn is_sse(&self) -> bool {
            use Register::*;
            matches!(
                self,
                XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 | XMM14 | XMM15
            )
        }
    }

    /// Offset from an address value stored in a register (eg RBP).
    #[derive(Clone, Copy, Deref, DerefMut, Debug)]
    pub struct MemoryAddressOffset(pub(in crate::stage4_asm_gen) i64);
}
