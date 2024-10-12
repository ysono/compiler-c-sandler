pub use self::{operand::*, operator::*};
use crate::common::{
    identifier::{JumpLabel, SymbolIdentifier},
    primitive::Const,
    symbol_table_frontend::{InitializerItem, StaticVisibility},
    types_backend::{Alignment, ScalarAssemblyType},
};
use derive_more::{AsMut, Constructor, From};
use std::fmt::Debug;
use std::rc::Rc;

pub trait AsmAstVariant {
    type Instructions: Debug;
    type Operand: Debug;
}

#[derive(Debug)]
pub struct Program<A: AsmAstVariant> {
    pub static_vars: Vec<StaticVariable>,
    pub static_consts: Vec<StaticConstant>,
    pub funs: Vec<Function<A>>,
}

#[derive(Debug)]
pub struct StaticVariable {
    pub ident: Rc<SymbolIdentifier>,
    pub visibility: StaticVisibility,
    pub alignment: Alignment,
    pub inits: Vec<InitializerItem<Const>>,
}

#[derive(Debug)]
pub struct StaticConstant {
    pub ident: Rc<SymbolIdentifier>,
    pub alignment: Alignment,
    pub init: Const,
}

#[derive(Debug)]
pub struct Function<A: AsmAstVariant> {
    pub ident: Rc<SymbolIdentifier>,
    pub visibility: StaticVisibility,
    pub instrs: A::Instructions,
}

#[derive(Debug)]
pub enum Instruction<A: AsmAstVariant> {
    Mov {
        asm_type: ScalarAssemblyType,
        src: A::Operand,
        dst: A::Operand,
    },
    Movsx {
        src: A::Operand,
        dst: A::Operand,
    },
    MovZeroExtend {
        src: A::Operand,
        dst: A::Operand,
    },
    Lea {
        src: A::Operand,
        dst: A::Operand,
    },
    Cvttsd2si {
        dst_asm_type: ScalarAssemblyType,
        src: A::Operand,
        dst: A::Operand,
    },
    Cvtsi2sd {
        src_asm_type: ScalarAssemblyType,
        src: A::Operand,
        dst: A::Operand,
    },
    Unary(UnaryOperator, ScalarAssemblyType, A::Operand),
    Binary {
        op: BinaryOperator,
        asm_type: ScalarAssemblyType,
        arg: A::Operand, // Semantic RHS. Asm operand #1 in AT&A syntax.
        tgt: A::Operand, // Semantic LHS, as well as output. Asm operand #2.
    },
    Cmp {
        asm_type: ScalarAssemblyType,
        arg: A::Operand, // Semantic RHS. Asm operand #1 in AT&A syntax.
        tgt: A::Operand, // Semantic LHS, non-modified. Asm operand #2.
    },
    Idiv(ScalarAssemblyType, A::Operand),
    Div(ScalarAssemblyType, A::Operand),
    Cdq(ScalarAssemblyType),
    Jmp(Rc<JumpLabel>),
    JmpCC(ConditionCode, Rc<JumpLabel>),
    SetCC(ConditionCode, A::Operand),
    Label(Rc<JumpLabel>),
    Push(A::Operand),
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
        PseudoRegOrMem(Rc<SymbolIdentifier>),
    }
    impl PreFinalOperand {
        pub fn is_on_mem(&self) -> bool {
            match self {
                Self::O(o) => o.is_on_mem(),
                Self::PseudoRegOrMem(_) => true, // Unconditionally on mem, for now.
            }
        }
    }

    #[derive(Clone, Debug)]
    pub enum Operand {
        ImmediateValue(i64),
        Register(Register),
        Memory(Register, MemoryOffset),
        ReadWriteData(Rc<SymbolIdentifier>),
        ReadonlyData(Rc<SymbolIdentifier>),
    }
    impl Operand {
        pub fn is_on_mem(&self) -> bool {
            match self {
                Self::ImmediateValue(_) | Self::Register(_) => false,
                Self::Memory(..) | Self::ReadWriteData(_) | Self::ReadonlyData(_) => true,
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
        /* scratch */
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
        /* scratch */
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
    #[derive(Constructor, Clone, Copy, AsMut, Debug)]
    pub struct MemoryOffset(i64);
    impl MemoryOffset {
        pub fn as_int(&self) -> i64 {
            self.0
        }
    }
}
