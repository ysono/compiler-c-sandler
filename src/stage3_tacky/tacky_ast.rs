pub use self::instruction::*;
pub use crate::stage2_parse::c_ast_resolved::Const;
use crate::symbol_table::{ResolvedIdentifier, StaticVisibility};
use getset::Getters;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug)]
pub struct Program {
    pub funs: Vec<Function>,
    pub vars: Vec<StaticVariable>,
}

#[derive(Debug)]
pub struct Function {
    pub ident: Rc<ResolvedIdentifier>,
    pub visibility: StaticVisibility,
    pub params: Vec<Rc<ResolvedIdentifier>>,
    pub instrs: Vec<Instruction>,
}

#[derive(Debug)]
pub struct StaticVariable {
    pub ident: Rc<ResolvedIdentifier>,
    pub visibility: StaticVisibility,
    pub init: Const,
}

#[derive(Debug)]
pub enum Instruction {
    Return(ReadableValue),
    Unary(Unary),
    Binary(Binary),
    Copy(Copy),
    Jump(Rc<LabelIdentifier>),
    JumpIfZero(JumpIf),
    JumpIfNotZero(JumpIf),
    Label(Rc<LabelIdentifier>),
    FunCall(FunCall),
}
mod instruction {
    use super::*;

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
    pub struct Copy {
        pub src: ReadableValue,
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

#[derive(Debug)]
pub enum BinaryOperator {
    /* -> int */
    Sub,
    Add,
    Mul,
    Div,
    Rem,
    /* -(compare)-> bool */
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}

#[derive(Debug)]
pub enum ReadableValue {
    Constant(i32),
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
