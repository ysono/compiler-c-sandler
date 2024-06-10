pub use self::instruction::*;
pub use crate::stage2_parse::c_ast_resolved::{Identifier, Variable};
use getset::Getters;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug)]
pub struct Program {
    pub func: Function,
}

#[derive(Debug)]
pub struct Function {
    pub ident: Identifier,
    pub instructions: Vec<Instruction>,
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
}
mod instruction {
    use super::*;

    #[derive(Debug)]
    pub struct Unary {
        pub op: UnaryOperator,
        pub src: ReadableValue,
        pub dst: Rc<Variable>,
    }

    #[derive(Debug)]
    pub struct Binary {
        pub op: BinaryOperator,
        pub src1: ReadableValue,
        pub src2: ReadableValue,
        pub dst: Rc<Variable>,
    }

    #[derive(Debug)]
    pub struct Copy {
        pub src: ReadableValue,
        pub dst: Rc<Variable>,
    }

    #[derive(Debug)]
    pub struct JumpIf {
        pub condition: ReadableValue,
        pub tgt: Rc<LabelIdentifier>,
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
    Variable(Rc<Variable>),
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
