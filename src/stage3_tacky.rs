use crate::{stage1_lexer::tokens::Const, stage2_parser::c_ast};
use std::rc::Rc;

pub mod tacky_ir {
    use crate::{stage1_lexer::tokens::Identifier, stage2_parser::c_ast::UnaryOperator};
    use derive_more::From;
    use std::rc::Rc;
    use std::sync::atomic::{AtomicU64, Ordering};

    #[derive(Debug)]
    pub struct Program {
        pub func: Function,
    }
    #[derive(Debug)]
    pub struct Function {
        pub ident: Identifier,
        pub instructions: Vec<Instruction>,
    }
    #[derive(From, Debug)]
    pub enum Instruction {
        Return(ReadableValue),
        Unary {
            op: UnaryOperator,
            src: ReadableValue,
            dst: Rc<Variable>,
        },
    }
    #[derive(Debug)]
    pub enum ReadableValue {
        Constant(i32),
        Variable(Rc<Variable>),
    }
    #[derive(PartialEq, Eq, Hash, Debug)]
    pub struct Variable {
        id: u64,
    }
    impl Variable {
        pub(super) fn new() -> Self {
            static NEXT_ID: AtomicU64 = AtomicU64::new(0);
            let curr_id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
            Self { id: curr_id }
        }
    }
}
use tacky_ir::*;

pub struct Tackifier {}
impl Tackifier {
    pub fn tackify_program(c_prog: c_ast::Program) -> Program {
        let c_ast::Program { func } = c_prog;
        let func = Self::tackify_func(func);
        Program { func }
    }
    fn tackify_func(c_func: c_ast::Function) -> Function {
        let c_ast::Function { ident, stmt } = c_func;
        let instructions = Self::tackify_stmt(stmt);
        Function {
            ident,
            instructions,
        }
    }
    fn tackify_stmt(c_stmt: c_ast::Statement) -> Vec<Instruction> {
        let c_ast::Statement::Return(c_root_exp) = c_stmt;
        let mut instrs = vec![];
        let root_exp = Self::tackify_exp(c_root_exp, &mut instrs);
        let root_instr = Instruction::Return(root_exp);
        instrs.push(root_instr);
        instrs
    }
    fn tackify_exp(c_exp: c_ast::Expression, instrs: &mut Vec<Instruction>) -> ReadableValue {
        match c_exp {
            c_ast::Expression::Const(Const::Int(intval)) => ReadableValue::Constant(intval),
            c_ast::Expression::Unary(op, sub_exp) => {
                let src = Self::tackify_exp(*sub_exp, instrs);
                let dst = Rc::new(Variable::new());
                let instr = Instruction::Unary {
                    op,
                    src,
                    dst: Rc::clone(&dst),
                };
                instrs.push(instr);
                ReadableValue::Variable(dst)
            }
        }
    }
}
