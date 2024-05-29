use crate::{stage1_lexer::tokens, stage2_parser::c_ast};
use std::rc::Rc;

pub mod tacky_ir {
    use crate::stage1_lexer::tokens::Identifier;
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

    #[derive(Debug)]
    pub enum Instruction {
        Return(ReadableValue),
        Unary {
            op: UnaryOperator,
            src: ReadableValue,
            dst: Rc<Variable>,
        },
        Binary {
            op: BinaryOperator,
            src1: ReadableValue,
            src2: ReadableValue,
            dst: Rc<Variable>,
        },
        Copy {
            src: ReadableValue,
            dst: Rc<Variable>,
        },
        Jump(Rc<LabelIdentifier>),
        JumpIfZero {
            condition: ReadableValue,
            tgt: Rc<LabelIdentifier>,
        },
        JumpIfNotZero {
            condition: ReadableValue,
            tgt: Rc<LabelIdentifier>,
        },
        Label(Rc<LabelIdentifier>),
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

    #[derive(Debug)]
    pub struct LabelIdentifier {
        #[allow(dead_code)]
        id: u64,
        #[allow(dead_code)]
        name: Option<String>,
    }
    impl LabelIdentifier {
        pub(super) fn new(name: Option<String>) -> Self {
            static NEXT_ID: AtomicU64 = AtomicU64::new(0);
            let curr_id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
            Self { id: curr_id, name }
        }
    }
}
use tacky_ir::*;

enum BinaryOperatorType {
    EvaluateBothHands(tacky_ir::BinaryOperator),
    ShortCircuit(ShortCircuitBOT),
}
enum ShortCircuitBOT {
    And,
    Or,
}

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
            c_ast::Expression::Const(tokens::Const::Int(intval)) => ReadableValue::Constant(intval),
            c_ast::Expression::Unary(op, sub_exp) => {
                let op = Self::convert_unary_op(op);
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
            c_ast::Expression::Binary(op, exp1, exp2) => {
                match Self::convert_binary_op(op) {
                    BinaryOperatorType::EvaluateBothHands(op) => {
                        let src1 = Self::tackify_exp(*exp1, instrs);
                        let src2 = Self::tackify_exp(*exp2, instrs);
                        let dst = Rc::new(Variable::new());
                        let instr = Instruction::Binary {
                            op,
                            src1,
                            src2,
                            dst: Rc::clone(&dst),
                        };
                        instrs.push(instr);
                        ReadableValue::Variable(dst)
                    }
                    BinaryOperatorType::ShortCircuit(op) => {
                        let op_name = match op {
                            ShortCircuitBOT::And => "And",
                            ShortCircuitBOT::Or => "Or",
                        };
                        let label_shortcirc = Rc::new(LabelIdentifier::new(Some(format!(
                            "{op_name}_shortcircuit"
                        ))));
                        let label_end =
                            Rc::new(LabelIdentifier::new(Some(format!("{op_name}_end"))));
                        let new_shortcircuit_jump_instr = |condition: ReadableValue| {
                            let tgt = Rc::clone(&label_shortcirc);
                            match op {
                                ShortCircuitBOT::And => Instruction::JumpIfZero { condition, tgt },
                                ShortCircuitBOT::Or => {
                                    Instruction::JumpIfNotZero { condition, tgt }
                                }
                            }
                        };
                        let (shortcirc_val, fully_evald_val) = match op {
                            ShortCircuitBOT::And => (0, 1),
                            ShortCircuitBOT::Or => (1, 0),
                        };
                        let dst = Rc::new(Variable::new());

                        /* Begin instructions */

                        let lhs_val = Self::tackify_exp(*exp1, instrs);

                        instrs.push(new_shortcircuit_jump_instr(lhs_val));

                        let rhs_val = Self::tackify_exp(*exp2, instrs);

                        instrs.push(new_shortcircuit_jump_instr(rhs_val));

                        instrs.push(Instruction::Copy {
                            src: ReadableValue::Constant(fully_evald_val),
                            dst: Rc::clone(&dst),
                        });

                        instrs.push(Instruction::Jump(Rc::clone(&label_end)));

                        instrs.push(Instruction::Label(label_shortcirc));

                        instrs.push(Instruction::Copy {
                            src: ReadableValue::Constant(shortcirc_val),
                            dst: Rc::clone(&dst),
                        });

                        instrs.push(Instruction::Label(label_end));

                        ReadableValue::Variable(dst)
                    }
                }
            }
        }
    }
    fn convert_unary_op(c_unary_op: c_ast::UnaryOperator) -> UnaryOperator {
        use c_ast::UnaryOperator as CUO;
        match c_unary_op {
            CUO::Complement => UnaryOperator::Complement,
            CUO::Negate => UnaryOperator::Negate,
            CUO::Not => UnaryOperator::Not,
        }
    }
    fn convert_binary_op(c_binary_op: c_ast::BinaryOperator) -> BinaryOperatorType {
        use c_ast::BinaryOperator as CBO;
        use tacky_ir::BinaryOperator as TBO;
        use BinaryOperatorType as BOT;
        use ShortCircuitBOT as SBOT;
        match c_binary_op {
            CBO::And => BOT::ShortCircuit(SBOT::And),
            CBO::Or => BOT::ShortCircuit(SBOT::Or),
            CBO::Sub => BOT::EvaluateBothHands(TBO::Sub),
            CBO::Add => BOT::EvaluateBothHands(TBO::Add),
            CBO::Mul => BOT::EvaluateBothHands(TBO::Mul),
            CBO::Div => BOT::EvaluateBothHands(TBO::Div),
            CBO::Rem => BOT::EvaluateBothHands(TBO::Rem),
            CBO::Eq => BOT::EvaluateBothHands(TBO::Eq),
            CBO::Neq => BOT::EvaluateBothHands(TBO::Neq),
            CBO::Lt => BOT::EvaluateBothHands(TBO::Lt),
            CBO::Lte => BOT::EvaluateBothHands(TBO::Lte),
            CBO::Gt => BOT::EvaluateBothHands(TBO::Gt),
            CBO::Gte => BOT::EvaluateBothHands(TBO::Gte),
        }
    }
}
