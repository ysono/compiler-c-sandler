pub mod tacky_ir {
    pub use self::instruction::*;
    pub use crate::stage2b_validate::c_ast::{Identifier, Variable};
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

    #[derive(Debug)]
    pub struct LabelIdentifier {
        id: u64,
        name: Option<String>,
    }
    impl LabelIdentifier {
        pub(super) fn new(name: Option<String>) -> Self {
            static NEXT_ID: AtomicU64 = AtomicU64::new(0);
            let curr_id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
            Self { id: curr_id, name }
        }
        pub fn id(&self) -> u64 {
            self.id
        }
        pub fn name(&self) -> &Option<String> {
            &self.name
        }
    }
}

use self::tacky_ir::*;
use crate::stage2b_validate::c_ast;
use derive_more::Display;
use std::rc::Rc;

enum BinaryOperatorType {
    EvaluateBothHands(tacky_ir::BinaryOperator),
    ShortCircuit(ShortCircuitBOT),
}
#[derive(Display)]
enum ShortCircuitBOT {
    And,
    Or,
}

pub struct Tackifier {}
impl Tackifier {
    pub fn tackify_program(c_prog: c_ast::Program) -> Program {
        let c_ast::Program { func } = c_prog;

        let gen_instrs = TackyIrGenerator::default();
        let func = gen_instrs.tackify_func(func);

        Program { func }
    }
}

#[derive(Default)]
struct TackyIrGenerator {
    instrs: Vec<Instruction>,
}
impl TackyIrGenerator {
    fn tackify_func(mut self, c_func: c_ast::Function) -> Function {
        let c_ast::Function { ident, body } = c_func;

        self.tackify_block(body);

        let ret_kon = c_ast::Const::Int(0);
        let ret_exp = c_ast::Expression::Const(ret_kon);
        let ret_stmt = c_ast::Statement::Return(ret_exp);
        self.tackify_stmt(ret_stmt);

        Function {
            ident,
            instructions: self.instrs,
        }
    }
    fn tackify_block(&mut self, c_block: c_ast::Block) {
        for c_item in c_block.items {
            match c_item {
                c_ast::BlockItem::Declaration(c_decl) => self.tackify_decl(c_decl),
                c_ast::BlockItem::Statement(c_stmt) => self.tackify_stmt(c_stmt),
            }
        }
    }
    fn tackify_decl(&mut self, c_decl: c_ast::Declaration) {
        let c_ast::Declaration { var, init } = c_decl;
        match init {
            None => { /* No-op. */ }
            Some(init_exp) => {
                self.tackify_assignment_exp(var, init_exp);
            }
        }
    }
    fn tackify_stmt(&mut self, c_stmt: c_ast::Statement) {
        match c_stmt {
            c_ast::Statement::Return(c_root_exp) => {
                let t_root_val = self.tackify_exp(c_root_exp);
                self.instrs.push(Instruction::Return(t_root_val));
            }
            c_ast::Statement::Expression(c_root_exp) => {
                self.tackify_exp(c_root_exp);
            }
            c_ast::Statement::If(c_if) => self.tackify_conditional_stmt(c_if),
            c_ast::Statement::Compound(c_block) => self.tackify_block(c_block),
            c_ast::Statement::Null => { /* No-op. */ }
        }
    }
    fn tackify_exp(&mut self, c_exp: c_ast::Expression) -> ReadableValue {
        match c_exp {
            c_ast::Expression::Const(c_ast::Const::Int(i)) => ReadableValue::Constant(i),
            c_ast::Expression::Var(var) => ReadableValue::Variable(var),
            c_ast::Expression::Unary(unary) => self.tackify_unary_exp(unary),
            c_ast::Expression::Binary(binary) => self.tackify_binary_exp(binary),
            c_ast::Expression::Assignment(c_ast::Assignment { var, rhs }) => {
                self.tackify_assignment_exp(var, *rhs)
            }
            c_ast::Expression::Conditional(c_cond) => self.tackify_conditional_exp(c_cond),
        }
    }

    /* C Unary */

    fn tackify_unary_exp(&mut self, c_unary: c_ast::Unary) -> ReadableValue {
        let op = Self::convert_unary_op(c_unary.op);
        let src = self.tackify_exp(*c_unary.sub_exp);
        let dst = Rc::new(Variable::new_anon());
        self.instrs.push(Instruction::Unary(Unary {
            op,
            src,
            dst: Rc::clone(&dst),
        }));
        ReadableValue::Variable(dst)
    }

    /* C Binary */

    fn tackify_binary_exp(&mut self, c_binary: c_ast::Binary) -> ReadableValue {
        match Self::convert_binary_op(&c_binary.op) {
            BinaryOperatorType::EvaluateBothHands(t_op) => {
                self.tackify_binary_evalboth_exp(t_op, c_binary)
            }
            BinaryOperatorType::ShortCircuit(t_op) => {
                self.tackify_binary_shortcirc_exp(t_op, c_binary)
            }
        }
    }
    fn tackify_binary_evalboth_exp(
        &mut self,
        op: tacky_ir::BinaryOperator,
        c_binary: c_ast::Binary,
    ) -> ReadableValue {
        let src1 = self.tackify_exp(*c_binary.lhs);
        let src2 = self.tackify_exp(*c_binary.rhs);
        let dst = Rc::new(Variable::new_anon());
        self.instrs.push(Instruction::Binary(Binary {
            op,
            src1,
            src2,
            dst: Rc::clone(&dst),
        }));
        ReadableValue::Variable(dst)
    }
    fn tackify_binary_shortcirc_exp(
        &mut self,
        op_type: ShortCircuitBOT,
        c_binary: c_ast::Binary,
    ) -> ReadableValue {
        let result = Rc::new(Variable::new_anon());

        let name = result.id();
        let label_shortcirc = Rc::new(LabelIdentifier::new(Some(format!(
            "{op_type}.{name:x}.shortcircuit",
        ))));
        let label_end = Rc::new(LabelIdentifier::new(Some(format!(
            "{op_type}.{name:x}.end",
        ))));

        let new_shortcirc_jump_instr = |condition: ReadableValue| {
            let tgt = Rc::clone(&label_shortcirc);
            let jumpif = JumpIf { condition, tgt };
            match op_type {
                ShortCircuitBOT::And => Instruction::JumpIfZero(jumpif),
                ShortCircuitBOT::Or => Instruction::JumpIfNotZero(jumpif),
            }
        };

        let (shortcirc_val, fully_evald_val) = match op_type {
            ShortCircuitBOT::And => (0, 1),
            ShortCircuitBOT::Or => (1, 0),
        };

        /* Begin instructions */

        let lhs_val = self.tackify_exp(*c_binary.lhs);

        self.instrs.push(new_shortcirc_jump_instr(lhs_val));

        let rhs_val = self.tackify_exp(*c_binary.rhs);

        self.instrs.push(new_shortcirc_jump_instr(rhs_val));

        self.instrs.push(Instruction::Copy(Copy {
            src: ReadableValue::Constant(fully_evald_val),
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Jump(Rc::clone(&label_end)));

        self.instrs.push(Instruction::Label(label_shortcirc));

        self.instrs.push(Instruction::Copy(Copy {
            src: ReadableValue::Constant(shortcirc_val),
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Label(label_end));

        ReadableValue::Variable(result)
    }

    /* C Operator */

    fn convert_unary_op(c_unary_op: c_ast::UnaryOperator) -> UnaryOperator {
        use c_ast::UnaryOperator as CUO;
        match c_unary_op {
            CUO::Complement => UnaryOperator::Complement,
            CUO::Negate => UnaryOperator::Negate,
            CUO::Not => UnaryOperator::Not,
        }
    }
    fn convert_binary_op(c_binary_op: &c_ast::BinaryOperator) -> BinaryOperatorType {
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

    /* C Assignment */

    fn tackify_assignment_exp(
        &mut self,
        var: Rc<Variable>,
        rhs: c_ast::Expression,
    ) -> ReadableValue {
        let rhs = self.tackify_exp(rhs);

        self.instrs.push(Instruction::Copy(Copy {
            src: rhs,
            dst: Rc::clone(&var),
        }));

        ReadableValue::Variable(var)
    }

    /* Conditional */

    fn tackify_conditional_stmt(&mut self, c_if: c_ast::If) {
        let c_ast::If {
            condition,
            then,
            elze,
        } = c_if;

        match elze {
            None => {
                let label_end = Rc::new(LabelIdentifier::new(Some(format!("stmt_cond_end"))));

                /* Begin instructions */

                let condition = self.tackify_exp(condition);

                self.instrs.push(Instruction::JumpIfZero(JumpIf {
                    condition,
                    tgt: Rc::clone(&label_end),
                }));

                self.tackify_stmt(*then);

                self.instrs.push(Instruction::Label(label_end));
            }
            Some(elze) => {
                let name = &*then as *const c_ast::Statement as usize;
                let label_else = Rc::new(LabelIdentifier::new(Some(format!(
                    "stmt_cond.{name:x}.else"
                ))));
                let label_end = Rc::new(LabelIdentifier::new(Some(format!(
                    "stmt_cond.{name:x}.end"
                ))));

                /* Begin instructions */

                let condition = self.tackify_exp(condition);

                self.instrs.push(Instruction::JumpIfZero(JumpIf {
                    condition,
                    tgt: Rc::clone(&label_else),
                }));

                self.tackify_stmt(*then);

                self.instrs.push(Instruction::Jump(Rc::clone(&label_end)));

                self.instrs.push(Instruction::Label(label_else));

                self.tackify_stmt(*elze);

                self.instrs.push(Instruction::Label(label_end));
            }
        }
    }
    fn tackify_conditional_exp(&mut self, c_cond: c_ast::Conditional) -> ReadableValue {
        let c_ast::Conditional {
            condition,
            then,
            elze,
        } = c_cond;

        let result = Rc::new(Variable::new_anon());

        let name = result.id();
        let label_else = Rc::new(LabelIdentifier::new(Some(format!(
            "exp_cond.{name:x}.else"
        ))));
        let label_end = Rc::new(LabelIdentifier::new(Some(
            format!("exp_cond.{name:x}.end",),
        )));

        /* Begin instructions */

        let condition = self.tackify_exp(*condition);

        self.instrs.push(Instruction::JumpIfZero(JumpIf {
            condition,
            tgt: Rc::clone(&label_else),
        }));

        let then = self.tackify_exp(*then);

        self.instrs.push(Instruction::Copy(Copy {
            src: then,
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Jump(Rc::clone(&label_end)));

        self.instrs.push(Instruction::Label(label_else));

        let elze = self.tackify_exp(*elze);

        self.instrs.push(Instruction::Copy(Copy {
            src: elze,
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Label(label_end));

        ReadableValue::Variable(result)
    }
}
