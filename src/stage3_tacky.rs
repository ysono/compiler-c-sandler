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
        name: String,
    }
    impl LabelIdentifier {
        pub(super) fn new(name: String) -> Self {
            static NEXT_ID: AtomicU64 = AtomicU64::new(0);
            let curr_id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
            Self { id: curr_id, name }
        }
        pub fn id(&self) -> u64 {
            self.id
        }
        pub fn name(&self) -> &String {
            &self.name
        }
    }
}

use self::tacky_ir::*;
use crate::stage2b_validate::c_ast as c;
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
    pub fn tackify_program(c_prog: c::Program) -> Program {
        let c::Program { func } = c_prog;

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
    fn tackify_func(mut self, c_func: c::Function) -> Function {
        let c::Function { ident, body } = c_func;

        self.gen_block(body);

        let ret_kon = c::Const::Int(0);
        let ret_exp = c::Expression::Const(ret_kon);
        let ret_stmt = c::Statement::Return(ret_exp);
        self.gen_stmt(ret_stmt);

        Function {
            ident,
            instructions: self.instrs,
        }
    }
    fn gen_block(&mut self, c_block: c::Block) {
        for c_item in c_block.items {
            match c_item {
                c::BlockItem::Declaration(c_decl) => self.gen_decl(c_decl),
                c::BlockItem::Statement(c_stmt) => self.gen_stmt(c_stmt),
            }
        }
    }
    fn gen_decl(&mut self, c_decl: c::Declaration) {
        let c::Declaration { var, init } = c_decl;
        match init {
            None => { /* No-op. */ }
            Some(init_exp) => {
                self.gen_exp_assignment(var, init_exp);
            }
        }
    }
    fn gen_stmt(&mut self, c_stmt: c::Statement) {
        match c_stmt {
            c::Statement::Return(c_root_exp) => {
                let t_root_val = self.gen_exp(c_root_exp);
                self.instrs.push(Instruction::Return(t_root_val));
            }
            c::Statement::Expression(c_root_exp) => {
                self.gen_exp(c_root_exp);
            }
            c::Statement::If(c_if) => self.gen_stmt_conditional(c_if),
            c::Statement::Compound(c_block) => self.gen_block(c_block),
            c::Statement::Break(_lbl) => todo!(),
            c::Statement::Continue(_lbl) => todo!(),
            c::Statement::While(_lbl, _condbody) => todo!(),
            c::Statement::DoWhile(_lbl, _condbody) => todo!(),
            c::Statement::For(_lbl, _foor) => todo!(),
            c::Statement::Null => { /* No-op. */ }
        }
    }
    fn gen_exp(&mut self, c_exp: c::Expression) -> ReadableValue {
        match c_exp {
            c::Expression::Const(c::Const::Int(i)) => ReadableValue::Constant(i),
            c::Expression::Var(var) => ReadableValue::Variable(var),
            c::Expression::Unary(unary) => self.gen_exp_unary(unary),
            c::Expression::Binary(binary) => self.gen_exp_binary(binary),
            c::Expression::Assignment(c::Assignment { var, rhs }) => {
                self.gen_exp_assignment(var, *rhs)
            }
            c::Expression::Conditional(c_cond) => self.gen_exp_conditional(c_cond),
        }
    }

    /* C Unary */

    fn gen_exp_unary(&mut self, c_unary: c::Unary) -> ReadableValue {
        let op = Self::convert_op_unary(c_unary.op);
        let src = self.gen_exp(*c_unary.sub_exp);
        let dst = Rc::new(Variable::new_anon());
        self.instrs.push(Instruction::Unary(Unary {
            op,
            src,
            dst: Rc::clone(&dst),
        }));
        ReadableValue::Variable(dst)
    }

    /* C Binary */

    fn gen_exp_binary(&mut self, c_binary: c::Binary) -> ReadableValue {
        match Self::convert_op_binary(&c_binary.op) {
            BinaryOperatorType::EvaluateBothHands(t_op) => {
                self.gen_exp_binary_evalboth(t_op, c_binary)
            }
            BinaryOperatorType::ShortCircuit(t_op) => self.gen_exp_binary_shortcirc(t_op, c_binary),
        }
    }
    fn gen_exp_binary_evalboth(
        &mut self,
        op: tacky_ir::BinaryOperator,
        c_binary: c::Binary,
    ) -> ReadableValue {
        let src1 = self.gen_exp(*c_binary.lhs);
        let src2 = self.gen_exp(*c_binary.rhs);
        let dst = Rc::new(Variable::new_anon());
        self.instrs.push(Instruction::Binary(Binary {
            op,
            src1,
            src2,
            dst: Rc::clone(&dst),
        }));
        ReadableValue::Variable(dst)
    }
    fn gen_exp_binary_shortcirc(
        &mut self,
        op_type: ShortCircuitBOT,
        c_binary: c::Binary,
    ) -> ReadableValue {
        let result = Rc::new(Variable::new_anon());

        let name = result.id();
        let label_shortcirc = Rc::new(LabelIdentifier::new(format!(
            "{op_type}.{name:x}.shortcircuit",
        )));
        let label_end = Rc::new(LabelIdentifier::new(format!("{op_type}.{name:x}.end",)));

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

        let lhs_val = self.gen_exp(*c_binary.lhs);

        self.instrs.push(new_shortcirc_jump_instr(lhs_val));

        let rhs_val = self.gen_exp(*c_binary.rhs);

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

    fn convert_op_unary(c_unary_op: c::UnaryOperator) -> UnaryOperator {
        use c::UnaryOperator as CUO;
        match c_unary_op {
            CUO::Complement => UnaryOperator::Complement,
            CUO::Negate => UnaryOperator::Negate,
            CUO::Not => UnaryOperator::Not,
        }
    }
    fn convert_op_binary(c_binary_op: &c::BinaryOperator) -> BinaryOperatorType {
        use c::BinaryOperator as CBO;
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

    fn gen_exp_assignment(&mut self, var: Rc<Variable>, rhs: c::Expression) -> ReadableValue {
        let rhs = self.gen_exp(rhs);

        self.instrs.push(Instruction::Copy(Copy {
            src: rhs,
            dst: Rc::clone(&var),
        }));

        ReadableValue::Variable(var)
    }

    /* Conditional */

    fn gen_stmt_conditional(&mut self, c_if: c::If) {
        let c::If {
            condition,
            then,
            elze,
        } = c_if;

        match elze {
            None => {
                let label_end = Rc::new(LabelIdentifier::new(format!("stmt_cond_end")));

                /* Begin instructions */

                let condition = self.gen_exp(condition);

                self.instrs.push(Instruction::JumpIfZero(JumpIf {
                    condition,
                    tgt: Rc::clone(&label_end),
                }));

                self.gen_stmt(*then);

                self.instrs.push(Instruction::Label(label_end));
            }
            Some(elze) => {
                let name = &*then as *const c::Statement as usize;
                let label_else = Rc::new(LabelIdentifier::new(format!("stmt_cond.{name:x}.else")));
                let label_end = Rc::new(LabelIdentifier::new(format!("stmt_cond.{name:x}.end")));

                /* Begin instructions */

                let condition = self.gen_exp(condition);

                self.instrs.push(Instruction::JumpIfZero(JumpIf {
                    condition,
                    tgt: Rc::clone(&label_else),
                }));

                self.gen_stmt(*then);

                self.instrs.push(Instruction::Jump(Rc::clone(&label_end)));

                self.instrs.push(Instruction::Label(label_else));

                self.gen_stmt(*elze);

                self.instrs.push(Instruction::Label(label_end));
            }
        }
    }
    fn gen_exp_conditional(&mut self, c_cond: c::Conditional) -> ReadableValue {
        let c::Conditional {
            condition,
            then,
            elze,
        } = c_cond;

        let result = Rc::new(Variable::new_anon());

        let name = result.id();
        let label_else = Rc::new(LabelIdentifier::new(format!("exp_cond.{name:x}.else")));
        let label_end = Rc::new(LabelIdentifier::new(format!("exp_cond.{name:x}.end",)));

        /* Begin instructions */

        let condition = self.gen_exp(*condition);

        self.instrs.push(Instruction::JumpIfZero(JumpIf {
            condition,
            tgt: Rc::clone(&label_else),
        }));

        let then = self.gen_exp(*then);

        self.instrs.push(Instruction::Copy(Copy {
            src: then,
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Jump(Rc::clone(&label_end)));

        self.instrs.push(Instruction::Label(label_else));

        let elze = self.gen_exp(*elze);

        self.instrs.push(Instruction::Copy(Copy {
            src: elze,
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Label(label_end));

        ReadableValue::Variable(result)
    }
}
