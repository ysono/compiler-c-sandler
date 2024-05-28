use crate::{stage2_parser::c_ast, stage3_tacky::tacky_ir};
use std::collections::HashMap;
use std::mem::{self, MaybeUninit};
use std::rc::Rc;

pub mod asm_code {
    use crate::{stage1_lexer::tokens::Identifier, stage3_tacky::tacky_ir::Variable};
    use derive_more::{Deref, From};
    use std::rc::Rc;

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
        Mov { src: Operand, dst: Operand },
        Unary(UnaryOperator, Operand),
        AllocateStack(StackPosition),
        Ret,
    }
    #[derive(Debug)]
    pub enum UnaryOperator {
        Not,
        Neg,
    }
    #[derive(From, Clone, Debug)]
    pub enum Operand {
        ImmediateValue(i32),
        Register(Register),
        PseudoRegister(Rc<Variable>),
        StackPosition(StackPosition),
    }
    #[derive(Clone, Copy, Debug)]
    pub enum Register {
        AX,
        R10,
    }
    /// Abs offset from RBP. I.e. negation of at-runtime offset from RBP.
    #[derive(Clone, Copy, Deref, Debug)]
    pub struct StackPosition(pub(super) usize);
}
use asm_code::*;

pub struct AsmCodeGenerator {}
impl AsmCodeGenerator {
    pub fn gen_program(t_prog: tacky_ir::Program) -> Program {
        let tacky_ir::Program { func } = t_prog;
        let func = Self::gen_func(func);
        Program { func }
    }
    fn gen_func(t_func: tacky_ir::Function) -> Function {
        let tacky_ir::Function {
            ident,
            instructions: t_intrs,
        } = t_func;

        let asm_instrs = Self::gen_instructions(t_intrs);

        let mut opt = FuncOptimizer::new();
        let asm_instrs = opt.optimize_instrs(asm_instrs);

        Function {
            ident,
            instructions: asm_instrs,
        }
    }
    fn gen_instructions(t_instrs: Vec<tacky_ir::Instruction>) -> impl Iterator<Item = Instruction> {
        t_instrs.into_iter().flat_map(|t_instr| match t_instr {
            tacky_ir::Instruction::Return(t_val) => {
                let asm_src = Self::convert_operand(t_val);
                let asm_dst = Operand::Register(Register::AX);
                let asm_instr_1 = Instruction::Mov {
                    src: asm_src,
                    dst: asm_dst,
                };

                let asm_instr_2 = Instruction::Ret;

                [asm_instr_1, asm_instr_2]
            }
            tacky_ir::Instruction::Unary { op, src, dst } => {
                let asm_src = Self::convert_operand(src);
                let asm_dst = Self::convert_operand(tacky_ir::ReadableValue::Variable(dst));
                let asm_instr_1 = Instruction::Mov {
                    src: asm_src,
                    dst: asm_dst.clone(),
                };

                let asm_op = Self::convert_unary_op(op);
                let asm_instr_2 = Instruction::Unary(asm_op, asm_dst);

                [asm_instr_1, asm_instr_2]
            }
            tacky_ir::Instruction::Binary { .. } => {
                panic!("Non-supported Tacky IR: {t_instr:?}");
            }
        })
    }
    fn convert_unary_op(c_op: c_ast::UnaryOperator) -> UnaryOperator {
        match c_op {
            c_ast::UnaryOperator::Complement => UnaryOperator::Not,
            c_ast::UnaryOperator::Negate => UnaryOperator::Neg,
        }
    }
    fn convert_operand(t_val: tacky_ir::ReadableValue) -> Operand {
        match t_val {
            tacky_ir::ReadableValue::Constant(intval) => Operand::ImmediateValue(intval),
            tacky_ir::ReadableValue::Variable(v) => Operand::PseudoRegister(v),
        }
    }
}

struct FuncOptimizer {
    last_used_stack_pos: StackPosition,
    var_to_stack_pos: HashMap<Rc<tacky_ir::Variable>, StackPosition>,
}
impl FuncOptimizer {
    fn new() -> Self {
        Self {
            last_used_stack_pos: StackPosition(0),
            var_to_stack_pos: HashMap::new(),
        }
    }
    fn optimize_instrs<I>(&mut self, in_instrs: I) -> Vec<Instruction>
    where
        I: Iterator<Item = Instruction>,
    {
        let instrs = in_instrs.into_iter();
        let instrs = self.convert_pseudo_to_stackpos(instrs);
        let instrs = Self::correct_invalid_movs(instrs);

        #[allow(invalid_value)]
        let dummy_alloc_stk_instr = unsafe { MaybeUninit::uninit().assume_init() };
        let mut out_instrs = vec![dummy_alloc_stk_instr];

        out_instrs.extend(instrs);

        /* We must evaluate self.last_used_stack_pos only after the iterator of `Instruction`s has been completely traversed. */
        let alloc_stk_instr = Instruction::AllocateStack(self.last_used_stack_pos);
        out_instrs[0] = alloc_stk_instr;

        out_instrs
    }

    fn convert_pseudo_to_stackpos<'a, I>(
        &'a mut self,
        in_instrs: I,
    ) -> impl 'a + Iterator<Item = Instruction>
    where
        I: 'a + Iterator<Item = Instruction>,
    {
        in_instrs.map(move |in_instr| match in_instr {
            Instruction::Mov { src, dst } => {
                let src = self.operand_to_non_pseudo(src);
                let dst = self.operand_to_non_pseudo(dst);
                Instruction::Mov { src, dst }
            }
            Instruction::Unary(op, operand) => {
                let operand = self.operand_to_non_pseudo(operand);
                Instruction::Unary(op, operand)
            }
            Instruction::AllocateStack(_) | Instruction::Ret => in_instr,
        })
    }
    fn operand_to_non_pseudo(&mut self, operand: Operand) -> Operand {
        match operand {
            Operand::PseudoRegister(var) => self.var_to_stack_pos(var).into(),
            _ => operand,
        }
    }
    fn var_to_stack_pos(&mut self, var: Rc<tacky_ir::Variable>) -> StackPosition {
        /* For now, all C AST `Expression`s and all Tacky `Value`s are 32-bit values. */
        const VAL_BYTELEN: usize = mem::size_of::<i32>();
        let pos = self.var_to_stack_pos.entry(var).or_insert_with(|| {
            self.last_used_stack_pos.0 += VAL_BYTELEN;
            self.last_used_stack_pos
        });
        *pos
    }

    fn correct_invalid_movs<'a, I>(in_instrs: I) -> impl 'a + Iterator<Item = Instruction>
    where
        I: 'a + Iterator<Item = Instruction>,
    {
        in_instrs.flat_map(|in_instr| match in_instr {
            Instruction::Mov {
                src: src @ Operand::StackPosition(_),
                dst: dst @ Operand::StackPosition(_),
            } => {
                let temp_reg = Operand::Register(Register::R10);
                let out_instr_1 = Instruction::Mov {
                    src,
                    dst: temp_reg.clone(),
                };
                let out_instr_2 = Instruction::Mov { src: temp_reg, dst };
                vec![out_instr_1, out_instr_2]
            }
            _ => vec![in_instr],
        })
    }
}
