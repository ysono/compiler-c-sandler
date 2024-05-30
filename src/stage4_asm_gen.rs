use crate::stage3_tacky::tacky_ir;
use std::collections::HashMap;
use std::mem::{self, MaybeUninit};
use std::rc::Rc;

pub mod asm_code {
    use crate::{stage1_lexer::tokens::Identifier, stage3_tacky::tacky_ir::Variable};
    use derive_more::{Deref, From};
    use std::rc::Rc;

    #[derive(Debug)]
    pub struct Program<Oprnd> {
        pub func: Function<Oprnd>,
    }

    #[derive(Debug)]
    pub struct Function<Oprnd> {
        pub ident: Identifier,
        pub instructions: Vec<Instruction<Oprnd>>,
    }

    #[derive(Debug)]
    pub enum Instruction<Oprnd> {
        Mov {
            src: Oprnd,
            dst: Oprnd,
        },
        Unary(UnaryOperator, Oprnd),
        Binary {
            op: BinaryOperator,
            arg: Oprnd, // Semantic RHS. Asm operand #1.
            tgt: Oprnd, // Semantic LHS, as well as output. Asm operand #2.
        },
        Idiv(Oprnd),
        Cdq,
        AllocateStack(StackPosition),
        Ret,
    }

    #[derive(Debug)]
    pub enum UnaryOperator {
        BitwiseComplement,
        TwosComplement,
    }

    #[derive(Debug)]
    pub enum BinaryOperator {
        Add,
        Sub,
        Mul,
    }

    #[derive(From, Clone, Debug)]
    pub(super) enum PreFinalOperand {
        ImmediateValue(i32),
        Register(Register),
        PseudoRegister(Rc<Variable>),
    }

    #[derive(From, Clone, Debug)]
    pub enum Operand {
        ImmediateValue(i32),
        Register(Register),
        StackPosition(StackPosition),
    }

    #[derive(Clone, Copy, Debug)]
    pub enum Register {
        AX,
        DX,
        R10,
        R11,
    }

    /// Abs offset from RBP. I.e. negation of at-runtime offset from RBP.
    #[derive(Clone, Copy, Deref, Debug)]
    pub struct StackPosition(pub(super) usize);
}
use asm_code::*;

pub struct AsmCodeGenerator {}
impl AsmCodeGenerator {
    pub fn gen_program(t_prog: tacky_ir::Program) -> Program<Operand> {
        let tacky_ir::Program { func } = t_prog;
        let func = Self::gen_func(func);
        Program { func }
    }
    fn gen_func(t_func: tacky_ir::Function) -> Function<Operand> {
        let tacky_ir::Function {
            ident,
            instructions: t_intrs,
        } = t_func;

        let asm_instrs = Self::gen_instructions(t_intrs);

        let mut fin = InstrsFinalizer::new();
        let asm_instrs = fin.finalize_instrs(asm_instrs);

        Function {
            ident,
            instructions: asm_instrs,
        }
    }
    fn gen_instructions(
        t_instrs: Vec<tacky_ir::Instruction>,
    ) -> impl Iterator<Item = Instruction<PreFinalOperand>> {
        t_instrs.into_iter().flat_map(|t_instr| match t_instr {
            tacky_ir::Instruction::Return(t_val) => Self::gen_return_instrs(t_val),
            tacky_ir::Instruction::Unary(unary) => Self::gen_unary_instrs(unary),
            tacky_ir::Instruction::Binary(binary) => Self::gen_binary_instrs(binary),
            tacky_ir::Instruction::Copy { .. } => todo!(),
            tacky_ir::Instruction::Jump(..) => todo!(),
            tacky_ir::Instruction::JumpIfZero { .. } => todo!(),
            tacky_ir::Instruction::JumpIfNotZero { .. } => todo!(),
            tacky_ir::Instruction::Label(..) => todo!(),
        })
    }

    /* Tacky Return */

    fn gen_return_instrs(t_val: tacky_ir::ReadableValue) -> Vec<Instruction<PreFinalOperand>> {
        let asm_src = Self::convert_val_operand(t_val);
        let asm_instr_1 = Instruction::Mov {
            src: asm_src,
            dst: Register::AX.into(),
        };

        let asm_instr_2 = Instruction::Ret;

        vec![asm_instr_1, asm_instr_2]
    }

    /* Tacky Unary */

    fn gen_unary_instrs(
        t_unary: tacky_ir::instruction::Unary,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let asm_src = Self::convert_val_operand(t_unary.src);
        let asm_dst = Self::convert_var_operand(t_unary.dst);
        let asm_instr_1 = Instruction::Mov {
            src: asm_src,
            dst: asm_dst.clone(),
        };

        let asm_op = match t_unary.op {
            tacky_ir::UnaryOperator::Complement => UnaryOperator::BitwiseComplement,
            tacky_ir::UnaryOperator::Negate => UnaryOperator::TwosComplement,
            tacky_ir::UnaryOperator::Not => todo!(),
        };
        let asm_instr_2 = Instruction::Unary(asm_op, asm_dst);

        vec![asm_instr_1, asm_instr_2]
    }

    /* Tacky Binary */

    fn gen_binary_instrs(
        t_binary: tacky_ir::instruction::Binary,
    ) -> Vec<Instruction<PreFinalOperand>> {
        use tacky_ir::BinaryOperator as TBO;

        match t_binary.op {
            TBO::Add => Self::gen_arithmetic_instrs(BinaryOperator::Add, t_binary),
            TBO::Sub => Self::gen_arithmetic_instrs(BinaryOperator::Sub, t_binary),
            TBO::Mul => Self::gen_arithmetic_instrs(BinaryOperator::Mul, t_binary),

            TBO::Div => Self::gen_divrem_instrs(Register::AX, t_binary),
            TBO::Rem => Self::gen_divrem_instrs(Register::DX, t_binary),

            TBO::Eq | TBO::Neq | TBO::Lt | TBO::Lte | TBO::Gt | TBO::Gte => todo!(),
        }
    }
    fn gen_arithmetic_instrs(
        asm_op: asm_code::BinaryOperator,
        t_binary: tacky_ir::instruction::Binary,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let asm_src1 = Self::convert_val_operand(t_binary.src1);
        let asm_src2 = Self::convert_val_operand(t_binary.src2);
        let asm_dst = Self::convert_var_operand(t_binary.dst);

        let asm_instr_1 = Instruction::Mov {
            src: asm_src1,
            dst: asm_dst.clone(),
        };
        let asm_instr_2 = Instruction::Binary {
            op: asm_op,
            tgt: asm_dst,
            arg: asm_src2,
        };
        vec![asm_instr_1, asm_instr_2]
    }
    fn gen_divrem_instrs(
        ans_reg: asm_code::Register,
        t_binary: tacky_ir::instruction::Binary,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let asm_src1 = Self::convert_val_operand(t_binary.src1);
        let asm_src2 = Self::convert_val_operand(t_binary.src2);
        let asm_dst = Self::convert_var_operand(t_binary.dst);

        let asm_instr_1 = Instruction::Mov {
            src: asm_src1,
            dst: Register::AX.into(),
        };
        let asm_instr_2 = Instruction::Cdq;
        let asm_instr_3 = Instruction::Idiv(asm_src2);
        let asm_instr_4 = Instruction::Mov {
            src: ans_reg.into(),
            dst: asm_dst,
        };
        vec![asm_instr_1, asm_instr_2, asm_instr_3, asm_instr_4]
    }

    /* Operand */

    fn convert_var_operand(t_var: Rc<tacky_ir::Variable>) -> PreFinalOperand {
        Self::convert_val_operand(tacky_ir::ReadableValue::Variable(t_var))
    }
    fn convert_val_operand(t_val: tacky_ir::ReadableValue) -> PreFinalOperand {
        match t_val {
            tacky_ir::ReadableValue::Constant(i) => PreFinalOperand::ImmediateValue(i),
            tacky_ir::ReadableValue::Variable(v) => PreFinalOperand::PseudoRegister(v),
        }
    }
}

struct InstrsFinalizer {
    last_used_stack_pos: StackPosition,
    var_to_stack_pos: HashMap<Rc<tacky_ir::Variable>, StackPosition>,
}
impl InstrsFinalizer {
    fn new() -> Self {
        Self {
            last_used_stack_pos: StackPosition(0),
            var_to_stack_pos: HashMap::new(),
        }
    }
    fn finalize_instrs(
        &mut self,
        in_instrs: impl Iterator<Item = Instruction<PreFinalOperand>>,
    ) -> Vec<Instruction<Operand>> {
        let instrs = in_instrs.into_iter();
        let instrs = self.convert_operands(instrs);
        let instrs = correct_invalid_operands::correct_invalid_operands(instrs);

        #[allow(invalid_value)]
        let dummy_alloc_stk_instr = unsafe { MaybeUninit::uninit().assume_init() };
        let mut out_instrs = vec![dummy_alloc_stk_instr];

        out_instrs.extend(instrs);

        /* We must evaluate self.last_used_stack_pos only after the iterator of `Instruction`s has been completely traversed. */
        let alloc_stk_instr = Instruction::AllocateStack(self.last_used_stack_pos);
        out_instrs[0] = alloc_stk_instr;

        out_instrs
    }

    fn convert_operands<'a>(
        &'a mut self,
        in_instrs: impl 'a + Iterator<Item = Instruction<PreFinalOperand>>,
    ) -> impl 'a + Iterator<Item = Instruction<Operand>> {
        in_instrs.map(move |in_instr| match in_instr {
            Instruction::Mov { src, dst } => {
                let src = self.convert_operand(src);
                let dst = self.convert_operand(dst);
                Instruction::Mov { src, dst }
            }
            Instruction::Unary(op, operand) => {
                let operand = self.convert_operand(operand);
                Instruction::Unary(op, operand)
            }
            Instruction::Binary { op, arg, tgt } => {
                let arg = self.convert_operand(arg);
                let tgt = self.convert_operand(tgt);
                Instruction::Binary { op, arg, tgt }
            }
            Instruction::Idiv(operand) => {
                let operand = self.convert_operand(operand);
                Instruction::Idiv(operand)
            }
            Instruction::Cdq => Instruction::Cdq,
            Instruction::AllocateStack(s) => Instruction::AllocateStack(s),
            Instruction::Ret => Instruction::Ret,
        })
    }
    fn convert_operand(&mut self, pfo: PreFinalOperand) -> Operand {
        use PreFinalOperand as PFO;
        match pfo {
            PFO::PseudoRegister(var) => self.var_to_stack_pos(var).into(),
            PFO::ImmediateValue(i) => Operand::ImmediateValue(i),
            PFO::Register(r) => Operand::Register(r),
        }
    }
    fn var_to_stack_pos(&mut self, var: Rc<tacky_ir::Variable>) -> StackPosition {
        /* For now, all Tacky Values represent 32-bit values. */
        const VAL_BYTELEN: usize = mem::size_of::<i32>();
        let pos = self.var_to_stack_pos.entry(var).or_insert_with(|| {
            self.last_used_stack_pos.0 += VAL_BYTELEN;
            self.last_used_stack_pos
        });
        *pos
    }
}

mod correct_invalid_operands {
    use super::*;

    pub fn correct_invalid_operands<'a>(
        in_instrs: impl 'a + Iterator<Item = Instruction<Operand>>,
    ) -> impl 'a + Iterator<Item = Instruction<Operand>> {
        in_instrs.flat_map(|in_instr| match in_instr {
            Instruction::Mov {
                src: src @ Operand::StackPosition(_),
                dst: dst @ Operand::StackPosition(_),
            } => {
                let reg = Register::R10;
                let instr_at_reg = Instruction::Mov {
                    src: reg.into(),
                    dst,
                };
                to_reg(src, reg, instr_at_reg)
            }
            Instruction::Binary {
                op: op @ (BinaryOperator::Add | BinaryOperator::Sub),
                arg: arg @ Operand::StackPosition(_),
                tgt: tgt @ Operand::StackPosition(_),
            } => {
                let reg = Register::R10;
                let instr_at_reg = Instruction::Binary {
                    op,
                    arg: reg.into(),
                    tgt,
                };
                to_reg(arg, reg, instr_at_reg)
            }
            Instruction::Binary {
                op: op @ BinaryOperator::Mul,
                arg,
                tgt: tgt @ Operand::StackPosition(_),
            } => {
                let reg = Register::R11;
                let instr_at_reg = Instruction::Binary {
                    op,
                    arg,
                    tgt: reg.into(),
                };
                to_and_from_reg(tgt, reg, instr_at_reg)
            }
            Instruction::Idiv(imm @ Operand::ImmediateValue(_)) => {
                let reg = Register::R10;
                let instr_at_reg = Instruction::Idiv(reg.into());
                to_reg(imm, reg, instr_at_reg)
            }
            _ => vec![in_instr],
        })
    }
    fn to_reg(
        operand_to_reg: Operand,
        reg: Register,
        instr_at_reg: Instruction<Operand>,
    ) -> Vec<Instruction<Operand>> {
        let instr_to_reg = Instruction::Mov {
            src: operand_to_reg,
            dst: reg.into(),
        };
        vec![instr_to_reg, instr_at_reg]
    }
    fn to_and_from_reg(
        operand_to_and_from_reg: Operand,
        reg: Register,
        instr_at_reg: Instruction<Operand>,
    ) -> Vec<Instruction<Operand>> {
        let instr_to_reg = Instruction::Mov {
            src: operand_to_and_from_reg.clone(),
            dst: reg.into(),
        };
        let instr_from_reg = Instruction::Mov {
            src: reg.into(),
            dst: operand_to_and_from_reg,
        };
        vec![instr_to_reg, instr_at_reg, instr_from_reg]
    }
}
