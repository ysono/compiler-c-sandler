use crate::{
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::{asm_ast::*, phase2_finalize::InstrsFinalizer},
};
use std::cmp;
use std::rc::Rc;

pub struct AsmCodeGenerator {}
impl AsmCodeGenerator {
    pub fn gen_program(t::Program { funs }: t::Program) -> Program {
        let funs = funs.into_iter().map(Self::gen_fun).collect::<Vec<_>>();
        Program { funs }
    }

    /* Tacky Function */

    const ARG_REGS: [Register; 6] = [
        Register::DI,
        Register::SI,
        Register::DX,
        Register::CX,
        Register::R8,
        Register::R9,
    ];
    fn gen_fun(
        t::Function {
            ident,
            params,
            instrs: t_instrs,
        }: t::Function,
    ) -> Function {
        /*
        When the next instruction is executed, the stack has been arrangd to look like:
            |      ...
            | 24(%rbp) : Arg #8
            | 16(%rbp) : Arg #7
            |  8(%rbp) : The RIP at which to resume, when the current function returns.
            |  0(%rbp) : The previous stack frame's RBP.

        I believe it's always redundant to copy args[6..], which are in the previous stack frame, to pseudoregisters, which are in the current stack frame. Presumably this can be optimized away in the future.
        */
        let mut extra_arg_stack_pos = StackPosition(8);
        let asm_instrs_copy_args = params.into_iter().enumerate().map(|(i, param_ident)| {
            let src = match Self::ARG_REGS.get(i) {
                Some(reg) => PreFinalOperand::Register(*reg),
                None => {
                    *extra_arg_stack_pos += 8;
                    PreFinalOperand::StackPosition(extra_arg_stack_pos)
                }
            };
            let dst = PreFinalOperand::from(param_ident);
            Instruction::Mov { src, dst }
        });

        let asm_instrs_body = Self::gen_instructions(t_instrs);

        let asm_instrs = asm_instrs_copy_args.chain(asm_instrs_body);

        let mut fin = InstrsFinalizer::default();
        let asm_instrs = fin.finalize_instrs(asm_instrs);

        Function {
            ident,
            instrs: asm_instrs,
        }
    }
    fn gen_funcall_instrs(
        t::FunCall { ident, args, dst }: t::FunCall,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let mut asm_instrs = vec![];

        let reg_args_count = cmp::min(Self::ARG_REGS.len(), args.len());
        let stack_args_count = args.len() - reg_args_count;

        /*
        RBP ->  | 0x???0 -  The previous stack frame's RBP
                | 0x???1 \
                |    ...  |
                |    ...  |
        RSP@1-> | 0x???0 -+ Current function's local variables; and maybe padding
                | 0x???8 \
                |    ...  |
                |    ...  |
        RSP@2-> | 0x???0 -+ Maybe padding; and args to the callee

        When the next instruction is executed, RSP is 16-byte aligned (at "RSP@1"). The instruction finalizer will have ensured this.

        To prepare for a function-call, the next instructions must save necessary caller-saved registers into the stack.
        But, all our usages elsewhere of these registers entail immediately returning them or saving them to a memory location. Eg we always copy function args. Eg we always copy outputs of operations to the memory.
        Hence, we omit saving these registers here.

        The next instructions will push 0+ args into the stack, while also ensuring that after the pushing, RSP will be 16-byte aligned again (at "RSP@2").
        RSP is always 8-byte aligned. To ensure 16-byte alignment, we pad 0-or-1 unit of 8 bytes.

        As a consequence, when the `call` instruction will be executed, RSP is guaranteed to be 16-byte aligned.
        */
        let stack_padding_bytelen = if (stack_args_count & 1) == 1 { 8 } else { 0 };

        if stack_padding_bytelen != 0 {
            asm_instrs.push(Instruction::AllocateStack(StackPosition(
                stack_padding_bytelen,
            )));
        }

        /* Note, we must push args[6..] to the stack in the reverse order. */
        for (i, arg_val) in args.into_iter().enumerate().rev() {
            let arg_operand = Self::convert_val_operand(arg_val);
            match Self::ARG_REGS.get(i) {
                None => match &arg_operand {
                    PreFinalOperand::ImmediateValue(_) | PreFinalOperand::Register(_) => {
                        asm_instrs.push(Instruction::Push(arg_operand));
                    }
                    PreFinalOperand::PseudoRegister(_) | PreFinalOperand::StackPosition(_) => {
                        /* `pushq` operation always reads and pushes 8 bytes.
                        In case (arg's memory address + (8-1)) lies outside the readable memory, we cannot `pushq` directly. */
                        asm_instrs.push(Instruction::Mov {
                            src: arg_operand,
                            dst: Register::AX.into(),
                        });
                        asm_instrs.push(Instruction::Push(Register::AX.into()));
                    }
                },
                Some(reg) => {
                    asm_instrs.push(Instruction::Mov {
                        src: arg_operand,
                        dst: (*reg).into(),
                    });
                }
            }
        }

        asm_instrs.push(Instruction::Call(ident));

        /* Each arg is pushed into the stack as an 8-byte item, b/c the `push` operation reads-and-pushes an 8-byte operand. */
        let stack_pop_bytelen = 8 * (stack_args_count as isize) + stack_padding_bytelen;
        if stack_pop_bytelen != 0 {
            asm_instrs.push(Instruction::DeallocateStack(StackPosition(
                stack_pop_bytelen,
            )));
        }

        let dst = Self::convert_var_operand(dst);
        asm_instrs.push(Instruction::Mov {
            src: Register::AX.into(),
            dst,
        });

        asm_instrs
    }

    /* Tacky Instruction */

    fn gen_instructions(
        t_instrs: Vec<t::Instruction>,
    ) -> impl Iterator<Item = Instruction<PreFinalOperand>> {
        t_instrs.into_iter().flat_map(|t_instr| match t_instr {
            t::Instruction::Return(t_val) => Self::gen_return_instrs(t_val),
            t::Instruction::Unary(unary) => Self::gen_unary_instrs(unary),
            t::Instruction::Binary(binary) => Self::gen_binary_instrs(binary),
            t::Instruction::Copy(copy) => Self::gen_copy_instrs(copy),
            t::Instruction::Jump(lbl) => vec![Instruction::Jmp(lbl)],
            t::Instruction::JumpIfZero(jumpif) => Self::gen_jumpif_instrs(ConditionCode::E, jumpif),
            t::Instruction::JumpIfNotZero(jumpif) => {
                Self::gen_jumpif_instrs(ConditionCode::Ne, jumpif)
            }
            t::Instruction::Label(lbl) => vec![Instruction::Label(lbl)],
            t::Instruction::FunCall(fun_call) => Self::gen_funcall_instrs(fun_call),
        })
    }

    /* Tacky Return */

    fn gen_return_instrs(t_val: t::ReadableValue) -> Vec<Instruction<PreFinalOperand>> {
        let asm_src = Self::convert_val_operand(t_val);
        let asm_instr_1 = Instruction::Mov {
            src: asm_src,
            dst: Register::AX.into(),
        };

        let asm_instr_2 = Instruction::Ret;

        vec![asm_instr_1, asm_instr_2]
    }

    /* Tacky Unary */

    fn gen_unary_instrs(t_unary: t::Unary) -> Vec<Instruction<PreFinalOperand>> {
        use t::UnaryOperator as TUO;

        match t_unary.op {
            TUO::Complement => {
                Self::gen_unary_inplace_instrs(UnaryOperator::BitwiseComplement, t_unary)
            }
            TUO::Negate => Self::gen_unary_inplace_instrs(UnaryOperator::TwosComplement, t_unary),
            TUO::Not => Self::gen_unary_comparison_instrs(t_unary),
        }
    }
    fn gen_unary_inplace_instrs(
        asm_op: UnaryOperator,
        t::Unary { op: _, src, dst }: t::Unary,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let asm_src = Self::convert_val_operand(src);
        let asm_dst = Self::convert_var_operand(dst);

        let asm_instr_1 = Instruction::Mov {
            src: asm_src,
            dst: asm_dst.clone(),
        };
        let asm_instr_2 = Instruction::Unary(asm_op, asm_dst);
        vec![asm_instr_1, asm_instr_2]
    }
    fn gen_unary_comparison_instrs(
        t::Unary { op: _, src, dst }: t::Unary,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let asm_src = Self::convert_val_operand(src);
        let asm_dst = Self::convert_var_operand(dst);

        Self::gen_comparison_instrs_from_asm(
            ConditionCode::E,
            asm_src,
            PreFinalOperand::ImmediateValue(0),
            asm_dst,
        )
    }

    /* Tacky Binary */

    fn gen_binary_instrs(t_binary: t::Binary) -> Vec<Instruction<PreFinalOperand>> {
        use t::BinaryOperator as TBO;

        match t_binary.op {
            TBO::Add => Self::gen_arithmetic_instrs(BinaryOperator::Add, t_binary),
            TBO::Sub => Self::gen_arithmetic_instrs(BinaryOperator::Sub, t_binary),
            TBO::Mul => Self::gen_arithmetic_instrs(BinaryOperator::Mul, t_binary),

            TBO::Div => Self::gen_divrem_instrs(Register::AX, t_binary),
            TBO::Rem => Self::gen_divrem_instrs(Register::DX, t_binary),

            TBO::Eq => Self::gen_comparison_instrs(ConditionCode::E, t_binary),
            TBO::Neq => Self::gen_comparison_instrs(ConditionCode::Ne, t_binary),
            TBO::Lt => Self::gen_comparison_instrs(ConditionCode::L, t_binary),
            TBO::Lte => Self::gen_comparison_instrs(ConditionCode::Le, t_binary),
            TBO::Gt => Self::gen_comparison_instrs(ConditionCode::G, t_binary),
            TBO::Gte => Self::gen_comparison_instrs(ConditionCode::Ge, t_binary),
        }
    }
    fn gen_arithmetic_instrs(
        asm_op: BinaryOperator,
        t::Binary {
            op: _,
            src1,
            src2,
            dst,
        }: t::Binary,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let asm_src1 = Self::convert_val_operand(src1);
        let asm_src2 = Self::convert_val_operand(src2);
        let asm_dst = Self::convert_var_operand(dst);

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
        ans_reg: Register,
        t::Binary {
            op: _,
            src1,
            src2,
            dst,
        }: t::Binary,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let asm_src1 = Self::convert_val_operand(src1);
        let asm_src2 = Self::convert_val_operand(src2);
        let asm_dst = Self::convert_var_operand(dst);

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
    fn gen_comparison_instrs(
        cmp_0_cc: ConditionCode,
        t::Binary {
            op: _,
            src1,
            src2,
            dst,
        }: t::Binary,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let asm_src1 = Self::convert_val_operand(src1);
        let asm_src2 = Self::convert_val_operand(src2);
        let asm_dst = Self::convert_var_operand(dst);

        Self::gen_comparison_instrs_from_asm(cmp_0_cc, asm_src1, asm_src2, asm_dst)
    }
    fn gen_comparison_instrs_from_asm(
        cmp_0_cc: ConditionCode,
        asm_src1: PreFinalOperand,
        asm_src2: PreFinalOperand,
        asm_dst: PreFinalOperand,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let asm_instr_1 = Instruction::Cmp {
            tgt: asm_src1,
            arg: asm_src2,
        };
        let asm_instr_2 = Instruction::Mov {
            src: PreFinalOperand::ImmediateValue(0),
            dst: asm_dst.clone(),
        };
        let asm_instr_3 = Instruction::SetCC(cmp_0_cc, asm_dst);
        vec![asm_instr_1, asm_instr_2, asm_instr_3]
    }

    /* Tacky Copy */

    fn gen_copy_instrs(t_copy: t::Copy) -> Vec<Instruction<PreFinalOperand>> {
        let src = Self::convert_val_operand(t_copy.src);
        let dst = Self::convert_var_operand(t_copy.dst);
        vec![Instruction::Mov { src, dst }]
    }

    /* Tacky Jump */

    fn gen_jumpif_instrs(
        cmp_0_cc: ConditionCode,
        t::JumpIf { condition, tgt }: t::JumpIf,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let condition = Self::convert_val_operand(condition);
        let asm_instr_1 = Instruction::Cmp {
            tgt: condition,
            arg: PreFinalOperand::ImmediateValue(0),
        };
        let asm_instr_2 = Instruction::JmpCC(cmp_0_cc, tgt);
        vec![asm_instr_1, asm_instr_2]
    }

    /* Tacky Value -> Asm Operand */

    fn convert_var_operand(ident: Rc<ResolvedIdentifier>) -> PreFinalOperand {
        Self::convert_val_operand(t::ReadableValue::Variable(ident))
    }
    fn convert_val_operand(t_val: t::ReadableValue) -> PreFinalOperand {
        match t_val {
            t::ReadableValue::Constant(i) => PreFinalOperand::ImmediateValue(i),
            t::ReadableValue::Variable(v) => PreFinalOperand::PseudoRegister(v),
        }
    }
}
