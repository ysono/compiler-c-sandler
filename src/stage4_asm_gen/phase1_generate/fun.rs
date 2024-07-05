use crate::{
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::{
        asm_ast::*,
        phase1_generate::{GeneratedAsmAst, InstrsGenerator},
    },
    types_backend::AssemblyType,
};
use std::cmp;

impl<'slf> InstrsGenerator<'slf> {
    const ARG_REGS: [Register; 6] = [
        Register::DI,
        Register::SI,
        Register::DX,
        Register::CX,
        Register::R8,
        Register::R9,
    ];
    /// See documentation at [`crate::stage4_asm_gen`].
    pub fn convert_fun(
        &self,
        t::Function {
            ident,
            visibility,
            param_idents,
            instrs: t_instrs,
        }: t::Function,
    ) -> Function<GeneratedAsmAst> {
        let mut extra_arg_stack_pos = StackPosition(8);
        let mut asm_instrs = param_idents
            .into_iter()
            .enumerate()
            .map(|(i, param_ident)| {
                let src = match Self::ARG_REGS.get(i) {
                    Some(reg) => PreFinalOperand::Register(*reg),
                    None => {
                        *extra_arg_stack_pos += 8;
                        PreFinalOperand::StackPosition(extra_arg_stack_pos)
                    }
                };
                let (dst, _, asm_type) = self.convert_value(param_ident);
                Instruction::Mov { asm_type, src, dst }
            })
            .collect::<Vec<_>>();

        asm_instrs.extend(self.gen_instructions(t_instrs));

        Function {
            ident,
            visibility,
            instrs: asm_instrs,
        }
    }
    /// See documentation at [`crate::stage4_asm_gen`].
    pub(super) fn gen_funcall_instrs(
        &self,
        t::FunCall { ident, args, dst }: t::FunCall,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let mut asm_instrs = vec![];

        let reg_args_count = cmp::min(Self::ARG_REGS.len(), args.len());
        let stack_args_count = args.len() - reg_args_count;

        let stack_padding_bytelen = if (stack_args_count & 1) == 1 { 8 } else { 0 };

        if stack_padding_bytelen != 0 {
            asm_instrs.push(Instruction::Binary {
                op: BinaryOperator::Sub,
                asm_type: AssemblyType::Quadword,
                tgt: Register::SP.into(),
                arg: PreFinalOperand::ImmediateValue(stack_padding_bytelen),
            });
        }

        for (i, arg_val) in args.into_iter().enumerate().rev() {
            let (arg_operand, _, arg_asm_type) = self.convert_value(arg_val);
            match Self::ARG_REGS.get(i) {
                None => match (&arg_operand, arg_asm_type) {
                    (PreFinalOperand::ImmediateValue(_) | PreFinalOperand::Register(_), _)
                    | (_, AssemblyType::Quadword) => {
                        asm_instrs.push(Instruction::Push(arg_operand));
                    }
                    (
                        PreFinalOperand::Pseudo(_) | PreFinalOperand::StackPosition(_),
                        AssemblyType::Longword,
                    ) => {
                        /* `pushq` operation always reads and pushes 8 bytes.
                        In case (arg's memory address + (8-1)) lies outside the readable memory, we cannot `pushq` directly. */
                        asm_instrs.push(Instruction::Mov {
                            asm_type: AssemblyType::Longword,
                            src: arg_operand,
                            dst: Register::AX.into(),
                        });
                        asm_instrs.push(Instruction::Push(Register::AX.into()));
                    }
                },
                Some(reg) => {
                    asm_instrs.push(Instruction::Mov {
                        asm_type: arg_asm_type,
                        src: arg_operand,
                        dst: (*reg).into(),
                    });
                }
            }
        }

        asm_instrs.push(Instruction::Call(ident));

        let stack_pop_bytelen = 8 * (stack_args_count as u64) + stack_padding_bytelen;
        if stack_pop_bytelen != 0 {
            asm_instrs.push(Instruction::Binary {
                op: BinaryOperator::Add,
                asm_type: AssemblyType::Quadword,
                tgt: Register::SP.into(),
                arg: PreFinalOperand::ImmediateValue(stack_pop_bytelen),
            });
        }

        let (dst, _, dst_type) = self.convert_value(dst);
        asm_instrs.push(Instruction::Mov {
            asm_type: dst_type,
            src: Register::AX.into(),
            dst,
        });

        asm_instrs
    }
}
