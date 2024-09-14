use super::{GeneratedAsmAst, InstrsGenerator};
use crate::{
    common::{identifier::SymbolIdentifier, types_backend::AssemblyType, types_frontend::VarType},
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::asm_ast::*,
};
use std::rc::Rc;

impl InstrsGenerator {
    /// See documentation at [`crate::stage4_asm_gen`].
    pub(super) fn gen_fun_instrs(
        &mut self,
        param_idents: Vec<Rc<SymbolIdentifier>>,
        t_instrs: Vec<t::Instruction>,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        /* Instructions that copy incoming args into the current function's stack frame. */
        let mut arg_reg_resolver = ArgRegResolver::default();
        let mut extra_arg_stack_pos = StackPosition(8);
        let mut asm_instrs = param_idents
            .into_iter()
            .map(|param_ident| {
                let arg_type = self.frontend_symtab.get_var_type(&param_ident).unwrap();

                let arg_reg = arg_reg_resolver.next_reg(arg_type);

                let src = match arg_reg {
                    Some(reg) => PreFinalOperand::Register(reg),
                    None => {
                        *extra_arg_stack_pos += 8;
                        PreFinalOperand::StackPosition(extra_arg_stack_pos)
                    }
                };
                let (dst, _, asm_type) = self.convert_value(param_ident);
                Instruction::Mov { asm_type, src, dst }
            })
            .collect::<Vec<_>>();

        /* The function body's instructions. */
        asm_instrs.extend(self.gen_instructions(t_instrs));

        asm_instrs
    }

    /// See documentation at [`crate::stage4_asm_gen`].
    pub(super) fn gen_funcall_instrs(
        &mut self,
        t::FunCall { ident, args, dst }: t::FunCall,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        use AssemblyType as AT;
        use PreFinalOperand as PFO;

        let mut asm_instrs = vec![];

        /* Push arg-copying instructions in the _reverse_ order. */
        let mut arg_reg_resolver = ArgRegResolver::default();
        let mut stack_args_count = 0;
        for arg_val in args {
            let arg_var_type = match &arg_val {
                t::ReadableValue::Constant(konst) => konst.var_type(),
                t::ReadableValue::Variable(ident) => {
                    self.frontend_symtab.get_var_type(ident).unwrap()
                }
            };

            let arg_reg = arg_reg_resolver.next_reg(arg_var_type);

            let (arg_operand, _, arg_asm_type) = self.convert_value(arg_val);

            match arg_reg {
                Some(reg) => {
                    asm_instrs.push(Instruction::Mov {
                        asm_type: arg_asm_type,
                        src: arg_operand,
                        dst: PreFinalOperand::Register(reg),
                    });
                }
                None => {
                    stack_args_count += 1;

                    match (&arg_operand, arg_asm_type) {
                        (PFO::ImmediateValue(_) | PFO::Register(_), _)
                        | (_, AT::Quadword | AT::Double) => {
                            asm_instrs.push(Instruction::Push(arg_operand));
                        }
                        (PFO::Pseudo(_) | PFO::StackPosition(_) | PFO::Data(_), AT::Longword) => {
                            /* `pushq` operation always reads and pushes 8 bytes.
                            In case (arg's memory address + (8-1)) lies outside the readable memory, we cannot `pushq` directly. */
                            asm_instrs.extend(
                                [
                                    Instruction::Mov {
                                        asm_type: arg_asm_type,
                                        src: arg_operand,
                                        dst: PreFinalOperand::Register(Register::AX),
                                    },
                                    Instruction::Push(PreFinalOperand::Register(Register::AX)),
                                ]
                                .into_iter()
                                .rev(),
                            );
                        }
                    }
                }
            }
        }

        /* Evaluate the need for padding. */
        let stack_padding_bytelen = if (stack_args_count & 1) == 1 { 8 } else { 0 };

        /* Potentially push the padding instruction. */
        if stack_padding_bytelen != 0 {
            asm_instrs.push(Instruction::Binary {
                op: BinaryOperator::Sub,
                asm_type: AssemblyType::Quadword,
                tgt: PreFinalOperand::Register(Register::SP),
                arg: PreFinalOperand::ImmediateValue(stack_padding_bytelen),
            });
        }

        /* Finalize the order of the padding + arg-copying instructions. */
        asm_instrs.reverse();

        /* Push the `call` instruction. */
        asm_instrs.push(Instruction::Call(ident));

        /* Push the instruction that pops the padding + args. */
        let stack_pop_bytelen = 8 * (stack_args_count as u64) + stack_padding_bytelen;
        if stack_pop_bytelen != 0 {
            asm_instrs.push(Instruction::Binary {
                op: BinaryOperator::Add,
                asm_type: AssemblyType::Quadword,
                tgt: PreFinalOperand::Register(Register::SP),
                arg: PreFinalOperand::ImmediateValue(stack_pop_bytelen),
            });
        }

        /* Push the instruction that `mov`s the return value. */
        let (dst, _, dst_asm_type) = self.convert_value(dst);
        let ret_reg = match dst_asm_type {
            AssemblyType::Longword | AssemblyType::Quadword => Register::AX,
            AssemblyType::Double => Register::XMM0,
        };
        asm_instrs.push(Instruction::Mov {
            asm_type: dst_asm_type,
            src: PreFinalOperand::Register(ret_reg),
            dst,
        });

        asm_instrs
    }

    pub(super) fn gen_return_instrs(
        &mut self,
        t_val: t::ReadableValue,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, asm_type) = self.convert_value(t_val);
        let dst_reg = match asm_type {
            AssemblyType::Longword | AssemblyType::Quadword => Register::AX,
            AssemblyType::Double => Register::XMM0,
        };
        let asm_instr_1 = Instruction::Mov {
            asm_type,
            src,
            dst: PreFinalOperand::Register(dst_reg),
        };

        let asm_instr_2 = Instruction::Ret;

        vec![asm_instr_1, asm_instr_2]
    }
}

const INT_ARG_REGS: [Register; 6] = [
    Register::DI,
    Register::SI,
    Register::DX,
    Register::CX,
    Register::R8,
    Register::R9,
];
const FLOAT_ARG_REGS: [Register; 8] = [
    Register::XMM0,
    Register::XMM1,
    Register::XMM2,
    Register::XMM3,
    Register::XMM4,
    Register::XMM5,
    Register::XMM6,
    Register::XMM7,
];

struct ArgRegResolver {
    int_regs_count: usize,
    float_regs_count: usize,
}
#[allow(clippy::derivable_impls)]
impl Default for ArgRegResolver {
    fn default() -> Self {
        Self {
            int_regs_count: 0,
            float_regs_count: 0,
        }
    }
}
impl ArgRegResolver {
    fn next_reg(&mut self, arg_type: VarType) -> Option<Register> {
        match arg_type {
            VarType::Int | VarType::Long | VarType::UInt | VarType::ULong => {
                let ret = INT_ARG_REGS.get(self.int_regs_count).cloned();
                self.int_regs_count += 1;
                ret
            }
            VarType::Double => {
                let ret = FLOAT_ARG_REGS.get(self.float_regs_count).cloned();
                self.float_regs_count += 1;
                ret
            }
        }
    }
}
