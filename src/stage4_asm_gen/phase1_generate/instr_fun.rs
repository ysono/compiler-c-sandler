use super::{GeneratedAsmAst, InstrsGenerator};
use crate::{
    common::{
        identifier::SymbolIdentifier,
        types_backend::{AssemblyType, OperandByteLen},
    },
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
        let mut extra_arg_stack_pos = MemoryAddressOffset(8);
        let mut asm_instrs = param_idents
            .into_iter()
            .map(|param_ident| {
                let (dst, _, asm_type) = self.value_to_operand_and_type(param_ident);

                let arg_reg = arg_reg_resolver.next_reg(asm_type);

                let src = match arg_reg {
                    Some(reg) => Operand::Register(reg).into(),
                    None => {
                        *extra_arg_stack_pos += 8;
                        Operand::MemoryAddress(Register::BP, extra_arg_stack_pos).into()
                    }
                };

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
        let mut asm_instrs = vec![];

        /* Push arg-copying instructions in the _reverse_ order. */
        let mut arg_reg_resolver = ArgRegResolver::default();
        let mut stack_args_count = 0;
        for arg_val in args {
            let (arg_operand, _, arg_asm_type) = self.value_to_operand_and_type(arg_val);

            let arg_reg = arg_reg_resolver.next_reg(arg_asm_type);

            match arg_reg {
                Some(reg) => {
                    asm_instrs.push(Instruction::Mov {
                        asm_type: arg_asm_type,
                        src: arg_operand,
                        dst: Operand::Register(reg).into(),
                    });
                }
                None => {
                    stack_args_count += 1;

                    match (arg_operand.is_on_mem(), OperandByteLen::from(arg_asm_type)) {
                        (false, _) | (true, OperandByteLen::B8) => {
                            asm_instrs.push(Instruction::Push(arg_operand));
                        }
                        (true, OperandByteLen::B4 | OperandByteLen::B1) => {
                            /* `pushq` operation always reads and pushes 8 bytes.
                            In case (arg's memory address + (8-1)) lies outside the readable memory, we cannot `pushq` directly. */
                            let reg = || Operand::Register(Register::AX).into();
                            asm_instrs.extend(
                                [
                                    Instruction::Mov {
                                        asm_type: arg_asm_type,
                                        src: arg_operand,
                                        dst: reg(),
                                    },
                                    Instruction::Push(reg()),
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
                tgt: Operand::Register(Register::SP).into(),
                arg: Operand::ImmediateValue(stack_padding_bytelen).into(),
            });
        }

        /* Finalize the order of the padding + arg-copying instructions. */
        asm_instrs.reverse();

        /* Push the `call` instruction. */
        asm_instrs.push(Instruction::Call(ident));

        /* Push the instruction that pops the padding + args. */
        let stack_pop_bytelen = 8 * (stack_args_count as i64) + stack_padding_bytelen;
        if stack_pop_bytelen != 0 {
            asm_instrs.push(Instruction::Binary {
                op: BinaryOperator::Add,
                asm_type: AssemblyType::Quadword,
                tgt: Operand::Register(Register::SP).into(),
                arg: Operand::ImmediateValue(stack_pop_bytelen).into(),
            });
        }

        /* Push the instruction that `mov`s the return value. */
        let (dst, _, dst_asm_type) = self.value_to_operand_and_type(dst);
        let ret_reg = derive_return_register(dst_asm_type);
        asm_instrs.push(Instruction::Mov {
            asm_type: dst_asm_type,
            src: Operand::Register(ret_reg).into(),
            dst,
        });

        asm_instrs
    }

    pub(super) fn gen_return_instrs(
        &mut self,
        t_val: t::Value,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, asm_type) = self.value_to_operand_and_type(t_val);
        let ret_reg = derive_return_register(asm_type);
        let asm_instr_1 = Instruction::Mov {
            asm_type,
            src,
            dst: Operand::Register(ret_reg).into(),
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
    int_args_count: usize,
    float_args_count: usize,
}
#[allow(clippy::derivable_impls)]
impl Default for ArgRegResolver {
    fn default() -> Self {
        Self {
            int_args_count: 0,
            float_args_count: 0,
        }
    }
}
impl ArgRegResolver {
    fn next_reg(&mut self, asm_type: AssemblyType) -> Option<Register> {
        match asm_type {
            AssemblyType::Longword | AssemblyType::Quadword => {
                let ret = INT_ARG_REGS.get(self.int_args_count).cloned();
                self.int_args_count += 1;
                ret
            }
            AssemblyType::Double => {
                let ret = FLOAT_ARG_REGS.get(self.float_args_count).cloned();
                self.float_args_count += 1;
                ret
            }
        }
    }
}

fn derive_return_register(asm_type: AssemblyType) -> Register {
    match asm_type {
        AssemblyType::Longword | AssemblyType::Quadword => Register::AX,
        AssemblyType::Double => Register::XMM0,
    }
}
