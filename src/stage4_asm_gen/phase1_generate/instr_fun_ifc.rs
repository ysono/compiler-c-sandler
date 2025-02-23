use super::{GeneratedAsmAst, InstrsGenerator};
use crate::{
    common::{
        identifier::SymbolIdentifier,
        types_backend::{OperandByteLen, ScalarAssemblyType},
        types_frontend::ScalarFunType,
    },
    ds_n_a::singleton::Singleton,
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::asm_ast::*,
};
use std::rc::Rc;

impl InstrsGenerator {
    /// See documentation at [`crate::stage4_asm_gen`].
    pub(super) fn gen_fun_copy_incoming_args(
        &mut self,
        fun_typ: Singleton<ScalarFunType>,
        param_idents: Vec<Rc<SymbolIdentifier>>,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let mut arg_reg_resolver = ArgRegResolver::default();
        let mut last_onstack_arg_pos = MemoryOffset::new(8);
        let params = fun_typ.params.iter().zip(param_idents);
        params
            .map(|(param_type, param_ident)| {
                let asm_type = ScalarAssemblyType::from(param_type.as_ref());
                let arg_reg = arg_reg_resolver.next_reg(asm_type);

                let dst = self.object_to_operand(param_ident);

                let src = match arg_reg {
                    Some(reg) => Operand::Register(reg).into(),
                    None => {
                        *last_onstack_arg_pos.as_mut() += 8;
                        Operand::Memory(Register::BP, last_onstack_arg_pos).into()
                    }
                };

                Instruction::Mov { asm_type, src, dst }
            })
            .collect()
    }

    /// See documentation at [`crate::stage4_asm_gen`].
    pub(super) fn gen_funcall(
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
                    let reg_operand = || Operand::Register(reg).into();

                    let instr1 = Some(Instruction::Mov {
                        asm_type: arg_asm_type,
                        src: arg_operand,
                        dst: reg_operand(),
                    });
                    let instr2 = Self::get_instr_widen_arg(arg_asm_type, reg_operand());

                    asm_instrs.extend([instr2, instr1].into_iter().flatten());
                }
                None => {
                    stack_args_count += 1;

                    let (instr1, instr2);
                    match (arg_operand.is_on_mem(), OperandByteLen::from(arg_asm_type)) {
                        (false, _) | (true, OperandByteLen::B8) => {
                            instr1 = Some(Instruction::Push(arg_operand));
                            instr2 = None;
                        }
                        (true, OperandByteLen::B4 | OperandByteLen::B1) => {
                            /* `pushq` operation always reads and pushes 8 bytes.
                            In case (arg's memory address + (8-1)) lies outside the readable memory, we cannot `pushq` directly. */
                            let reg_operand = || Operand::Register(Register::AX).into();

                            instr1 = Some(Instruction::Mov {
                                asm_type: arg_asm_type,
                                src: arg_operand,
                                dst: reg_operand(),
                            });
                            instr2 = Some(Instruction::Push(reg_operand()));
                        }
                    }

                    let instr3 = Self::get_instr_widen_arg(
                        arg_asm_type,
                        Operand::Memory(Register::SP, MemoryOffset::new(0)).into(),
                    );

                    asm_instrs.extend([instr3, instr2, instr1].into_iter().flatten());
                }
            }
        }

        /* Evaluate the need for padding. */
        let stack_padding_bytelen = if (stack_args_count & 1) == 1 { 8 } else { 0 };

        /* Potentially push the padding instruction. */
        if stack_padding_bytelen != 0 {
            asm_instrs.push(Instruction::Binary {
                op: BinaryOperator::Sub,
                asm_type: ScalarAssemblyType::Quadword,
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
                asm_type: ScalarAssemblyType::Quadword,
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

    /// Each argument that is narrower than 4 bytes (henceforth "narrow arg") is handled in subtly different ways by different compilers.
    /// Clang produces code such that
    ///     + Caller sign-extends each outgoing narrow arg to 4 bytes.
    ///     + Callee assumes that each incoming narrow arg has already been sign-extended by the caller; and,
    ///         given some optimizations are enabled,
    ///         when the narrow-typed-but-actually-widened value is semantically cast to a wider type,
    ///         does _not_ sign-extend the value.
    /// Gcc produces code such that
    ///     + Caller sign-extends each outgoing narrow arg to 4 bytes.
    ///     + Callee sign-extends each incoming narrow arg to 4 bytes (I'm not sure at what timing).
    /// Our compiler produces code such that
    ///     + Caller sign-extends each outgoing narrow arg to 4 bytes. -- Emitted by this function.
    ///     + Callee widens or narrows any value upon casting (whether explicit or implicit). -- Emitted during the Tacky stage.
    ///
    /// See book ch 16, section "Assembly Generation", block "Clang goes rogue", page 444.
    fn get_instr_widen_arg(
        src_asm_type: ScalarAssemblyType,
        operand: PreFinalOperand,
    ) -> Option<Instruction<GeneratedAsmAst>> {
        if src_asm_type == ScalarAssemblyType::Byte {
            Some(Instruction::Movsx {
                src_asm_type,
                dst_asm_type: ScalarAssemblyType::Longword,
                src: operand.clone(),
                dst: operand,
            })
        } else {
            None
        }
    }

    pub(super) fn gen_return(&mut self, t_val: t::Value) -> Vec<Instruction<GeneratedAsmAst>> {
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
    fn next_reg(&mut self, asm_type: ScalarAssemblyType) -> Option<Register> {
        match asm_type {
            ScalarAssemblyType::Byte
            | ScalarAssemblyType::Longword
            | ScalarAssemblyType::Quadword => {
                let ret = INT_ARG_REGS.get(self.int_args_count).cloned();
                self.int_args_count += 1;
                ret
            }
            ScalarAssemblyType::Double => {
                let ret = FLOAT_ARG_REGS.get(self.float_args_count).cloned();
                self.float_args_count += 1;
                ret
            }
        }
    }
}

fn derive_return_register(asm_type: ScalarAssemblyType) -> Register {
    match asm_type {
        ScalarAssemblyType::Byte | ScalarAssemblyType::Longword | ScalarAssemblyType::Quadword => {
            Register::AX
        }
        ScalarAssemblyType::Double => Register::XMM0,
    }
}
