use crate::{
    stage4_asm_gen::{asm_ast::*, phase2_finalize::FinalizedAsmAst},
    types_backend::AssemblyType,
};

pub struct OperandFixer {}
impl OperandFixer {
    pub fn fix_invalid_operands<'a>(
        in_instrs: impl 'a + Iterator<Item = Instruction<FinalizedAsmAst>>,
    ) -> impl 'a + Iterator<Item = Instruction<FinalizedAsmAst>> {
        in_instrs.flat_map(|in_instr| match in_instr {
            Instruction::Mov { asm_type, mut src, dst } => {
                if let (AssemblyType::Longword, Operand::ImmediateValue(i)) = (asm_type, &src) {
                    src = Operand::ImmediateValue(*i as u32 as u64); // Zero-out most significant bytes.
                }

                let src_to_reg1 =
                    (src.is_on_mem() && dst.is_on_mem())
                    || (matches!(asm_type, AssemblyType::Quadword)
                        && matches!(&src, Operand::ImmediateValue(i) if i32::try_from(*i).is_err())
                        && dst.is_on_mem());

                let new_instr = |src: Operand, dst: Operand| Instruction::Mov { asm_type, src, dst };

                Self::maybe_use_2_regs(
                    (asm_type, src),
                    (asm_type, dst),
                    src_to_reg1,
                    (false, false),
                    new_instr,
                )
            }
            Instruction::Movsx { src, dst } => {
                let src_to_reg1 = matches!(&src, Operand::ImmediateValue(_));
                let reg2_to_dst = dst.is_on_mem();
                let new_instr = |src: Operand, dst: Operand| Instruction::Movsx { src, dst };
                Self::maybe_use_2_regs(
                    (AssemblyType::Longword, src),
                    (AssemblyType::Quadword, dst),
                    src_to_reg1,
                    (false, reg2_to_dst),
                    new_instr,
                )
            }
            Instruction::MovZeroExtend { src, dst } => {
                if matches!(&dst, Operand::Register(_)) {
                    vec![Instruction::Mov{
                        asm_type: AssemblyType::Longword,
                        src,
                        dst
                    }]
                } else { /* Then, the operand is on memory. */
                    let reg = Register::R11;
                    let instr_at_reg = Instruction::Mov{
                        asm_type: AssemblyType::Quadword,
                        src: reg.into(),
                        dst,
                    };
                    Self::to_reg(AssemblyType::Longword, src, reg, instr_at_reg)
                }
            }
            Instruction::Cvttsd2si { dst_asm_type, src, dst } => {
                let reg2_to_dst = dst.is_on_mem();
                let new_instr = |src: Operand, dst: Operand| Instruction::Cvttsd2si { dst_asm_type, src, dst };
                Self::maybe_use_2_regs(
                    (AssemblyType::Double, src),
                    (dst_asm_type, dst),
                    false,
                    (false, reg2_to_dst),
                    new_instr
                )
            }
            Instruction::Cvtsi2sd { src_asm_type, src, dst } => {
                let src_to_reg1 = matches!(&src, Operand::ImmediateValue(_));
                let reg2_to_dst = matches!(&dst, Operand::Register(_)) == false;
                let new_instr =|src: Operand, dst: Operand| Instruction::Cvtsi2sd { src_asm_type, src, dst };
                Self::maybe_use_2_regs(
                    (src_asm_type, src),
                    (AssemblyType::Double, dst),
                    src_to_reg1,
                    (false, reg2_to_dst),
                    new_instr
                )
            }
            Instruction::Binary { op, asm_type, arg, tgt } => {
                use BinaryOperator as BO;

                let (src_to_reg1, dst_to_and_from_reg2);
                match asm_type {
                    AssemblyType::Longword|AssemblyType::Quadword => {
                        src_to_reg1 = (matches!(&op, BO::Add | BO::Sub | BO::And | BO::Or)
                                && arg.is_on_mem()
                                && tgt.is_on_mem())
                            || (matches!(&asm_type, AssemblyType::Quadword)
                                && matches!(&arg, Operand::ImmediateValue(i) if i32::try_from(*i).is_err()));
                        dst_to_and_from_reg2 = matches!(&op, BinaryOperator::Mul) && tgt.is_on_mem();
                    }
                    AssemblyType::Double => {
                        /* The xorpd instruction requires either a register or a 16-byte-aligned memory address as its source operand,
                        but we don’t need a rewrite rule for this
                        since all the xorpd instructions we generate already satisfy this requirement. */
                        src_to_reg1 = false;
                        dst_to_and_from_reg2 = matches!(&tgt, Operand::Register(_)) == false;
                    }
                }
                let new_instr = |arg: Operand, tgt: Operand| Instruction::Binary { op, asm_type, arg, tgt };
                Self::maybe_use_2_regs(
                    (asm_type, arg),
                    (asm_type,tgt),
                    src_to_reg1,
                    (dst_to_and_from_reg2, dst_to_and_from_reg2),
                    new_instr,
                )
            }
            Instruction::Cmp { asm_type, arg, tgt } => {
                let (src_to_reg1, dst_to_reg2);
                match asm_type {
                    AssemblyType::Longword|AssemblyType::Quadword => {
                        src_to_reg1 =
                            (arg.is_on_mem() && tgt.is_on_mem())
                            || (matches!(&arg, Operand::ImmediateValue(i) if i32::try_from(*i).is_err()));
                        dst_to_reg2 = matches!(&tgt, Operand::ImmediateValue(_));
                    }
                    AssemblyType::Double => {
                        src_to_reg1 = false;
                        dst_to_reg2 = matches!(&tgt, Operand::Register(_)) == false;
                    }
                }
                let new_instr = |arg: Operand, tgt: Operand| Instruction::Cmp { asm_type, arg, tgt };
                Self::maybe_use_2_regs(
                    (asm_type, arg),
                    (asm_type,tgt),
                    src_to_reg1,
                    (dst_to_reg2, false),
                    new_instr,
                )
            }
            Instruction::Idiv(asm_type, imm @ Operand::ImmediateValue(_)) => {
                let reg = Register::R10;
                let instr_at_reg = Instruction::Idiv(asm_type, reg.into());
                Self::to_reg(asm_type, imm, reg, instr_at_reg)
            }
            Instruction::Div(asm_type, imm @ Operand::ImmediateValue(_)) => {
                let reg = Register::R10;
                let instr_at_reg = Instruction::Div(asm_type, reg.into());
                Self::to_reg(asm_type, imm, reg, instr_at_reg)
            }
            Instruction::Push(operand @ Operand::ImmediateValue(i)) if i32::try_from(i).is_err() => {
                let reg = Register::R10;
                let instr_at_reg = Instruction::Push(reg.into());
                Self::to_reg(AssemblyType::Quadword, operand, reg, instr_at_reg)
            }
            _ => vec![in_instr],
        })
    }
    fn to_reg(
        asm_type: AssemblyType,
        operand_to_reg: Operand,
        reg: Register,
        instr_at_reg: Instruction<FinalizedAsmAst>,
    ) -> Vec<Instruction<FinalizedAsmAst>> {
        let instr_to_reg = Instruction::Mov {
            asm_type,
            src: operand_to_reg,
            dst: reg.into(),
        };
        vec![instr_to_reg, instr_at_reg]
    }
    fn maybe_use_2_regs(
        (src_asm_type, mut src): (AssemblyType, Operand),
        (dst_asm_type, mut dst): (AssemblyType, Operand),
        src_to_reg1: bool,
        (dst_to_reg2, reg2_to_dst): (bool, bool),
        new_instr: impl FnOnce(Operand, Operand) -> Instruction<FinalizedAsmAst>,
    ) -> Vec<Instruction<FinalizedAsmAst>> {
        let reg1 = match src_asm_type {
            AssemblyType::Longword | AssemblyType::Quadword => Register::R10,
            AssemblyType::Double => Register::XMM14,
        };
        let reg2 = match dst_asm_type {
            AssemblyType::Longword | AssemblyType::Quadword => Register::R11,
            AssemblyType::Double => Register::XMM15,
        };
        let reg1 = move || reg1.into();
        let reg2 = move || reg2.into();

        let mov = |asm_type: AssemblyType, src: Operand, dst: Operand| Instruction::Mov {
            asm_type,
            src,
            dst,
        };

        let mut instr_to_reg1 = None;
        if src_to_reg1 {
            instr_to_reg1 = Some(mov(src_asm_type, src, reg1()));
            src = reg1();
        }

        let mut instr_to_reg2 = None;
        let mut instr_from_reg2 = None;
        match (dst_to_reg2, reg2_to_dst) {
            (false, false) => { /* No-op. */ }
            (false, true) => {
                instr_from_reg2 = Some(mov(dst_asm_type, reg2(), dst));
                dst = reg2();
            }
            (true, false) => {
                instr_to_reg2 = Some(mov(dst_asm_type, dst, reg2()));
                dst = reg2();
            }
            (true, true) => {
                instr_to_reg2 = Some(mov(dst_asm_type, dst.clone(), reg2()));
                instr_from_reg2 = Some(mov(dst_asm_type, reg2(), dst));
                dst = reg2();
            }
        }

        let instr = new_instr(src, dst);

        [instr_to_reg1, instr_to_reg2, Some(instr), instr_from_reg2]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()
    }
}
