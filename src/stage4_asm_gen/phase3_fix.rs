//! Ensure that each x86-64 instruction accesses operands in an allowed way.

use crate::{
    common::types_backend::AssemblyType,
    stage4_asm_gen::{asm_ast::*, phase2_finalize::FinalizedAsmAst},
};

pub struct OperandFixer {}
impl OperandFixer {
    pub fn fix_invalid_operands<'a>(
        in_instrs: impl 'a + Iterator<Item = Instruction<FinalizedAsmAst>>,
    ) -> impl 'a + Iterator<Item = Instruction<FinalizedAsmAst>> {
        in_instrs.flat_map(|in_instr| match in_instr {
            Instruction::Mov { asm_type, mut src, dst } => {
                if let (AssemblyType::Longword, Operand::ImmediateValue(i)) = (asm_type, &src) {
                    /* In case this `mov` is truncating from quadword to longword,
                    zero-out the most significant bytes, in order to prevent warning from gcc's assembler. */
                    src = Operand::ImmediateValue(*i as u32 as i64);
                }

                let src_to_reg1 =
                    (src.is_on_mem() && dst.is_on_mem())
                    || (Self::is_imm_outside_i32(asm_type, &src)
                        && dst.is_on_mem());
                let src_to_reg1 = src_to_reg1.then_some(asm_type);

                let new_instr = |src: Operand, dst: Operand| Instruction::Mov { asm_type, src, dst };

                Self::maybe_use_2_regs(src, dst, src_to_reg1, None, new_instr)
            }
            Instruction::Movsx { src, dst } => {
                let src_to_reg1 = matches!(&src, Operand::ImmediateValue(_));
                let src_to_reg1 = src_to_reg1.then_some(AssemblyType::Longword);

                let reg2_to_dst = dst.is_on_mem();
                let reg2_to_dst = reg2_to_dst.then_some((ToFromReg::FromReg, AssemblyType::Quadword));

                let new_instr = |src: Operand, dst: Operand| Instruction::Movsx { src, dst };

                Self::maybe_use_2_regs(src, dst, src_to_reg1, reg2_to_dst, new_instr)
            }
            Instruction::MovZeroExtend { src, dst } => {
                if dst.is_on_mem() {
                    let reg = Register::R11;
                    let instr_at_reg = Instruction::Mov{
                        asm_type: AssemblyType::Quadword,
                        src: reg.into(),
                        dst,
                    };
                    Self::to_reg(AssemblyType::Longword, src, reg, instr_at_reg)
                } else {
                    vec![Instruction::Mov{
                        asm_type: AssemblyType::Longword,
                        src,
                        dst
                    }]
                }
            }
            Instruction::Cvttsd2si { dst_asm_type, src, dst } => {
                let reg2_to_dst = dst.is_on_mem();
                let reg2_to_dst = reg2_to_dst.then_some((ToFromReg::FromReg, dst_asm_type));

                let new_instr = |src: Operand, dst: Operand| Instruction::Cvttsd2si { dst_asm_type, src, dst };

                Self::maybe_use_2_regs(src, dst, None, reg2_to_dst, new_instr)
            }
            Instruction::Cvtsi2sd { src_asm_type, src, dst } => {
                let src_to_reg1 = matches!(&src, Operand::ImmediateValue(_));
                let src_to_reg1 = src_to_reg1.then_some(src_asm_type);

                let reg2_to_dst = matches!(&dst, Operand::Register(_)) == false;
                let reg2_to_dst = reg2_to_dst.then_some((ToFromReg::FromReg, AssemblyType::Double));

                let new_instr = |src: Operand, dst: Operand| Instruction::Cvtsi2sd { src_asm_type, src, dst };

                Self::maybe_use_2_regs(src, dst, src_to_reg1, reg2_to_dst, new_instr)
            }
            Instruction::Binary { op, asm_type, arg, tgt } => {
                use BinaryOperator as BO;

                let (src_to_reg1, dst_to_and_from_reg2);
                match asm_type {
                    AssemblyType::Longword | AssemblyType::Quadword => {
                        src_to_reg1 =
                            (matches!(&op, BO::Add | BO::Sub | BO::And | BO::Or)
                                && arg.is_on_mem()
                                && tgt.is_on_mem())
                            || Self::is_imm_outside_i32(asm_type, &arg);
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
                let src_to_reg1 = src_to_reg1.then_some(asm_type);
                let dst_to_and_from_reg2 = dst_to_and_from_reg2.then_some((ToFromReg::ToAndFromReg, asm_type));

                let new_instr = |arg: Operand, tgt: Operand| Instruction::Binary { op, asm_type, arg, tgt };

                Self::maybe_use_2_regs(arg, tgt, src_to_reg1, dst_to_and_from_reg2, new_instr)
            }
            Instruction::Cmp { asm_type, arg, tgt } => {
                let (src_to_reg1, dst_to_reg2);
                match asm_type {
                    AssemblyType::Longword | AssemblyType::Quadword => {
                        src_to_reg1 =
                            (arg.is_on_mem() && tgt.is_on_mem())
                            || Self::is_imm_outside_i32(asm_type, &arg);
                        dst_to_reg2 = matches!(&tgt, Operand::ImmediateValue(_));
                    }
                    AssemblyType::Double => {
                        src_to_reg1 = false;
                        dst_to_reg2 = matches!(&tgt, Operand::Register(_)) == false;
                    }
                }
                let src_to_reg1 = src_to_reg1.then_some(asm_type);
                let dst_to_reg2 = dst_to_reg2.then_some((ToFromReg::ToReg, asm_type));

                let new_instr = |arg: Operand, tgt: Operand| Instruction::Cmp { asm_type, arg, tgt };

                Self::maybe_use_2_regs(arg, tgt, src_to_reg1, dst_to_reg2, new_instr)
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
            Instruction::Push(operand) if Self::is_imm_outside_i32(AssemblyType::Quadword, &operand) => {
                let reg = Register::R10;
                let instr_at_reg = Instruction::Push(reg.into());
                Self::to_reg(AssemblyType::Quadword, operand, reg, instr_at_reg)
            }
            _ => vec![in_instr],
        })
    }

    /// Some quadword (ie 64-bit) instructions take 32-bit immediate-value operands and sign-extend them to 64 bits.
    /// This checks whether such sign-extension would corrupt the value.
    fn is_imm_outside_i32(asm_type: AssemblyType, operand: &Operand) -> bool {
        (asm_type == AssemblyType::Quadword)
            && match operand {
                Operand::ImmediateValue(i) => {
                    /* We might choose to change `ImmediateValue`'s wrapped rust type, which represents bits.
                    Here, we explicitly cast the bits to i64, and then evaluate its compatibility with i32. */
                    #[allow(clippy::unnecessary_cast)]
                    let i: i64 = *i as i64;
                    i32::try_from(i).is_err()
                }
                _ => false,
            }
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
        mut src: Operand,
        mut dst: Operand,
        src_to_reg1: Option<AssemblyType>,
        dst_tofrom_reg2: Option<(ToFromReg, AssemblyType)>,
        new_instr: impl FnOnce(Operand, Operand) -> Instruction<FinalizedAsmAst>,
    ) -> Vec<Instruction<FinalizedAsmAst>> {
        let reg1 = |src_asm_type: AssemblyType| match src_asm_type {
            AssemblyType::Longword | AssemblyType::Quadword => Register::R10,
            AssemblyType::Double => Register::XMM14,
        };
        let reg2 = |dst_asm_type: AssemblyType| match dst_asm_type {
            AssemblyType::Longword | AssemblyType::Quadword => Register::R11,
            AssemblyType::Double => Register::XMM15,
        };

        let mov = |asm_type: AssemblyType, src: Operand, dst: Operand| Instruction::Mov {
            asm_type,
            src,
            dst,
        };

        let mut instr_to_reg1 = None;
        if let Some(src_asm_type) = src_to_reg1 {
            let reg1 = reg1(src_asm_type);
            instr_to_reg1 = Some(mov(src_asm_type, src, reg1.into()));
            src = reg1.into();
        }

        let mut instr_to_reg2 = None;
        let mut instr_from_reg2 = None;
        if let Some((to_from_reg, dst_asm_type)) = dst_tofrom_reg2 {
            let reg2 = reg2(dst_asm_type);
            match to_from_reg {
                ToFromReg::ToReg => {
                    instr_to_reg2 = Some(mov(dst_asm_type, dst, reg2.into()));
                }
                ToFromReg::FromReg => {
                    instr_from_reg2 = Some(mov(dst_asm_type, reg2.into(), dst));
                }
                ToFromReg::ToAndFromReg => {
                    instr_to_reg2 = Some(mov(dst_asm_type, dst.clone(), reg2.into()));
                    instr_from_reg2 = Some(mov(dst_asm_type, reg2.into(), dst));
                }
            }
            dst = reg2.into();
        }

        let instr = new_instr(src, dst);

        [instr_to_reg1, instr_to_reg2, Some(instr), instr_from_reg2]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()
    }
}

#[allow(clippy::enum_variant_names)]
enum ToFromReg {
    ToReg,
    FromReg,
    ToAndFromReg,
}
