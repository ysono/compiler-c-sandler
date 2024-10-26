//! Ensure that each x86-64 instruction accesses operands in an allowed way.

use crate::{
    common::types_backend::ScalarAssemblyType,
    stage4_asm_gen::{asm_ast::*, phase2_finalize::FinalizedAsmAst},
    utils::noop,
};

pub struct OperandFixer {}
impl OperandFixer {
    pub fn fix_invalid_operands<'a>(
        in_instrs: impl 'a + Iterator<Item = Instruction<FinalizedAsmAst>>,
    ) -> impl 'a + Iterator<Item = Instruction<FinalizedAsmAst>> {
        in_instrs.flat_map(|in_instr| match in_instr {
            Instruction::Mov { asm_type, mut src, dst } => {
                if let Operand::ImmediateValue(i) = &src {
                    /* In case this `mov` is truncating an immediate-value,
                    do the truncation at compile-time, in order to prevent warning from gcc's assembler. */
                    match asm_type {
                        ScalarAssemblyType::Byte => {
                            src = Operand::ImmediateValue(*i as u8 as i64);
                        }
                        ScalarAssemblyType::Longword => {
                            src = Operand::ImmediateValue(*i as u32 as i64);
                        }
                        ScalarAssemblyType::Quadword | ScalarAssemblyType::Double => noop!(),
                    }
                }

                let src_to_reg1 =
                    (src.is_on_mem() && dst.is_on_mem())
                    || (Self::is_imm_outside_i32(asm_type, &src)
                        && dst.is_on_mem());
                let src_to_reg1 = src_to_reg1.then_some(asm_type);

                let new_instr = |src: Operand, dst: Operand| Instruction::Mov { asm_type, src, dst };

                Self::maybe_use_2_regs(src, dst, src_to_reg1, None, new_instr)
            }
            Instruction::Movsx { src_asm_type, dst_asm_type, src, dst } => {
                let src_to_reg1 = matches!(&src, Operand::ImmediateValue(_));
                let src_to_reg1 = src_to_reg1.then_some(src_asm_type);

                let reg2_to_dst = dst.is_on_mem();
                let reg2_to_dst = reg2_to_dst.then_some((ToFromReg::FromReg, dst_asm_type));

                let new_instr = |src: Operand, dst: Operand| Instruction::Movsx { src_asm_type, dst_asm_type, src, dst };

                Self::maybe_use_2_regs(src, dst, src_to_reg1, reg2_to_dst, new_instr)
            }
            Instruction::MovZeroExtend { src_asm_type, dst_asm_type, src, dst } => {
                match src_asm_type {
                    ScalarAssemblyType::Byte => {
                        let src_to_reg1 = matches!(&src, Operand::ImmediateValue(_)) == true;
                        let src_to_reg1 = src_to_reg1.then_some(src_asm_type);

                        let reg2_to_dst = matches!(&dst, Operand::Register(_)) == false;
                        let reg2_to_dst = reg2_to_dst.then_some((ToFromReg::FromReg, dst_asm_type));

                        let new_instr = |src: Operand, dst: Operand| Instruction::MovZeroExtend { src_asm_type, dst_asm_type, src, dst };

                        Self::maybe_use_2_regs(src, dst, src_to_reg1, reg2_to_dst, new_instr)
                    }
                    ScalarAssemblyType::Longword => {
                        if dst.is_on_mem() {
                            let reg = Register::R11;
                            let instr_at_reg = Instruction::Mov{
                                asm_type: ScalarAssemblyType::Quadword,
                                src: Operand::Register(reg),
                                dst,
                            };
                            Self::to_reg(ScalarAssemblyType::Longword, src, reg, instr_at_reg)
                        } else {
                            vec![Instruction::Mov{
                                asm_type: ScalarAssemblyType::Longword,
                                src,
                                dst
                            }]
                        }
                    }
                    ScalarAssemblyType::Quadword | ScalarAssemblyType::Double => unreachable!(),
                }
            }
            Instruction::Lea { src, dst } => {
                debug_assert!(src.is_on_mem(), "Lea src {src:?}");

                let reg2_to_dst = matches!(&dst, Operand::Register(_)) == false;
                let reg2_to_dst = reg2_to_dst.then_some((ToFromReg::FromReg, ScalarAssemblyType::Quadword));

                let new_instr = |src: Operand, dst: Operand| Instruction::Lea { src, dst };

                Self::maybe_use_2_regs(src, dst, None, reg2_to_dst, new_instr)
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
                let reg2_to_dst = reg2_to_dst.then_some((ToFromReg::FromReg, ScalarAssemblyType::Double));

                let new_instr = |src: Operand, dst: Operand| Instruction::Cvtsi2sd { src_asm_type, src, dst };

                Self::maybe_use_2_regs(src, dst, src_to_reg1, reg2_to_dst, new_instr)
            }
            Instruction::Binary { op, asm_type, arg, tgt } => {
                use BinaryOperator as BO;

                let (src_to_reg1, dst_to_and_from_reg2);
                match asm_type {
                    ScalarAssemblyType::Byte => unreachable!("Any integer narrower than 4 bytes was previously promoted to `int`."),
                    ScalarAssemblyType::Longword | ScalarAssemblyType::Quadword => {
                        src_to_reg1 =
                            (matches!(&op, BO::Add | BO::Sub | BO::And | BO::Or)
                                && arg.is_on_mem()
                                && tgt.is_on_mem())
                            || Self::is_imm_outside_i32(asm_type, &arg);
                        dst_to_and_from_reg2 = matches!(&op, BinaryOperator::Mul) && tgt.is_on_mem();
                    }
                    ScalarAssemblyType::Double => {
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
                    ScalarAssemblyType::Byte | ScalarAssemblyType::Longword | ScalarAssemblyType::Quadword => {
                        src_to_reg1 =
                            (arg.is_on_mem() && tgt.is_on_mem())
                            || Self::is_imm_outside_i32(asm_type, &arg);
                        dst_to_reg2 = matches!(&tgt, Operand::ImmediateValue(_));
                    }
                    ScalarAssemblyType::Double => {
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
                let instr_at_reg = Instruction::Idiv(asm_type, Operand::Register(reg));
                Self::to_reg(asm_type, imm, reg, instr_at_reg)
            }
            Instruction::Div(asm_type, imm @ Operand::ImmediateValue(_)) => {
                let reg = Register::R10;
                let instr_at_reg = Instruction::Div(asm_type, Operand::Register(reg));
                Self::to_reg(asm_type, imm, reg, instr_at_reg)
            }
            Instruction::Push(operand) => {
                if matches!(&operand, Operand::Register(r) if r.is_sse()) {
                    vec![
                        Instruction::Binary {
                            op: BinaryOperator::Sub,
                            asm_type: ScalarAssemblyType::Quadword,
                            tgt: Operand::Register(Register::SP),
                            arg: Operand::ImmediateValue(8)
                        },
                        Instruction::Mov {
                            asm_type: ScalarAssemblyType::Double,
                            src: operand,
                            dst: Operand::Memory(Register::SP, MemoryOffset::new(0))
                        }
                    ]
                } else if Self::is_imm_outside_i32(ScalarAssemblyType::Quadword, &operand)  {
                    let reg = Register::R10;
                    let instr_at_reg = Instruction::Push(Operand::Register(reg));
                    Self::to_reg(ScalarAssemblyType::Quadword, operand, reg, instr_at_reg)
                } else {
                    vec![Instruction::Push(operand)]
                }
            }
            _ => vec![in_instr],
        })
    }

    /// Some quadword (ie 64-bit) instructions take 32-bit immediate-value operands and sign-extend them to 64 bits.
    /// This checks whether such sign-extension would corrupt the value.
    fn is_imm_outside_i32(asm_type: ScalarAssemblyType, operand: &Operand) -> bool {
        (asm_type == ScalarAssemblyType::Quadword)
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
        asm_type: ScalarAssemblyType,
        operand_to_reg: Operand,
        reg: Register,
        instr_at_reg: Instruction<FinalizedAsmAst>,
    ) -> Vec<Instruction<FinalizedAsmAst>> {
        let instr_to_reg = Instruction::Mov {
            asm_type,
            src: operand_to_reg,
            dst: Operand::Register(reg),
        };
        vec![instr_to_reg, instr_at_reg]
    }
    fn maybe_use_2_regs(
        mut src: Operand,
        mut dst: Operand,
        src_to_reg1: Option<ScalarAssemblyType>,
        dst_tofrom_reg2: Option<(ToFromReg, ScalarAssemblyType)>,
        new_instr: impl FnOnce(Operand, Operand) -> Instruction<FinalizedAsmAst>,
    ) -> Vec<Instruction<FinalizedAsmAst>> {
        use ScalarAssemblyType as SAT;

        let reg1 = |src_asm_type: SAT| {
            let reg = match src_asm_type {
                SAT::Byte | SAT::Longword | SAT::Quadword => Register::R10,
                SAT::Double => Register::XMM14,
            };
            Operand::Register(reg)
        };
        let reg2 = |dst_asm_type: SAT| {
            let reg = match dst_asm_type {
                SAT::Byte | SAT::Longword | SAT::Quadword => Register::R11,
                SAT::Double => Register::XMM15,
            };
            Operand::Register(reg)
        };

        let mov =
            |asm_type: SAT, src: Operand, dst: Operand| Instruction::Mov { asm_type, src, dst };

        let mut instr_to_reg1 = None;
        if let Some(src_asm_type) = src_to_reg1 {
            let reg1 = reg1(src_asm_type);
            instr_to_reg1 = Some(mov(src_asm_type, src, reg1.clone()));
            src = reg1;
        }

        let mut instr_to_reg2 = None;
        let mut instr_from_reg2 = None;
        if let Some((to_from_reg, dst_asm_type)) = dst_tofrom_reg2 {
            let reg2 = reg2(dst_asm_type);
            match to_from_reg {
                ToFromReg::ToReg => {
                    instr_to_reg2 = Some(mov(dst_asm_type, dst, reg2.clone()));
                }
                ToFromReg::FromReg => {
                    instr_from_reg2 = Some(mov(dst_asm_type, reg2.clone(), dst));
                }
                ToFromReg::ToAndFromReg => {
                    instr_to_reg2 = Some(mov(dst_asm_type, dst.clone(), reg2.clone()));
                    instr_from_reg2 = Some(mov(dst_asm_type, reg2.clone(), dst));
                }
            }
            dst = reg2;
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
