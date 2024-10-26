use super::{GeneratedAsmAst, InstrsGenerator};
use crate::{
    common::{
        identifier::{JumpLabel, UniqueId},
        primitive::Const,
        types_backend::ScalarAssemblyType,
    },
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::asm_ast::*,
};
use std::rc::Rc;

impl InstrsGenerator {
    /* Purely bitwise transformations */

    pub(super) fn gen_signextend(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, src_asm_type) = self.value_to_operand_and_type(src);
        let (dst, _, dst_asm_type) = self.value_to_operand_and_type(dst);
        vec![Instruction::Movsx {
            src_asm_type,
            dst_asm_type,
            src,
            dst,
        }]
    }
    pub(super) fn gen_zeroextend(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, src_asm_type) = self.value_to_operand_and_type(src);
        let (dst, _, dst_asm_type) = self.value_to_operand_and_type(dst);
        vec![Instruction::MovZeroExtend {
            src_asm_type,
            dst_asm_type,
            src,
            dst,
        }]
    }
    pub(super) fn gen_truncate(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let src = self.value_to_operand(src);
        let (dst, _, asm_type) = self.value_to_operand_and_type(dst);
        vec![Instruction::Mov { asm_type, src, dst }]
    }

    /* Transformation across integer and floating-point types */
    /*
    Note, wherever we use `Instruction::Cvttsd2si`,
        if the src float value is out of the range of the dst int type,
        the result is the special "indefinite integer" value, which is the minimum value the dst int type supports,
        and a status flag is set.
    In such case, all downstream values are corrupted.
    We choose not to warn about this edge case.
    */

    pub(super) fn gen_double_to_sgn_integ(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let src = self.value_to_operand(src);
        let (dst, _, dst_asm_type) = self.value_to_operand_and_type(dst);

        match dst_asm_type {
            ScalarAssemblyType::Byte => Self::gen_double_to_sgn_or_unsgn_integ_simple(
                src,
                ScalarAssemblyType::Longword,
                ScalarAssemblyType::Byte,
                dst,
            ), // f64 -> i32 -> i8
            _ => vec![
                Instruction::Cvttsd2si { dst_asm_type, src, dst }, // f64 -> {i64 or i32}
            ],
        }
    }
    pub(super) fn gen_sgn_integ_to_double(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, src_asm_type) = self.value_to_operand_and_type(src);
        let dst = self.value_to_operand(dst);

        match src_asm_type {
            ScalarAssemblyType::Byte => Self::gen_sgn_or_unsgn_integ_to_double_simple(
                src,
                true,
                ScalarAssemblyType::Byte,
                ScalarAssemblyType::Longword,
                dst,
            ), // i8 -> i32 -> f64
            _ => vec![
                Instruction::Cvtsi2sd { src_asm_type, src, dst }, // {i32 or i64} -> f64
            ],
        }
    }
    pub(super) fn gen_double_to_unsgn_integ(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let src = self.value_to_operand(src);
        let (dst, _, uint_asm_type) = self.value_to_operand_and_type(dst);
        match uint_asm_type {
            ScalarAssemblyType::Byte => Self::gen_double_to_sgn_or_unsgn_integ_simple(
                src,
                ScalarAssemblyType::Longword,
                ScalarAssemblyType::Byte,
                dst,
            ), // f64 -> i32 -> u8
            ScalarAssemblyType::Longword => Self::gen_double_to_sgn_or_unsgn_integ_simple(
                src,
                ScalarAssemblyType::Quadword,
                ScalarAssemblyType::Longword,
                dst,
            ), // f64 -> i64 -> u32
            ScalarAssemblyType::Quadword => self.gen_double_to_ulong(src, dst),
            ScalarAssemblyType::Double => unreachable!("double-to-uint dst {uint_asm_type:?}"),
        }
    }
    pub(super) fn gen_unsgn_integ_to_double(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, uint_asm_type) = self.value_to_operand_and_type(src);
        let dst = self.value_to_operand(dst);
        match uint_asm_type {
            ScalarAssemblyType::Byte => Self::gen_sgn_or_unsgn_integ_to_double_simple(
                src,
                false,
                ScalarAssemblyType::Byte,
                ScalarAssemblyType::Longword,
                dst,
            ), // u8 -> u32 (as i32) -> f64
            ScalarAssemblyType::Longword => Self::gen_sgn_or_unsgn_integ_to_double_simple(
                src,
                false,
                ScalarAssemblyType::Longword,
                ScalarAssemblyType::Quadword,
                dst,
            ), // u32 -> u64 (as i64) -> f64
            ScalarAssemblyType::Quadword => Self::gen_ulong_to_double(src, dst),
            ScalarAssemblyType::Double => unreachable!("uint-to-double src {uint_asm_type:?}"),
        }
    }

    fn gen_double_to_sgn_or_unsgn_integ_simple(
        src: PreFinalOperand,
        intermediary_asm_type: ScalarAssemblyType,
        dst_asm_type: ScalarAssemblyType,
        dst: PreFinalOperand,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let gp_reg = || Operand::Register(Register::AX).into();
        vec![
            Instruction::Cvttsd2si {
                dst_asm_type: intermediary_asm_type,
                src,
                dst: gp_reg(),
            },
            Instruction::Mov {
                asm_type: dst_asm_type,
                src: gp_reg(),
                dst,
            },
        ]
    }

    fn gen_double_to_ulong(
        &mut self,
        src: PreFinalOperand,
        dst: PreFinalOperand,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let i64_ceil_as_u64 = (i64::MAX as u64) + 1;
        let i64_ceil_as_f64 = i64_ceil_as_u64 as f64;
        let i64_ceil_as_f64 =
            self.get_or_new_static_readonly_operand(None, Const::Double(i64_ceil_as_f64));

        let [lbl_out_of_range, lbl_end] =
            JumpLabel::create(UniqueId::new(), "f64_u64", ["oor", "end"]);

        let sse_reg = || Operand::Register(Register::XMM0).into();

        vec![
            Instruction::Cmp {
                asm_type: ScalarAssemblyType::Double,
                tgt: src.clone(),
                arg: i64_ceil_as_f64.clone(),
            },
            Instruction::JmpCC(ConditionCode::Ae, Rc::clone(&lbl_out_of_range)),
            /* Below: iff src f64 < (1<<63) */
            Instruction::Cvttsd2si {
                dst_asm_type: ScalarAssemblyType::Quadword,
                src: src.clone(),
                dst: dst.clone(),
            },
            Instruction::Jmp(Rc::clone(&lbl_end)),
            /* Below: iff src f64 >= (1<<63) */
            Instruction::Label(lbl_out_of_range),
            Instruction::Mov {
                asm_type: ScalarAssemblyType::Double,
                src,
                dst: sse_reg(),
            },
            Instruction::Binary {
                op: BinaryOperator::Sub,
                asm_type: ScalarAssemblyType::Double,
                tgt: sse_reg(),
                arg: i64_ceil_as_f64,
            }, // src f64 - (1<<63)f64
            Instruction::Cvttsd2si {
                dst_asm_type: ScalarAssemblyType::Quadword,
                src: sse_reg(),
                dst: dst.clone(),
            }, // ( src f64 - (1<<63)f64 ) as i64
            Instruction::Binary {
                op: BinaryOperator::Add,
                asm_type: ScalarAssemblyType::Quadword,
                tgt: dst,
                arg: Operand::ImmediateValue(i64_ceil_as_u64 as i64).into(),
            }, // (i64 as u64) + (1<<63)u64
            Instruction::Label(lbl_end),
        ]
        /* The final Binary instruction is condensed from two Binary instructions as printed in the book.
        See https://norasandler.com/book/#errata for explanation. */
    }

    fn gen_sgn_or_unsgn_integ_to_double_simple(
        src: PreFinalOperand,
        src_is_signed: bool,
        src_asm_type: ScalarAssemblyType,
        intermediary_asm_type: ScalarAssemblyType,
        dst: PreFinalOperand,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let gp_reg = || Operand::Register(Register::AX).into();

        let instr1 = if src_is_signed {
            Instruction::Movsx {
                src_asm_type,
                dst_asm_type: intermediary_asm_type,
                src,
                dst: gp_reg(),
            }
        } else {
            Instruction::MovZeroExtend {
                src_asm_type,
                dst_asm_type: intermediary_asm_type,
                src,
                dst: gp_reg(),
            }
        };
        let instr2 = Instruction::Cvtsi2sd {
            src_asm_type: intermediary_asm_type,
            src: gp_reg(),
            dst,
        };

        vec![instr1, instr2]
    }

    fn gen_ulong_to_double(
        src: PreFinalOperand,
        dst: PreFinalOperand,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let [lbl_out_of_range, lbl_end] =
            JumpLabel::create(UniqueId::new(), "u64_f64", ["oor", "end"]);

        let gp_reg_1 = || Operand::Register(Register::AX).into();
        let gp_reg_2 = || Operand::Register(Register::DX).into();

        vec![
            Instruction::Cmp {
                asm_type: ScalarAssemblyType::Quadword,
                tgt: src.clone(),
                arg: Operand::ImmediateValue(0).into(),
            },
            Instruction::JmpCC(ConditionCode::L, Rc::clone(&lbl_out_of_range)),
            /* Below: iff src i64 >= 0, ie u64 < (1<<63) */
            Instruction::Cvtsi2sd {
                src_asm_type: ScalarAssemblyType::Quadword,
                src: src.clone(),
                dst: dst.clone(),
            },
            Instruction::Jmp(Rc::clone(&lbl_end)),
            /* Below: iff src i64 < 0, ie u64 >= (1<<63) */
            Instruction::Label(lbl_out_of_range),
            Instruction::Mov {
                asm_type: ScalarAssemblyType::Quadword,
                src,
                dst: gp_reg_1(),
            },
            Instruction::Mov {
                asm_type: ScalarAssemblyType::Quadword,
                src: gp_reg_1(),
                dst: gp_reg_2(),
            },
            Instruction::Unary(UnaryOperator::Shr, ScalarAssemblyType::Quadword, gp_reg_2()), // src u64 >> 1
            Instruction::Binary {
                op: BinaryOperator::And,
                asm_type: ScalarAssemblyType::Quadword,
                arg: Operand::ImmediateValue(1).into(),
                tgt: gp_reg_1(),
            }, // src u64 & 1
            Instruction::Binary {
                op: BinaryOperator::Or,
                asm_type: ScalarAssemblyType::Quadword,
                arg: gp_reg_1(),
                tgt: gp_reg_2(),
            }, // (src u64 >> 1) | (src u64 & 1)
            Instruction::Cvtsi2sd {
                src_asm_type: ScalarAssemblyType::Quadword,
                src: gp_reg_2(),
                dst: dst.clone(),
            }, // ( (src u64 >> 1) | (src u64 & 1) ) as f64
            Instruction::Binary {
                op: BinaryOperator::Add,
                asm_type: ScalarAssemblyType::Double,
                arg: dst.clone(),
                tgt: dst,
            }, // f64 * 2
            Instruction::Label(lbl_end),
        ]
    }
}
