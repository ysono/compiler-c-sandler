use super::{GeneratedAsmAst, InstrsGenerator};
use crate::{
    common::{
        identifier::{JumpLabel, UniqueId},
        types_backend::AssemblyType,
        types_frontend::Const,
    },
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::asm_ast::*,
};
use std::rc::Rc;

impl InstrsGenerator {
    pub(super) fn gen_signextend_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, _) = self.convert_value(src);
        let (dst, _, _) = self.convert_value(dst);
        vec![Instruction::Movsx { src, dst }]
    }
    pub(super) fn gen_truncate_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, _) = self.convert_value(src);
        let (dst, _, _) = self.convert_value(dst);
        vec![Instruction::Mov {
            asm_type: AssemblyType::Longword,
            src,
            dst,
        }]
    }
    pub(super) fn gen_zeroextend_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, _) = self.convert_value(src);
        let (dst, _, _) = self.convert_value(dst);
        vec![Instruction::MovZeroExtend { src, dst }]
    }
    pub(super) fn gen_double_to_int_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, _) = self.convert_value(src);
        let (dst, _, dst_asm_type) = self.convert_value(dst);
        vec![Instruction::Cvttsd2si { dst_asm_type, src, dst }]
        /* If the src float value is out of the range of the dst int type,
        the result is the special "indefinite integer" value, which is the minimum value the dst int type supports,
        and a status flag is set.
        We choose not to warn about this edge case.
        This design policy applies to all other logic which we implement using any `Instruction::Cvttsd2si`. */
    }
    pub(super) fn gen_int_to_double_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, src_asm_type) = self.convert_value(src);
        let (dst, _, _) = self.convert_value(dst);
        vec![Instruction::Cvtsi2sd { src_asm_type, src, dst }]
    }
    pub(super) fn gen_double_to_uint_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, _) = self.convert_value(src);
        let (dst, _, uint_asm_type) = self.convert_value(dst);
        match uint_asm_type {
            AssemblyType::Longword => {
                let gp_reg = PreFinalOperand::Register(Register::AX);
                vec![
                    Instruction::Cvttsd2si {
                        dst_asm_type: AssemblyType::Quadword,
                        src,
                        dst: gp_reg.clone(),
                    }, // f64 to i64
                    Instruction::Mov {
                        asm_type: AssemblyType::Longword,
                        src: gp_reg,
                        dst,
                    }, // i64 to u32
                ]
            }
            AssemblyType::Quadword => {
                let i64_ceil_as_u64 = (i64::MAX as u64) + 1;
                let i64_ceil_as_f64 = i64_ceil_as_u64 as f64;
                let i64_ceil_as_f64 =
                    self.get_or_new_static_constant_operand(None, Const::Double(i64_ceil_as_f64));

                let [lbl_out_of_range, lbl_end] =
                    JumpLabel::create(UniqueId::new(), "f64_u64", ["oor", "end"]);

                let sse_reg = PreFinalOperand::Register(Register::XMM0);

                vec![
                    Instruction::Cmp {
                        asm_type: AssemblyType::Double,
                        tgt: src.clone(),
                        arg: i64_ceil_as_f64.clone(),
                    },
                    Instruction::JmpCC(ConditionCode::Ae, Rc::clone(&lbl_out_of_range)),
                    /* Below: iff src f64 < (1<<63) */
                    Instruction::Cvttsd2si {
                        dst_asm_type: AssemblyType::Quadword,
                        src: src.clone(),
                        dst: dst.clone(),
                    },
                    Instruction::Jmp(Rc::clone(&lbl_end)),
                    /* Below: iff src f64 >= (1<<63) */
                    Instruction::Label(lbl_out_of_range),
                    Instruction::Mov {
                        asm_type: AssemblyType::Double,
                        src,
                        dst: sse_reg.clone(),
                    },
                    Instruction::Binary {
                        op: BinaryOperator::Sub,
                        asm_type: AssemblyType::Double,
                        tgt: sse_reg.clone(),
                        arg: i64_ceil_as_f64,
                    }, // src f64 - (1<<63)f64
                    Instruction::Cvttsd2si {
                        dst_asm_type: AssemblyType::Quadword,
                        src: sse_reg,
                        dst: dst.clone(),
                    }, // ( src f64 - (1<<63)f64 ) as i64
                    Instruction::Binary {
                        op: BinaryOperator::Add,
                        asm_type: AssemblyType::Quadword,
                        tgt: dst,
                        arg: PreFinalOperand::ImmediateValue(i64_ceil_as_u64),
                    }, // (i64 as u64) + (1<<63)u64
                    Instruction::Label(lbl_end),
                ]
                /* The final Binary instruction is condensed from two Binary instructions as printed in the book.
                See https://norasandler.com/book/#errata for explanation. */
            }
            AssemblyType::Double => unreachable!("double-to-uint dst {uint_asm_type:?}"),
        }
    }
    pub(super) fn gen_uint_to_double_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, uint_asm_type) = self.convert_value(src);
        let (dst, _, _) = self.convert_value(dst);
        match uint_asm_type {
            AssemblyType::Longword => {
                let gp_reg = || PreFinalOperand::Register(Register::AX);
                vec![
                    Instruction::MovZeroExtend { src, dst: gp_reg() }, // u32 to u64
                    Instruction::Cvtsi2sd {
                        src_asm_type: AssemblyType::Quadword,
                        src: gp_reg(),
                        dst,
                    }, // (u64 as i64) to f64
                ]
            }
            AssemblyType::Quadword => {
                let [lbl_out_of_range, lbl_end] =
                    JumpLabel::create(UniqueId::new(), "u64_f64", ["oor", "end"]);

                let gp_reg_1 = PreFinalOperand::Register(Register::AX);
                let gp_reg_2 = PreFinalOperand::Register(Register::DX);

                vec![
                    Instruction::Cmp {
                        asm_type: AssemblyType::Quadword,
                        tgt: src.clone(),
                        arg: PreFinalOperand::ImmediateValue(0),
                    },
                    Instruction::JmpCC(ConditionCode::L, Rc::clone(&lbl_out_of_range)),
                    /* Below: iff src i64 >= 0, ie u64 < (1<<63) */
                    Instruction::Cvtsi2sd {
                        src_asm_type: AssemblyType::Quadword,
                        src: src.clone(),
                        dst: dst.clone(),
                    },
                    Instruction::Jmp(Rc::clone(&lbl_end)),
                    /* Below: iff src i64 < 0, ie u64 >= (1<<63) */
                    Instruction::Label(lbl_out_of_range),
                    Instruction::Mov {
                        asm_type: AssemblyType::Quadword,
                        src,
                        dst: gp_reg_1.clone(),
                    },
                    Instruction::Mov {
                        asm_type: AssemblyType::Quadword,
                        src: gp_reg_1.clone(),
                        dst: gp_reg_2.clone(),
                    },
                    Instruction::Unary(
                        UnaryOperator::Shr,
                        AssemblyType::Quadword,
                        gp_reg_2.clone(),
                    ), // src u64 >> 1
                    Instruction::Binary {
                        op: BinaryOperator::And,
                        asm_type: AssemblyType::Quadword,
                        arg: PreFinalOperand::ImmediateValue(1),
                        tgt: gp_reg_1.clone(),
                    }, // src u64 & 1
                    Instruction::Binary {
                        op: BinaryOperator::Or,
                        asm_type: AssemblyType::Quadword,
                        arg: gp_reg_1,
                        tgt: gp_reg_2.clone(),
                    }, // (src u64 >> 1) | (src u64 & 1)
                    Instruction::Cvtsi2sd {
                        src_asm_type: AssemblyType::Quadword,
                        src: gp_reg_2,
                        dst: dst.clone(),
                    }, // ( (src u64 >> 1) | (src u64 & 1) ) as f64
                    Instruction::Binary {
                        op: BinaryOperator::Add,
                        asm_type: AssemblyType::Double,
                        arg: dst.clone(),
                        tgt: dst,
                    }, // f64 * 2
                    Instruction::Label(lbl_end),
                ]
            }
            AssemblyType::Double => unreachable!("uint-to-double src {uint_asm_type:?}"),
        }
    }
}
