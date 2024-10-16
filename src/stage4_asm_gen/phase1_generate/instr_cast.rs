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
    pub(super) fn gen_signextend_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let src = self.value_to_operand(src);
        let dst = self.value_to_operand(dst);
        vec![Instruction::Movsx { src, dst }]
    }
    pub(super) fn gen_truncate_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let src = self.value_to_operand(src);
        let dst = self.value_to_operand(dst);
        vec![Instruction::Mov {
            asm_type: ScalarAssemblyType::Longword,
            src,
            dst,
        }]
    }
    pub(super) fn gen_zeroextend_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let src = self.value_to_operand(src);
        let dst = self.value_to_operand(dst);
        vec![Instruction::MovZeroExtend { src, dst }]
    }
    pub(super) fn gen_double_to_int_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let src = self.value_to_operand(src);
        let (dst, _, dst_asm_type) = self.value_to_operand_and_type(dst);
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
        let (src, _, src_asm_type) = self.value_to_operand_and_type(src);
        let dst = self.value_to_operand(dst);
        vec![Instruction::Cvtsi2sd { src_asm_type, src, dst }]
    }
    pub(super) fn gen_double_to_uint_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let src = self.value_to_operand(src);
        let (dst, _, uint_asm_type) = self.value_to_operand_and_type(dst);
        match uint_asm_type {
            ScalarAssemblyType::Longword => {
                let gp_reg = || Operand::Register(Register::AX).into();
                vec![
                    Instruction::Cvttsd2si {
                        dst_asm_type: ScalarAssemblyType::Quadword,
                        src,
                        dst: gp_reg(),
                    }, // f64 to i64
                    Instruction::Mov {
                        asm_type: ScalarAssemblyType::Longword,
                        src: gp_reg(),
                        dst,
                    }, // i64 to u32
                ]
            }
            ScalarAssemblyType::Quadword => {
                let i64_ceil_as_u64 = (i64::MAX as u64) + 1;
                let i64_ceil_as_f64 = i64_ceil_as_u64 as f64;
                let i64_ceil_as_f64 = {
                    self.get_or_new_static_constant_operand(None, Const::Double(i64_ceil_as_f64))
                    /* `Double(i64_ceil_as_f64)` is equivalent to `Double(-0.0)`,
                    AS LONG AS `Const impl PartialEq` compares `f64`s by their bits, not by rust's default comparison logic. */
                };

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
            ScalarAssemblyType::Double => unreachable!("double-to-uint dst {uint_asm_type:?}"),
        }
    }
    pub(super) fn gen_uint_to_double_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, uint_asm_type) = self.value_to_operand_and_type(src);
        let dst = self.value_to_operand(dst);
        match uint_asm_type {
            ScalarAssemblyType::Longword => {
                let gp_reg = || Operand::Register(Register::AX).into();
                vec![
                    Instruction::MovZeroExtend { src, dst: gp_reg() }, // u32 to u64
                    Instruction::Cvtsi2sd {
                        src_asm_type: ScalarAssemblyType::Quadword,
                        src: gp_reg(),
                        dst,
                    }, // (u64 as i64) to f64
                ]
            }
            ScalarAssemblyType::Quadword => {
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
                    Instruction::Unary(
                        UnaryOperator::Shr,
                        ScalarAssemblyType::Quadword,
                        gp_reg_2(),
                    ), // src u64 >> 1
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
            ScalarAssemblyType::Double => unreachable!("uint-to-double src {uint_asm_type:?}"),
        }
    }
}
