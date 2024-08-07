use crate::{
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::{
        asm_ast::*,
        phase1_generate::{GeneratedAsmAst, InstrsGenerator},
    },
    types_backend::AssemblyType,
    types_frontend::{Const, VarType},
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
        let (dst, uint_var_type, _) = self.convert_value(dst);
        if uint_var_type == VarType::UInt {
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
        } else {
            /* Then, uint_var_type == VarType::ULong */
            let i64_ceil_as_u64 = (i64::MAX as u64) + 1;
            let i64_ceil_as_f64 = i64_ceil_as_u64 as f64;
            let i64_ceil_as_f64 =
                self.get_or_new_static_constant_operand(None, Const::Double(i64_ceil_as_f64));

            let lbl_out_of_range = Rc::new(LabelIdentifier::new(String::from("f64_u64.oor")));
            let lbl_end = Rc::new(LabelIdentifier::new(String::from("f64_u64.end")));

            let sse_reg = PreFinalOperand::Register(Register::XMM0);

            vec![
                Instruction::Cmp {
                    asm_type: AssemblyType::Double,
                    tgt: src.clone(),
                    arg: i64_ceil_as_f64.clone(),
                },
                Instruction::JmpCC(ConditionCode::Ae, Rc::clone(&lbl_out_of_range)),
                /* Below: src f64 < (1<<63) */
                Instruction::Cvttsd2si {
                    dst_asm_type: AssemblyType::Quadword,
                    src: src.clone(),
                    dst: dst.clone(),
                },
                Instruction::Jmp(Rc::clone(&lbl_end)),
                /* Below: src f64 >= (1<<63) */
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
                }, // (i64 as u64) + (1<<63)
                Instruction::Label(lbl_end),
            ]
        }
    }
    pub(super) fn gen_uint_to_double_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, uint_var_type, _) = self.convert_value(src);
        let (dst, _, _) = self.convert_value(dst);
        if uint_var_type == VarType::UInt {
            let gp_reg = || PreFinalOperand::Register(Register::AX);
            vec![
                Instruction::MovZeroExtend { src, dst: gp_reg() }, // u32 to u64
                Instruction::Cvtsi2sd {
                    src_asm_type: AssemblyType::Quadword,
                    src: gp_reg(),
                    dst,
                }, // i64 to f64
            ]
        } else {
            /* Then, uint_var_type == VarType::ULong */
            let lbl_out_of_range = Rc::new(LabelIdentifier::new(String::from("u64_f64.oor")));
            let lbl_end = Rc::new(LabelIdentifier::new(String::from("u64_f64.end")));

            let gp_reg_1 = PreFinalOperand::Register(Register::AX);
            let gp_reg_2 = PreFinalOperand::Register(Register::DX);

            vec![
                Instruction::Cmp {
                    asm_type: AssemblyType::Quadword,
                    tgt: src.clone(),
                    arg: PreFinalOperand::ImmediateValue(0),
                },
                Instruction::JmpCC(ConditionCode::L, Rc::clone(&lbl_out_of_range)),
                /* Below: src i64 >= 0, ie u64 < (1<<63) */
                Instruction::Cvtsi2sd {
                    src_asm_type: AssemblyType::Quadword,
                    src: src.clone(),
                    dst: dst.clone(),
                },
                Instruction::Jmp(Rc::clone(&lbl_end)),
                /* Below: src i64 < 0, ie u64 >= (1<<63) */
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
                Instruction::Unary(UnaryOperator::Shr, AssemblyType::Quadword, gp_reg_2.clone()), // src u64 >> 1
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
                },
                Instruction::Binary {
                    op: BinaryOperator::Add,
                    asm_type: AssemblyType::Double,
                    arg: dst.clone(),
                    tgt: dst,
                },
                Instruction::Label(lbl_end),
            ]
        }
    }
}
