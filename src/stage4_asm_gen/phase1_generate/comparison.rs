use crate::{
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::{
        asm_ast::*,
        phase1_generate::{GeneratedAsmAst, InstrsGenerator},
    },
    types_backend::AssemblyType,
};

impl<'slf> InstrsGenerator<'slf> {
    pub(super) fn gen_unary_comparison_instrs(
        &self,
        t_op: t::ComparisonUnaryOperator,
        t::Unary { op: _, src, dst }: t::Unary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let cc = match t_op {
            t::ComparisonUnaryOperator::Not => ConditionCode::E,
        };
        let (asm_src, asm_src_type, _) = self.convert_value(src);
        let (asm_dst, asm_dst_type, _) = self.convert_value(dst);

        Self::gen_comparison_instrs_from_asm(
            asm_src_type,
            asm_src,
            PreFinalOperand::ImmediateValue(0),
            cc,
            asm_dst_type,
            asm_dst,
        )
    }
    pub(super) fn gen_comparison_instrs(
        &self,
        t_op: t::ComparisonBinaryOperator,
        t::Binary { op: _, src1, src2, dst }: t::Binary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        use t::ComparisonBinaryOperator as TBOC;
        use ConditionCode as CC;

        let (asm_src1, asm_src_type, is_signed) = self.convert_value(src1);
        let (asm_src2, _, _) = self.convert_value(src2);
        let (asm_dst, asm_dst_type, _) = self.convert_value(dst);
        let cmp_0_cc = match (t_op, is_signed) {
            (TBOC::Eq, _) => CC::E,
            (TBOC::Neq, _) => CC::Ne,
            (TBOC::Lt, true) => CC::L,
            (TBOC::Lt, false) => CC::B,
            (TBOC::Lte, true) => CC::Le,
            (TBOC::Lte, false) => CC::Be,
            (TBOC::Gt, true) => CC::G,
            (TBOC::Gt, false) => CC::A,
            (TBOC::Gte, true) => CC::Ge,
            (TBOC::Gte, false) => CC::Ae,
        };

        Self::gen_comparison_instrs_from_asm(
            asm_src_type,
            asm_src1,
            asm_src2,
            cmp_0_cc,
            asm_dst_type,
            asm_dst,
        )
    }
    fn gen_comparison_instrs_from_asm(
        asm_src_type: AssemblyType,
        asm_src1: PreFinalOperand,
        asm_src2: PreFinalOperand,
        cmp_0_cc: ConditionCode,
        asm_dst_type: AssemblyType,
        asm_dst: PreFinalOperand,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let asm_instr_1 = Instruction::Cmp {
            asm_type: asm_src_type,
            tgt: asm_src1,
            arg: asm_src2,
        };
        let asm_instr_2 = Instruction::Mov {
            asm_type: asm_dst_type,
            src: PreFinalOperand::ImmediateValue(0),
            dst: asm_dst.clone(),
        };
        let asm_instr_3 = Instruction::SetCC(cmp_0_cc, asm_dst);
        vec![asm_instr_1, asm_instr_2, asm_instr_3]
    }

    pub(super) fn gen_jumpif_instrs(
        &self,
        cmp_0_cc: ConditionCode,
        t::JumpIf { condition, tgt }: t::JumpIf,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (condition, asm_type, _) = self.convert_value(condition);
        let asm_instr_1 = Instruction::Cmp {
            asm_type,
            tgt: condition,
            arg: PreFinalOperand::ImmediateValue(0),
        };
        let asm_instr_2 = Instruction::JmpCC(cmp_0_cc, tgt);
        vec![asm_instr_1, asm_instr_2]
    }
}
