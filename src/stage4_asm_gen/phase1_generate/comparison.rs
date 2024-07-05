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
        let (src, _, src_asm_type) = self.convert_value(src);
        let (dst, _, dst_asm_type) = self.convert_value(dst);

        let mut asm_instrs = vec![Self::gen_cmp_vs_zero(src, src_asm_type)];
        asm_instrs.extend(Self::gen_setcc(cc, dst, dst_asm_type));
        asm_instrs
    }

    pub(super) fn gen_comparison_instrs(
        &self,
        t_op: t::ComparisonBinaryOperator,
        t::Binary { op: _, src1, src2, dst }: t::Binary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        use t::ComparisonBinaryOperator as TBOC;
        use ConditionCode as CC;

        let (src1, src_var_type, src_asm_type) = self.convert_value(src1);
        let (src2, _, _) = self.convert_value(src2);
        let (dst, _, dst_asm_type) = self.convert_value(dst);
        let cc = match (t_op, src_var_type.is_signed()) {
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

        let mut asm_instrs = vec![Instruction::Cmp {
            asm_type: src_asm_type,
            tgt: src1,
            arg: src2,
        }];
        asm_instrs.extend(Self::gen_setcc(cc, dst, dst_asm_type));
        asm_instrs
    }

    pub(super) fn gen_jumpif_instrs(
        &self,
        t::JumpIf { condition, jump_crit, lbl }: t::JumpIf,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (condition, _, asm_type) = self.convert_value(condition);

        let cc = match jump_crit {
            t::JumpCriterion::JumpIfZero => ConditionCode::E,
            t::JumpCriterion::JumpIfNotZero => ConditionCode::Ne,
        };

        vec![
            Self::gen_cmp_vs_zero(condition, asm_type),
            Instruction::JmpCC(cc, lbl),
        ]
    }

    fn gen_cmp_vs_zero(
        src: PreFinalOperand,
        src_asm_type: AssemblyType,
    ) -> Instruction<GeneratedAsmAst> {
        Instruction::Cmp {
            asm_type: src_asm_type,
            tgt: src,
            arg: PreFinalOperand::ImmediateValue(0),
        }
    }

    fn gen_setcc(
        cc: ConditionCode,
        dst: PreFinalOperand,
        dst_asm_type: AssemblyType,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        vec![
            Instruction::Mov {
                asm_type: dst_asm_type,
                src: PreFinalOperand::ImmediateValue(0),
                dst: dst.clone(),
            },
            Instruction::SetCC(cc, dst),
        ]
    }
}
