use super::{GeneratedAsmAst, InstrsGenerator};
use crate::{
    common::{types_backend::AssemblyType, types_frontend::VarType},
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::asm_ast::*,
};

impl InstrsGenerator {
    pub(super) fn gen_unary_comparison_instrs(
        &mut self,
        t_op: t::ComparisonUnaryOperator,
        t::Unary { op: _, src, dst }: t::Unary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, src_asm_type) = self.value_to_operand_and_type(src);
        let (dst, _, dst_asm_type) = self.value_to_operand_and_type(dst);
        let cc = match t_op {
            t::ComparisonUnaryOperator::Not => ConditionCode::E,
        };

        let mut asm_instrs = Self::gen_cmp_vs_zero(src, src_asm_type);
        asm_instrs.extend(Self::gen_setcc(cc, dst, dst_asm_type));
        asm_instrs
    }

    pub(super) fn gen_binary_comparison_instrs(
        &mut self,
        t_op: t::ComparisonBinaryOperator,
        t::Binary { op: _, lhs, rhs, dst }: t::Binary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        use t::ComparisonBinaryOperator as TBOC;
        use ConditionCode as CC;

        let (lhs, src_var_type, src_asm_type) = self.value_to_operand_and_type(lhs);
        let rhs = self.value_to_operand(rhs);
        let (dst, _, dst_asm_type) = self.value_to_operand_and_type(dst);

        let cc_is_lg_family = match src_var_type {
            VarType::Int | VarType::Long | VarType::UInt | VarType::ULong => {
                src_var_type.is_signed()
            }
            VarType::Double => false,
        };
        let cc = match (t_op, cc_is_lg_family) {
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
            tgt: lhs,
            arg: rhs,
        }];
        asm_instrs.extend(Self::gen_setcc(cc, dst, dst_asm_type));
        asm_instrs
    }

    pub(super) fn gen_jumpif_instrs(
        &mut self,
        t::JumpIf { condition, jump_crit, lbl }: t::JumpIf,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (condition, _, asm_type) = self.value_to_operand_and_type(condition);

        let cc = match jump_crit {
            t::JumpCriterion::JumpIfZero => ConditionCode::E,
            t::JumpCriterion::JumpIfNotZero => ConditionCode::Ne,
        };

        let mut asm_instrs = Self::gen_cmp_vs_zero(condition, asm_type);
        asm_instrs.push(Instruction::JmpCC(cc, lbl));
        asm_instrs
    }

    fn gen_cmp_vs_zero(
        src: PreFinalOperand,
        src_asm_type: AssemblyType,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        match src_asm_type {
            AssemblyType::Longword | AssemblyType::Quadword => {
                vec![Instruction::Cmp {
                    asm_type: src_asm_type,
                    tgt: src,
                    arg: Operand::ImmediateValue(0).into(),
                    /* On each integer variant of the `cmp*` instr,
                    operand #2 (ie `tgt`) must eventually _not_ be an immediate value. */
                }]
            }
            AssemblyType::Double => {
                let reg = || Operand::Register(Register::XMM0).into();
                vec![
                    Instruction::Binary {
                        op: BinaryOperator::Xor,
                        asm_type: src_asm_type,
                        tgt: reg(),
                        arg: reg(),
                    },
                    Instruction::Cmp {
                        asm_type: src_asm_type,
                        tgt: reg(),
                        arg: src,
                        /* On each floating-point variant of the `cmp*` instr,
                        operand #2 (ie `tgt`) must eventually be a register. */
                    },
                ]
            }
        }
    }

    fn gen_setcc(
        cc: ConditionCode,
        dst: PreFinalOperand,
        dst_asm_type: AssemblyType, // This is expected to be Longword.
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        vec![
            Instruction::Mov {
                asm_type: dst_asm_type,
                src: Operand::ImmediateValue(0).into(),
                dst: dst.clone(),
            },
            Instruction::SetCC(cc, dst),
        ]
    }
}
