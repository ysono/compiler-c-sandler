use crate::{
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::{
        asm_ast::*,
        phase1_generate::{GeneratedAsmAst, InstrsGenerator},
    },
};

impl<'slf> InstrsGenerator<'slf> {
    /* Tacky Unary */

    pub(super) fn gen_unary_instrs(&self, t_unary: t::Unary) -> Vec<Instruction<GeneratedAsmAst>> {
        match &t_unary.op {
            t::UnaryOperator::Numeric(op) => self.gen_unary_numeric_instrs(*op, t_unary),
            t::UnaryOperator::Comparison(op) => self.gen_unary_comparison_instrs(*op, t_unary),
        }
    }
    fn gen_unary_numeric_instrs(
        &self,
        t_op: t::NumericUnaryOperator,
        t::Unary { op: _, src, dst }: t::Unary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let asm_op = match t_op {
            t::NumericUnaryOperator::Complement => UnaryOperator::BitwiseComplement,
            t::NumericUnaryOperator::Negate => UnaryOperator::TwosComplement,
        };
        let (asm_src, asm_type, _) = self.convert_value(src);
        let (asm_dst, _, _) = self.convert_value(dst);

        let asm_instr_1 = Instruction::Mov {
            asm_type,
            src: asm_src,
            dst: asm_dst.clone(),
        };
        let asm_instr_2 = Instruction::Unary(asm_op, asm_type, asm_dst);
        vec![asm_instr_1, asm_instr_2]
    }

    /* Tacky Binary */

    pub(super) fn gen_binary_instrs(
        &self,
        t_binary: t::Binary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        use t::BinaryOperator as TBO;

        match &t_binary.op {
            TBO::Arithmetic(t_op) => self.gen_arithmetic_instrs(*t_op, t_binary),
            TBO::DivRem(t_op) => self.gen_divrem_instrs(*t_op, t_binary),
            TBO::Comparison(t_op) => self.gen_comparison_instrs(*t_op, t_binary),
        }
    }
    fn gen_arithmetic_instrs(
        &self,
        t_op: t::ArithmeticBinaryOperator,
        t::Binary { op: _, src1, src2, dst }: t::Binary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let asm_op = match t_op {
            t::ArithmeticBinaryOperator::Sub => BinaryOperator::Sub,
            t::ArithmeticBinaryOperator::Add => BinaryOperator::Add,
            t::ArithmeticBinaryOperator::Mul => BinaryOperator::Mul,
        };
        let (asm_src1, asm_type, _) = self.convert_value(src1);
        let (asm_src2, _, _) = self.convert_value(src2);
        let (asm_dst, _, _) = self.convert_value(dst);

        let asm_instr_1 = Instruction::Mov {
            asm_type,
            src: asm_src1,
            dst: asm_dst.clone(),
        };
        let asm_instr_2 = Instruction::Binary {
            asm_type,
            op: asm_op,
            tgt: asm_dst,
            arg: asm_src2,
        };
        vec![asm_instr_1, asm_instr_2]
    }
    fn gen_divrem_instrs(
        &self,
        t_op: t::DivRemBinaryOperator,
        t::Binary { op: _, src1, src2, dst }: t::Binary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let ans_reg = match t_op {
            t::DivRemBinaryOperator::Div => Register::AX,
            t::DivRemBinaryOperator::Rem => Register::DX,
        };
        let (asm_src1, asm_type, is_signed) = self.convert_value(src1);
        let (asm_src2, _, _) = self.convert_value(src2);
        let (asm_dst, _, _) = self.convert_value(dst);

        let asm_instr_1 = Instruction::Mov {
            asm_type,
            src: asm_src1,
            dst: Register::AX.into(),
        };
        let asm_instr_2 = if is_signed {
            Instruction::Cdq(asm_type)
        } else {
            Instruction::Mov {
                asm_type,
                src: PreFinalOperand::ImmediateValue(0),
                dst: Register::DX.into(),
            }
        };
        let asm_instr_3 = if is_signed {
            Instruction::Idiv(asm_type, asm_src2)
        } else {
            Instruction::Div(asm_type, asm_src2)
        };
        let asm_instr_4 = Instruction::Mov {
            asm_type,
            src: ans_reg.into(),
            dst: asm_dst,
        };
        vec![asm_instr_1, asm_instr_2, asm_instr_3, asm_instr_4]
    }
}
