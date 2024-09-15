use super::{GeneratedAsmAst, InstrsGenerator};
use crate::{
    common::{
        types_backend::{Alignment, AssemblyType},
        types_frontend::Const,
    },
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::asm_ast::*,
};

impl InstrsGenerator {
    /* Tacky Unary */

    pub(super) fn gen_unary_instrs(
        &mut self,
        t_unary: t::Unary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        match &t_unary.op {
            t::UnaryOperator::Numeric(op) => self.gen_unary_numeric_instrs(*op, t_unary),
            t::UnaryOperator::Comparison(op) => self.gen_unary_comparison_instrs(*op, t_unary),
        }
    }
    fn gen_unary_numeric_instrs(
        &mut self,
        t_op: t::NumericUnaryOperator,
        t::Unary { op: _, src, dst }: t::Unary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, asm_type) = self.value_to_operand_and_type(src);
        let dst = self.value_to_operand(dst);

        let asm_instr_1 = Instruction::Mov { asm_type, src, dst: dst.clone() };
        let asm_instr_2 = match asm_type {
            AssemblyType::Longword | AssemblyType::Quadword => {
                let asm_op = match t_op {
                    t::NumericUnaryOperator::Complement => UnaryOperator::BitwiseComplement,
                    t::NumericUnaryOperator::Negate => UnaryOperator::TwosComplement,
                };
                Instruction::Unary(asm_op, asm_type, dst)
            }
            AssemblyType::Double => {
                let asm_op = match t_op {
                    t::NumericUnaryOperator::Complement => {
                        unreachable!("Invalid operation {t_op:?} {asm_type:?}")
                    }
                    t::NumericUnaryOperator::Negate => BinaryOperator::Xor,
                };
                let neg0 = self
                    .get_or_new_static_constant_operand(Some(Alignment::B16), Const::Double(-0.0));
                Instruction::Binary {
                    asm_type,
                    op: asm_op,
                    tgt: dst,
                    arg: neg0,
                }
            }
        };
        vec![asm_instr_1, asm_instr_2]
    }

    /* Tacky Binary */

    pub(super) fn gen_binary_instrs(
        &mut self,
        t_binary: t::Binary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        use t::BinaryOperator as TBO;

        match &t_binary.op {
            TBO::Arithmetic(t_op) => self.gen_binary_arithmetic_instrs(*t_op, t_binary),
            TBO::DivRem(t_op) => self.gen_binary_divrem_instrs(*t_op, t_binary),
            TBO::Comparison(t_op) => self.gen_binary_comparison_instrs(*t_op, t_binary),
        }
    }
    fn gen_binary_arithmetic_instrs(
        &mut self,
        t_op: t::ArithmeticBinaryOperator,
        t_binary: t::Binary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let asm_op = match t_op {
            t::ArithmeticBinaryOperator::Sub => BinaryOperator::Sub,
            t::ArithmeticBinaryOperator::Add => BinaryOperator::Add,
            t::ArithmeticBinaryOperator::Mul => BinaryOperator::Mul,
        };
        self.do_gen_binary_arithmetic_instrs(asm_op, t_binary)
    }
    fn do_gen_binary_arithmetic_instrs(
        &mut self,
        asm_op: BinaryOperator,
        t::Binary { op: _, lhs, rhs, dst }: t::Binary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (lhs, _, asm_type) = self.value_to_operand_and_type(lhs);
        let rhs = self.value_to_operand(rhs);
        let dst = self.value_to_operand(dst);

        let asm_instr_1 = Instruction::Mov {
            asm_type,
            src: lhs,
            dst: dst.clone(),
        };
        let asm_instr_2 = Instruction::Binary {
            asm_type,
            op: asm_op,
            tgt: dst,
            arg: rhs,
        };
        vec![asm_instr_1, asm_instr_2]
    }
    fn gen_binary_divrem_instrs(
        &mut self,
        t_op: t::DivRemBinaryOperator,
        t_binary: t::Binary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (_, asm_type) = self.value_to_type(&t_binary.lhs);
        match asm_type {
            AssemblyType::Longword | AssemblyType::Quadword => {
                self.do_gen_integer_divrem_instrs(t_op, t_binary)
            }
            AssemblyType::Double => {
                let asm_op = match t_op {
                    t::DivRemBinaryOperator::Div => BinaryOperator::DivDouble,
                    t::DivRemBinaryOperator::Rem => {
                        unreachable!("Invalid operation {t_op:?} {t_binary:?}")
                    }
                };
                self.do_gen_binary_arithmetic_instrs(asm_op, t_binary)
            }
        }
    }
    fn do_gen_integer_divrem_instrs(
        &mut self,
        t_op: t::DivRemBinaryOperator,
        t::Binary { op: _, lhs, rhs, dst }: t::Binary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let ans_reg = match t_op {
            t::DivRemBinaryOperator::Div => Register::AX,
            t::DivRemBinaryOperator::Rem => Register::DX,
        };
        let (lhs, ari_type, asm_type) = self.value_to_operand_and_type(lhs);
        let rhs = self.value_to_operand(rhs);
        let dst = self.value_to_operand(dst);

        let asm_instr_1 = Instruction::Mov {
            asm_type,
            src: lhs,
            dst: Operand::Register(Register::AX).into(),
        };
        let asm_instr_2 = if ari_type.is_signed() {
            Instruction::Cdq(asm_type)
        } else {
            Instruction::Mov {
                asm_type,
                src: Operand::ImmediateValue(0).into(),
                dst: Operand::Register(Register::DX).into(),
            }
        };
        let asm_instr_3 = if ari_type.is_signed() {
            Instruction::Idiv(asm_type, rhs)
        } else {
            Instruction::Div(asm_type, rhs)
        };
        let asm_instr_4 = Instruction::Mov {
            asm_type,
            src: Operand::Register(ans_reg).into(),
            dst,
        };
        vec![asm_instr_1, asm_instr_2, asm_instr_3, asm_instr_4]
    }
}
