use crate::{
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::{
        asm_ast::*,
        phase1_generate::{GeneratedAsmAst, InstrsGenerator},
    },
    types_backend::{Alignment, AssemblyType},
    types_frontend::{Const, VarType},
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
        let (asm_src, _, asm_type) = self.convert_value(src);
        let (asm_dst, _, _) = self.convert_value(dst);

        let asm_instr_1 = Instruction::Mov {
            asm_type,
            src: asm_src,
            dst: asm_dst.clone(),
        };
        let asm_instr_2 = match asm_type {
            AssemblyType::Longword | AssemblyType::Quadword => {
                let asm_op = match t_op {
                    t::NumericUnaryOperator::Complement => UnaryOperator::BitwiseComplement,
                    t::NumericUnaryOperator::Negate => UnaryOperator::TwosComplement,
                };
                Instruction::Unary(asm_op, asm_type, asm_dst)
            }
            AssemblyType::Double => {
                let asm_op = match t_op {
                    t::NumericUnaryOperator::Complement => {
                        panic!("Invalid operation {t_op:?} {asm_type:?}")
                    }
                    t::NumericUnaryOperator::Negate => BinaryOperator::Xor,
                };
                let neg0 = self
                    .get_or_new_static_constant_operand(Some(Alignment::B16), Const::Double(-0.0));
                Instruction::Binary {
                    asm_type,
                    op: asm_op,
                    arg: neg0,
                    tgt: asm_dst,
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
        t::Binary { op: _, src1, src2, dst }: t::Binary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (asm_src1, _, asm_type) = self.convert_value(src1);
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
    fn gen_binary_divrem_instrs(
        &mut self,
        t_op: t::DivRemBinaryOperator,
        t_binary: t::Binary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let var_type = self.frontend_symtab.use_var(&t_binary.dst).unwrap();
        match var_type {
            VarType::Int | VarType::Long | VarType::UInt | VarType::ULong => {
                self.do_gen_integer_divrem_instrs(t_op, t_binary)
            }
            VarType::Double => {
                let asm_op = match t_op {
                    t::DivRemBinaryOperator::Div => BinaryOperator::DivDouble,
                    t::DivRemBinaryOperator::Rem => {
                        panic!("Invalid operation {t_op:?} {var_type:?}")
                    }
                };
                self.do_gen_binary_arithmetic_instrs(asm_op, t_binary)
            }
        }
    }
    fn do_gen_integer_divrem_instrs(
        &mut self,
        t_op: t::DivRemBinaryOperator,
        t::Binary { op: _, src1, src2, dst }: t::Binary,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let ans_reg = match t_op {
            t::DivRemBinaryOperator::Div => Register::AX,
            t::DivRemBinaryOperator::Rem => Register::DX,
        };
        let (asm_src1, var_type, asm_type) = self.convert_value(src1);
        let (asm_src2, _, _) = self.convert_value(src2);
        let (asm_dst, _, _) = self.convert_value(dst);

        let asm_instr_1 = Instruction::Mov {
            asm_type,
            src: asm_src1,
            dst: PreFinalOperand::Register(Register::AX),
        };
        let asm_instr_2 = if var_type.is_signed() {
            Instruction::Cdq(asm_type)
        } else {
            Instruction::Mov {
                asm_type,
                src: PreFinalOperand::ImmediateValue(0),
                dst: PreFinalOperand::Register(Register::DX),
            }
        };
        let asm_instr_3 = if var_type.is_signed() {
            Instruction::Idiv(asm_type, asm_src2)
        } else {
            Instruction::Div(asm_type, asm_src2)
        };
        let asm_instr_4 = Instruction::Mov {
            asm_type,
            src: PreFinalOperand::Register(ans_reg),
            dst: asm_dst,
        };
        vec![asm_instr_1, asm_instr_2, asm_instr_3, asm_instr_4]
    }
}
