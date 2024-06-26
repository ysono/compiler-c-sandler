use crate::{
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::{asm_ast::*, phase2_finalize::InstrsFinalizer},
    symbol_table_backend::BackendSymbolTable,
    symbol_table_frontend::SymbolTable,
    types_backend::{Alignment, AssemblyType},
};
use std::cmp;
use std::rc::Rc;

pub struct AsmCodeGenerator {
    symbol_table: SymbolTable,
    backend_symbol_table: Rc<BackendSymbolTable>,
}
impl AsmCodeGenerator {
    pub fn new(symbol_table: SymbolTable, backend_symbol_table: Rc<BackendSymbolTable>) -> Self {
        Self {
            symbol_table,
            backend_symbol_table,
        }
    }

    pub fn gen_program(&self, t::Program { static_vars, funs }: t::Program) -> Program {
        let static_vars = static_vars
            .into_iter()
            .map(Self::convert_static_var)
            .collect::<Vec<_>>();

        let funs = funs
            .into_iter()
            .map(|fun| self.convert_fun(fun))
            .collect::<Vec<_>>();

        Program { static_vars, funs }
    }

    /* Tacky StaticVariable */

    fn convert_static_var(
        t::StaticVariable { ident, visibility, typ, init }: t::StaticVariable,
    ) -> StaticVariable {
        let alignment = Alignment::from(typ);
        StaticVariable {
            ident,
            visibility,
            alignment,
            init,
        }
    }

    /* Tacky Function */

    const ARG_REGS: [Register; 6] = [
        Register::DI,
        Register::SI,
        Register::DX,
        Register::CX,
        Register::R8,
        Register::R9,
    ];
    /// See documentation at [`crate::stage4_asm_gen`].
    fn convert_fun(
        &self,
        t::Function {
            ident,
            visibility,
            param_idents,
            instrs: t_instrs,
        }: t::Function,
    ) -> Function {
        let mut extra_arg_stack_pos = StackPosition(8);
        let asm_instrs_copy_args = param_idents
            .into_iter()
            .enumerate()
            .map(|(i, param_ident)| {
                let src = match Self::ARG_REGS.get(i) {
                    Some(reg) => PreFinalOperand::Register(*reg),
                    None => {
                        *extra_arg_stack_pos += 8;
                        PreFinalOperand::StackPosition(extra_arg_stack_pos)
                    }
                };
                let (dst, asm_type, _) = self.convert_value(param_ident);
                Instruction::Mov { asm_type, src, dst }
            });

        let asm_instrs_body = self.gen_instructions(t_instrs);

        let asm_instrs = asm_instrs_copy_args.chain(asm_instrs_body);

        let mut fin = InstrsFinalizer::new(Rc::clone(&self.backend_symbol_table));
        let asm_instrs = fin.finalize_instrs(asm_instrs);

        Function {
            ident,
            visibility,
            instrs: asm_instrs,
        }
    }
    /// See documentation at [`crate::stage4_asm_gen`].
    fn gen_funcall_instrs(
        &self,
        t::FunCall { ident, args, dst }: t::FunCall,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let mut asm_instrs = vec![];

        let reg_args_count = cmp::min(Self::ARG_REGS.len(), args.len());
        let stack_args_count = args.len() - reg_args_count;

        let stack_padding_bytelen = if (stack_args_count & 1) == 1 { 8 } else { 0 };

        if stack_padding_bytelen != 0 {
            asm_instrs.push(Instruction::Binary {
                op: BinaryOperator::Sub,
                asm_type: AssemblyType::Quadword,
                tgt: Register::SP.into(),
                arg: PreFinalOperand::ImmediateValue(stack_padding_bytelen),
            });
        }

        for (i, arg_val) in args.into_iter().enumerate().rev() {
            let (arg_operand, arg_asm_type, _) = self.convert_value(arg_val);
            match Self::ARG_REGS.get(i) {
                None => match (&arg_operand, arg_asm_type) {
                    (PreFinalOperand::ImmediateValue(_) | PreFinalOperand::Register(_), _)
                    | (_, AssemblyType::Quadword) => {
                        asm_instrs.push(Instruction::Push(arg_operand));
                    }
                    (
                        PreFinalOperand::Pseudo(_) | PreFinalOperand::StackPosition(_),
                        AssemblyType::Longword,
                    ) => {
                        /* `pushq` operation always reads and pushes 8 bytes.
                        In case (arg's memory address + (8-1)) lies outside the readable memory, we cannot `pushq` directly. */
                        asm_instrs.push(Instruction::Mov {
                            asm_type: AssemblyType::Longword,
                            src: arg_operand,
                            dst: Register::AX.into(),
                        });
                        asm_instrs.push(Instruction::Push(Register::AX.into()));
                    }
                },
                Some(reg) => {
                    asm_instrs.push(Instruction::Mov {
                        asm_type: arg_asm_type,
                        src: arg_operand,
                        dst: (*reg).into(),
                    });
                }
            }
        }

        asm_instrs.push(Instruction::Call(ident));

        let stack_pop_bytelen = 8 * (stack_args_count as i64) + stack_padding_bytelen;
        if stack_pop_bytelen != 0 {
            asm_instrs.push(Instruction::Binary {
                op: BinaryOperator::Add,
                asm_type: AssemblyType::Quadword,
                tgt: Register::SP.into(),
                arg: PreFinalOperand::ImmediateValue(stack_pop_bytelen),
            });
        }

        let (dst, dst_type, _) = self.convert_value(dst);
        asm_instrs.push(Instruction::Mov {
            asm_type: dst_type,
            src: Register::AX.into(),
            dst,
        });

        asm_instrs
    }

    /* Tacky Instruction */

    fn gen_instructions(
        &self,
        t_instrs: Vec<t::Instruction>,
    ) -> impl '_ + Iterator<Item = Instruction<PreFinalOperand>> {
        t_instrs.into_iter().flat_map(|t_instr| match t_instr {
            t::Instruction::Return(t_val) => self.gen_return_instrs(t_val),
            t::Instruction::SignExtend(t_srcdst) => self.gen_signextend_instrs(t_srcdst),
            t::Instruction::Truncate(t_srcdst) => self.gen_truncate_instrs(t_srcdst),
            t::Instruction::ZeroExtend(t_srcdst) => self.gen_zeroextend_instrs(t_srcdst),
            t::Instruction::Unary(t_unary) => self.gen_unary_instrs(t_unary),
            t::Instruction::Binary(t_binary) => self.gen_binary_instrs(t_binary),
            t::Instruction::Copy(t_srcdst) => self.gen_copy_instrs(t_srcdst),
            t::Instruction::Jump(lbl) => vec![Instruction::Jmp(lbl)],
            t::Instruction::JumpIfZero(t_jumpif) => {
                self.gen_jumpif_instrs(ConditionCode::E, t_jumpif)
            }
            t::Instruction::JumpIfNotZero(t_jumpif) => {
                self.gen_jumpif_instrs(ConditionCode::Ne, t_jumpif)
            }
            t::Instruction::Label(lbl) => vec![Instruction::Label(lbl)],
            t::Instruction::FunCall(t_fun_call) => self.gen_funcall_instrs(t_fun_call),
        })
    }

    /* Tacky Return */

    fn gen_return_instrs(&self, t_val: t::ReadableValue) -> Vec<Instruction<PreFinalOperand>> {
        let (src, asm_type, _) = self.convert_value(t_val);
        let asm_instr_1 = Instruction::Mov {
            asm_type,
            src,
            dst: Register::AX.into(),
        };

        let asm_instr_2 = Instruction::Ret;

        vec![asm_instr_1, asm_instr_2]
    }

    /* Tacky assembly type transformation */

    fn gen_signextend_instrs(
        &self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let (src, _, _) = self.convert_value(src);
        let (dst, _, _) = self.convert_value(dst);
        vec![Instruction::Movsx { src, dst }]
    }
    fn gen_truncate_instrs(
        &self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let (src, _, _) = self.convert_value(src);
        let (dst, _, _) = self.convert_value(dst);
        vec![Instruction::Mov {
            asm_type: AssemblyType::Longword,
            src,
            dst,
        }]
    }
    fn gen_zeroextend_instrs(
        &self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let (src, _, _) = self.convert_value(src);
        let (dst, _, _) = self.convert_value(dst);
        vec![Instruction::MovZeroExtend { src, dst }]
    }

    /* Tacky Unary */

    fn gen_unary_instrs(&self, t_unary: t::Unary) -> Vec<Instruction<PreFinalOperand>> {
        use t::UnaryOperator as TUO;

        match t_unary.op {
            TUO::Complement => {
                self.gen_unary_numeric_instrs(UnaryOperator::BitwiseComplement, t_unary)
            }
            TUO::Negate => self.gen_unary_numeric_instrs(UnaryOperator::TwosComplement, t_unary),
            TUO::Not => self.gen_unary_comparison_instrs(t_unary),
        }
    }
    fn gen_unary_numeric_instrs(
        &self,
        asm_op: UnaryOperator,
        t::Unary { op: _, src, dst }: t::Unary,
    ) -> Vec<Instruction<PreFinalOperand>> {
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
    fn gen_unary_comparison_instrs(
        &self,
        t::Unary { op: _, src, dst }: t::Unary,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let (asm_src, asm_src_type, _) = self.convert_value(src);
        let (asm_dst, asm_dst_type, _) = self.convert_value(dst);

        Self::gen_comparison_instrs_from_asm(
            asm_src_type,
            asm_src,
            PreFinalOperand::ImmediateValue(0),
            ConditionCode::E,
            asm_dst_type,
            asm_dst,
        )
    }

    /* Tacky Binary */

    fn gen_binary_instrs(&self, t_binary: t::Binary) -> Vec<Instruction<PreFinalOperand>> {
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
    ) -> Vec<Instruction<PreFinalOperand>> {
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
    ) -> Vec<Instruction<PreFinalOperand>> {
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
    fn gen_comparison_instrs(
        &self,
        t_op: t::ComparisonBinaryOperator,
        t::Binary { op: _, src1, src2, dst }: t::Binary,
    ) -> Vec<Instruction<PreFinalOperand>> {
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
    ) -> Vec<Instruction<PreFinalOperand>> {
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

    /* Tacky Copy */

    fn gen_copy_instrs(
        &self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let (src, asm_type, _) = self.convert_value(src);
        let (dst, _, _) = self.convert_value(dst);
        vec![Instruction::Mov { asm_type, src, dst }]
    }

    /* Tacky Jump */

    fn gen_jumpif_instrs(
        &self,
        cmp_0_cc: ConditionCode,
        t::JumpIf { condition, tgt }: t::JumpIf,
    ) -> Vec<Instruction<PreFinalOperand>> {
        let (condition, asm_type, _) = self.convert_value(condition);
        let asm_instr_1 = Instruction::Cmp {
            asm_type,
            tgt: condition,
            arg: PreFinalOperand::ImmediateValue(0),
        };
        let asm_instr_2 = Instruction::JmpCC(cmp_0_cc, tgt);
        vec![asm_instr_1, asm_instr_2]
    }

    /* Tacky Value -> Asm Operand and other info */

    fn convert_value<V: Into<t::ReadableValue>>(
        &self,
        t_val: V,
    ) -> (PreFinalOperand, AssemblyType, bool) {
        match t_val.into() {
            t::ReadableValue::Constant(konst) => {
                let operand = PreFinalOperand::ImmediateValue(konst.as_raw());

                let var_type = konst.var_type();
                let asm_type = AssemblyType::from(var_type);
                let is_signed = var_type.is_signed();

                (operand, asm_type, is_signed)
            }
            t::ReadableValue::Variable(ident) => {
                let var_type = self.symbol_table.use_var(&ident).unwrap();
                let asm_type = AssemblyType::from(var_type);
                let is_signed = var_type.is_signed();

                let operand = PreFinalOperand::Pseudo(ident);

                (operand, asm_type, is_signed)
            }
        }
    }
}
