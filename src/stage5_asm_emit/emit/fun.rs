use super::{label::LabelLocality, AsmCodeEmitter, TAB};
use crate::{
    common::{
        identifier::SymbolIdentifier,
        symbol_table_backend::{AsmFun, AsmObj, ObjLocation},
        types_backend::{AssemblyType, OperandByteLen},
    },
    stage4_asm_gen::{asm_ast::*, FinalizedAsmAst},
};
use std::io::{self, Write};

impl<W: Write> AsmCodeEmitter<W> {
    pub(super) fn write_fun(
        &mut self,
        Function { ident, visibility, instrs }: Function<FinalizedAsmAst>,
    ) -> Result<(), io::Error> {
        self.write_symbol_visibility(&ident, visibility, LabelLocality::OF_FUN)?;
        writeln!(&mut self.w, "{TAB}.text")?;
        self.write_symbol_decl(&ident, LabelLocality::OF_FUN)?;
        writeln!(&mut self.w, "{TAB}pushq{TAB}%rbp")?;
        writeln!(&mut self.w, "{TAB}movq{TAB}%rsp, %rbp")?;
        for instr in instrs {
            self.write_instr(instr)?;
        }
        Ok(())
    }
    fn write_instr(&mut self, instr: Instruction<FinalizedAsmAst>) -> Result<(), io::Error> {
        match instr {
            Instruction::Mov { asm_type, src, dst } => {
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                self.write_instr_two_args(("mov", instr_sfx), (src, bytelen), (dst, bytelen))?;
            }
            Instruction::Movsx { src, dst } => {
                self.write_instr_two_args(
                    ("movs", "lq"),
                    (src, OperandByteLen::B4),
                    (dst, OperandByteLen::B8),
                )?;
            }
            Instruction::MovZeroExtend { .. } => {
                unreachable!("MovZeroExtend is a pre-final-only instruction.")
            }
            Instruction::Cvttsd2si { dst_asm_type, src, dst } => {
                let instr_sfx = Self::get_instr_sfx_wordlen(dst_asm_type);
                let src_bytelen = OperandByteLen::from(AssemblyType::Double);
                let dst_bytelen = OperandByteLen::from(dst_asm_type);

                self.write_instr_two_args(
                    ("cvttsd2si", instr_sfx),
                    (src, src_bytelen),
                    (dst, dst_bytelen),
                )?;
            }
            Instruction::Cvtsi2sd { src_asm_type, src, dst } => {
                let instr_sfx = Self::get_instr_sfx_wordlen(src_asm_type);
                let src_bytelen = OperandByteLen::from(src_asm_type);
                let dst_bytelen = OperandByteLen::from(AssemblyType::Double);

                self.write_instr_two_args(
                    ("cvtsi2sd", instr_sfx),
                    (src, src_bytelen),
                    (dst, dst_bytelen),
                )?;
            }
            Instruction::Unary(op, asm_type, operand) => {
                let instr = match op {
                    UnaryOperator::BitwiseComplement => "not",
                    UnaryOperator::TwosComplement => "neg",
                    UnaryOperator::Shr => "shr",
                };
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                self.write_instr_one_arg((instr, instr_sfx), (operand, bytelen))?;
            }
            Instruction::Binary { op, asm_type, arg, tgt } => {
                let regular_instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let (instr, instr_sfx);
                match asm_type {
                    AssemblyType::Longword | AssemblyType::Quadword => {
                        instr = match op {
                            BinaryOperator::Add => "add",
                            BinaryOperator::Sub => "sub",
                            BinaryOperator::Mul => "imul",
                            BinaryOperator::DivDouble => {
                                unreachable!("Invalid instr {op:?} {asm_type:?}")
                            }
                            BinaryOperator::And => "and",
                            BinaryOperator::Or => "or",
                            BinaryOperator::Xor => "xor",
                        };
                        instr_sfx = regular_instr_sfx;
                    }
                    AssemblyType::Double => {
                        (instr, instr_sfx) = match op {
                            BinaryOperator::Add => ("add", regular_instr_sfx),
                            BinaryOperator::Sub => ("sub", regular_instr_sfx),
                            BinaryOperator::Mul => ("mul", regular_instr_sfx),
                            BinaryOperator::DivDouble => ("div", regular_instr_sfx),
                            BinaryOperator::And | BinaryOperator::Or => {
                                unreachable!("Invalid instr {op:?} {asm_type:?}")
                            }
                            BinaryOperator::Xor => ("xor", "pd"),
                        };
                    }
                }
                let bytelen = OperandByteLen::from(asm_type);

                self.write_instr_two_args((instr, instr_sfx), (arg, bytelen), (tgt, bytelen))?;
            }
            Instruction::Cmp { asm_type, arg, tgt } => {
                let instr = match asm_type {
                    AssemblyType::Longword | AssemblyType::Quadword => "cmp",
                    AssemblyType::Double => "comi",
                };
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                self.write_instr_two_args((instr, instr_sfx), (arg, bytelen), (tgt, bytelen))?;
            }
            Instruction::Idiv(asm_type, operand) => {
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                self.write_instr_one_arg(("idiv", instr_sfx), (operand, bytelen))?;
            }
            Instruction::Div(asm_type, operand) => {
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                self.write_instr_one_arg(("div", instr_sfx), (operand, bytelen))?;
            }
            Instruction::Cdq(asm_type) => {
                let instr = match asm_type {
                    AssemblyType::Longword => "cdq",
                    AssemblyType::Quadword => "cqo",
                    AssemblyType::Double => unreachable!("Invalid instr cdq {asm_type:?}"),
                };

                writeln!(&mut self.w, "{TAB}{instr}")?;
            }
            Instruction::Jmp(lbl) => {
                write!(&mut self.w, "{TAB}jmp{TAB}")?;
                self.write_jump_name(&lbl)?;
                writeln!(&mut self.w)?;
            }
            Instruction::JmpCC(cc, lbl) => {
                let instr_sfx = Self::get_instr_sfx_condition(cc);

                write!(&mut self.w, "{TAB}j{instr_sfx}{TAB}")?;
                self.write_jump_name(&lbl)?;
                writeln!(&mut self.w)?;
            }
            Instruction::SetCC(cc, operand) => {
                let instr_sfx = Self::get_instr_sfx_condition(cc);

                self.write_instr_one_arg(("set", instr_sfx), (operand, OperandByteLen::B1))?;
            }
            Instruction::Label(lbl) => self.write_jump_decl(&lbl)?,
            Instruction::Push(operand) => {
                self.write_instr_one_arg(("push", "q"), (operand, OperandByteLen::B8))?;
            }
            Instruction::Call(ident) => {
                write!(&mut self.w, "{TAB}call{TAB}")?;
                self.write_symbol_name(&ident, LabelLocality::OF_FUN)?;
                self.write_fun_call_sfx(&ident)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Ret => {
                writeln!(&mut self.w, "{TAB}movq{TAB}%rbp, %rsp")?;
                writeln!(&mut self.w, "{TAB}popq{TAB}%rbp")?;
                writeln!(&mut self.w, "{TAB}ret")?;
            }
        }
        Ok(())
    }

    fn get_instr_sfx_wordlen(asm_type: AssemblyType) -> &'static str {
        match asm_type {
            AssemblyType::Longword => "l",
            AssemblyType::Quadword => "q",
            AssemblyType::Double => "sd",
        }
    }
    fn get_instr_sfx_condition(cc: ConditionCode) -> &'static str {
        match cc {
            ConditionCode::E => "e",
            ConditionCode::Ne => "ne",
            ConditionCode::L => "l",
            ConditionCode::Le => "le",
            ConditionCode::G => "g",
            ConditionCode::Ge => "ge",
            ConditionCode::A => "a",
            ConditionCode::Ae => "ae",
            ConditionCode::B => "b",
            ConditionCode::Be => "be",
        }
    }

    fn write_instr_one_arg(
        &mut self,
        instr: (&'static str, &'static str),
        operand: (Operand, OperandByteLen),
    ) -> Result<(), io::Error> {
        self.do_write_instr(instr, operand, None)
    }
    fn write_instr_two_args(
        &mut self,
        instr: (&'static str, &'static str),
        operand1: (Operand, OperandByteLen),
        operand2: (Operand, OperandByteLen),
    ) -> Result<(), io::Error> {
        self.do_write_instr(instr, operand1, Some(operand2))
    }
    fn do_write_instr(
        &mut self,
        (instr, instr_sfx): (&'static str, &'static str),
        (operand1, operand1_bytelen): (Operand, OperandByteLen),
        operand2: Option<(Operand, OperandByteLen)>,
    ) -> Result<(), io::Error> {
        write!(&mut self.w, "{TAB}{instr}{instr_sfx}{TAB}")?;
        self.write_operand(operand1, operand1_bytelen)?;
        if let Some((operand2, operand2_bytelen)) = operand2 {
            write!(&mut self.w, ", ")?;
            self.write_operand(operand2, operand2_bytelen)?;
        }
        writeln!(&mut self.w)?;
        Ok(())
    }

    fn write_operand(&mut self, operand: Operand, obl: OperandByteLen) -> Result<(), io::Error> {
        use OperandByteLen as OBL;
        match operand {
            Operand::ImmediateValue(val) => {
                write!(&mut self.w, "${val}")?;
            }
            Operand::Register(reg) => {
                let reg_str = match (reg, obl) {
                    (Register::AX, OBL::B8) => "%rax",
                    (Register::AX, OBL::B4) => "%eax",
                    (Register::AX, OBL::B1) => "%al",
                    (Register::CX, OBL::B8) => "%rcx",
                    (Register::CX, OBL::B4) => "%ecx",
                    (Register::CX, OBL::B1) => "%cl",
                    (Register::DX, OBL::B8) => "%rdx",
                    (Register::DX, OBL::B4) => "%edx",
                    (Register::DX, OBL::B1) => "%dl",
                    (Register::DI, OBL::B8) => "%rdi",
                    (Register::DI, OBL::B4) => "%edi",
                    (Register::DI, OBL::B1) => "%dil",
                    (Register::SI, OBL::B8) => "%rsi",
                    (Register::SI, OBL::B4) => "%esi",
                    (Register::SI, OBL::B1) => "%sil",
                    (Register::R8, OBL::B8) => "%r8",
                    (Register::R8, OBL::B4) => "%r8d",
                    (Register::R8, OBL::B1) => "%r8b",
                    (Register::R9, OBL::B8) => "%r9",
                    (Register::R9, OBL::B4) => "%r9d",
                    (Register::R9, OBL::B1) => "%r9b",
                    (Register::R10, OBL::B8) => "%r10",
                    (Register::R10, OBL::B4) => "%r10d",
                    (Register::R10, OBL::B1) => "%r10b",
                    (Register::R11, OBL::B8) => "%r11",
                    (Register::R11, OBL::B4) => "%r11d",
                    (Register::R11, OBL::B1) => "%r11b",
                    (Register::SP, _) => "%rsp",
                    (Register::XMM0, _) => "%xmm0",
                    (Register::XMM1, _) => "%xmm1",
                    (Register::XMM2, _) => "%xmm2",
                    (Register::XMM3, _) => "%xmm3",
                    (Register::XMM4, _) => "%xmm4",
                    (Register::XMM5, _) => "%xmm5",
                    (Register::XMM6, _) => "%xmm6",
                    (Register::XMM7, _) => "%xmm7",
                    (Register::XMM14, _) => "%xmm14",
                    (Register::XMM15, _) => "%xmm15",
                };
                write!(&mut self.w, "{reg_str}")?;
            }
            Operand::StackPosition(stkpos) => {
                write!(&mut self.w, "{}(%rbp)", *stkpos)?;
            }
            Operand::Data(ident) => {
                let AsmObj { loc, .. } = self.backend_symtab.objs().get(&ident).unwrap();
                match loc {
                    ObjLocation::Stack => {
                        unreachable!("Operand::Data cannot refer to a stack-allocated var.")
                    }
                    ObjLocation::StaticReadWrite => {
                        self.write_symbol_name(&ident, LabelLocality::OF_STATIC_VAR)?
                    }
                    ObjLocation::StaticReadonly => {
                        self.write_symbol_name(&ident, LabelLocality::OF_STATIC_CONST)?
                    }
                }
                write!(&mut self.w, "(%rip)")?;
            }
        }
        Ok(())
    }

    fn write_fun_call_sfx(&mut self, ident: &SymbolIdentifier) -> Result<(), io::Error> {
        if cfg!(target_os = "linux") {
            let AsmFun { is_defined } = self.backend_symtab.funs().get(ident).unwrap();
            if *is_defined == false {
                /* This is required iff the identifier will be lazily bound by a dynamic linker.
                It doesn't hurt to specify even if the identifier's address offset will become statically known at link-time. */
                write!(&mut self.w, "@PLT")?;
            }
        }
        Ok(())
    }
}
