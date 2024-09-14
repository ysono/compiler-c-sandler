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

                write!(&mut self.w, "{TAB}mov{instr_sfx}{TAB}")?;
                self.write_operand(src, bytelen)?;
                write!(&mut self.w, ", ")?;
                self.write_operand(dst, bytelen)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Movsx { src, dst } => {
                let instr_sfx = "lq";

                write!(&mut self.w, "{TAB}movs{instr_sfx}{TAB}")?;
                self.write_operand(src, OperandByteLen::B4)?;
                write!(&mut self.w, ", ")?;
                self.write_operand(dst, OperandByteLen::B8)?;
                writeln!(&mut self.w)?;
            }
            Instruction::MovZeroExtend { .. } => { /* No-op, b/c this instruction type is strictly pre-final. */
            }
            Instruction::Cvttsd2si { dst_asm_type, src, dst } => {
                let instr_sfx = Self::get_instr_sfx_wordlen(dst_asm_type);
                let src_bytelen = OperandByteLen::from(AssemblyType::Double);
                let dst_bytelen = OperandByteLen::from(dst_asm_type);

                write!(&mut self.w, "{TAB}cvttsd2si{instr_sfx}{TAB}")?;
                self.write_operand(src, src_bytelen)?;
                write!(&mut self.w, ", ")?;
                self.write_operand(dst, dst_bytelen)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Cvtsi2sd { src_asm_type, src, dst } => {
                let instr_sfx = Self::get_instr_sfx_wordlen(src_asm_type);
                let src_bytelen = OperandByteLen::from(src_asm_type);
                let dst_bytelen = OperandByteLen::from(AssemblyType::Double);

                write!(&mut self.w, "{TAB}cvtsi2sd{instr_sfx}{TAB}")?;
                self.write_operand(src, src_bytelen)?;
                write!(&mut self.w, ", ")?;
                self.write_operand(dst, dst_bytelen)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Unary(op, asm_type, operand) => {
                let instr = match op {
                    UnaryOperator::BitwiseComplement => "not",
                    UnaryOperator::TwosComplement => "neg",
                    UnaryOperator::Shr => "shr",
                };
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                write!(&mut self.w, "{TAB}{instr}{instr_sfx}{TAB}")?;
                self.write_operand(operand, bytelen)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Binary { op, asm_type, arg, tgt } => {
                let (instr, instr_sfx);
                match asm_type {
                    AssemblyType::Longword | AssemblyType::Quadword => {
                        instr = match op {
                            BinaryOperator::Add => "add",
                            BinaryOperator::Sub => "sub",
                            BinaryOperator::Mul => "imul",
                            BinaryOperator::DivDouble => {
                                panic!("Invalid instr {op:?} {asm_type:?}")
                            }
                            BinaryOperator::And => "and",
                            BinaryOperator::Or => "or",
                            BinaryOperator::Xor => "xor",
                        };
                        instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                    }
                    AssemblyType::Double => {
                        instr = match op {
                            BinaryOperator::Add => "addsd",
                            BinaryOperator::Sub => "subsd",
                            BinaryOperator::Mul => "mulsd",
                            BinaryOperator::DivDouble => "divsd",
                            BinaryOperator::And => panic!("Invalid instr {op:?} {asm_type:?}"),
                            BinaryOperator::Or => panic!("Invalid instr {op:?} {asm_type:?}"),
                            BinaryOperator::Xor => "xorpd",
                        };
                        instr_sfx = "";
                    }
                }
                let bytelen = OperandByteLen::from(asm_type);

                /* Begin emission */

                write!(&mut self.w, "{TAB}{instr}{instr_sfx}{TAB}")?;
                self.write_operand(arg, bytelen)?;
                write!(&mut self.w, ", ")?;
                self.write_operand(tgt, bytelen)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Cmp { asm_type, arg, tgt } => {
                let (instr, instr_sfx) = match asm_type {
                    AssemblyType::Longword | AssemblyType::Quadword => {
                        ("cmp", Self::get_instr_sfx_wordlen(asm_type))
                    }
                    AssemblyType::Double => ("comisd", ""),
                };
                let bytelen = OperandByteLen::from(asm_type);

                write!(&mut self.w, "{TAB}{instr}{instr_sfx}{TAB}")?;
                self.write_operand(arg, bytelen)?;
                write!(&mut self.w, ", ")?;
                self.write_operand(tgt, bytelen)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Idiv(asm_type, operand) => {
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                write!(&mut self.w, "{TAB}idiv{instr_sfx}{TAB}")?;
                self.write_operand(operand, bytelen)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Div(asm_type, operand) => {
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                write!(&mut self.w, "{TAB}div{instr_sfx}{TAB}")?;
                self.write_operand(operand, bytelen)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Cdq(asm_type) => {
                let instr = match asm_type {
                    AssemblyType::Longword => "cdq",
                    AssemblyType::Quadword => "cqo",
                    AssemblyType::Double => panic!("Invalid instr cdq {asm_type:?}"),
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
                write!(&mut self.w, "{TAB}set{instr_sfx}{TAB}")?;
                self.write_operand(operand, OperandByteLen::B1)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Label(lbl) => self.write_jump_decl(&lbl)?,
            Instruction::Push(operand) => {
                write!(&mut self.w, "{TAB}pushq{TAB}")?;
                self.write_operand(operand, OperandByteLen::B8)?;
                writeln!(&mut self.w)?;
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
                        panic!("Operand::Data cannot refer to a stack-allocated var.")
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
