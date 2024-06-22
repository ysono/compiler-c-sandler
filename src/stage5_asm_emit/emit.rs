use crate::{
    files::AsmFilepath,
    stage4_asm_gen::asm_ast::{
        BinaryOperator, ConditionCode, Const, Function, Instruction, LabelIdentifier, Operand,
        Program, Register, StaticVariable, UnaryOperator,
    },
    symbol_table::{ResolvedIdentifier, StaticVisibility},
    symbol_table_backend::{AsmEntry, AssemblyType, BackendSymbolTable, OperandByteLen},
};
use regex::Regex;
use std::fs::{File, OpenOptions};
use std::io::{self, BufWriter, Write};
use std::path::PathBuf;

const TAB: &str = "\t";

pub struct AsmCodeEmitter<'slf> {
    label_bad_char: Regex,

    bw: BufWriter<File>,

    backend_symbol_table: &'slf BackendSymbolTable,
}
impl<'slf> AsmCodeEmitter<'slf> {
    pub fn new<'a>(
        asm_filepath: &'a AsmFilepath,
        backend_symbol_table: &'slf BackendSymbolTable,
    ) -> Result<Self, io::Error> {
        let label_bad_char = Regex::new(r"[^a-zA-Z0-9._]").unwrap();

        let f = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(asm_filepath as &PathBuf)?;
        let bw = BufWriter::new(f);

        Ok(Self {
            label_bad_char,
            bw,
            backend_symbol_table,
        })
    }

    pub fn emit_program(mut self, Program { funs, vars }: Program) -> Result<(), io::Error> {
        for fun in funs {
            self.write_fun(fun)?;
        }

        for var in vars {
            self.write_static_var(var)?;
        }

        if cfg!(target_os = "linux") {
            writeln!(&mut self.bw, "{TAB}.section	.note.GNU-stack,\"\",@progbits")?;
        }

        self.bw.flush()?;
        Ok(())
    }
    fn write_fun(
        &mut self,
        Function {
            ident,
            visibility,
            instrs,
        }: Function,
    ) -> Result<(), io::Error> {
        match visibility {
            StaticVisibility::Global => {
                write!(&mut self.bw, "{TAB}.globl{TAB}")?;
                self.write_fun_name(&ident)?;
                writeln!(&mut self.bw)?;
            }
            StaticVisibility::NonGlobal => { /* No-op. */ }
        }
        writeln!(&mut self.bw, "{TAB}.text")?;
        self.write_fun_name(&ident)?;
        writeln!(&mut self.bw, ":")?;
        writeln!(&mut self.bw, "{TAB}pushq{TAB}%rbp")?;
        writeln!(&mut self.bw, "{TAB}movq{TAB}%rsp, %rbp")?;
        for instr in instrs {
            self.write_instr(instr)?;
        }
        Ok(())
    }
    fn write_instr(&mut self, instr: Instruction<Operand>) -> Result<(), io::Error> {
        match instr {
            Instruction::Mov { asm_type, src, dst } => {
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                write!(&mut self.bw, "{TAB}mov{instr_sfx}{TAB}")?;
                self.write_operand(src, bytelen)?;
                write!(&mut self.bw, ", ")?;
                self.write_operand(dst, bytelen)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::Movsx { src, dst } => {
                let instr_sfx = "lq";

                write!(&mut self.bw, "{TAB}movs{instr_sfx}{TAB}")?;
                self.write_operand(src, OperandByteLen::B4)?;
                write!(&mut self.bw, ", ")?;
                self.write_operand(dst, OperandByteLen::B8)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::Unary(op, asm_type, operand) => {
                let instr = match op {
                    UnaryOperator::BitwiseComplement => "not",
                    UnaryOperator::TwosComplement => "neg",
                };
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                write!(&mut self.bw, "{TAB}{instr}{instr_sfx}{TAB}")?;
                self.write_operand(operand, bytelen)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::Binary {
                op,
                asm_type,
                arg,
                tgt,
            } => {
                let instr = match op {
                    BinaryOperator::Add => "add",
                    BinaryOperator::Sub => "sub",
                    BinaryOperator::Mul => "imul",
                };
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                write!(&mut self.bw, "{TAB}{instr}{instr_sfx}{TAB}")?;
                self.write_operand(arg, bytelen)?;
                write!(&mut self.bw, ", ")?;
                self.write_operand(tgt, bytelen)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::Cmp { asm_type, arg, tgt } => {
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                write!(&mut self.bw, "{TAB}cmp{instr_sfx}{TAB}")?;
                self.write_operand(arg, bytelen)?;
                write!(&mut self.bw, ", ")?;
                self.write_operand(tgt, bytelen)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::Idiv(asm_type, operand) => {
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                write!(&mut self.bw, "{TAB}idiv{instr_sfx}{TAB}")?;
                self.write_operand(operand, bytelen)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::Cdq(asm_type) => {
                let instr = match asm_type {
                    AssemblyType::Longword => "cdq",
                    AssemblyType::Quadword => "cqo",
                };

                writeln!(&mut self.bw, "{TAB}{instr}")?;
            }
            Instruction::Jmp(lbl) => {
                write!(&mut self.bw, "{TAB}jmp{TAB}")?;
                self.write_label(&lbl)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::JmpCC(cc, lbl) => {
                let instr_sfx = Self::get_instr_sfx_condition(cc);
                write!(&mut self.bw, "{TAB}j{instr_sfx}{TAB}")?;
                self.write_label(&lbl)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::SetCC(cc, operand) => {
                let instr_sfx = Self::get_instr_sfx_condition(cc);
                write!(&mut self.bw, "{TAB}set{instr_sfx}{TAB}")?;
                self.write_operand(operand, OperandByteLen::B1)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::Label(lbl) => {
                self.write_label(&lbl)?;
                writeln!(&mut self.bw, ":")?;
            }
            Instruction::Push(operand) => {
                write!(&mut self.bw, "{TAB}pushq{TAB}")?;
                self.write_operand(operand, OperandByteLen::B8)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::Call(ident) => {
                write!(&mut self.bw, "{TAB}call{TAB}")?;
                self.write_fun_name(&ident)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::Ret => {
                writeln!(&mut self.bw, "{TAB}movq{TAB}%rbp, %rsp")?;
                writeln!(&mut self.bw, "{TAB}popq{TAB}%rbp")?;
                writeln!(&mut self.bw, "{TAB}ret")?;
            }
        }
        Ok(())
    }
    fn get_instr_sfx_wordlen(asm_type: AssemblyType) -> char {
        match asm_type {
            AssemblyType::Longword => 'l',
            AssemblyType::Quadword => 'q',
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
        }
    }
    fn write_operand(&mut self, operand: Operand, obl: OperandByteLen) -> Result<(), io::Error> {
        use OperandByteLen as OBL;
        match operand {
            Operand::ImmediateValue(val) => {
                write!(&mut self.bw, "${val}")?;
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
                };
                write!(&mut self.bw, "{reg_str}")?;
            }
            Operand::StackPosition(stkpos) => {
                write!(&mut self.bw, "{}(%rbp)", *stkpos)?;
            }
            Operand::Data(ident) => {
                write!(&mut self.bw, "{ident}(%rip)")?;
            }
        }
        Ok(())
    }
    fn write_label(&mut self, lbl: &LabelIdentifier) -> Result<(), io::Error> {
        const NAME_PFX: &str = if cfg!(target_os = "macos") {
            "L."
        } else {
            ".L."
        };

        let name = self.label_bad_char.replace_all(lbl.name(), "_");

        let id = lbl.id();

        write!(&mut self.bw, "{NAME_PFX}{name}.{id}")?;

        Ok(())
    }
    fn write_fun_name(&mut self, ident: &ResolvedIdentifier) -> Result<(), io::Error> {
        const IDENT_PFX: &str = if cfg!(target_os = "macos") { "_" } else { "" };

        write!(&mut self.bw, "{IDENT_PFX}{ident}")?;

        if cfg!(target_os = "linux") {
            match self.backend_symbol_table.get(ident).unwrap() {
                AsmEntry::Obj { .. } => { /* No-op. */ }
                AsmEntry::Fun { is_defined } => {
                    if *is_defined == false {
                        /* This is required iff the identifier will be lazily bound by a dynamic linker.
                        It doesn't hurt to specify even if the identifier's address offset will become statically known at link-time. */
                        write!(&mut self.bw, "@PLT")?;
                    }
                }
            }
        }

        Ok(())
    }
    fn write_static_var(
        &mut self,
        StaticVariable {
            ident,
            visibility,
            alignment,
            init,
        }: StaticVariable,
    ) -> Result<(), io::Error> {
        const IDENT_PFX: &str = if cfg!(target_os = "macos") { "_" } else { "" };
        let alignment = alignment as u8;
        let section = match init {
            Const::Int(0) => ".bss",
            Const::Int(_) => ".data",
            Const::Long(0) => ".bss",
            Const::Long(_) => ".data",
        };

        match visibility {
            StaticVisibility::Global => {
                writeln!(&mut self.bw, "{TAB}.globl{TAB}{IDENT_PFX}{ident}")?;
            }
            StaticVisibility::NonGlobal => { /* No-op. */ }
        }
        writeln!(&mut self.bw, "{TAB}{section}")?;
        writeln!(&mut self.bw, "{TAB}.balign {alignment}")?;
        writeln!(&mut self.bw, "{IDENT_PFX}{ident}:")?;
        match init {
            Const::Int(0) => {
                writeln!(&mut self.bw, "{TAB}.zero 4")?;
            }
            Const::Int(i) => {
                writeln!(&mut self.bw, "{TAB}.long {i}")?;
            }
            Const::Long(0) => {
                writeln!(&mut self.bw, "{TAB}.zero 8")?;
            }
            Const::Long(i) => {
                writeln!(&mut self.bw, "{TAB}.quad {i}")?;
            }
        };
        Ok(())
    }
}
