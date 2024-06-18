use crate::{
    files::AsmFilepath,
    stage4_asm_gen::asm_ast::{
        BinaryOperator, ConditionCode, Function, Instruction, LabelIdentifier, Operand, Program,
        Register, UnaryOperator,
    },
    symbol_table::{ResolvedIdentifier, SymbolTable, SymbolType},
};
use lazy_static::lazy_static;
use regex::Regex;
use std::fs::{File, OpenOptions};
use std::io::{self, BufWriter, Write};
use std::path::PathBuf;

const TAB: &str = "\t";

lazy_static! {
    pub static ref LABEL_BAD_CHAR: Regex = Regex::new(r"[^a-zA-Z0-9._]").unwrap();
}

enum OperandByteLen {
    B8,
    B4,
    B1,
}

pub struct AsmCodeEmitter<'slf> {
    bw: BufWriter<File>,

    symbol_table: &'slf SymbolTable,
}
impl<'slf> AsmCodeEmitter<'slf> {
    pub fn new<'a>(
        asm_filepath: &'a AsmFilepath,
        symbol_table: &'slf SymbolTable,
    ) -> Result<Self, io::Error> {
        let f = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(asm_filepath as &PathBuf)?;
        let bw = BufWriter::new(f);
        Ok(Self { bw, symbol_table })
    }

    pub fn emit_program(mut self, Program { funs }: Program) -> Result<(), io::Error> {
        for fun in funs {
            self.write_fun(fun)?;
        }

        if cfg!(target_os = "linux") {
            writeln!(&mut self.bw, "{TAB}.section	.note.GNU-stack,\"\",@progbits")?;
        }

        self.bw.flush()?;
        Ok(())
    }
    fn write_fun(&mut self, Function { ident, instrs }: Function) -> Result<(), io::Error> {
        write!(&mut self.bw, "{TAB}.globl{TAB}")?;
        self.write_fun_name(&ident)?;
        writeln!(&mut self.bw)?;
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
            Instruction::Mov { src, dst } => {
                write!(&mut self.bw, "{TAB}movl{TAB}")?;
                self.write_operand(src, OperandByteLen::B4)?;
                write!(&mut self.bw, ", ")?;
                self.write_operand(dst, OperandByteLen::B4)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::Unary(op, operand) => {
                let op = match op {
                    UnaryOperator::BitwiseComplement => "notl",
                    UnaryOperator::TwosComplement => "negl",
                };
                write!(&mut self.bw, "{TAB}{op}{TAB}")?;
                self.write_operand(operand, OperandByteLen::B4)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::Binary { op, arg, tgt } => {
                let op = match op {
                    BinaryOperator::Add => "addl",
                    BinaryOperator::Sub => "subl",
                    BinaryOperator::Mul => "imull",
                };
                write!(&mut self.bw, "{TAB}{op}{TAB}")?;
                self.write_operand(arg, OperandByteLen::B4)?;
                write!(&mut self.bw, ", ")?;
                self.write_operand(tgt, OperandByteLen::B4)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::Cmp { arg, tgt } => {
                write!(&mut self.bw, "{TAB}cmpl{TAB}")?;
                self.write_operand(arg, OperandByteLen::B4)?;
                write!(&mut self.bw, ", ")?;
                self.write_operand(tgt, OperandByteLen::B4)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::Idiv(operand) => {
                write!(&mut self.bw, "{TAB}idivl{TAB}")?;
                self.write_operand(operand, OperandByteLen::B4)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::Cdq => {
                writeln!(&mut self.bw, "cdq")?;
            }
            Instruction::Jmp(lbl) => {
                write!(&mut self.bw, "{TAB}jmp{TAB}")?;
                self.write_label(&lbl)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::JmpCC(cc, lbl) => {
                let cmd_sfx = Self::get_condition_sfx(cc);
                write!(&mut self.bw, "{TAB}j{cmd_sfx}{TAB}")?;
                self.write_label(&lbl)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::SetCC(cc, operand) => {
                let cmd_sfx = Self::get_condition_sfx(cc);
                write!(&mut self.bw, "{TAB}set{cmd_sfx}{TAB}")?;
                self.write_operand(operand, OperandByteLen::B1)?;
                writeln!(&mut self.bw)?;
            }
            Instruction::Label(lbl) => {
                self.write_label(&lbl)?;
                writeln!(&mut self.bw, ":")?;
            }
            Instruction::AllocateStack(stkpos) => {
                writeln!(&mut self.bw, "{TAB}subq{TAB}${}, %rsp", *stkpos)?;
            }
            Instruction::DeallocateStack(stkpos) => {
                writeln!(&mut self.bw, "{TAB}addq{TAB}${}, %rsp", *stkpos)?;
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
                };
                write!(&mut self.bw, "{reg_str}")?;
            }
            Operand::StackPosition(stkpos) => {
                write!(&mut self.bw, "{}(%rbp)", *stkpos)?;
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

        let name = LABEL_BAD_CHAR.replace_all(lbl.name(), "_");

        let id = lbl.id();

        write!(&mut self.bw, "{NAME_PFX}{name}.{id}")?;

        Ok(())
    }
    fn write_fun_name(&mut self, ident: &ResolvedIdentifier) -> Result<(), io::Error> {
        const IDENT_PFX: &str = if cfg!(target_os = "macos") { "_" } else { "" };

        match ident {
            ResolvedIdentifier::NoLinkage { id, orig } => {
                /* This case should never be encountered. */
                let ident = orig.as_ref().map(|s| s.as_str()).unwrap_or("");
                let id = id.as_int();
                write!(&mut self.bw, "{IDENT_PFX}UNEXPECTED.{ident}.{id:x}")?;
            }
            ResolvedIdentifier::ExternalLinkage(ident) => {
                let ident: &String = ident;
                write!(&mut self.bw, "{IDENT_PFX}{ident}")?;
            }
        }

        if cfg!(target_os = "linux") {
            match self.symbol_table.get(ident).unwrap() {
                SymbolType::Var { .. } => { /* No-op. */ }
                SymbolType::Function { is_defined, .. } => {
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
    fn get_condition_sfx(cc: ConditionCode) -> &'static str {
        match cc {
            ConditionCode::E => "e",
            ConditionCode::Ne => "ne",
            ConditionCode::L => "l",
            ConditionCode::Le => "le",
            ConditionCode::G => "g",
            ConditionCode::Ge => "ge",
        }
    }
}
