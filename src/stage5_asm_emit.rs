use crate::{
    files::AsmFilepath,
    stage3_tacky::tacky_ir::LabelIdentifier,
    stage4_asm_gen::asm_code::{
        BinaryOperator, ConditionCode, Function, Instruction, Operand, Program, Register,
        UnaryOperator,
    },
};
use anyhow::Result;
use lazy_static::lazy_static;
use regex::Regex;
use std::fs::{File, OpenOptions};
use std::io::{BufWriter, Write};
use std::path::PathBuf;

const TAB: &str = "\t";

lazy_static! {
    pub static ref LABEL_BAD_CHAR: Regex = Regex::new(r"[^a-zA-Z0-9._]").unwrap();
}

enum OperandByteLen {
    B4,
    B1,
}

pub struct AsmCodeEmitter {
    bw: BufWriter<File>,
}
impl<'a> TryFrom<&'a AsmFilepath> for AsmCodeEmitter {
    type Error = anyhow::Error;
    fn try_from(asm_filepath: &'a AsmFilepath) -> Result<Self> {
        let f = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(&asm_filepath as &PathBuf)?;
        let bw = BufWriter::new(f);
        Ok(Self { bw })
    }
}
impl AsmCodeEmitter {
    pub fn emit_program(mut self, prog: Program<Operand>) -> Result<()> {
        let Program { func } = prog;

        self.write_func(func)?;

        if cfg!(target_os = "linux") {
            writeln!(&mut self.bw, "{TAB}.section	.note.GNU-stack,\"\",@progbits")?;
        }

        self.bw.flush()?;
        Ok(())
    }
    fn write_func(&mut self, func: Function<Operand>) -> Result<()> {
        const IDENT_PFX: &str = if cfg!(target_os = "macos") { "_" } else { "" };
        let ident: String = func.ident.into();

        writeln!(&mut self.bw, "{TAB}.globl{TAB}{IDENT_PFX}{ident}")?;
        writeln!(&mut self.bw, "{IDENT_PFX}{ident}:")?;
        writeln!(&mut self.bw, "{TAB}pushq{TAB}%rbp")?;
        writeln!(&mut self.bw, "{TAB}movq{TAB}%rsp, %rbp")?;
        for instr in func.instructions.into_iter() {
            self.write_instr(instr)?;
        }
        Ok(())
    }
    fn write_instr(&mut self, instr: Instruction<Operand>) -> Result<()> {
        match instr {
            Instruction::Mov { src, dst } => {
                write!(&mut self.bw, "{TAB}movl{TAB}")?;
                self.write_operand(src, OperandByteLen::B4)?;
                write!(&mut self.bw, ", ")?;
                self.write_operand(dst, OperandByteLen::B4)?;
                writeln!(&mut self.bw, "")?;
            }
            Instruction::Unary(op, operand) => {
                let op = match op {
                    UnaryOperator::BitwiseComplement => "notl",
                    UnaryOperator::TwosComplement => "negl",
                };
                write!(&mut self.bw, "{TAB}{op}{TAB}")?;
                self.write_operand(operand, OperandByteLen::B4)?;
                writeln!(&mut self.bw, "")?;
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
                writeln!(&mut self.bw, "")?;
            }
            Instruction::Cmp { arg, tgt } => {
                write!(&mut self.bw, "{TAB}cmpl{TAB}")?;
                self.write_operand(arg, OperandByteLen::B4)?;
                write!(&mut self.bw, ", ")?;
                self.write_operand(tgt, OperandByteLen::B4)?;
                writeln!(&mut self.bw, "")?;
            }
            Instruction::Idiv(operand) => {
                write!(&mut self.bw, "{TAB}idivl{TAB}")?;
                self.write_operand(operand, OperandByteLen::B4)?;
                writeln!(&mut self.bw, "")?;
            }
            Instruction::Cdq => {
                writeln!(&mut self.bw, "cdq")?;
            }
            Instruction::Jmp(lbl) => {
                write!(&mut self.bw, "{TAB}jmp{TAB}")?;
                self.write_label(&lbl)?;
                writeln!(&mut self.bw, "")?;
            }
            Instruction::JmpCC(cc, lbl) => {
                let cmd_sfx = Self::get_condition_sfx(cc);
                write!(&mut self.bw, "{TAB}j{cmd_sfx}{TAB}")?;
                self.write_label(&lbl)?;
                writeln!(&mut self.bw, "")?;
            }
            Instruction::SetCC(cc, operand) => {
                let cmd_sfx = Self::get_condition_sfx(cc);
                write!(&mut self.bw, "{TAB}set{cmd_sfx}{TAB}")?;
                self.write_operand(operand, OperandByteLen::B1)?;
                writeln!(&mut self.bw, "")?;
            }
            Instruction::Label(lbl) => {
                self.write_label(&lbl)?;
                writeln!(&mut self.bw, ":")?;
            }
            Instruction::AllocateStack(stkpos) => {
                writeln!(&mut self.bw, "{TAB}subq{TAB}${}, %rsp", *stkpos)?;
            }
            Instruction::Ret => {
                writeln!(&mut self.bw, "{TAB}movq{TAB}%rbp, %rsp")?;
                writeln!(&mut self.bw, "{TAB}popq{TAB}%rbp")?;
                writeln!(&mut self.bw, "{TAB}ret")?;
            }
        }
        Ok(())
    }
    fn write_operand(&mut self, operand: Operand, obl: OperandByteLen) -> Result<()> {
        use OperandByteLen as OBL;
        match operand {
            Operand::ImmediateValue(val) => {
                write!(&mut self.bw, "${val}")?;
            }
            Operand::Register(reg) => {
                let reg_str = match (reg, obl) {
                    (Register::AX, OBL::B4) => "%eax",
                    (Register::AX, OBL::B1) => "%al",
                    (Register::DX, OBL::B4) => "%edx",
                    (Register::DX, OBL::B1) => "%dl",
                    (Register::R10, OBL::B4) => "%r10d",
                    (Register::R10, OBL::B1) => "%r10b",
                    (Register::R11, OBL::B4) => "%r11d",
                    (Register::R11, OBL::B1) => "%r11b",
                };
                write!(&mut self.bw, "{reg_str}")?;
            }
            Operand::StackPosition(stkpos) => {
                write!(&mut self.bw, "-{}(%rbp)", *stkpos)?;
            }
        }
        Ok(())
    }
    fn write_label(&mut self, lbl: &LabelIdentifier) -> Result<()> {
        const NAME_PFX: &str = if cfg!(target_os = "macos") {
            "L."
        } else {
            ".L."
        };

        let name = lbl.name().as_ref().map(|s| s.as_str()).unwrap_or("");
        let name = LABEL_BAD_CHAR.replace_all(&name, "_");

        let id = lbl.id();

        write!(&mut self.bw, "{NAME_PFX}{name}.{id}")?;

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
