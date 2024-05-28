use crate::{
    files::AsmFilepath,
    stage4_asm_gen::asm_code::{Function, Instruction, Operand, Program, Register, UnaryOperator},
};
use anyhow::{anyhow, Result};
use std::fs::{File, OpenOptions};
use std::io::{BufWriter, Write};
use std::path::PathBuf;

const TAB: &str = "\t";

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
    pub fn emit_program(mut self, prog: Program) -> Result<()> {
        let Program { func } = prog;
        self.write_func(func)?;
        writeln!(&mut self.bw, "{TAB}.section	.note.GNU-stack,\"\",@progbits")?;
        self.bw.flush()?;
        Ok(())
    }
    fn write_func(&mut self, func: Function) -> Result<()> {
        let Function {
            ident,
            instructions,
        } = func;
        let ident: String = ident.into();
        writeln!(&mut self.bw, "{TAB}.globl{TAB}{ident}")?;
        writeln!(&mut self.bw, "{ident}:")?;
        writeln!(&mut self.bw, "{TAB}pushq{TAB}%rbp")?;
        writeln!(&mut self.bw, "{TAB}movq{TAB}%rsp, %rbp")?;
        for instr in instructions.into_iter() {
            self.write_instr(instr)?;
        }
        Ok(())
    }
    fn write_instr(&mut self, instr: Instruction) -> Result<()> {
        match instr {
            Instruction::Mov { src, dst } => {
                write!(&mut self.bw, "{TAB}movl{TAB}")?;
                self.write_operand(src)?;
                write!(&mut self.bw, ", ")?;
                self.write_operand(dst)?;
                writeln!(&mut self.bw, "")?;
            }
            Instruction::Unary(op, operand) => {
                let op = match op {
                    UnaryOperator::Not => "notl",
                    UnaryOperator::Neg => "negl",
                };
                write!(&mut self.bw, "{TAB}{op}{TAB}")?;
                self.write_operand(operand)?;
                writeln!(&mut self.bw, "")?;
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
    fn write_operand(&mut self, operand: Operand) -> Result<()> {
        match operand {
            Operand::ImmediateValue(val) => {
                write!(&mut self.bw, "${val}")?;
            }
            Operand::Register(reg) => match reg {
                Register::AX => {
                    write!(&mut self.bw, "%eax")?;
                }
                Register::R10 => {
                    write!(&mut self.bw, "%r10d")?;
                }
            },
            Operand::PseudoRegister(_) => {
                return Err(anyhow!("Unexpected {operand:?}"));
            }
            Operand::StackPosition(stkpos) => {
                write!(&mut self.bw, "-{}(%rbp)", *stkpos)?;
            }
        }
        Ok(())
    }
}
