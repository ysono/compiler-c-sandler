use crate::{
    asm_codegen::{Function, Instruction, Operand, Program, Register},
    files::AsmFilepath,
    lexer::Identifier,
};
use anyhow::Result;
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
        let Identifier(ident) = ident;
        writeln!(&mut self.bw, "{TAB}.globl{TAB}{ident}")?;
        writeln!(&mut self.bw, "{ident}:")?;
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
            Instruction::Ret => {
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
            Operand::Register(Register::Eax) => {
                write!(&mut self.bw, "%eax")?;
            }
        }
        Ok(())
    }
}
