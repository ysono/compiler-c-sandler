use crate::{
    lexer::{Const, Identifier},
    parser::{
        Expression as CExpression, Function as CFunction, Program as CProgram,
        Statement as CStatement,
    },
};
use anyhow::{anyhow, Result};

#[derive(Debug)]
pub struct Program {
    pub func: Function,
}
#[derive(Debug)]
pub struct Function {
    pub ident: Identifier,
    pub instructions: Vec<Instruction>,
}
#[derive(Debug)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Ret,
}
#[derive(Debug)]
pub enum Operand {
    ImmediateValue(i64),
    Register(Register),
}
#[derive(Debug)]
pub enum Register {
    Eax,
}

pub struct AsmCodeGenerator {}
impl AsmCodeGenerator {
    pub fn gen_program(c_prog: CProgram) -> Result<Program> {
        let CProgram { func } = c_prog;
        let func = Self::gen_func(func)?;
        Ok(Program { func })
    }
    fn gen_func(c_func: CFunction) -> Result<Function> {
        let CFunction { ident, stmt } = c_func;
        let mut instructions = vec![];
        Self::gen_instruction(stmt, &mut instructions)?;
        Ok(Function {
            ident,
            instructions,
        })
    }
    fn gen_instruction(c_stmt: CStatement, asm_instructions: &mut Vec<Instruction>) -> Result<()> {
        let CStatement::Return(exp) = c_stmt;

        let val = match exp {
            CExpression::Const(Const::Int(val_)) => val_,
            CExpression::Unary(_, _) => {
                return Err(anyhow!("Unary expression is not supported yet."))
            }
        };

        let instr = Instruction::Mov {
            src: Operand::ImmediateValue(val as i64),
            dst: Operand::Register(Register::Eax),
        };
        asm_instructions.push(instr);

        let instr = Instruction::Ret;
        asm_instructions.push(instr);

        Ok(())
    }
}
