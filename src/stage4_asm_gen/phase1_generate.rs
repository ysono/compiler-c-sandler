//! + Translate each Tacky instruction, which is abstract, into concrete x86-64 instruction(s).
//! + Translate each Tacky operand, which exists in an abstract location, into a semi-concrete location.
//! + Declare new intermediary operands (which are, at this time, static constants only).

mod instr_ary;
mod instr_cast;
mod instr_cmp;
mod instr_fun;
mod instr_misc;
mod operand;

use crate::{
    common::{
        identifier::SymbolIdentifier, symbol_table_frontend::SymbolTable, types_backend::Alignment,
        types_frontend::Const,
    },
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::asm_ast::*,
};
use derive_more::Into;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct GeneratedAsmAst(());
impl AsmAstVariant for GeneratedAsmAst {
    type Instructions = Vec<Instruction<GeneratedAsmAst>>;
    type Operand = PreFinalOperand;
}

#[derive(Into)]
pub struct InstrsGenerator {
    frontend_symtab: SymbolTable,

    static_constants: HashMap<(Alignment, Const), Rc<SymbolIdentifier>>,
}
impl InstrsGenerator {
    pub fn new(frontend_symtab: SymbolTable) -> Self {
        Self {
            frontend_symtab,
            static_constants: HashMap::default(),
        }
    }

    /* Tacky Function */

    pub fn convert_fun(
        &mut self,
        t::Function {
            ident,
            visibility,
            param_idents,
            instrs,
        }: t::Function,
    ) -> Function<GeneratedAsmAst> {
        let instrs = self.gen_fun_instrs(param_idents, instrs);
        Function { ident, visibility, instrs }
    }

    /* Tacky Instruction */

    fn gen_instructions(
        &mut self,
        t_instrs: Vec<t::Instruction>,
    ) -> impl '_ + Iterator<Item = Instruction<GeneratedAsmAst>> {
        t_instrs.into_iter().flat_map(|t_instr| match t_instr {
            t::Instruction::Return(t_val) => self.gen_return_instrs(t_val),
            t::Instruction::SignExtend(t_srcdst) => self.gen_signextend_instrs(t_srcdst),
            t::Instruction::Truncate(t_srcdst) => self.gen_truncate_instrs(t_srcdst),
            t::Instruction::ZeroExtend(t_srcdst) => self.gen_zeroextend_instrs(t_srcdst),
            t::Instruction::DoubleToInt(t_srcdst) => self.gen_double_to_int_instrs(t_srcdst),
            t::Instruction::DoubleToUInt(t_srcdst) => self.gen_double_to_uint_instrs(t_srcdst),
            t::Instruction::IntToDouble(t_srcdst) => self.gen_int_to_double_instrs(t_srcdst),
            t::Instruction::UIntToDouble(t_srcdst) => self.gen_uint_to_double_instrs(t_srcdst),
            t::Instruction::Unary(t_unary) => self.gen_unary_instrs(t_unary),
            t::Instruction::Binary(t_binary) => self.gen_binary_instrs(t_binary),
            t::Instruction::Copy(t_srcdst) => self.gen_copy_instrs(t_srcdst),
            t::Instruction::GetAddress(_) => todo!(),
            t::Instruction::Load(_) => todo!(),
            t::Instruction::Store(_) => todo!(),
            t::Instruction::Jump(lbl) => vec![Instruction::Jmp(lbl)],
            t::Instruction::JumpIf(t_jumpif) => self.gen_jumpif_instrs(t_jumpif),
            t::Instruction::Label(lbl) => vec![Instruction::Label(lbl)],
            t::Instruction::FunCall(t_fun_call) => self.gen_funcall_instrs(t_fun_call),
        })
    }
}
