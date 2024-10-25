//! + Translate each Tacky instruction, which is abstract, into concrete x86-64 instruction(s).
//! + Translate each Tacky operand, which exists in an abstract location, into a semi-concrete location.
//! + Declare new operands that have static storage duration (at this time, we declare new static constants only).

mod instr_addr;
mod instr_ary;
mod instr_cast;
mod instr_cmp;
mod instr_copy;
mod instr_fun;
mod operand;

use crate::{
    common::{symbol_table_backend::BackendSymbolTable, symbol_table_frontend::SymbolTable},
    ds_n_a::immutable_owned::ImmutableOwned,
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::asm_ast::*,
};
use derive_more::Into;

#[derive(Debug)]
pub struct GeneratedAsmAst(());
impl AsmAstVariant for GeneratedAsmAst {
    type Instructions = Vec<Instruction<Self>>;
    type Operand = PreFinalOperand;
}

#[derive(Into)]
pub struct InstrsGenerator {
    frontend_symtab: ImmutableOwned<SymbolTable>,
    backend_symtab: BackendSymbolTable,
}
impl InstrsGenerator {
    pub fn new(frontend_symtab: ImmutableOwned<SymbolTable>) -> Self {
        Self {
            frontend_symtab,
            backend_symtab: BackendSymbolTable::default(),
        }
    }

    /* Tacky Function */

    pub fn convert_fun(
        &mut self,
        t::Function {
            ident,
            typ,
            visibility,
            param_idents,
            instrs,
        }: t::Function,
    ) -> Function<GeneratedAsmAst> {
        let instrs = self.gen_fun_instrs(typ, param_idents, instrs);
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
            t::Instruction::GetAddress(t_getaddr) => self.gen_getaddr_instrs(t_getaddr),
            t::Instruction::Load(t_load) => self.gen_load_instrs(t_load),
            t::Instruction::Store(t_store) => self.gen_store_instrs(t_store),
            t::Instruction::AddPtr(t_addptr) => self.gen_addptr_instrs(t_addptr),
            t::Instruction::CopyToOffset(t_cto) => self.gen_copytooffset_instrs(t_cto),
            t::Instruction::Jump(lbl) => vec![Instruction::Jmp(lbl)],
            t::Instruction::JumpIf(t_jumpif) => self.gen_jumpif_instrs(t_jumpif),
            t::Instruction::Label(lbl) => vec![Instruction::Label(lbl)],
            t::Instruction::FunCall(t_fun_call) => self.gen_funcall_instrs(t_fun_call),
        })
    }
}
