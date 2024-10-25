//! + Translate each Tacky instruction, which is abstract, into concrete x86-64 instruction(s).
//! + Translate each Tacky operand, which exists in an abstract location, into a semi-concrete location.
//! + Declare new operands that have static storage duration (at this time, we declare new static readonly objs only).

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
            t::Instruction::Return(t_val) => self.gen_return(t_val),
            t::Instruction::SignExtend(t_srcdst) => self.gen_signextend(t_srcdst),
            t::Instruction::ZeroExtend(t_srcdst) => self.gen_zeroextend(t_srcdst),
            t::Instruction::Truncate(t_srcdst) => self.gen_truncate(t_srcdst),
            t::Instruction::DoubleToSInteg(t_srcdst) => self.gen_double_to_sgn_integ(t_srcdst),
            t::Instruction::DoubleToUInteg(t_srcdst) => self.gen_double_to_unsgn_integ(t_srcdst),
            t::Instruction::SIntegToDouble(t_srcdst) => self.gen_sgn_integ_to_double(t_srcdst),
            t::Instruction::UIntegToDouble(t_srcdst) => self.gen_unsgn_integ_to_double(t_srcdst),
            t::Instruction::Unary(t_unary) => self.gen_unary(t_unary),
            t::Instruction::Binary(t_binary) => self.gen_binary(t_binary),
            t::Instruction::Copy(t_srcdst) => self.gen_copy(t_srcdst),
            t::Instruction::GetAddress(t_getaddr) => self.gen_getaddr(t_getaddr),
            t::Instruction::Load(t_load) => self.gen_load(t_load),
            t::Instruction::Store(t_store) => self.gen_store(t_store),
            t::Instruction::AddPtr(t_addptr) => self.gen_addptr(t_addptr),
            t::Instruction::CopyToOffset(t_cto) => self.gen_copytooffset(t_cto),
            t::Instruction::Jump(lbl) => vec![Instruction::Jmp(lbl)],
            t::Instruction::JumpIf(t_jumpif) => self.gen_jumpif(t_jumpif),
            t::Instruction::Label(lbl) => vec![Instruction::Label(lbl)],
            t::Instruction::FunCall(t_fun_call) => self.gen_funcall(t_fun_call),
        })
    }
}
