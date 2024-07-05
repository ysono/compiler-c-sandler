mod ary;
mod comparison;
mod fun;
mod txform;

use crate::{
    stage3_tacky::tacky_ast as t, stage4_asm_gen::asm_ast::*, symbol_table_frontend::SymbolTable,
    types_backend::AssemblyType,
};

#[derive(Debug)]
pub struct GeneratedAsmAst(());
impl AsmAstVariant for GeneratedAsmAst {
    type Instructions = Vec<Instruction<GeneratedAsmAst>>;
    type Operand = PreFinalOperand;
}

pub struct InstrsGenerator<'slf> {
    frontend_symtab: &'slf SymbolTable,
}
impl<'slf> InstrsGenerator<'slf> {
    pub fn new(frontend_symtab: &'slf SymbolTable) -> Self {
        Self { frontend_symtab }
    }

    /* Tacky Instruction */

    fn gen_instructions(
        &self,
        t_instrs: Vec<t::Instruction>,
    ) -> impl '_ + Iterator<Item = Instruction<GeneratedAsmAst>> {
        t_instrs.into_iter().flat_map(|t_instr| match t_instr {
            t::Instruction::Return(t_val) => self.gen_return_instrs(t_val),
            t::Instruction::SignExtend(t_srcdst) => self.gen_signextend_instrs(t_srcdst),
            t::Instruction::Truncate(t_srcdst) => self.gen_truncate_instrs(t_srcdst),
            t::Instruction::ZeroExtend(t_srcdst) => self.gen_zeroextend_instrs(t_srcdst),
            t::Instruction::DoubleToInt(_) => todo!(),
            t::Instruction::DoubleToUInt(_) => todo!(),
            t::Instruction::IntToDouble(_) => todo!(),
            t::Instruction::UIntToDouble(_) => todo!(),
            t::Instruction::Unary(t_unary) => self.gen_unary_instrs(t_unary),
            t::Instruction::Binary(t_binary) => self.gen_binary_instrs(t_binary),
            t::Instruction::Copy(t_srcdst) => self.gen_copy_instrs(t_srcdst),
            t::Instruction::Jump(lbl) => vec![Instruction::Jmp(lbl)],
            t::Instruction::JumpIf(t_jumpif) => self.gen_jumpif_instrs(t_jumpif),
            t::Instruction::Label(lbl) => vec![Instruction::Label(lbl)],
            t::Instruction::FunCall(t_fun_call) => self.gen_funcall_instrs(t_fun_call),
        })
    }

    /* Tacky Return */

    fn gen_return_instrs(&self, t_val: t::ReadableValue) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, asm_type, _) = self.convert_value(t_val);
        let asm_instr_1 = Instruction::Mov {
            asm_type,
            src,
            dst: Register::AX.into(),
        };

        let asm_instr_2 = Instruction::Ret;

        vec![asm_instr_1, asm_instr_2]
    }

    /* Tacky Copy */

    fn gen_copy_instrs(
        &self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, asm_type, _) = self.convert_value(src);
        let (dst, _, _) = self.convert_value(dst);
        vec![Instruction::Mov { asm_type, src, dst }]
    }

    /* Tacky Value -> Asm Operand and other info */

    fn convert_value<V: Into<t::ReadableValue>>(
        &self,
        t_val: V,
    ) -> (PreFinalOperand, AssemblyType, bool) {
        match t_val.into() {
            t::ReadableValue::Constant(konst) => {
                let operand = PreFinalOperand::ImmediateValue(konst.as_raw());

                let var_type = konst.var_type();
                let asm_type = AssemblyType::from(var_type);
                let is_signed = var_type.is_signed();

                (operand, asm_type, is_signed)
            }
            t::ReadableValue::Variable(ident) => {
                let var_type = self.frontend_symtab.use_var(&ident).unwrap();
                let asm_type = AssemblyType::from(var_type);
                let is_signed = var_type.is_signed();

                let operand = PreFinalOperand::Pseudo(ident);

                (operand, asm_type, is_signed)
            }
        }
    }
}
