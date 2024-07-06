mod ary;
mod comparison;
mod fun;
mod txform;

use crate::{
    identifier::UniqueIdentifier,
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::asm_ast::*,
    symbol_table_frontend::SymbolTable,
    types_backend::{Alignment, AssemblyType},
    types_frontend::{Const, VarType},
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

    static_constants: HashMap<(Alignment, Const), Rc<UniqueIdentifier>>,
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
            t::Instruction::Jump(lbl) => vec![Instruction::Jmp(lbl)],
            t::Instruction::JumpIf(t_jumpif) => self.gen_jumpif_instrs(t_jumpif),
            t::Instruction::Label(lbl) => vec![Instruction::Label(lbl)],
            t::Instruction::FunCall(t_fun_call) => self.gen_funcall_instrs(t_fun_call),
        })
    }

    /* Tacky Return */

    fn gen_return_instrs(&mut self, t_val: t::ReadableValue) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, asm_type) = self.convert_value(t_val);
        let dst_reg = match asm_type {
            AssemblyType::Longword | AssemblyType::Quadword => Register::AX,
            AssemblyType::Double => Register::XMM0,
        };
        let asm_instr_1 = Instruction::Mov {
            asm_type,
            src,
            dst: PreFinalOperand::Register(dst_reg),
        };

        let asm_instr_2 = Instruction::Ret;

        vec![asm_instr_1, asm_instr_2]
    }

    /* Tacky Copy */

    fn gen_copy_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, asm_type) = self.convert_value(src);
        let (dst, _, _) = self.convert_value(dst);
        vec![Instruction::Mov { asm_type, src, dst }]
    }

    /* Tacky Value -> Asm Operand and other info */

    fn convert_value<V: Into<t::ReadableValue>>(
        &mut self,
        t_val: V,
    ) -> (PreFinalOperand, VarType, AssemblyType) {
        let t_val = t_val.into();
        let (var_type, asm_type) = self.convert_value_to_type_info(&t_val);
        let operand = self.convert_value_to_operand(t_val);
        (operand, var_type, asm_type)
    }
    fn convert_value_to_type_info(&self, t_val: &t::ReadableValue) -> (VarType, AssemblyType) {
        match t_val {
            t::ReadableValue::Constant(konst) => {
                let var_type = konst.var_type();
                let asm_type = AssemblyType::from(var_type);
                (var_type, asm_type)
            }
            t::ReadableValue::Variable(ident) => {
                let var_type = self.frontend_symtab.use_var(ident).unwrap();
                let asm_type = AssemblyType::from(var_type);
                (var_type, asm_type)
            }
        }
    }
    fn convert_value_to_operand(&mut self, t_val: t::ReadableValue) -> PreFinalOperand {
        match t_val {
            t::ReadableValue::Constant(konst) => match konst {
                Const::Int(_) | Const::Long(_) | Const::UInt(_) | Const::ULong(_) => {
                    PreFinalOperand::ImmediateValue(konst.to_bits())
                }
                Const::Double(_) => self.get_or_new_static_constant_operand(None, konst),
            },
            t::ReadableValue::Variable(ident) => PreFinalOperand::Pseudo(ident),
        }
    }

    /* Asm static constant */

    fn get_or_new_static_constant_operand(
        &mut self,
        custom_alignment: Option<Alignment>,
        konst: Const,
    ) -> PreFinalOperand {
        let alignment = custom_alignment.unwrap_or_else(|| Alignment::default_of(konst.var_type()));
        let ident = self
            .static_constants
            .entry((alignment, konst))
            .or_insert_with(|| Rc::new(UniqueIdentifier::new_generated(None)));
        PreFinalOperand::Data(Rc::clone(ident))
    }
}
