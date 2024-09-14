use super::InstrsGenerator;
use crate::{
    common::{
        identifier::UniqueIdentifier,
        types_backend::{Alignment, AssemblyType},
        types_frontend::{Const, VarType},
    },
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::asm_ast::*,
};
use std::rc::Rc;

impl InstrsGenerator {
    /* Tacky Value -> Asm Operand and other info */

    pub(super) fn convert_value<V: Into<t::ReadableValue>>(
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
                    PreFinalOperand::ImmediateValue(konst.as_bits())
                }
                Const::Double(_) => self.get_or_new_static_constant_operand(None, konst),
            },
            t::ReadableValue::Variable(ident) => PreFinalOperand::Pseudo(ident),
        }
    }

    /* Asm static constant */

    pub(super) fn get_or_new_static_constant_operand(
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
