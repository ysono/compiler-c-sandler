use super::InstrsGenerator;
use crate::{
    common::{
        identifier::SymbolIdentifier,
        primitive::Const,
        types_backend::{Alignment, AssemblyType},
        types_frontend::ArithmeticType,
    },
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::asm_ast::*,
};
use std::rc::Rc;

impl InstrsGenerator {
    /* Tacky Value -> Asm Operand and type info */

    pub(super) fn value_to_operand_and_type<V: Into<t::Value>>(
        &mut self,
        t_val: V,
    ) -> (PreFinalOperand, ArithmeticType, AssemblyType) {
        let t_val = t_val.into();
        let (ari_type, asm_type) = self.value_to_type(&t_val);
        let operand = self.value_to_operand(t_val);
        (operand, ari_type, asm_type)
    }
    pub(super) fn value_to_type(&self, t_val: &t::Value) -> (ArithmeticType, AssemblyType) {
        match t_val {
            t::Value::Constant(konst) => {
                let ari_type = konst.arithmetic_type();
                let asm_type = AssemblyType::from(ari_type);
                (ari_type, asm_type)
            }
            t::Value::Variable(ident) => {
                let var_type = self.frontend_symtab.get_var_type(ident).unwrap();
                let ari_type = var_type.effective_arithmetic_type();
                let asm_type = AssemblyType::from(ari_type);
                (ari_type, asm_type)
            }
        }
    }
    pub(super) fn value_to_operand<V: Into<t::Value>>(&mut self, t_val: V) -> PreFinalOperand {
        match t_val.into() {
            t::Value::Constant(konst) => match konst {
                Const::Int(_) | Const::Long(_) | Const::UInt(_) | Const::ULong(_) => {
                    Operand::ImmediateValue(konst.as_bits()).into()
                }
                Const::Double(_) => {
                    /* Floating-point constant operands cannot be immediate-values.
                    Here, we specify the default alignment.
                    In the fixup phase, we can ensure that each SSE instruction accesses 16-bytes-aligned operands. */
                    self.get_or_new_static_constant_operand(None, konst)
                }
            },
            t::Value::Variable(ident) => PreFinalOperand::Pseudo(ident),
        }
    }

    /* Asm static constant */

    pub(super) fn get_or_new_static_constant_operand(
        &mut self,
        custom_alignment: Option<Alignment>,
        konst: Const,
    ) -> PreFinalOperand {
        let alignment =
            custom_alignment.unwrap_or_else(|| Alignment::default_of(konst.arithmetic_type()));
        let ident = self
            .static_constants
            .entry((alignment, konst))
            .or_insert_with(|| Rc::new(SymbolIdentifier::new_generated()));
        Operand::Data(Rc::clone(ident)).into()
    }
}
