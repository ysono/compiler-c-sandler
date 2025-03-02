use super::InstrsGenerator;
use crate::{
    common::{
        identifier::SymbolIdentifier,
        primitive::Const,
        types_backend::{Alignment, ByteLen, ScalarAssemblyType},
        types_frontend::{ArithmeticType, ObjType},
    },
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::asm_ast::*,
};
use std::rc::Rc;

/// Tacky Value -> Asm Operand and type info
impl InstrsGenerator {
    pub(super) fn value_to_operand_and_type(
        &mut self,
        t_val: t::Value,
    ) -> (PreFinalOperand, ArithmeticType, ScalarAssemblyType) {
        let (ari_type, asm_type) = self.value_to_type(&t_val);
        let operand = self.value_to_operand(t_val);
        (operand, ari_type, asm_type)
    }
    pub(super) fn value_to_type(&self, t_val: &t::Value) -> (ArithmeticType, ScalarAssemblyType) {
        match t_val {
            t::Value::Constant(konst) => {
                let ari_type = konst.arithmetic_type();
                let asm_type = ScalarAssemblyType::from(ari_type);
                (ari_type, asm_type)
            }
            t::Value::Variable(ident, _sca_typ_witness) => {
                let obj_type = self.frontend_symtab.get_obj_type(ident).unwrap();
                let sca_type = match obj_type.as_ref() {
                    ObjType::Void => todo!(),
                    ObjType::Scalar(s) => s,
                    ObjType::Array(_) => unreachable!(),
                };
                let ari_type = sca_type.effective_arithmetic_type();
                let asm_type = ScalarAssemblyType::from(ari_type);
                (ari_type, asm_type)
            }
        }
    }
    pub(super) fn value_to_operand(&mut self, t_val: t::Value) -> PreFinalOperand {
        match t_val {
            t::Value::Constant(konst) => match konst {
                Const::Char(_)
                | Const::UChar(_)
                | Const::Int(_)
                | Const::Long(_)
                | Const::UInt(_)
                | Const::ULong(_) => Operand::ImmediateValue(konst.as_bits()).into(),
                Const::Double(_) => {
                    /* Floating-point constant operands cannot be immediate-values.
                    Here, we specify the default alignment.
                    In the fixup phase, we can ensure that each SSE instruction accesses 16-bytes-aligned operands. */
                    self.get_or_new_static_readonly_operand(None, konst)
                }
            },
            t::Value::Variable(ident, _sca_typ_witness) => PreFinalOperand::PseudoRegOrMem(ident),
        }
    }

    pub(super) fn object_to_operand(&self, ident: Rc<SymbolIdentifier>) -> PreFinalOperand {
        match self.frontend_symtab.get_obj_type(&ident).unwrap().as_ref() {
            ObjType::Void => todo!(),
            ObjType::Scalar(_) => PreFinalOperand::PseudoRegOrMem(ident),
            ObjType::Array(_) => PreFinalOperand::PseudoMem {
                obj: ident,
                offset: ByteLen::new(0),
            },
        }
    }
}

/// Asm static readonly objs
impl InstrsGenerator {
    pub(super) fn get_or_new_static_readonly_operand(
        &mut self,
        custom_alignment: Option<Alignment>,
        konst: Const,
    ) -> PreFinalOperand {
        let alignment = custom_alignment
            .unwrap_or_else(|| Alignment::default_of_scalar(konst.arithmetic_type()));

        let ident = self
            .backend_symtab
            .get_or_new_static_readonly_scalar(alignment, konst);

        Operand::ReadonlyData(ident).into()
    }
}
