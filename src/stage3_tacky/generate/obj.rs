use super::FunInstrsGenerator;
use crate::{
    common::types_frontend::{NonAggrType, ScalarType, SubObjType},
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
};

/// C Assignment
impl FunInstrsGenerator<'_> {
    pub(super) fn gen_exp_assignment(
        &mut self,
        c::Assignment { lhs, rhs }: c::Assignment<TypeCheckedCAst>,
    ) -> Value {
        let lhs = self.gen_lexp(*lhs);
        let rhs = self.gen_sca_exp_and_get_value(*rhs);
        self.gen_assignment(lhs, rhs)
    }
    fn gen_assignment(&mut self, dst_obj: Object<SubObjType<ScalarType>>, src_val: Value) -> Value {
        match dst_obj {
            Object::Direct(ident, sca_typ_witness) => {
                let dst_val = Value::Variable(ident, sca_typ_witness);
                self.instrs.push(Instruction::Copy(SrcDst {
                    src: src_val,
                    dst: dst_val.clone(),
                }));
                dst_val
            }
            Object::Pointee { addr, typ: _ } => {
                self.instrs.push(Instruction::Store(Store {
                    src: src_val.clone(),
                    dst_addr: addr,
                }));
                src_val
            }
        }
    }
}

/// Object <-> pointer-typed Value
impl FunInstrsGenerator<'_> {
    pub(super) fn gen_exp_deref<LTyp>(
        &mut self,
        c::Dereference(sub_exp): c::Dereference<TypeCheckedCAst>,
        ifc_typ: LTyp,
    ) -> Object<LTyp> {
        let addr = self.gen_sca_exp_and_get_value(*sub_exp);
        Object::Pointee { addr, typ: ifc_typ }
    }
    pub(super) fn gen_exp_addrof(
        &mut self,
        c::AddrOf { sub_exp, concrete_typ }: c::AddrOf<TypeCheckedCAst>,
        ifc_typ: NonAggrType,
    ) -> Value {
        match self.gen_lexp(*sub_exp) {
            Object::Direct(ident, _nonvoid_typ_witness) => {
                let dst = self.register_new_value(Self::extract_sca_typ(ifc_typ, concrete_typ));
                self.instrs.push(Instruction::GetAddress(GetAddress {
                    src_obj: ident,
                    dst_addr: dst.clone(),
                }));
                dst
            }
            Object::Pointee { addr, typ: _ } => {
                /* We implicitly elide `&*foo` expression into the lvalue-conversion of `foo` expression. */
                addr
            }
        }
    }
}
