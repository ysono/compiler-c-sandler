use super::FunInstrsGenerator;
use crate::{
    common::types_frontend::VarType,
    ds_n_a::singleton::Singleton,
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
};
use std::rc::Rc;

impl<'a> FunInstrsGenerator<'a> {
    /* C Assignment */

    pub(super) fn gen_exp_assignment(
        &mut self,
        c::Assignment { lhs, rhs }: c::Assignment<TypeCheckedCAst>,
    ) -> Value {
        let lhs = self.gen_exp_lvalue(*lhs);
        let rhs = self.gen_exp_and_get_value(*rhs);
        self.gen_assignment(lhs, rhs)
    }
    pub(super) fn gen_assignment(&mut self, dst: Object, src: Value) -> Value {
        match dst {
            Object::Direct(dst) => {
                self.instrs
                    .push(Instruction::Copy(SrcDst { src, dst: Rc::clone(&dst) }));
                Value::Variable(dst)
            }
            Object::Pointee { addr, typ: _ } => {
                self.instrs.push(Instruction::Store(Store {
                    src: src.clone(),
                    dst_addr: addr,
                }));
                src
            }
        }
    }

    /* Pointer */

    pub(super) fn gen_exp_deref(
        &mut self,
        c::Dereference(sub_exp): c::Dereference<TypeCheckedCAst>,
        pointee_type: Singleton<VarType>,
    ) -> Object {
        let addr = self.gen_exp_and_get_value(*sub_exp);
        Object::Pointee { addr, typ: pointee_type }
    }
    pub(super) fn gen_exp_addrof(
        &mut self,
        c::AddrOf(sub_exp): c::AddrOf<TypeCheckedCAst>,
        pointer_type: Singleton<VarType>,
    ) -> Value {
        match self.gen_exp_lvalue(*sub_exp) {
            Object::Direct(ident) => {
                let dst = self.symbol_table.declare_var_anon(pointer_type);
                self.instrs.push(Instruction::GetAddress(GetAddress {
                    src: ident,
                    dst_addr: Rc::clone(&dst),
                }));
                Value::Variable(dst)
            }
            Object::Pointee { addr, typ: _ } => {
                /* We implicitly elide `&*foo` expression into the lvalue-conversion of `foo` expression. */
                addr
            }
        }
    }
}
