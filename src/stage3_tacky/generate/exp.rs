use super::FunInstrsGenerator;
use crate::{
    common::{
        identifier::SymbolIdentifier,
        symbol_table_frontend::{ObjAttrs, Symbol},
        types_frontend::{ScalarType, SubObjType},
    },
    ds_n_a::witness::Witness,
    stage2_parse::c_ast as c,
    stage3_tacky::tacky_ast::*,
};
use std::rc::Rc;

/// Expression
impl FunInstrsGenerator<'_> {
    pub(super) fn gen_exp<LTyp>(&mut self, typed_exp: c::TypedExp<LTyp>) {
        match typed_exp {
            c::TypedExp::R(typed_rexp) => {
                self.gen_rexp(typed_rexp);
            }
            c::TypedExp::L(typed_lexp) => {
                self.gen_lexp(typed_lexp);
            }
        }
    }

    /// 1. Generate tacky instructions; and get as the result either a value or an object.
    /// 1. If the given expression was an lvalue-expression, ie if the expression designated an object,
    ///     then lvalue-convert the expression, ie extract the value out of the object.
    pub(super) fn gen_exp_and_get_value(&mut self, typed_exp: c::TypedExp<ScalarType>) -> Value {
        match typed_exp {
            c::TypedExp::R(typed_rexp) => self.gen_rexp(typed_rexp),
            c::TypedExp::L(typed_lexp) => {
                let obj = self.gen_lexp(typed_lexp);
                match obj {
                    Object::Direct(ident, sca_typ_witness) => {
                        Value::Variable(ident, sca_typ_witness)
                    }
                    Object::Pointee { addr, typ } => {
                        let dst = self.register_new_value(typ);
                        self.instrs
                            .push(Instruction::Load(Load { src_addr: addr, dst: dst.clone() }));
                        dst
                    }
                }
            }
        }
    }

    pub(super) fn gen_rexp(&mut self, c::TypedRExp { exp, typ }: c::TypedRExp) -> Value {
        match exp {
            c::RExp::Const(konst) => Value::Constant(konst),
            c::RExp::Cast(c_cast) => self.gen_exp_cast(c_cast, typ),
            c::RExp::Unary(c_unary) => self.gen_exp_unary(c_unary, typ),
            c::RExp::Binary(c_binary) => self.gen_exp_binary(c_binary, typ),
            c::RExp::Conditional(c_cond) => self.gen_exp_conditional(c_cond, typ),
            c::RExp::FunctionCall(c_fun_call) => self.gen_exp_fun_call(c_fun_call, typ),
            c::RExp::Assignment(c_assign) => self.gen_exp_assignment(c_assign),
            c::RExp::AddrOf(c_addrof) => self.gen_exp_addrof(c_addrof, typ),
        }
    }

    pub(super) fn gen_lexp<LTyp>(
        &mut self,
        c::TypedLExp { exp, typ }: c::TypedLExp<LTyp>,
    ) -> Object<LTyp> {
        match exp {
            c::LExp::String(ident) => Object::Direct(ident, Witness::new(&typ)),
            c::LExp::Var(ident) => Object::Direct(ident, Witness::new(&typ)),
            c::LExp::Dereference(c_deref) => self.gen_exp_deref(c_deref, typ),
            c::LExp::Subscript(c_subscr) => self.gen_exp_subscript(c_subscr, typ),
        }
    }
}

/// Helpers
impl FunInstrsGenerator<'_> {
    pub(super) fn register_new_value(&mut self, sca_typ: SubObjType<ScalarType>) -> Value {
        let ident = Rc::new(SymbolIdentifier::new_generated());

        let val = Value::Variable(Rc::clone(&ident), Witness::new(&sca_typ));

        self.frontend_symtab.as_mut().insert(
            ident,
            Symbol::Obj {
                typ: sca_typ.into_owner(),
                attrs: ObjAttrs::AutomaticStorageDuration,
            },
        );

        val
    }
}
