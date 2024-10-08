use super::FunInstrsGenerator;
use crate::{
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
};
use std::rc::Rc;

impl<'a> FunInstrsGenerator<'a> {
    pub(super) fn gen_exp_and_get_value(
        &mut self,
        exp: c::TypedExpression<c::Expression<TypeCheckedCAst>>,
    ) -> Value {
        match self.gen_exp(exp) {
            ExpResult::Value(val) => val,
            ExpResult::Object(obj) => self.convert_object_to_value(obj),
        }
    }
    pub(super) fn gen_exp(
        &mut self,
        c::TypedExpression { exp, typ }: c::TypedExpression<c::Expression<TypeCheckedCAst>>,
    ) -> ExpResult {
        match exp {
            c::Expression::Const(konst) => Value::Constant(konst).into(),
            c::Expression::Var(ident) => Object::Direct(ident).into(),
            c::Expression::Cast(c_cast) => self.gen_exp_cast(c_cast).into(),
            c::Expression::Unary(c_unary) => self.gen_exp_unary(c_unary, typ).into(),
            c::Expression::Binary(c_binary) => self.gen_exp_binary(c_binary, typ).into(),
            c::Expression::Assignment(c_assign) => self.gen_exp_assignment(c_assign).into(),
            c::Expression::Conditional(c_cond) => self.gen_exp_conditional(c_cond, typ).into(),
            c::Expression::FunctionCall(c_fun_call) => {
                self.gen_exp_fun_call(c_fun_call, typ).into()
            }
            c::Expression::Dereference(c_deref) => self.gen_exp_deref(c_deref, typ).into(),
            c::Expression::AddrOf(c_addrof) => self.gen_exp_addrof(c_addrof, typ).into(),
        }
    }
    pub(super) fn gen_exp_lvalue(
        &mut self,
        c::TypedExpression { exp, typ }: c::TypedExpression<c::LvalueExpression<TypeCheckedCAst>>,
    ) -> Object {
        match exp {
            c::LvalueExpression::Var(ident) => Object::Direct(ident),
            c::LvalueExpression::Dereference(c_deref) => self.gen_exp_deref(c_deref, typ),
        }
    }
    /// Aka lvalue-converting an lvalue expression.
    fn convert_object_to_value(&mut self, object: Object) -> Value {
        match object {
            Object::Direct(ident) => Value::Variable(ident),
            Object::Pointee { addr, typ } => {
                let dst = self.symbol_table.declare_var_anon(typ);
                self.instrs.push(Instruction::Load(Load {
                    src_addr: addr,
                    dst: Rc::clone(&dst),
                }));
                Value::Variable(dst)
            }
        }
    }
}
