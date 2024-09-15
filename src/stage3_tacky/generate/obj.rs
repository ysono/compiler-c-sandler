use super::FunInstrsGenerator;
use crate::{
    common::identifier::SymbolIdentifier,
    stage2_parse::{
        c_ast::{self as c, LvalueExpression},
        phase3_typecheck::TypeCheckedCAst,
    },
    stage3_tacky::tacky_ast::*,
};
use std::rc::Rc;

impl<'a> FunInstrsGenerator<'a> {
    /* C Assignment */

    pub(super) fn gen_exp_assignment(
        &mut self,
        lhs: c::TypedExpression<c::LvalueExpression<TypeCheckedCAst>>,
        rhs: c::TypedExpression<c::Expression<TypeCheckedCAst>>,
    ) -> Value {
        match lhs.exp {
            LvalueExpression::Var(ident) => self.gen_assignment(ident, rhs),
            LvalueExpression::Dereference(_) => todo!(),
        }
    }
    pub(super) fn gen_assignment(
        &mut self,
        ident: Rc<SymbolIdentifier>,
        rhs: c::TypedExpression<c::Expression<TypeCheckedCAst>>,
    ) -> Value {
        let rhs = self.gen_exp(rhs);

        self.instrs.push(Instruction::Copy(SrcDst {
            src: rhs,
            dst: Rc::clone(&ident),
        }));

        Value::Variable(ident)
    }
}
