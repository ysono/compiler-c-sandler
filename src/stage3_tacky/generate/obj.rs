use super::FunInstrsGenerator;
use crate::{
    common::identifier::SymbolIdentifier,
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
};
use std::rc::Rc;

impl<'a> FunInstrsGenerator<'a> {
    /* C Assignment */

    pub(super) fn gen_exp_assignment(
        &mut self,
        ident: Rc<SymbolIdentifier>,
        rhs: c::TypedExpression<TypeCheckedCAst>,
    ) -> ReadableValue {
        let rhs = self.gen_exp(rhs);

        self.instrs.push(Instruction::Copy(SrcDst {
            src: rhs,
            dst: Rc::clone(&ident),
        }));

        ReadableValue::Variable(ident)
    }
}
