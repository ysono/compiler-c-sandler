use super::TypeChecker;
use crate::{
    common::types_frontend::{ObjType, PointerType, ScalarType, SubObjType},
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
};
use anyhow::{Result, anyhow};
use owning_ref::OwningRef;
use std::borrow::Cow;

impl TypeChecker {
    pub(super) fn typecheck_exp_assignment(
        &mut self,
        Assignment { lhs, rhs }: Assignment<ResolvedCAst>,
    ) -> Result<TypedRExp> {
        let lhs = {
            /*
            The conceptual flow:
                1. If the LHS expression is array-typed, it's converted into the pointer-to-the-zeroth-element expression, which is an rvalue-expression.
                1. Assert that the LHS is an lvalue expression.
            The equivalent condensed flow:
                1. Assert that the LHS expression is scalar-typed.
            */
            let lexp = extract_lexp(*lhs)?;
            let TypedLExp { typ, exp } = self.typecheck_lexp(lexp)?;
            let typ = Self::extract_scalar_type(typ.into_owner())
                .map_err(|typ| anyhow!("Cannot assign to {typ:#?}"))?;
            TypedLExp { typ, exp }
        };

        let typ = lhs.typ.clone();
        let rhs = self.cast_by_assignment(Cow::Borrowed(&typ), *rhs)?;

        let exp = RExp::Assignment(Assignment {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        });
        Ok(TypedRExp { typ, exp })
    }

    pub(super) fn typecheck_exp_addrof(
        &mut self,
        AddrOf(sub_exp): AddrOf<ResolvedCAst>,
    ) -> Result<TypedRExp> {
        let sub_exp = {
            let lexp = extract_lexp(*sub_exp)?;
            let obj_typed_lexp = self.typecheck_lexp(lexp)?;
            obj_typed_lexp
        };

        let pointee_type = sub_exp.typ.as_owner().clone();
        let typ = self.get_scalar_type(PointerType { pointee_type });

        let exp = RExp::AddrOf(AddrOf(Box::new(sub_exp)));
        Ok(TypedRExp { typ, exp })
    }

    pub(super) fn typecheck_exp_deref(
        &mut self,
        Dereference(sub_exp): Dereference<ResolvedCAst>,
    ) -> Result<TypedLExp<SubObjType<ObjType>>> {
        let sub_exp = self.typecheck_exp_and_convert_to_scalar(*sub_exp)?;

        let typ = match sub_exp.typ().as_ref() {
            ScalarType::Ptr(PointerType { pointee_type }) => pointee_type.clone(),
            _ => return Err(anyhow!("Cannot dereference {sub_exp:#?}")),
        };
        let typ = OwningRef::new(typ);

        let sub_exp = Box::new(sub_exp);
        let exp = LExp::Dereference(Dereference(sub_exp));
        Ok(TypedLExp { typ, exp })
    }
}

fn extract_lexp(exp: Expression<ResolvedCAst>) -> Result<LExp<ResolvedCAst>> {
    match exp {
        Expression::R(rexp) => Err(anyhow!("Expected lvalue expression, but found {rexp:#?}")),
        Expression::L(lexp) => Ok(lexp),
    }
}
