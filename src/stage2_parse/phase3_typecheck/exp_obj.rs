use super::TypeChecker;
use crate::{
    common::types_frontend::{NonVoidType, ObjType, PointerType, ScalarType, SubObjType},
    ds_n_a::{singleton::Singleton, witness::Witness},
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
};
use anyhow::{Result, anyhow};
use std::borrow::Cow;

impl TypeChecker {
    pub(super) fn typecheck_exp_assignment(
        &mut self,
        Assignment { lhs, rhs }: Assignment<ResolvedCAst>,
    ) -> Result<TypedRExp<SubObjType<ScalarType>>> {
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
            let typ = match typ {
                NonVoidType::Scalar(s) => s,
                NonVoidType::Array(a) => return Err(anyhow!("Cannot assign to {a:#?}")),
            };
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
        AddrOf { sub_exp, concrete_typ: () }: AddrOf<ResolvedCAst>,
    ) -> Result<TypedRExp<SubObjType<ScalarType>>> {
        let sub_exp = {
            let lexp = extract_lexp(*sub_exp)?;
            let nonvoid_lexp = self.typecheck_lexp(lexp)?;
            nonvoid_lexp
        };

        let pointee_type: Singleton<ObjType> = sub_exp.typ.clone().into();
        let typ = self.get_scalar_type(PointerType { pointee_type });

        let exp = RExp::AddrOf(AddrOf {
            sub_exp: Box::new(sub_exp),
            concrete_typ: Witness::new(&typ),
        });
        Ok(TypedRExp { typ, exp })
    }

    pub(super) fn typecheck_exp_deref(
        &mut self,
        Dereference(sub_exp): Dereference<ResolvedCAst>,
    ) -> Result<TypedLExp<NonVoidType>> {
        let sub_exp = self.typecheck_exp_then_convert_array_then_assert_scalar(*sub_exp)?;

        let typ = match sub_exp.typ().as_ref() {
            ScalarType::Ptr(PointerType { pointee_type }) => {
                NonVoidType::try_from(pointee_type.clone()).map_err(|_typ| ())
            }
            _ => Err(()),
        };
        let typ = typ.map_err(|()| anyhow!("Cannot dereference {sub_exp:#?}"))?;
        /* Note, disallowing deref(ptr_to_void) also disallows addrof(deref(ptr_to_void)).
        This is non-compliant with the C standard. */

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
