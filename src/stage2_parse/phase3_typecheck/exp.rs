use super::TypeChecker;
use crate::{
    common::types_frontend::{NonVoidType, ScalarType, SubObjType},
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
};
use anyhow::Result;

impl TypeChecker {
    /// + Validate the input types.
    /// + Annotate the output type.
    pub(super) fn typecheck_exp(&mut self, exp: Expression<ResolvedCAst>) -> Result<AnyExp> {
        match exp {
            Expression::R(rexp) => self.typecheck_rexp(rexp).map(TypedExp::R),
            Expression::L(lexp) => self.typecheck_lexp(lexp).map(TypedExp::L),
        }
    }

    /// 1. Typecheck the given expression.
    /// 1. If the expression is typed array, then transform it into
    ///     an expression that evaluates to a pointer to the zeroth element (_not_ a pointer to the array).
    pub(super) fn typecheck_exp_and_convert_to_scalar(
        &mut self,
        exp: Expression<ResolvedCAst>,
    ) -> Result<ScalarExp> {
        match exp {
            Expression::R(rexp) => self.typecheck_rexp(rexp).map(TypedExp::R),
            Expression::L(lexp) => {
                let nonvoid_lexp = self.typecheck_lexp(lexp)?;
                let sca_exp = match &nonvoid_lexp.typ {
                    NonVoidType::Scalar(sca_typ) => TypedExp::L(TypedLExp {
                        exp: nonvoid_lexp.exp,
                        typ: sca_typ.clone(),
                    }),
                    NonVoidType::Array(arr_typ) => {
                        let ptr_typ = arr_typ.as_ptr_to_elem();
                        let sca_typ = self.get_scalar_type(ptr_typ);
                        TypedExp::R(TypedRExp {
                            exp: RExp::AddrOf(AddrOf(Box::new(nonvoid_lexp))),
                            typ: sca_typ,
                        })
                    }
                };
                Ok(sca_exp)
            }
        }
    }

    fn typecheck_rexp(
        &mut self,
        rexp: RExp<ResolvedCAst>,
    ) -> Result<TypedRExp<SubObjType<ScalarType>>> {
        match rexp {
            RExp::Const(konst) => {
                let typ = self.get_scalar_type(konst.arithmetic_type());
                let exp = RExp::Const(konst);
                Ok(TypedRExp { typ, exp })
            }
            RExp::Cast(cast) => self.cast_explicitly(cast),
            RExp::Unary(unary) => self.typecheck_exp_unary(unary),
            RExp::Binary(binary) => self.typecheck_exp_binary(binary),
            RExp::Conditional(cond) => self.typecheck_exp_conditional(cond),
            RExp::FunctionCall(funcall) => self.typecheck_exp_funcall(funcall),
            RExp::Assignment(assignment) => self.typecheck_exp_assignment(assignment),
            RExp::AddrOf(addrof) => self.typecheck_exp_addrof(addrof),
            RExp::SizeOfType(_) => todo!(),
            RExp::SizeOfExp(_) => todo!(),
        }
    }

    pub(super) fn typecheck_lexp(
        &mut self,
        lexp: LExp<ResolvedCAst>,
    ) -> Result<TypedLExp<NonVoidType>> {
        match lexp {
            LExp::String(chars) => {
                let (ident, typ) = self.define_static_readonly_string(chars);
                let exp = LExp::String(ident);
                Ok(TypedLExp { typ, exp })
            }
            LExp::Var(ident) => {
                let typ = self.frontend_symtab.symtab().get_obj_type(&ident)?.clone();
                let exp = LExp::Var(ident);
                Ok(TypedLExp { typ, exp })
            }
            LExp::Dereference(deref) => self.typecheck_exp_deref(deref),
            LExp::Subscript(subscr) => self.typecheck_exp_subscript(subscr),
        }
    }
}
