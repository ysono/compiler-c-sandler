use super::TypeChecker;
use crate::{
    common::types_frontend::{ArithmeticType, ScalarType},
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
};
use anyhow::{Result, anyhow};
use std::borrow::Cow;

impl TypeChecker {
    pub(super) fn typecheck_exp_unary(
        &mut self,
        Unary { op, sub_exp }: Unary<ResolvedCAst>,
    ) -> Result<TypedRExp> {
        let sub_exp = self.typecheck_exp_and_convert_to_scalar(*sub_exp)?;

        let err_invalid_op = || Err(anyhow!("Cannot apply {op:#?} on {sub_exp:#?}"));

        let (out_typ, final_sub_exp);
        match &op {
            UnaryOperator::Complement => match sub_exp.typ().as_ref() {
                ScalarType::Arith(ArithmeticType::Double) | ScalarType::Ptr(_) => {
                    return err_invalid_op();
                }
                ScalarType::Arith(_) => {
                    final_sub_exp = self.promote_character_to_int(sub_exp);
                    out_typ = final_sub_exp.typ().clone();
                }
            },
            UnaryOperator::Negate => match sub_exp.typ().as_ref() {
                ScalarType::Ptr(_) => {
                    return err_invalid_op();
                }
                ScalarType::Arith(_) => {
                    final_sub_exp = self.promote_character_to_int(sub_exp);
                    out_typ = final_sub_exp.typ().clone();
                }
            },
            UnaryOperator::Not => {
                final_sub_exp = sub_exp;
                out_typ = self.get_scalar_type(ArithmeticType::Int);
            }
        }

        let exp = RExp::Unary(Unary {
            op,
            sub_exp: Box::new(final_sub_exp),
        });
        Ok(TypedRExp { typ: out_typ, exp })
    }

    pub(super) fn typecheck_exp_conditional(
        &mut self,
        Conditional { condition, then, elze }: Conditional<ResolvedCAst>,
    ) -> Result<TypedRExp> {
        let condition = self.typecheck_exp_and_convert_to_scalar(*condition)?;
        let then = self.typecheck_exp_and_convert_to_scalar(*then)?;
        let elze = self.typecheck_exp_and_convert_to_scalar(*elze)?;

        let (then, elze) = self.cast_to_common_type(then, elze)?;
        let typ = then.typ().clone();

        let exp = RExp::Conditional(Conditional {
            condition: Box::new(condition),
            then: Box::new(then),
            elze: Box::new(elze),
        });
        Ok(TypedRExp { typ, exp })
    }

    pub(super) fn typecheck_exp_funcall(
        &mut self,
        FunctionCall { ident, args }: FunctionCall<ResolvedCAst>,
    ) -> Result<TypedRExp> {
        let fun_typ = self.frontend_symtab.symtab().get_fun_type(&ident)?;
        if fun_typ.params.len() != args.len() {
            return Err(anyhow!(
                "Mismatched signature. {ident:#?} : {fun_typ:#?} vs {args:#?}"
            ));
        }
        let fun_typ = fun_typ.clone();

        let args = fun_typ
            .params
            .iter()
            .zip(args.into_iter())
            .map(|(param_typ, arg_exp)| self.cast_by_assignment(Cow::Borrowed(param_typ), arg_exp))
            .collect::<Result<Vec<_>>>()?;

        let typ = fun_typ.ret.clone();
        let exp = RExp::FunctionCall(FunctionCall { ident, args });
        Ok(TypedRExp { typ, exp })
    }
}
