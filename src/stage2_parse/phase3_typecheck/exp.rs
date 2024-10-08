use super::TypeChecker;
use crate::{
    common::types_frontend::{ArithmeticType, PointerType, VarType},
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
};
use anyhow::{anyhow, Result};

impl TypeChecker {
    /// + Validate the input types.
    /// + Annotate the output type.
    pub(super) fn typecheck_exp(&mut self, exp: Expression<ResolvedCAst>) -> Result<TypedExp> {
        match exp {
            Expression::R(rexp) => self.typecheck_rexp(rexp).map(TypedExp::R),
            Expression::L(lexp) => self.typecheck_lexp(lexp).map(TypedExp::L),
        }
    }
    fn typecheck_rexp(&mut self, rexp: RExp<ResolvedCAst>) -> Result<TypedRExp> {
        let typed_rexp = match rexp {
            RExp::Const(konst) => {
                let typ = self
                    .var_type_repo
                    .get_or_new(konst.arithmetic_type().into());
                let exp = RExp::Const(konst);
                TypedRExp { typ, exp }
            }
            RExp::Cast(Cast { typ, sub_exp }) => {
                let sub_exp = self.typecheck_exp(*sub_exp)?;

                Self::cast_explicitly(typ, sub_exp)?
            }
            RExp::Unary(Unary { op, sub_exp }) => {
                let sub_exp = Box::new(self.typecheck_exp(*sub_exp)?);

                if matches!(
                    (&op, sub_exp.typ().as_ref()),
                    (
                        UnaryOperator::Complement,
                        VarType::Arith(ArithmeticType::Double)
                    ) | (
                        UnaryOperator::Negate | UnaryOperator::Complement,
                        VarType::Ptr(_)
                    )
                ) {
                    return Err(anyhow!("Cannot apply {op:?} on {sub_exp:?}"));
                }

                let typ = match &op {
                    UnaryOperator::Not => self.var_type_repo.get_or_new(ArithmeticType::Int.into()),
                    UnaryOperator::Complement | UnaryOperator::Negate => sub_exp.typ().clone(),
                };
                let exp = RExp::Unary(Unary { op, sub_exp });
                TypedRExp { typ, exp }
            }
            RExp::Binary(binary) => self.typecheck_exp_binary(binary)?,
            RExp::Conditional(Conditional { condition, then, elze }) => {
                let condition = self.typecheck_exp(*condition)?;
                let then = self.typecheck_exp(*then)?;
                let elze = self.typecheck_exp(*elze)?;

                let (then, elze) = Self::cast_to_common_type(then, elze)?;
                let typ = then.typ().clone();

                let exp = RExp::Conditional(Conditional {
                    condition: Box::new(condition),
                    then: Box::new(then),
                    elze: Box::new(elze),
                });
                TypedRExp { typ, exp }
            }
            RExp::FunctionCall(FunctionCall { ident, args }) => {
                let fun_typ = self.symbol_table.get_fun_type(&ident)?;
                if fun_typ.params.len() != args.len() {
                    return Err(anyhow!(
                        "Mismatched signature. {ident:?} : {fun_typ:?} vs {args:?}"
                    ));
                }
                let fun_typ = fun_typ.clone();

                let args = fun_typ
                    .params
                    .iter()
                    .zip(args.into_iter())
                    .map(|(param_typ, arg_exp)| self.cast_by_assignment(param_typ, arg_exp))
                    .collect::<Result<Vec<_>>>()?;

                let typ = fun_typ.ret.clone();
                let exp = RExp::FunctionCall(FunctionCall { ident, args });
                TypedRExp { typ, exp }
            }
            RExp::Assignment(Assignment { lhs, rhs }) => {
                let lhs = extract_lexp(*lhs)?;
                let lhs = self.typecheck_lexp(lhs)?;
                let typ = lhs.typ.clone();

                let rhs = self.cast_by_assignment(&typ, *rhs)?;

                let exp = RExp::Assignment(Assignment {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                });
                TypedRExp { typ, exp }
            }
            RExp::AddrOf(AddrOf(sub_exp)) => {
                let sub_exp = extract_lexp(*sub_exp)?;
                let sub_exp = Box::new(self.typecheck_lexp(sub_exp)?);

                let pointee_type = sub_exp.typ.clone();
                let typ = self
                    .var_type_repo
                    .get_or_new(PointerType { pointee_type }.into());

                let exp = RExp::AddrOf(AddrOf(sub_exp));
                TypedRExp { typ, exp }
            }
        };
        Ok(typed_rexp)
    }
    fn typecheck_lexp(&mut self, lexp: LExp<ResolvedCAst>) -> Result<TypedLExp> {
        let typed_lexp = match lexp {
            LExp::Var(ident) => {
                let typ = self.symbol_table.get_var_type(&ident)?.clone();
                let exp = LExp::Var(ident);
                TypedLExp { typ, exp }
            }
            LExp::Dereference(Dereference(sub_exp)) => {
                let sub_exp = Box::new(self.typecheck_exp(*sub_exp)?);

                let typ = match sub_exp.typ().as_ref() {
                    VarType::Ptr(PointerType { pointee_type }) => pointee_type.clone(),
                    _ => return Err(anyhow!("Cannot dereference {sub_exp:?}")),
                };

                let exp = LExp::Dereference(Dereference(sub_exp));
                TypedLExp { typ, exp }
            }
        };
        Ok(typed_lexp)
    }
}

fn extract_lexp(exp: Expression<ResolvedCAst>) -> Result<LExp<ResolvedCAst>> {
    match exp {
        Expression::R(rexp) => Err(anyhow!("Expected lvalue expression, but found {rexp:?}")),
        Expression::L(lexp) => Ok(lexp),
    }
}
