use super::{TypeCheckedCAst, TypeChecker};
use crate::{
    common::types_frontend::VarType,
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
};
use anyhow::{anyhow, Result};

impl TypeChecker {
    /// + Validate the input types.
    /// + Annotate the output type.
    pub(super) fn typecheck_exp(
        &mut self,
        exp: Expression<ResolvedCAst>,
    ) -> Result<TypedExpression<TypeCheckedCAst>> {
        let exp = match exp {
            Expression::Const(konst) => {
                let typ = konst.var_type();
                let exp = Expression::Const(konst);
                TypedExpression { typ, exp }
            }
            Expression::Var(ident) => {
                let typ = self.symbol_table.get_var_type(&ident)?;
                let exp = Expression::Var(ident);
                TypedExpression { typ, exp }
            }
            Expression::Cast(Cast { typ, sub_exp }) => {
                let sub_exp = Box::new(self.typecheck_exp(*sub_exp)?);
                let exp = Expression::Cast(Cast { typ, sub_exp });
                TypedExpression { typ, exp }
            }
            Expression::Unary(Unary { op, sub_exp }) => {
                let sub_exp = Box::new(self.typecheck_exp(*sub_exp)?);

                if matches!(
                    (&op, sub_exp.typ),
                    (UnaryOperator::Complement, VarType::Double)
                ) {
                    return Err(anyhow!("Cannot apply {op:?} on {sub_exp:?}"));
                }

                let typ = match &op {
                    UnaryOperator::Not => VarType::Int,
                    UnaryOperator::Complement | UnaryOperator::Negate => sub_exp.typ,
                };
                let exp = Expression::Unary(Unary { op, sub_exp });
                TypedExpression { typ, exp }
            }
            Expression::Binary(Binary { op, lhs, rhs }) => {
                use BinaryOperator as BO;

                let lhs = self.typecheck_exp(*lhs)?;
                let rhs = self.typecheck_exp(*rhs)?;

                let common_typ = VarType::derive_common_type(lhs.typ, rhs.typ);
                let cast_exp =
                    |exp: TypedExpression<TypeCheckedCAst>| Self::maybe_cast_exp(exp, common_typ);

                if matches!((&op, common_typ), (BO::Rem, VarType::Double)) {
                    return Err(anyhow!("Cannot apply {op:?} on {lhs:?} and {rhs:?}"));
                }

                let (out_typ, lhs, rhs) = match &op {
                    BO::And | BO::Or => (VarType::Int, lhs, rhs),
                    BO::Eq | BO::Neq | BO::Lt | BO::Lte | BO::Gt | BO::Gte => {
                        (VarType::Int, cast_exp(lhs), cast_exp(rhs))
                    }
                    BO::Sub | BO::Add | BO::Mul | BO::Div | BO::Rem => {
                        (common_typ, cast_exp(lhs), cast_exp(rhs))
                    }
                };
                let out_exp = Expression::Binary(Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                });
                TypedExpression { typ: out_typ, exp: out_exp }
            }
            Expression::Assignment(Assignment { lhs, rhs }) => {
                let typ = self.symbol_table.get_var_type(&lhs)?;
                let rhs = self.typecheck_exp(*rhs)?;
                let rhs = Self::maybe_cast_exp(rhs, typ);
                let exp = Expression::Assignment(Assignment { lhs, rhs: Box::new(rhs) });
                TypedExpression { typ, exp }
            }
            Expression::Conditional(Conditional { condition, then, elze }) => {
                let condition = self.typecheck_exp(*condition)?;
                let then = self.typecheck_exp(*then)?;
                let elze = self.typecheck_exp(*elze)?;

                let typ = VarType::derive_common_type(then.typ, elze.typ);
                let then = Self::maybe_cast_exp(then, typ);
                let elze = Self::maybe_cast_exp(elze, typ);

                let exp = Expression::Conditional(Conditional {
                    condition: Box::new(condition),
                    then: Box::new(then),
                    elze: Box::new(elze),
                });
                TypedExpression { typ, exp }
            }
            Expression::FunctionCall(FunctionCall { ident, args }) => {
                let fun_typ = self.symbol_table.get_fun_type(&ident)?;
                if fun_typ.params.len() != args.len() {
                    return Err(anyhow!(
                        "Mismatched signature. {ident:?} : {fun_typ:?} vs {args:?}"
                    ));
                }
                let fun_typ = fun_typ.clone();

                let mut out_args = Vec::with_capacity(args.len());
                for (param_typ, arg_exp) in fun_typ.params.iter().cloned().zip(args.into_iter()) {
                    let arg_exp = self.typecheck_exp(arg_exp)?;
                    let arg_exp = Self::maybe_cast_exp(arg_exp, param_typ);
                    out_args.push(arg_exp);
                }
                let exp = Expression::FunctionCall(FunctionCall { ident, args: out_args });
                TypedExpression { typ: fun_typ.ret, exp }
            }
        };
        Ok(exp)
    }
}
