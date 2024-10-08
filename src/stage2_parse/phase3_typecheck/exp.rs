use super::{TypeCheckedCAst, TypeChecker};
use crate::{
    common::types_frontend::{ArithmeticType, PointerType, VarType},
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
    utils::noop,
};
use anyhow::{anyhow, Result};

impl TypeChecker {
    /// + Validate the input types.
    /// + Annotate the output type.
    pub(super) fn typecheck_exp(
        &mut self,
        exp: Expression<ResolvedCAst>,
    ) -> Result<TypedExpression<Expression<TypeCheckedCAst>>> {
        let exp = match exp {
            Expression::Const(konst) => {
                let typ = self
                    .var_type_repo
                    .get_or_new(konst.arithmetic_type().into());
                let exp = Expression::Const(konst);
                TypedExpression { typ, exp }
            }
            Expression::Var(ident) => {
                let typ = self.symbol_table.get_var_type(&ident)?.clone();
                let exp = Expression::Var(ident);
                TypedExpression { typ, exp }
            }
            Expression::Cast(Cast { typ, sub_exp }) => {
                let sub_exp = self.typecheck_exp(*sub_exp)?;

                Self::cast_explicitly(typ, sub_exp)?
            }
            Expression::Unary(Unary { op, sub_exp }) => {
                let sub_exp = Box::new(self.typecheck_exp(*sub_exp)?);

                if matches!(
                    (&op, sub_exp.typ.as_ref()),
                    (
                        UnaryOperator::Complement,
                        VarType::Arithmetic(ArithmeticType::Double)
                    ) | (
                        UnaryOperator::Negate | UnaryOperator::Complement,
                        VarType::Pointer(_)
                    )
                ) {
                    return Err(anyhow!("Cannot apply {op:?} on {sub_exp:?}"));
                }

                let typ = match &op {
                    UnaryOperator::Not => self.var_type_repo.get_or_new(ArithmeticType::Int.into()),
                    UnaryOperator::Complement | UnaryOperator::Negate => sub_exp.typ.clone(),
                };
                let exp = Expression::Unary(Unary { op, sub_exp });
                TypedExpression { typ, exp }
            }
            Expression::Binary(Binary { op, lhs, rhs }) => {
                use BinaryOperator as BO;

                let lhs = self.typecheck_exp(*lhs)?;
                let rhs = self.typecheck_exp(*rhs)?;

                let (out_typ, out_lhs, out_rhs) = match &op {
                    BO::And | BO::Or => {
                        let int_typ = self.var_type_repo.get_or_new(ArithmeticType::Int.into());
                        (int_typ, lhs, rhs)
                    }
                    BO::Eq | BO::Neq | BO::Lt | BO::Lte | BO::Gt | BO::Gte => {
                        let (_, lhs, rhs) = Self::cast_to_common_type(lhs, rhs)?;
                        let int_typ = self.var_type_repo.get_or_new(ArithmeticType::Int.into());
                        (int_typ, lhs, rhs)
                    }
                    BO::Sub | BO::Add | BO::Mul | BO::Div | BO::Rem => {
                        let (common_typ, lhs, rhs) = Self::cast_to_common_type(lhs, rhs)?;

                        if matches!(
                            (&op, common_typ.as_ref()),
                            (BO::Rem, VarType::Arithmetic(ArithmeticType::Double))
                                | (_, VarType::Pointer(_))
                        ) {
                            return Err(anyhow!("Cannot apply {op:?} on {lhs:?} and {rhs:?}"));
                        }

                        (common_typ, lhs, rhs)
                    }
                };
                let out_exp = Expression::Binary(Binary {
                    op,
                    lhs: Box::new(out_lhs),
                    rhs: Box::new(out_rhs),
                });
                TypedExpression { typ: out_typ, exp: out_exp }
            }
            Expression::Assignment(Assignment { lhs, rhs }) => {
                let lhs = self.typecheck_exp_lvalue(*lhs)?;
                let typ = lhs.typ.clone();

                let rhs = self.cast_by_assignment(&typ, *rhs)?;

                let exp = Expression::Assignment(Assignment {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                });
                TypedExpression { typ, exp }
            }
            Expression::Conditional(Conditional { condition, then, elze }) => {
                let condition = self.typecheck_exp(*condition)?;
                let then = self.typecheck_exp(*then)?;
                let elze = self.typecheck_exp(*elze)?;

                let (typ, then, elze) = Self::cast_to_common_type(then, elze)?;

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

                let args = fun_typ
                    .params
                    .iter()
                    .zip(args.into_iter())
                    .map(|(param_typ, arg_exp)| self.cast_by_assignment(param_typ, arg_exp))
                    .collect::<Result<Vec<_>>>()?;

                let typ = fun_typ.ret.clone();
                let exp = Expression::FunctionCall(FunctionCall { ident, args });
                TypedExpression { typ, exp }
            }
            Expression::Dereference(Dereference(sub_exp)) => {
                let sub_exp = Box::new(self.typecheck_exp(*sub_exp)?);

                let typ = match sub_exp.typ.as_ref() {
                    VarType::Pointer(PointerType { pointee_type }) => pointee_type.clone(),
                    _ => return Err(anyhow!("Cannot dereference {sub_exp:?}")),
                };

                let exp = Expression::Dereference(Dereference(sub_exp));
                TypedExpression { typ, exp }
            }
            Expression::AddrOf(AddrOf(sub_exp)) => {
                let sub_exp = Box::new(self.typecheck_exp_lvalue(*sub_exp)?);

                let pointee_type = sub_exp.typ.clone();
                let typ = self
                    .var_type_repo
                    .get_or_new(PointerType { pointee_type }.into());

                let exp = Expression::AddrOf(AddrOf(sub_exp));
                TypedExpression { typ, exp }
            }
        };
        Ok(exp)
    }
    fn typecheck_exp_lvalue(
        &mut self,
        exp: Expression<ResolvedCAst>,
    ) -> Result<TypedExpression<LvalueExpression<TypeCheckedCAst>>> {
        match exp {
            Expression::Var(_) => noop!(),
            Expression::Dereference(_) => noop!(),
            actual => return Err(anyhow!("Expected lvalue expression, but found {actual:?}")),
        }
        let TypedExpression { typ, exp } = self.typecheck_exp(exp)?;
        let exp = match exp {
            Expression::Var(ident) => LvalueExpression::Var(ident),
            Expression::Dereference(deref) => LvalueExpression::Dereference(deref),
            _ => unreachable!(),
        };
        Ok(TypedExpression { typ, exp })
    }
}
