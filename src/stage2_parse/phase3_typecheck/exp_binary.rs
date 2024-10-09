use super::{TypeCheckedCAst, TypeChecker};
use crate::{
    common::types_frontend::{ArithmeticType, VarType},
    ds_n_a::singleton::Singleton,
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
    utils::noop,
};
use anyhow::{anyhow, Result};

type TypedExp = TypedExpression<Expression<TypeCheckedCAst>>;

impl TypeChecker {
    pub(super) fn typecheck_exp_binary(
        &mut self,
        Binary { op, lhs, rhs }: Binary<ResolvedCAst>,
    ) -> Result<TypedExp> {
        use BinaryOperator as O;

        let lhs = self.typecheck_exp(*lhs)?;
        let rhs = self.typecheck_exp(*rhs)?;

        match op {
            O::Logic(op) => {
                let int_typ = self.var_type_repo.get_or_new(ArithmeticType::Int.into());
                Ok(new_binary_exp(op, lhs, rhs, int_typ))
            }
            O::Cmp(op) => self.typecheck_cmp(op, lhs, rhs),
            O::Arith(op) => self.typecheck_arith(op, lhs, rhs),
        }
    }

    fn typecheck_cmp(
        &mut self,
        op: ComparisonBinaryOperator,
        lhs: TypedExp,
        rhs: TypedExp,
    ) -> Result<TypedExp> {
        use ComparisonBinaryOperator as OC;

        let (lhs, rhs) = match op {
            OC::Eq | OC::Neq => Self::cast_to_common_type(lhs, rhs)?,
            OC::Lt | OC::Lte | OC::Gt | OC::Gte => match (lhs.typ.as_ref(), rhs.typ.as_ref()) {
                (VarType::Arith(_), VarType::Arith(_)) => Self::cast_to_common_type(lhs, rhs)?,
                _ => unimplemented!("ch15"),
            },
        };

        let int_typ = self.var_type_repo.get_or_new(ArithmeticType::Int.into());

        Ok(new_binary_exp(op, lhs, rhs, int_typ))
    }

    fn typecheck_arith(
        &mut self,
        op: ArithmeticBinaryOperator,
        lhs: TypedExp,
        rhs: TypedExp,
    ) -> Result<TypedExp> {
        use ArithmeticBinaryOperator as OA;

        match (lhs.typ.as_ref(), rhs.typ.as_ref()) {
            (VarType::Arith(_), VarType::Arith(_)) => noop!(),
            _ => unimplemented!("ch15"),
        }

        let (lhs, rhs) = Self::cast_to_common_type(lhs, rhs)?;
        let common_typ = lhs.typ.clone();

        if matches!(
            (&op, common_typ.as_ref()),
            (OA::Rem, VarType::Arith(ArithmeticType::Double))
        ) {
            return Err(anyhow!("Cannot apply {op:?} on {lhs:?} and {rhs:?}"));
        }

        Ok(new_binary_exp(op, lhs, rhs, common_typ))
    }
}

fn new_binary_exp<Op: Into<BinaryOperator>>(
    op: Op,
    lhs: TypedExp,
    rhs: TypedExp,
    typ: Singleton<VarType>,
) -> TypedExp {
    TypedExp {
        exp: Expression::Binary(Binary {
            op: op.into(),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }),
        typ,
    }
}
