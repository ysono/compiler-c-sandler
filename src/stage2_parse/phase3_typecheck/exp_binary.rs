use self::helpers::*;
use super::TypeChecker;
use crate::{
    common::types_frontend::{ArithmeticType, NonVoidType, PointerType, ScalarType, SubObjType},
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
};
use anyhow::{Result, anyhow};
use std::borrow::Cow;

/// Binary
impl TypeChecker {
    pub(super) fn typecheck_exp_binary(
        &mut self,
        Binary { op, lhs, rhs }: Binary<ResolvedCAst>,
    ) -> Result<TypedRExp<SubObjType<ScalarType>>> {
        use BinaryOperator as O;

        let lhs = self.typecheck_exp_then_convert_array_then_assert_scalar(*lhs)?;
        let rhs = self.typecheck_exp_then_convert_array_then_assert_scalar(*rhs)?;

        match op {
            O::Logic(op) => {
                let int_typ = self.get_scalar_type(ArithmeticType::Int);
                Ok(new_binary_exp(op, lhs, rhs, int_typ))
            }
            O::Cmp(op) => self.typecheck_cmp(op, lhs, rhs),
            O::Arith(op) => self.typecheck_arith(op, lhs, rhs),
        }
    }

    fn typecheck_cmp(
        &mut self,
        op: ComparisonBinaryOperator,
        lhs: ScalarExp,
        rhs: ScalarExp,
    ) -> Result<TypedRExp<SubObjType<ScalarType>>> {
        use ComparisonBinaryOperator as OC;

        let (lhs, rhs) = match op {
            OC::Eq | OC::Neq => self.cast_to_common_type(lhs, rhs)?,
            OC::Lt | OC::Lte | OC::Gt | OC::Gte => {
                match (lhs.typ().as_ref(), rhs.typ().as_ref()) {
                    (ScalarType::Arith(_), ScalarType::Arith(_)) => {
                        self.cast_to_common_type(lhs, rhs)?
                    }
                    (ScalarType::Ptr(p1), ScalarType::Ptr(p2)) if p1 == p2 => (lhs, rhs),
                    _ => return Err(anyhow!("Can't compare {lhs:#?} and {rhs:#?}")),
                }
                /* Note, gcc and clang allow comparing pointer vs constexpr-zero-integer as well.
                If we choose to allow this, we can validate using `cast_to_common_type()` on all scalar types. */
            }
        };

        let int_typ = self.get_scalar_type(ArithmeticType::Int);

        Ok(new_binary_exp(op, lhs, rhs, int_typ))
    }

    fn typecheck_arith(
        &mut self,
        op: ArithmeticBinaryOperator,
        lhs: ScalarExp,
        rhs: ScalarExp,
    ) -> Result<TypedRExp<SubObjType<ScalarType>>> {
        use ArithmeticBinaryOperator as OA;

        match op {
            OA::Add => self.typecheck_arith_add(lhs, rhs),
            OA::Sub => self.typecheck_arith_sub(lhs, rhs),
            OA::Mul | OA::Div | OA::Rem => self.typecheck_arith_muldivrem(op, lhs, rhs),
        }
    }

    fn typecheck_arith_add(
        &mut self,
        lhs: ScalarExp,
        rhs: ScalarExp,
    ) -> Result<TypedRExp<SubObjType<ScalarType>>> {
        match (lhs.typ().as_ref(), rhs.typ().as_ref()) {
            (ScalarType::Arith(_), ScalarType::Arith(_)) => {
                let op = ArithmeticBinaryOperator::Add;

                let (lhs, rhs) = self.cast_to_common_type(lhs, rhs)?;
                let common_typ = lhs.typ().clone();

                Ok(new_binary_exp(op, lhs, rhs, common_typ))
            }
            (ScalarType::Ptr(p), ScalarType::Arith(a)) if can_addsub_ptr_and_arith(p, a) => {
                Ok(self.typecheck_arith_add_ptr(lhs, rhs))
            }
            (ScalarType::Arith(a), ScalarType::Ptr(p)) if can_addsub_ptr_and_arith(p, a) => {
                Ok(self.typecheck_arith_add_ptr(rhs, lhs))
            }
            _ => Err(anyhow!("Can't add {lhs:#?} and {rhs:#?}")),
        }
    }
    fn typecheck_arith_add_ptr(
        &mut self,
        ptr_exp: ScalarExp,
        integ_exp: ScalarExp,
    ) -> TypedRExp<SubObjType<ScalarType>> {
        let op = PointerArithmeticBinaryOperator::PointerPlusInteger;

        let long_typ = self.get_scalar_type(ArithmeticType::Long);
        let integ_exp = Self::maybe_insert_cast_node(Cow::Owned(long_typ), integ_exp);

        let ptr_typ = ptr_exp.typ().clone();

        new_binary_exp(op, ptr_exp, integ_exp, ptr_typ)
    }

    fn typecheck_arith_sub(
        &mut self,
        lhs: ScalarExp,
        rhs: ScalarExp,
    ) -> Result<TypedRExp<SubObjType<ScalarType>>> {
        match (lhs.typ().as_ref(), rhs.typ().as_ref()) {
            (ScalarType::Arith(_), ScalarType::Arith(_)) => {
                let op = ArithmeticBinaryOperator::Sub;

                let (lhs, rhs) = self.cast_to_common_type(lhs, rhs)?;
                let common_typ = lhs.typ().clone();

                Ok(new_binary_exp(op, lhs, rhs, common_typ))
            }
            (ScalarType::Ptr(p), ScalarType::Arith(a)) if can_addsub_ptr_and_arith(p, a) => {
                let op = PointerArithmeticBinaryOperator::PointerMinusInteger;

                let long_typ = self.get_scalar_type(ArithmeticType::Long);
                let rhs = Self::maybe_insert_cast_node(Cow::Owned(long_typ), rhs);

                let ptr_typ = lhs.typ().clone();

                Ok(new_binary_exp(op, lhs, rhs, ptr_typ))
            }
            (ScalarType::Ptr(p1), ScalarType::Ptr(p2)) if can_sub_ptr_and_ptr(p1, p2) => {
                let op = PointerArithmeticBinaryOperator::PointerMinusPointer;

                let long_typ = self.get_scalar_type(ArithmeticType::Long);

                Ok(new_binary_exp(op, lhs, rhs, long_typ))
            }
            _ => Err(anyhow!("Can't sub {lhs:#?} and {rhs:#?}")),
        }
    }

    fn typecheck_arith_muldivrem(
        &mut self,
        op: ArithmeticBinaryOperator,
        lhs: ScalarExp,
        rhs: ScalarExp,
    ) -> Result<TypedRExp<SubObjType<ScalarType>>> {
        use ArithmeticBinaryOperator as OA;

        let (lhs, rhs) = self.cast_to_common_type(lhs, rhs)?;
        let common_typ = lhs.typ().clone();

        if matches!(
            (&op, common_typ.as_ref()),
            (OA::Rem, ScalarType::Arith(ArithmeticType::Double)) | (_, ScalarType::Ptr(_))
        ) {
            return Err(anyhow!("Can't apply {op:#?} on {lhs:#?} and {rhs:#?}"));
        }

        Ok(new_binary_exp(op, lhs, rhs, common_typ))
    }
}

/// Helpers
mod helpers {
    use super::*;

    pub fn can_addsub_ptr_and_arith(
        PointerType { pointee_type }: &PointerType,
        arith_typ: &ArithmeticType,
    ) -> bool {
        NonVoidType::is_nonvoid(pointee_type.as_ref()) && (arith_typ.is_integer())
    }
    pub fn validate_addsub_ptr_and_arith(
        PointerType { pointee_type }: &PointerType,
        arith_typ: &ArithmeticType,
    ) -> Result<NonVoidType, ()> {
        if arith_typ.is_integer() {
            match NonVoidType::try_from(pointee_type.clone()) {
                Ok(nv) => Ok(nv),
                Err(_) => Err(()),
            }
        } else {
            Err(())
        }
    }

    pub fn can_sub_ptr_and_ptr(
        PointerType { pointee_type: t1 }: &PointerType,
        PointerType { pointee_type: t2 }: &PointerType,
    ) -> bool {
        (t1 == t2) && NonVoidType::is_nonvoid(t1)
    }

    pub fn new_binary_exp<Op: Into<TypeCheckedBinaryOperator>>(
        op: Op,
        lhs: ScalarExp,
        rhs: ScalarExp,
        typ: SubObjType<ScalarType>,
    ) -> TypedRExp<SubObjType<ScalarType>> {
        TypedRExp {
            exp: RExp::Binary(Binary {
                op: op.into(),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }),
            typ,
        }
    }
}

/// Subscript
impl TypeChecker {
    pub(super) fn typecheck_exp_subscript(
        &mut self,
        Subscript { exp1, exp2 }: Subscript<ResolvedCAst>,
    ) -> Result<TypedLExp<NonVoidType>> {
        let exp1 = self.typecheck_exp_then_convert_array_then_assert_scalar(*exp1)?;
        let exp2 = self.typecheck_exp_then_convert_array_then_assert_scalar(*exp2)?;

        let res = match (exp1.typ().as_ref(), exp2.typ().as_ref()) {
            (ScalarType::Ptr(p), ScalarType::Arith(a)) => {
                match validate_addsub_ptr_and_arith(p, a) {
                    Ok(pointee_typ) => Ok((exp1, exp2, pointee_typ)),
                    Err(()) => Err((exp1, exp2)),
                }
            }
            (ScalarType::Arith(a), ScalarType::Ptr(p)) => {
                match validate_addsub_ptr_and_arith(p, a) {
                    Ok(pointee_typ) => Ok((exp2, exp1, pointee_typ)),
                    Err(()) => Err((exp1, exp2)),
                }
            }
            _ => Err((exp1, exp2)),
        };
        let (ptr_exp, idx_exp, pointee_typ) =
            res.map_err(|(exp1, exp2)| anyhow!("Can't subscript {exp1:#?} and {exp2:#?}"))?;

        let long_typ = self.get_scalar_type(ArithmeticType::Long);
        let idx_exp = Self::maybe_insert_cast_node(Cow::Owned(long_typ), idx_exp);

        let typed_lexp = TypedLExp {
            exp: LExp::Subscript(Subscript {
                exp1: Box::new(ptr_exp),
                exp2: Box::new(idx_exp),
            }),
            typ: pointee_typ,
        };
        Ok(typed_lexp)
    }
}
