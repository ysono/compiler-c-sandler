use super::{TypeCheckedCAst, TypeChecker};
use crate::{
    common::{
        primitive::Const,
        types_backend::OperandByteLen,
        types_frontend::{ArithmeticType, PointerType, VarType},
    },
    ds_n_a::singleton::Singleton,
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
    utils::Either,
};
use anyhow::{anyhow, Result};
use std::borrow::Borrow;
use std::cmp::Ordering;

type TypedExp = TypedExpression<Expression<TypeCheckedCAst>>;

impl TypeChecker {
    /* Common type */

    pub(super) fn cast_to_common_type(
        mut exp1: TypedExp,
        mut exp2: TypedExp,
    ) -> Result<(TypedExp, TypedExp)> {
        match Self::derive_common_type(&exp1, &exp2)? {
            Either::Left(()) => {
                exp2 = Self::maybe_insert_cast_node(&exp1.typ, exp2);
            }
            Either::Right(()) => {
                exp1 = Self::maybe_insert_cast_node(&exp2.typ, exp1);
            }
        }
        Ok((exp1, exp2))
    }
    fn derive_common_type(exp1: &TypedExp, exp2: &TypedExp) -> Result<Either<(), ()>> {
        match (exp1.typ.as_ref(), exp2.typ.as_ref()) {
            (VarType::Arith(a1), VarType::Arith(a2)) => {
                Ok(Self::derive_common_arithmetic_type(*a1, *a2))
            }
            (VarType::Ptr(ptr_typ), _) => {
                Self::derive_common_pointer_type(ptr_typ, exp2).map(Either::Left)
            }
            (_, VarType::Ptr(ptr_typ)) => {
                Self::derive_common_pointer_type(ptr_typ, exp1).map(Either::Right)
            }
        }
    }
    /// Aka "usual arithmetic conversion".
    fn derive_common_arithmetic_type(a1: ArithmeticType, a2: ArithmeticType) -> Either<(), ()> {
        match (a1, a2) {
            _ if a1 == a2 => Either::Left(()),
            (ArithmeticType::Double, _) => Either::Left(()),
            (_, ArithmeticType::Double) => Either::Right(()),
            _ => {
                let bytelen1 = OperandByteLen::from(a1);
                let bytelen2 = OperandByteLen::from(a2);
                match bytelen1.cmp(&bytelen2) {
                    Ordering::Greater => Either::Left(()),
                    Ordering::Less => Either::Right(()),
                    Ordering::Equal => {
                        if a1.is_signed() {
                            Either::Right(())
                        } else {
                            Either::Left(())
                        }
                    }
                }
            }
        }
    }
    fn derive_common_pointer_type(ptr_typ: &PointerType, other_exp: &TypedExp) -> Result<()> {
        #[allow(clippy::if_same_then_else)]
        if matches!(other_exp.typ.as_ref(), VarType::Ptr(ptr_typ_2) if ptr_typ == ptr_typ_2) {
            Ok(())
        } else if matches!(other_exp.exp, Expression::Const(k) if k.is_zero_integer()) {
            Ok(())
        } else {
            Err(anyhow!(
                "No common type between {ptr_typ:?} and {other_exp:?}"
            ))
        }
    }

    /* Casting explicitly */

    pub(super) fn cast_explicitly(to: Singleton<VarType>, from: TypedExp) -> Result<TypedExp> {
        let err = || Err(anyhow!("Cannot explicitly cast {to:?} <- {from:?}"));

        match (to.as_ref(), from.typ.as_ref()) {
            /* Elide redundant explicit cast. See more comment in the tacky stage. */
            (t2, t1) if t2 == t1 => Ok(from),

            (VarType::Ptr(_), VarType::Arith(ArithmeticType::Double)) => err(),
            (VarType::Arith(ArithmeticType::Double), VarType::Ptr(_)) => err(),

            _ => Ok(Self::insert_cast_node(to, from)),
        }
    }

    /* Casting "as if by assignment" */

    pub(super) fn cast_by_assignment<Vt: Borrow<Singleton<VarType>>>(
        &mut self,
        to: Vt,
        from: Expression<ResolvedCAst>,
    ) -> Result<TypedExp> {
        let from = self.typecheck_exp(from)?;

        let () = Self::can_cast_by_assignment(to.borrow(), &from)?;

        let typed_exp = Self::maybe_insert_cast_node(to, from);
        Ok(typed_exp)
    }
    pub(super) fn cast_statically_by_assignment(
        &mut self,
        to: &Singleton<VarType>,
        from: Expression<ResolvedCAst>,
    ) -> Result<Const> {
        let in_konst = match &from {
            Expression::Const(konst) => *konst,
            _ => return Err(anyhow!("Casting statically is supported on constexprs only. For each constexpr, only a simple const literal is supported."))
        };

        let from = self.typecheck_exp(from)?;

        let () = Self::can_cast_by_assignment(to, &from)?;

        let out_konst = in_konst.cast_at_compile_time(to);
        Ok(out_konst)
    }
    fn can_cast_by_assignment(to: &VarType, from: &TypedExp) -> Result<()> {
        let ok = match (to, from.typ.as_ref()) {
            (t2, t1) if t2 == t1 => Ok(()),

            (VarType::Arith(_), VarType::Arith(_)) => Ok(()),

            (VarType::Ptr(_), _) => match from {
                TypedExp { exp: Expression::Const(k), .. } if k.is_zero_integer() => Ok(()),
                _ => Err(()),
            },

            _ => Err(()),
        };
        ok.map_err(|()| anyhow!("Cannot \"convert as if by assignment\" {to:?} <- {from:?}"))
    }

    /* Helpers */

    fn maybe_insert_cast_node<Vt: Borrow<Singleton<VarType>>>(to: Vt, from: TypedExp) -> TypedExp {
        if to.borrow() == &from.typ {
            from
        } else {
            Self::insert_cast_node(to.borrow().clone(), from)
        }
    }
    fn insert_cast_node(to: Singleton<VarType>, from: TypedExp) -> TypedExp {
        TypedExpression {
            typ: to.clone(),
            exp: Expression::Cast(Cast { typ: to, sub_exp: Box::new(from) }),
        }
    }
}
