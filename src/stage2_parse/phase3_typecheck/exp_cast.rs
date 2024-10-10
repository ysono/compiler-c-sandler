use super::TypeChecker;
use crate::{
    common::{
        primitive::Const,
        types_backend::OperandByteLen,
        types_frontend::{ArithmeticType, ObjType, PointerType, ScalarType, SubObjType},
    },
    ds_n_a::singleton::Singleton,
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
    utils::Either,
};
use anyhow::{anyhow, Result};
use owning_ref::OwningRef;
use std::borrow::Borrow;
use std::cmp::Ordering;

impl TypeChecker {
    /* Common type */

    pub(super) fn cast_to_common_type(
        mut exp1: TypedExp,
        mut exp2: TypedExp,
    ) -> Result<(TypedExp, TypedExp)> {
        match Self::derive_common_type(&exp1, &exp2)? {
            Either::Left(()) => {
                exp2 = Self::maybe_insert_cast_node(exp1.typ(), exp2);
            }
            Either::Right(()) => {
                exp1 = Self::maybe_insert_cast_node(exp2.typ(), exp1);
            }
        }
        Ok((exp1, exp2))
    }
    fn derive_common_type(exp1: &TypedExp, exp2: &TypedExp) -> Result<Either<(), ()>> {
        match (exp1.typ().as_ref(), exp2.typ().as_ref()) {
            (ScalarType::Arith(a1), ScalarType::Arith(a2)) => {
                Ok(Self::derive_common_arithmetic_type(*a1, *a2))
            }
            (ScalarType::Ptr(ptr_typ), _) => {
                Self::derive_common_pointer_type(ptr_typ, exp2).map(Either::Left)
            }
            (_, ScalarType::Ptr(ptr_typ)) => {
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
        if matches!(other_exp.typ().as_ref(), ScalarType::Ptr(ptr_typ_2) if ptr_typ == ptr_typ_2) {
            Ok(())
        } else if matches!(other_exp, TypedExp::R(TypedRExp{exp: RExp::Const(k), ..}) if k.is_zero_integer())
        {
            Ok(())
        } else {
            Err(anyhow!(
                "No common type between {ptr_typ:?} and {other_exp:?}"
            ))
        }
    }

    /* Casting explicitly */

    pub(super) fn cast_explicitly(
        &mut self,
        Cast { typ: to, sub_exp: from }: Cast<ResolvedCAst>,
    ) -> Result<TypedRExp> {
        let to = Self::extract_scalar_type(to);

        let from = self.typecheck_exp(*from)?;

        let ok = match (to.as_ref(), from.typ().as_ref()) {
            (t2, t1) if t2 == t1 => Ok(()),

            (ScalarType::Ptr(_), ScalarType::Arith(ArithmeticType::Double)) => Err(()),
            (ScalarType::Arith(ArithmeticType::Double), ScalarType::Ptr(_)) => Err(()),

            _ => Ok(()),
        };
        let () = ok.map_err(|()| anyhow!("Cannot explicitly cast {to:?} <- {from:?}"))?;

        let typed_rexp = Self::insert_cast_node(to, from);
        Ok(typed_rexp)
    }

    /* Casting "as if by assignment" */

    pub(super) fn cast_by_assignment(
        &mut self,
        to: Singleton<ObjType>,
        from: Expression<ResolvedCAst>,
    ) -> Result<TypedExp> {
        let to = Self::extract_scalar_type(to);

        let from = self.typecheck_exp(from)?;

        let () = Self::can_cast_by_assignment(to.as_ref(), &from)?;

        let typed_exp = Self::maybe_insert_cast_node(to, from);
        Ok(typed_exp)
    }
    pub(super) fn cast_statically_by_assignment(
        &mut self,
        to: &Singleton<ObjType>,
        from: Expression<ResolvedCAst>,
    ) -> Result<Const> {
        let to = Self::extract_scalar_type_ref(to);

        let in_konst = match &from {
            Expression::R(RExp::Const(konst)) => *konst,
            _ => return Err(anyhow!("Casting statically is supported on constexprs only. For each constexpr, only a simple const literal is supported."))
        };

        let from = self.typecheck_exp(from)?;

        let () = Self::can_cast_by_assignment(to, &from)?;

        let out_konst = in_konst.cast_at_compile_time(to);
        Ok(out_konst)
    }
    fn can_cast_by_assignment(to: &ScalarType, from: &TypedExp) -> Result<()> {
        let ok = match (to, from.typ().as_ref()) {
            (t2, t1) if t2 == t1 => Ok(()),

            (ScalarType::Arith(_), ScalarType::Arith(_)) => Ok(()),

            (ScalarType::Ptr(_), _) => match from {
                TypedExp::R(TypedRExp { exp: RExp::Const(k), .. }) if k.is_zero_integer() => Ok(()),
                _ => Err(()),
            },

            _ => Err(()),
        };
        ok.map_err(|()| anyhow!("Cannot \"convert as if by assignment\" {to:?} <- {from:?}"))
    }

    /* Helpers on casting */

    fn maybe_insert_cast_node<St: Borrow<SubObjType<ScalarType>>>(
        to: St,
        from: TypedExp,
    ) -> TypedExp {
        if to.borrow() == from.typ() {
            from
        } else {
            let out_typed_rexp = Self::insert_cast_node(to.borrow().clone(), from);
            TypedExp::R(out_typed_rexp)
        }
    }
    fn insert_cast_node(to: SubObjType<ScalarType>, from: TypedExp) -> TypedRExp {
        TypedRExp {
            exp: RExp::Cast(Cast {
                typ: to.as_owner().clone(),
                sub_exp: Box::new(from),
            }),
            typ: to,
        }
    }

    /* Helpers on types */

    pub(super) fn get_scalar_type<Typ: Into<ScalarType>>(
        &mut self,
        typ: Typ,
    ) -> SubObjType<ScalarType> {
        let obj_typ = self.obj_type_repo.get_or_new(ObjType::Scalar(typ.into()));
        Self::extract_scalar_type(obj_typ)
    }
    pub(super) fn extract_scalar_type(obj_typ: Singleton<ObjType>) -> SubObjType<ScalarType> {
        OwningRef::new(obj_typ).map(|obj_typ| match obj_typ {
            ObjType::Scalar(s) => s,
        })
    }
    pub(super) fn extract_scalar_type_ref(obj_typ: &ObjType) -> &ScalarType {
        match obj_typ {
            ObjType::Scalar(s) => s,
        }
    }
}
