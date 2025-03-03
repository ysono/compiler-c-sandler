use super::TypeChecker;
use crate::{
    common::{
        types_backend::OperandByteLen,
        types_frontend::{
            ArithmeticType, NonAggrType, ObjType, PointerType, ScalarType, SubObjType,
        },
    },
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
    utils::Either,
};
use anyhow::{Result, anyhow};
use owning_ref::OwningRef;
use std::{borrow::Cow, cmp::Ordering, convert::identity};

/// Common type
///
/// Given an expression that requires its two sub-expressions to have the same type,
///     cast these sub-expressions to their "common type".
impl TypeChecker {
    pub(super) fn cast_to_common_nonaggr_type(
        &mut self,
        exp1: NonAggrExp,
        exp2: NonAggrExp,
    ) -> Result<(NonAggrExp, NonAggrExp)> {
        match (exp1.typ().as_ref(), exp2.typ().as_ref()) {
            (NonAggrType::Void(_), NonAggrType::Void(_)) => Ok((exp1, exp2)),
            (NonAggrType::Scalar(s1), NonAggrType::Scalar(s2)) => {
                let s1 = s1.clone();
                let s2 = s2.clone();
                let exp1 = exp1.map_typ(identity, |_| s1);
                let exp2 = exp2.map_typ(identity, |_| s2);

                let (exp1, exp2) = self.cast_to_common_scalar_type(exp1, exp2)?;

                let exp1 = exp1.map_typ(identity, NonAggrType::from);
                let exp2 = exp2.map_typ(identity, NonAggrType::from);
                Ok((exp1, exp2))
            }
            (t1 @ NonAggrType::Void(_), t2 @ NonAggrType::Scalar(_))
            | (t1 @ NonAggrType::Scalar(_), t2 @ NonAggrType::Void(_)) => {
                Err(anyhow!("No common type between {t1:#?} and {t2:#?}"))
            }
        }
    }
    pub(super) fn cast_to_common_scalar_type(
        &mut self,
        exp1: ScalarExp,
        exp2: ScalarExp,
    ) -> Result<(ScalarExp, ScalarExp)> {
        let common_typ = self.derive_common_scalar_type(&exp1, &exp2)?;

        let exp1 = Self::maybe_insert_cast_node(Cow::Borrowed(&common_typ), exp1);
        let exp2 = Self::maybe_insert_cast_node(Cow::Owned(common_typ), exp2);

        Ok((exp1, exp2))
    }
    fn derive_common_scalar_type(
        &mut self,
        exp1: &ScalarExp,
        exp2: &ScalarExp,
    ) -> Result<SubObjType<ScalarType>> {
        match (exp1.typ().as_ref(), exp2.typ().as_ref()) {
            (ScalarType::Arith(a1), ScalarType::Arith(a2)) => {
                let ari_typ = Self::derive_common_arithmetic_type(*a1, *a2);
                Ok(self.get_scalar_type(ari_typ))
            }
            (ScalarType::Ptr(ptr_typ), _) => Self::derive_common_pointer_type(ptr_typ, exp1, exp2),
            (_, ScalarType::Ptr(ptr_typ)) => Self::derive_common_pointer_type(ptr_typ, exp2, exp1),
        }
    }
    /// Aka "usual arithmetic conversion".
    fn derive_common_arithmetic_type(
        mut a1: ArithmeticType,
        mut a2: ArithmeticType,
    ) -> ArithmeticType {
        /* Promote any of the 3 character types to `int`. */
        if a1.is_character() {
            a1 = ArithmeticType::Int;
        }
        if a2.is_character() {
            a2 = ArithmeticType::Int;
        }

        match (a1, a2) {
            _ if a1 == a2 => a1,
            (ArithmeticType::Double, _) => a1,
            (_, ArithmeticType::Double) => a2,
            _ => {
                /* Then, each side is one of {Int, Long, UInt, ULong}. */
                let bytelen1 = OperandByteLen::from(a1);
                let bytelen2 = OperandByteLen::from(a2);
                match bytelen1.cmp(&bytelen2) {
                    Ordering::Greater => a1,
                    Ordering::Less => a2,
                    Ordering::Equal => {
                        if a1.is_signed() {
                            a2
                        } else {
                            a1
                        }
                    }
                }
            }
        }
    }
    fn derive_common_pointer_type(
        ptr_typ: &PointerType,
        ptr_exp: &ScalarExp,
        other_exp: &ScalarExp,
    ) -> Result<SubObjType<ScalarType>> {
        Self::detect_common_pointer_type(ptr_typ, other_exp)
            .map_err(|()| anyhow!("No common type between {ptr_typ:#?} and {other_exp:#?}"))
            .map(|either| match either {
                Either::Left => ptr_exp.typ().clone(),
                Either::Right => other_exp.typ().clone(),
            })
    }
    fn detect_common_pointer_type(
        ptr_typ: &PointerType,
        other_exp: &ScalarExp,
    ) -> Result<Either, ()> {
        match other_exp.typ().as_ref() {
            ScalarType::Arith(_) => match other_exp {
                TypedExp::R(TypedRExp { exp: RExp::Const(k), .. }) if k.is_zero_integer() => {
                    Ok(Either::Left)
                }
                _ => Err(()),
            },
            ScalarType::Ptr(ptr_typ_2) => {
                let PointerType { pointee_type: t1 } = ptr_typ;
                let PointerType { pointee_type: t2 } = ptr_typ_2;
                if t1 == t2 {
                    /* First compare by Rc::ptr_eq(). */
                    Ok(Either::Left)
                } else {
                    match (t1.as_ref(), t2.as_ref()) {
                        (ObjType::Void(_), _) => Ok(Either::Left),
                        (_, ObjType::Void(_)) => Ok(Either::Right),
                        _ => Err(()),
                    }
                }
            }
        }
    }
}

/// Casting explicitly
impl TypeChecker {
    pub(super) fn cast_explicitly(
        &mut self,
        Cast { typ: to, sub_exp: from }: Cast<ResolvedCAst>,
    ) -> Result<TypedRExp<NonAggrType>> {
        use NonAggrType as NAT;

        let to = to.into_res()?;
        let to = NonAggrType::try_from(to)
            .map_err(|typ| anyhow!("Cannot explicitly cast to {typ:#?}"))?;

        let from = self.typecheck_exp_then_convert_array(*from)?;

        let ok = match (&to, from.typ().as_ref()) {
            (NAT::Void(_), NAT::Void(_)) => Ok(()),
            (NAT::Void(_), NAT::Scalar(_)) => Ok(()),
            (NAT::Scalar(_), NAT::Void(_)) => Err(()),
            (NAT::Scalar(s2), NAT::Scalar(s1)) => match (s2.as_ref(), s1.as_ref()) {
                (ScalarType::Ptr(_), ScalarType::Arith(ArithmeticType::Double)) => Err(()),
                (ScalarType::Arith(ArithmeticType::Double), ScalarType::Ptr(_)) => Err(()),
                _ => Ok(()),
            },
        };
        let () = ok.map_err(|()| anyhow!("Cannot explicitly cast {to:#?} <- {from:#?}"))?;

        let typed_rexp = Self::insert_cast_node(to, from);
        Ok(typed_rexp)
    }
}

/// Casting "as if by assignment"
impl TypeChecker {
    /// Cast unidirectionally and implicitly, in various assignment-like contexts.
    pub(super) fn cast_by_assignment(
        &mut self,
        to: Cow<'_, SubObjType<ScalarType>>,
        from: Expression<ResolvedCAst>,
    ) -> Result<ScalarExp> {
        let from = self.typecheck_exp_then_convert_array_then_assert_scalar(from)?;

        let () = Self::can_cast_by_assignment(to.as_ref(), &from)?;

        let typed_exp = Self::maybe_insert_cast_node(to, from);
        Ok(typed_exp)
    }
    pub(super) fn can_cast_by_assignment(to: &ScalarType, from: &ScalarExp) -> Result<()> {
        let ok = match (to, from.typ().as_ref()) {
            (ScalarType::Arith(_), ScalarType::Arith(_)) => Ok(()),
            (ScalarType::Arith(_), ScalarType::Ptr(_)) => Err(()),
            (ScalarType::Ptr(ptr_to), _) => {
                Self::detect_common_pointer_type(ptr_to, from).map(|_either| ())
            }
        };
        ok.map_err(|()| anyhow!("Cannot \"convert as if by assignment\" {to:#?} <- {from:#?}"))
    }
}

/// Promotion
impl TypeChecker {
    pub(super) fn promote_character_to_int(&mut self, in_sca_exp: ScalarExp) -> ScalarExp {
        if matches!(in_sca_exp.typ().as_ref(), ScalarType::Arith(a) if a.is_character()) {
            let in_nonaggr_exp = in_sca_exp.map_typ(identity, NonAggrType::from);
            let out_sca_typ = self.get_scalar_type(ArithmeticType::Int);
            let out_sca_rexp = Self::insert_cast_node(out_sca_typ, in_nonaggr_exp);
            TypedExp::R(out_sca_rexp)
        } else {
            in_sca_exp
        }
    }
}

/// Helpers on casting
impl TypeChecker {
    pub(super) fn maybe_insert_cast_node(
        to: Cow<'_, SubObjType<ScalarType>>,
        from: ScalarExp,
    ) -> ScalarExp {
        if to.as_ref() == from.typ() {
            from
        } else {
            let from = from.map_typ(identity, NonAggrType::from);
            let out_sca_rexp = Self::insert_cast_node(to.into_owned(), from);
            TypedExp::R(out_sca_rexp)
        }
    }
    fn insert_cast_node<Typ: Clone + Into<NonAggrType>>(
        to: Typ,
        from: NonAggrExp,
    ) -> TypedRExp<Typ> {
        TypedRExp {
            exp: RExp::Cast(Cast {
                typ: to.clone().into(),
                sub_exp: Box::new(from),
            }),
            typ: to,
        }
    }
}

/// Helpers on types
impl TypeChecker {
    pub(super) fn get_scalar_type<Typ: Into<ScalarType>>(
        &mut self,
        typ: Typ,
    ) -> SubObjType<ScalarType> {
        let obj_typ = ObjType::Scalar(typ.into());
        let obj_typ_node = self.obj_type_repo.get_or_new(obj_typ);
        OwningRef::new(obj_typ_node).map(|o| match o {
            ObjType::Scalar(s) => s,
            _ => unreachable!(),
        })
    }
}
