use super::TypeChecker;
use crate::{
    common::{
        primitive::Const,
        types_backend::OperandByteLen,
        types_frontend::{ArithmeticType, ArrayType, ObjType, PointerType, ScalarType, SubObjType},
    },
    ds_n_a::singleton::Singleton,
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
};
use anyhow::{anyhow, Result};
use owning_ref::OwningRef;
use std::borrow::Borrow;
use std::cmp::Ordering;

impl TypeChecker {
    /* Common type */

    /// Given an expression that requires its two sub-expressions to have the same type,
    ///     cast these sub-expressions to their "common type".
    pub(super) fn cast_to_common_type(
        &mut self,
        exp1: TypedExp<ScalarType>,
        exp2: TypedExp<ScalarType>,
    ) -> Result<(TypedExp<ScalarType>, TypedExp<ScalarType>)> {
        let common_typ = self.derive_common_type(&exp1, &exp2)?;

        let exp1 = Self::maybe_insert_cast_node(&common_typ, exp1);
        let exp2 = Self::maybe_insert_cast_node(common_typ, exp2);

        Ok((exp1, exp2))
    }
    fn derive_common_type(
        &mut self,
        exp1: &TypedExp<ScalarType>,
        exp2: &TypedExp<ScalarType>,
    ) -> Result<SubObjType<ScalarType>> {
        match (exp1.typ().as_ref(), exp2.typ().as_ref()) {
            (ScalarType::Arith(a1), ScalarType::Arith(a2)) => {
                let ari_typ = Self::derive_common_arithmetic_type(*a1, *a2);
                Ok(self.get_scalar_type(ari_typ))
            }
            (ScalarType::Ptr(ptr_typ), _) => {
                Self::validate_common_pointer_type(ptr_typ, exp2)?;
                Ok(exp1.typ().clone())
            }
            (_, ScalarType::Ptr(ptr_typ)) => {
                Self::validate_common_pointer_type(ptr_typ, exp1)?;
                Ok(exp2.typ().clone())
            }
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
    fn validate_common_pointer_type(
        ptr_typ: &PointerType,
        other_exp: &TypedExp<ScalarType>,
    ) -> Result<()> {
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
        let to = Self::extract_scalar_type(to)
            .map_err(|typ| anyhow!("Cannot explicitly cast to {typ:?}"))?;

        let from = self.typecheck_exp_and_convert_to_scalar(*from)?;

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

    /// Cast unidirectionally and implicitly, in various assignment-like contexts.
    pub(super) fn cast_by_assignment(
        &mut self,
        to: Singleton<ObjType>,
        from: Expression<ResolvedCAst>,
    ) -> Result<TypedExp<ScalarType>> {
        let to = Self::extract_scalar_type(to)
            .map_err(|typ| anyhow!("Cannot \"convert as if by assignment\" to {typ:?}"))?;

        self.cast_scalar_by_assignment(to, from)
    }
    pub(super) fn cast_scalar_by_assignment<St: Borrow<SubObjType<ScalarType>>>(
        &mut self,
        to: St,
        from: Expression<ResolvedCAst>,
    ) -> Result<TypedExp<ScalarType>> {
        let from = self.typecheck_exp_and_convert_to_scalar(from)?;

        let () = Self::can_cast_by_assignment(to.borrow().as_ref(), &from)?;

        let typed_exp = Self::maybe_insert_cast_node(to, from);
        Ok(typed_exp)
    }
    pub(super) fn cast_statically_by_assignment(
        &mut self,
        to: &Singleton<ObjType>,
        from: Expression<ResolvedCAst>,
    ) -> Result<Const> {
        let to = Self::extract_scalar_type_ref(to)
            .map_err(|typ| anyhow!("Cannot \"convert as if by assignment\" to {typ:?}"))?;

        let in_konst = match &from {
            Expression::R(RExp::Const(konst)) => *konst,
            _ => return Err(anyhow!("Casting statically is supported on constexprs only. For each constexpr, only a simple const literal is supported."))
        };

        let from = self.typecheck_exp_and_convert_to_scalar(from)?;

        let () = Self::can_cast_by_assignment(to, &from)?;

        let out_konst = in_konst.cast_at_compile_time(to);
        Ok(out_konst)
    }
    fn can_cast_by_assignment(to: &ScalarType, from: &TypedExp<ScalarType>) -> Result<()> {
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

    /* Promotion */

    pub(super) fn promote_character_to_int(
        &mut self,
        in_typed_exp: TypedExp<ScalarType>,
    ) -> TypedExp<ScalarType> {
        if matches!(in_typed_exp.typ().as_ref(), ScalarType::Arith(a) if a.is_character()) {
            let int_typ = self.get_scalar_type(ArithmeticType::Int);
            let out_rexp = Self::insert_cast_node(int_typ, in_typed_exp);
            TypedExp::R(out_rexp)
        } else {
            in_typed_exp
        }
    }

    /* Helpers on casting */

    pub(super) fn maybe_insert_cast_node<St: Borrow<SubObjType<ScalarType>>>(
        to: St,
        from: TypedExp<ScalarType>,
    ) -> TypedExp<ScalarType> {
        if to.borrow() == from.typ() {
            from
        } else {
            let out_typed_rexp = Self::insert_cast_node(to.borrow().clone(), from);
            TypedExp::R(out_typed_rexp)
        }
    }
    fn insert_cast_node(to: SubObjType<ScalarType>, from: TypedExp<ScalarType>) -> TypedRExp {
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
        Self::extract_scalar_type(obj_typ).unwrap()
    }
    pub(super) fn extract_scalar_type(
        obj_typ: Singleton<ObjType>,
    ) -> Result<SubObjType<ScalarType>, SubObjType<ArrayType>> {
        match obj_typ.as_ref() {
            ObjType::Scalar(sca_typ) => {
                let sca_typ = sca_typ as *const ScalarType;
                let sca_typ = unsafe { &*sca_typ };
                let ownref = OwningRef::new(obj_typ).map(|_| sca_typ);
                Ok(ownref)
            }
            ObjType::Array(arr_typ) => {
                let arr_typ = arr_typ as *const ArrayType;
                let arr_typ = unsafe { &*arr_typ };
                let ownref = OwningRef::new(obj_typ).map(|_| arr_typ);
                Err(ownref)
            }
        }
    }
    pub(super) fn extract_scalar_type_ref(obj_typ: &ObjType) -> Result<&ScalarType, &ArrayType> {
        match obj_typ {
            ObjType::Scalar(s) => Ok(s),
            ObjType::Array(a) => Err(a),
        }
    }
}
