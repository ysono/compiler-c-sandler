use super::{TypeCheckedCAst, TypeChecker};
use crate::{
    common::{
        primitive::Const,
        types_backend::OperandByteLen,
        types_frontend::{ArithmeticType, VarType},
    },
    ds_n_a::singleton::Singleton,
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
    utils::noop,
};
use anyhow::{anyhow, Result};
use std::borrow::Borrow;
use std::cmp::Ordering;

type TypedExp = TypedExpression<Expression<TypeCheckedCAst>>;

impl TypeChecker {
    /* Common type */

    pub(super) fn cast_to_common_type(
        exp1: TypedExp,
        exp2: TypedExp,
    ) -> Result<(Singleton<VarType>, TypedExp, TypedExp)> {
        let common_typ = Self::derive_common_type(&exp1, &exp2)?;
        let exp1 = Self::maybe_insert_cast_node(&common_typ, exp1);
        let exp2 = Self::maybe_insert_cast_node(&common_typ, exp2);
        Ok((common_typ, exp1, exp2))
    }
    fn derive_common_type(exp1: &TypedExp, exp2: &TypedExp) -> Result<Singleton<VarType>> {
        match (exp1.typ.as_ref(), exp2.typ.as_ref()) {
            (VarType::Arithmetic(a1), VarType::Arithmetic(a2)) => {
                match Self::derive_common_arithmetic_type(*a1, *a2) {
                    None => Ok(exp1.typ.clone()),
                    Some(()) => Ok(exp2.typ.clone()),
                }
            }
            (VarType::Pointer(_), _) => Self::derive_common_pointer_type(exp1, exp2),
            (_, VarType::Pointer(_)) => Self::derive_common_pointer_type(exp2, exp1),
        }
    }
    /// Aka "usual arithmetic conversion".
    ///
    /// @return An "Either" type. a1 as `None`; a2 as `Some`.
    fn derive_common_arithmetic_type(a1: ArithmeticType, a2: ArithmeticType) -> Option<()> {
        let ret1 = None;
        let ret2 = Some(());

        match (a1, a2) {
            _ if a1 == a2 => ret1,
            (ArithmeticType::Double, _) => ret1,
            (_, ArithmeticType::Double) => ret2,
            _ => {
                let bytelen1 = OperandByteLen::from(a1);
                let bytelen2 = OperandByteLen::from(a2);
                match bytelen1.cmp(&bytelen2) {
                    Ordering::Greater => ret1,
                    Ordering::Less => ret2,
                    Ordering::Equal => {
                        if a1.is_signed() {
                            ret2
                        } else {
                            ret1
                        }
                    }
                }
            }
        }
    }
    fn derive_common_pointer_type(
        ptr_exp: &TypedExp,
        other_exp: &TypedExp,
    ) -> Result<Singleton<VarType>> {
        #[allow(clippy::if_same_then_else)]
        if ptr_exp.typ == other_exp.typ {
            noop!()
        } else if matches!(&other_exp.exp, Expression::Const(k) if k.is_zero_integer()) {
            noop!()
        } else {
            return Err(anyhow!(
                "No common type between {ptr_exp:?} and {other_exp:?}"
            ));
        }
        Ok(ptr_exp.typ.clone())
    }

    /* Casting explicitly */

    pub(super) fn cast_explicitly(to: Singleton<VarType>, from: TypedExp) -> Result<TypedExp> {
        let err = || Err(anyhow!("Cannot explicitly cast {to:?} <- {from:?}"));

        match (to.as_ref(), from.typ.as_ref()) {
            /* Elide redundant explicit cast. See more comment in the tacky stage. */
            (t2, t1) if t2 == t1 => Ok(from),

            (VarType::Pointer(_), VarType::Arithmetic(ArithmeticType::Double)) => err(),
            (VarType::Arithmetic(ArithmeticType::Double), VarType::Pointer(_)) => err(),

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

            (VarType::Arithmetic(_), VarType::Arithmetic(_)) => Ok(()),

            (VarType::Pointer(_), _) => match from {
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
