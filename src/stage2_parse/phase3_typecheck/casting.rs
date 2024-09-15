use super::{TypeCheckedCAst, TypeChecker};
use crate::{
    common::{
        types_backend::OperandByteLen,
        types_frontend::{ArithmeticType, VarType},
    },
    ds_n_a::singleton::Singleton,
    stage2_parse::c_ast::*,
};
use std::borrow::Borrow;
use std::cmp::Ordering;

type TypedExp = TypedExpression<Expression<TypeCheckedCAst>>;

impl TypeChecker {
    pub(super) fn cast_to_common_type(
        &mut self,
        exp1: TypedExp,
        exp2: TypedExp,
    ) -> (Singleton<VarType>, TypedExp, TypedExp) {
        let common_typ = self.derive_common_type(&exp1.typ, &exp2.typ);
        let exp1 = Self::maybe_cast_exp(&common_typ, exp1);
        let exp2 = Self::maybe_cast_exp(&common_typ, exp2);
        (common_typ, exp1, exp2)
    }
    fn derive_common_type(
        &mut self,
        typ1: &Singleton<VarType>,
        typ2: &Singleton<VarType>,
    ) -> Singleton<VarType> {
        match (typ1.as_ref(), typ2.as_ref()) {
            (VarType::Arithmetic(a1), VarType::Arithmetic(a2)) => {
                let common_ari_typ = Self::derive_common_arithmetic_type(*a1, *a2);
                self.var_type_repo.get_or_new(common_ari_typ.into())
            }
            _ => todo!(),
        }
    }
    fn derive_common_arithmetic_type(typ1: ArithmeticType, typ2: ArithmeticType) -> ArithmeticType {
        match (typ1, typ2) {
            _ if typ1 == typ2 => typ1,
            (ArithmeticType::Double, _) | (_, ArithmeticType::Double) => ArithmeticType::Double,
            _ => {
                let bytelen1 = OperandByteLen::from(typ1);
                let bytelen2 = OperandByteLen::from(typ2);
                match bytelen1.cmp(&bytelen2) {
                    Ordering::Equal => {
                        if typ1.is_signed() {
                            typ2
                        } else {
                            typ1
                        }
                    }
                    Ordering::Greater => typ1,
                    Ordering::Less => typ2,
                }
            }
        }
    }

    pub(super) fn maybe_cast_exp<Vt>(typ: Vt, exp: TypedExp) -> TypedExp
    where
        Vt: Borrow<Singleton<VarType>>,
    {
        if typ.borrow() == &exp.typ {
            exp
        } else {
            TypedExpression {
                typ: typ.borrow().clone(),
                exp: Expression::Cast(Cast {
                    typ: typ.borrow().clone(),
                    sub_exp: Box::new(exp),
                }),
            }
        }
    }
}
