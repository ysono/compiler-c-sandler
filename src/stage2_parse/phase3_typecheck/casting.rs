use super::{TypeCheckedCAst, TypeChecker};
use crate::{
    common::{types_backend::OperandByteLen, types_frontend::VarType},
    stage2_parse::c_ast::*,
};
use std::cmp::Ordering;

type TypedExp = TypedExpression<TypeCheckedCAst>;

impl TypeChecker {
    pub(super) fn cast_to_common_type(
        exp1: TypedExp,
        exp2: TypedExp,
    ) -> (VarType, TypedExp, TypedExp) {
        let common_typ = Self::derive_common_type(exp1.typ, exp2.typ);
        let exp1 = Self::maybe_cast_exp(common_typ, exp1);
        let exp2 = Self::maybe_cast_exp(common_typ, exp2);
        (common_typ, exp1, exp2)
    }
    fn derive_common_type(typ1: VarType, typ2: VarType) -> VarType {
        match (typ1, typ2) {
            _ if typ1 == typ2 => typ1,
            (VarType::Double, _) | (_, VarType::Double) => VarType::Double,
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

    pub(super) fn maybe_cast_exp(typ: VarType, exp: TypedExp) -> TypedExp {
        if typ == exp.typ {
            exp
        } else {
            let exp = Expression::Cast(Cast { typ, sub_exp: Box::new(exp) });
            TypedExpression { typ, exp }
        }
    }
}
