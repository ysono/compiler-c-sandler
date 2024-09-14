use super::{TypeCheckedCAst, TypeChecker};
use crate::{common::types_frontend::VarType, stage2_parse::c_ast::*};

impl TypeChecker {
    pub(super) fn maybe_cast_exp(
        exp: TypedExpression<TypeCheckedCAst>,
        typ: VarType,
    ) -> TypedExpression<TypeCheckedCAst> {
        if exp.typ == typ {
            exp
        } else {
            let exp = Expression::Cast(Cast { typ, sub_exp: Box::new(exp) });
            TypedExpression { typ, exp }
        }
    }
}
