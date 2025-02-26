use super::TypeChecker;
use crate::{
    common::types_frontend::{ArithmeticType, ObjType, PointerType, ScalarType},
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
};
use anyhow::{Result, anyhow};
use owning_ref::OwningRef;
use std::borrow::Cow;

impl TypeChecker {
    /// + Validate the input types.
    /// + Annotate the output type.
    pub(super) fn typecheck_exp(
        &mut self,
        exp: Expression<ResolvedCAst>,
    ) -> Result<TypedExp<ObjType>> {
        match exp {
            Expression::R(rexp) => self.typecheck_rexp(rexp).map(TypedExp::R),
            Expression::L(lexp) => self.typecheck_lexp(lexp).map(TypedExp::L),
        }
    }
    fn typecheck_rexp(&mut self, rexp: RExp<ResolvedCAst>) -> Result<TypedRExp> {
        let typed_rexp = match rexp {
            RExp::Const(konst) => {
                let typ = self.get_scalar_type(konst.arithmetic_type());
                let exp = RExp::Const(konst);
                TypedRExp { typ, exp }
            }
            RExp::Cast(cast) => self.cast_explicitly(cast)?,
            RExp::Unary(Unary { op, sub_exp }) => {
                let sub_exp = self.typecheck_exp_and_convert_to_scalar(*sub_exp)?;

                let err_invalid_op = || Err(anyhow!("Cannot apply {op:#?} on {sub_exp:#?}"));

                let (out_typ, final_sub_exp);
                match &op {
                    UnaryOperator::Complement => match sub_exp.typ().as_ref() {
                        ScalarType::Arith(ArithmeticType::Double) | ScalarType::Ptr(_) => {
                            return err_invalid_op();
                        }
                        ScalarType::Arith(_) => {
                            final_sub_exp = self.promote_character_to_int(sub_exp);
                            out_typ = final_sub_exp.typ().clone();
                        }
                    },
                    UnaryOperator::Negate => match sub_exp.typ().as_ref() {
                        ScalarType::Ptr(_) => {
                            return err_invalid_op();
                        }
                        ScalarType::Arith(_) => {
                            final_sub_exp = self.promote_character_to_int(sub_exp);
                            out_typ = final_sub_exp.typ().clone();
                        }
                    },
                    UnaryOperator::Not => {
                        final_sub_exp = sub_exp;
                        out_typ = self.get_scalar_type(ArithmeticType::Int);
                    }
                }

                let exp = RExp::Unary(Unary {
                    op,
                    sub_exp: Box::new(final_sub_exp),
                });
                TypedRExp { typ: out_typ, exp }
            }
            RExp::Binary(binary) => self.typecheck_exp_binary(binary)?,
            RExp::Conditional(Conditional { condition, then, elze }) => {
                let condition = self.typecheck_exp_and_convert_to_scalar(*condition)?;
                let then = self.typecheck_exp_and_convert_to_scalar(*then)?;
                let elze = self.typecheck_exp_and_convert_to_scalar(*elze)?;

                let (then, elze) = self.cast_to_common_type(then, elze)?;
                let typ = then.typ().clone();

                let exp = RExp::Conditional(Conditional {
                    condition: Box::new(condition),
                    then: Box::new(then),
                    elze: Box::new(elze),
                });
                TypedRExp { typ, exp }
            }
            RExp::FunctionCall(FunctionCall { ident, args }) => {
                let fun_typ = self.frontend_symtab.symtab().get_fun_type(&ident)?;
                if fun_typ.params.len() != args.len() {
                    return Err(anyhow!(
                        "Mismatched signature. {ident:#?} : {fun_typ:#?} vs {args:#?}"
                    ));
                }
                let fun_typ = fun_typ.clone();

                let args = fun_typ
                    .params
                    .iter()
                    .zip(args.into_iter())
                    .map(|(param_typ, arg_exp)| {
                        self.cast_by_assignment(Cow::Borrowed(param_typ), arg_exp)
                    })
                    .collect::<Result<Vec<_>>>()?;

                let typ = fun_typ.ret.clone();
                let exp = RExp::FunctionCall(FunctionCall { ident, args });
                TypedRExp { typ, exp }
            }
            RExp::Assignment(Assignment { lhs, rhs }) => {
                let lhs = {
                    let lexp = extract_lexp(*lhs)?;
                    let obj_typed_lexp = self.typecheck_lexp(lexp)?;
                    let sca_typed_exp = self.convert_lexp_to_scalar(obj_typed_lexp);
                    let sca_typed_lexp = extract_typed_lexp(sca_typed_exp)?;
                    sca_typed_lexp
                };

                let typ = lhs.typ.clone();
                let rhs = self.cast_by_assignment(Cow::Borrowed(&typ), *rhs)?;

                let exp = RExp::Assignment(Assignment {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                });
                TypedRExp { typ, exp }
            }
            RExp::AddrOf(AddrOf(sub_exp)) => {
                let sub_exp = {
                    let lexp = extract_lexp(*sub_exp)?;
                    let obj_typed_lexp = self.typecheck_lexp(lexp)?;
                    obj_typed_lexp
                };

                let pointee_type = sub_exp.typ.as_owner().clone();
                let typ = self.get_scalar_type(PointerType { pointee_type });

                let exp = RExp::AddrOf(AddrOf(Box::new(sub_exp)));
                TypedRExp { typ, exp }
            }
        };
        Ok(typed_rexp)
    }
    fn typecheck_lexp(&mut self, lexp: LExp<ResolvedCAst>) -> Result<TypedLExp<ObjType>> {
        let typed_lexp = match lexp {
            LExp::String(chars) => {
                let (static_obj_ident, typ) = self.define_static_readonly_string(chars);

                TypedLExp {
                    typ: OwningRef::new(typ),
                    exp: LExp::String(static_obj_ident),
                }
            }
            LExp::Var(ident) => {
                let typ = self.frontend_symtab.symtab().get_obj_type(&ident)?.clone();
                let typ = OwningRef::new(typ);
                let exp = LExp::Var(ident);
                TypedLExp { typ, exp }
            }
            LExp::Dereference(Dereference(sub_exp)) => {
                let sub_exp = Box::new(self.typecheck_exp_and_convert_to_scalar(*sub_exp)?);

                let typ = match sub_exp.typ().as_ref() {
                    ScalarType::Ptr(PointerType { pointee_type }) => pointee_type.clone(),
                    _ => return Err(anyhow!("Cannot dereference {sub_exp:#?}")),
                };
                let typ = OwningRef::new(typ);

                let exp = LExp::Dereference(Dereference(sub_exp));
                TypedLExp { typ, exp }
            }
            LExp::Subscript(subscr) => self.typecheck_exp_subscript(subscr)?,
        };
        Ok(typed_lexp)
    }

    pub(super) fn typecheck_exp_and_convert_to_scalar(
        &mut self,
        exp: Expression<ResolvedCAst>,
    ) -> Result<TypedExp<ScalarType>> {
        let obj_typed_exp = self.typecheck_exp(exp)?;

        let sca_typed_exp = match obj_typed_exp {
            TypedExp::R(sca_typed_rexp) => TypedExp::R(sca_typed_rexp),
            TypedExp::L(obj_typed_lexp) => self.convert_lexp_to_scalar(obj_typed_lexp),
        };
        Ok(sca_typed_exp)
    }
    /// Transforms an expression with type=array
    /// into an expression that evaluates to a pointer to the first element (_not_ a pointer to the array).
    fn convert_lexp_to_scalar(
        &mut self,
        obj_typed_lexp: TypedLExp<ObjType>,
    ) -> TypedExp<ScalarType> {
        match Self::extract_scalar_type(obj_typed_lexp.typ.as_owner().clone()) {
            Ok(sca_typ) => {
                let sca_typed_lexp = TypedLExp {
                    exp: obj_typed_lexp.exp,
                    typ: sca_typ,
                };
                TypedExp::L(sca_typed_lexp)
            }
            Err(arr_typ) => {
                let ptr_typ = arr_typ.as_ptr_to_elem();
                let sca_typ = self.get_scalar_type(ptr_typ);
                let sca_typed_rexp = TypedRExp {
                    exp: RExp::AddrOf(AddrOf(Box::new(obj_typed_lexp))),
                    typ: sca_typ,
                };
                TypedExp::R(sca_typed_rexp)
            }
        }
    }
}

fn extract_lexp(exp: Expression<ResolvedCAst>) -> Result<LExp<ResolvedCAst>> {
    match exp {
        Expression::R(rexp) => Err(anyhow!("Expected lvalue expression, but found {rexp:#?}")),
        Expression::L(lexp) => Ok(lexp),
    }
}
fn extract_typed_lexp<LTyp>(typed_exp: TypedExp<LTyp>) -> Result<TypedLExp<LTyp>> {
    match typed_exp {
        TypedExp::R(typed_rexp) => Err(anyhow!(
            "Expected lvalue expression, but found {typed_rexp:#?}"
        )),
        TypedExp::L(typed_lexp) => Ok(typed_lexp),
    }
}
