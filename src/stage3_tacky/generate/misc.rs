use super::FunInstrsGenerator;
use crate::{
    common::{
        types_backend::OperandByteLen,
        types_frontend::{ArithmeticType, ScalarType, SubObjType},
    },
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
};
use std::cmp::Ordering;

/// C Cast
impl FunInstrsGenerator<'_> {
    pub(super) fn gen_exp_cast(
        &mut self,
        c::Cast { typ: _, sub_exp }: c::Cast<TypeCheckedCAst>,
        dst_typ: SubObjType<ScalarType>,
    ) -> Value {
        let dst_ari_typ = dst_typ.effective_arithmetic_type();
        let src_ari_typ = sub_exp.typ().effective_arithmetic_type();

        let src = self.gen_exp_and_get_value(*sub_exp);

        if dst_ari_typ == src_ari_typ {
            /* Eg `int a = 10; int b = ((int) a) + (a = 1000);`
            If the input is an object, its value could be modified by another expression;
            therefore eliding could change the evaluation of a surrounding overall expression;
            but C does not define the ordering of evaluating 2+ causally independent expressions;
            therefore eliding is valid. */
            src
        } else {
            let dst = self.register_new_value(dst_typ);

            let srcdst = SrcDst { src, dst: dst.clone() };
            let instr = match (dst_ari_typ, src_ari_typ) {
                (_, ArithmeticType::Double) => {
                    if dst_ari_typ.is_signed() {
                        Instruction::DoubleToSInteg(srcdst)
                    } else {
                        Instruction::DoubleToUInteg(srcdst)
                    }
                }
                (ArithmeticType::Double, _) => {
                    if src_ari_typ.is_signed() {
                        Instruction::SIntegToDouble(srcdst)
                    } else {
                        Instruction::UIntegToDouble(srcdst)
                    }
                }
                _ => {
                    /* Then, casting between different integer types. */
                    let dst_bytelen = OperandByteLen::from(dst_ari_typ);
                    let src_bytelen = OperandByteLen::from(src_ari_typ);
                    match dst_bytelen.cmp(&src_bytelen) {
                        Ordering::Equal => Instruction::Copy(srcdst),
                        Ordering::Less => Instruction::Truncate(srcdst),
                        Ordering::Greater => {
                            if src_ari_typ.is_signed() {
                                Instruction::SignExtend(srcdst)
                            } else {
                                Instruction::ZeroExtend(srcdst)
                            }
                        }
                    }
                }
            };
            self.instrs.push(instr);

            dst
        }
    }
}

/// C Function Call
impl FunInstrsGenerator<'_> {
    pub(super) fn gen_exp_fun_call(
        &mut self,
        c::FunctionCall { ident, args }: c::FunctionCall<TypeCheckedCAst>,
        out_typ: SubObjType<ScalarType>,
    ) -> Value {
        let result = self.register_new_value(out_typ);

        /* Begin instructions */

        let args = args
            .into_iter()
            .map(|arg| self.gen_exp_and_get_value(arg))
            .collect::<Vec<_>>();

        self.instrs.push(Instruction::FunCall(FunCall {
            ident,
            args,
            dst: result.clone(),
        }));

        result
    }
}
