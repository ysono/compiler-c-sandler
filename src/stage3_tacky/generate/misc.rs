use super::FunInstrsGenerator;
use crate::{
    common::{
        types_backend::OperandByteLen,
        types_frontend::{ArithmeticType, VarType},
    },
    ds_n_a::singleton::Singleton,
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
};
use std::cmp::Ordering;
use std::rc::Rc;

impl<'a> FunInstrsGenerator<'a> {
    /* C Cast */

    pub(super) fn gen_exp_cast(
        &mut self,
        c::Cast { typ: dst_typ, sub_exp }: c::Cast<TypeCheckedCAst>,
    ) -> Value {
        let src_typ = sub_exp.typ.clone();
        let src_val = self.gen_exp(*sub_exp);
        if dst_typ == src_typ {
            /* Eg `int a = 10; int b = ((int) a) + (a = 1000);`
            If the input is an object, its value could be modified by another expression;
            therefore eliding could change the evaluation of a surrounding overall expression;
            but C does not define the ordering of evaluating 2+ causally independent expressions;
            therefore eliding is valid. */
            src_val
        } else {
            match (dst_typ.as_ref(), src_typ.as_ref()) {
                (VarType::Arithmetic(dst_ari_typ), VarType::Arithmetic(src_ari_typ)) => {
                    let dst = self.symbol_table.declare_var_anon(dst_typ.clone());
                    let srcdst = SrcDst {
                        src: src_val,
                        dst: Rc::clone(&dst),
                    };

                    let instr = match (dst_ari_typ, src_ari_typ) {
                        (_, ArithmeticType::Double) => {
                            if dst_ari_typ.is_signed() {
                                Instruction::DoubleToInt(srcdst)
                            } else {
                                Instruction::DoubleToUInt(srcdst)
                            }
                        }
                        (ArithmeticType::Double, _) => {
                            if src_ari_typ.is_signed() {
                                Instruction::IntToDouble(srcdst)
                            } else {
                                Instruction::UIntToDouble(srcdst)
                            }
                        }
                        _ => {
                            /* Then, casting between different integer types. */
                            let dst_bytelen = OperandByteLen::from(*dst_ari_typ);
                            let src_bytelen = OperandByteLen::from(*src_ari_typ);
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
                    Value::Variable(dst)
                }
                _ => todo!(),
            }
        }
    }

    /* Function call */

    pub(super) fn gen_exp_fun_call(
        &mut self,
        c::FunctionCall { ident, args }: c::FunctionCall<TypeCheckedCAst>,
        out_typ: Singleton<VarType>,
    ) -> Value {
        let result = self.symbol_table.declare_var_anon(out_typ);

        /* Begin instructions */

        let args = args
            .into_iter()
            .map(|arg| self.gen_exp(arg))
            .collect::<Vec<_>>();

        self.instrs.push(Instruction::FunCall(FunCall {
            ident,
            args,
            dst: Rc::clone(&result),
        }));

        Value::Variable(result)
    }
}
