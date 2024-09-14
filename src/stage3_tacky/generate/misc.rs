use super::FunInstrsGenerator;
use crate::{
    common::{types_backend::OperandByteLen, types_frontend::VarType},
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
};
use std::cmp::Ordering;
use std::rc::Rc;

impl<'a> FunInstrsGenerator<'a> {
    /* C Cast */

    pub(super) fn gen_exp_cast(
        &mut self,
        c::Cast { typ, sub_exp }: c::Cast<TypeCheckedCAst>,
    ) -> ReadableValue {
        let sub_typ = sub_exp.typ;
        let sub_val = self.gen_exp(*sub_exp);
        if sub_typ == typ {
            sub_val
        } else {
            let dst = self.symbol_table.declare_var_anon(typ);
            let srcdst = SrcDst {
                src: sub_val,
                dst: Rc::clone(&dst),
            };

            let instr = if sub_typ == VarType::Double {
                if typ.is_signed() {
                    Instruction::DoubleToInt(srcdst)
                } else {
                    Instruction::DoubleToUInt(srcdst)
                }
            } else if typ == VarType::Double {
                if sub_typ.is_signed() {
                    Instruction::IntToDouble(srcdst)
                } else {
                    Instruction::UIntToDouble(srcdst)
                }
            } else {
                /* Then, casting between different integer types. */
                let out_bytelen = OperandByteLen::from(typ);
                let sub_bytelen = OperandByteLen::from(sub_typ);
                match out_bytelen.cmp(&sub_bytelen) {
                    Ordering::Equal => Instruction::Copy(srcdst),
                    Ordering::Less => Instruction::Truncate(srcdst),
                    Ordering::Greater => {
                        if sub_typ.is_signed() {
                            Instruction::SignExtend(srcdst)
                        } else {
                            Instruction::ZeroExtend(srcdst)
                        }
                    }
                }
            };

            self.instrs.push(instr);
            ReadableValue::Variable(dst)
        }
    }

    /* Function call */

    pub(super) fn gen_exp_fun_call(
        &mut self,
        c::FunctionCall { ident, args }: c::FunctionCall<TypeCheckedCAst>,
        out_typ: VarType,
    ) -> ReadableValue {
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

        ReadableValue::Variable(result)
    }
}
