use super::FunInstrsGenerator;
use crate::{
    common::{
        identifier::JumpLabel,
        primitive::Const,
        types_frontend::{ScalarType, SubObjType},
    },
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
};
use std::rc::Rc;

/// C Unary
impl FunInstrsGenerator<'_> {
    pub(super) fn gen_exp_unary(
        &mut self,
        c::Unary { op, sub_exp }: c::Unary<TypeCheckedCAst>,
        out_typ: SubObjType<ScalarType>,
    ) -> Value {
        use c::UnaryOperator as CO;

        use ComparisonUnaryOperator as TOC;
        use NumericUnaryOperator as TON;

        let op = match op {
            CO::Complement => TON::Complement.into(),
            CO::Negate => TON::Negate.into(),
            CO::Not => TOC::Not.into(),
        };
        let src = self.gen_exp_and_get_value(*sub_exp);
        let dst = self.register_new_value(out_typ);
        self.instrs
            .push(Instruction::Unary(Unary { op, src, dst: dst.clone() }));
        dst
    }
}

/// C Binary
impl FunInstrsGenerator<'_> {
    pub(super) fn gen_exp_binary(
        &mut self,
        c_binary: c::Binary<TypeCheckedCAst>,
        out_typ: SubObjType<ScalarType>,
    ) -> Value {
        use c::TypeCheckedBinaryOperator as CO;

        use c::ArithmeticBinaryOperator as COA;
        use c::ComparisonBinaryOperator as COC;

        use ArithmeticBinaryOperator as TOA;
        use ComparisonBinaryOperator as TOC;
        use DivRemBinaryOperator as TOD;

        match &c_binary.op {
            CO::Arith(c_op_a) => {
                let t_op = match c_op_a {
                    COA::Sub => TOA::Sub.into(),
                    COA::Add => TOA::Add.into(),
                    COA::Mul => TOA::Mul.into(),
                    COA::Div => TOD::Div.into(),
                    COA::Rem => TOD::Rem.into(),
                };
                self.gen_exp_binary_evalboth(t_op, c_binary, out_typ)
            }
            CO::ArithPtr(c_op_p) => self.gen_exp_binary_ptr(*c_op_p, c_binary, out_typ),
            CO::Logic(c_op_l) => self.gen_exp_binary_shortcirc(*c_op_l, c_binary, out_typ),
            CO::Cmp(c_op_c) => {
                let t_op = match c_op_c {
                    COC::Eq => TOC::Eq.into(),
                    COC::Neq => TOC::Neq.into(),
                    COC::Lt => TOC::Lt.into(),
                    COC::Lte => TOC::Lte.into(),
                    COC::Gt => TOC::Gt.into(),
                    COC::Gte => TOC::Gte.into(),
                };
                self.gen_exp_binary_evalboth(t_op, c_binary, out_typ)
            }
        }
    }
    fn gen_exp_binary_evalboth(
        &mut self,
        op: BinaryOperator,
        c::Binary { op: _, lhs, rhs }: c::Binary<TypeCheckedCAst>,
        out_typ: SubObjType<ScalarType>,
    ) -> Value {
        let lhs = self.gen_exp_and_get_value(*lhs);
        let rhs = self.gen_exp_and_get_value(*rhs);
        let dst = self.register_new_value(out_typ);
        self.instrs.push(Instruction::Binary(Binary {
            op,
            lhs,
            rhs,
            dst: dst.clone(),
        }));
        dst
    }
    fn gen_exp_binary_shortcirc(
        &mut self,
        op: c::LogicBinaryOperator,
        c::Binary { op: _, lhs, rhs }: c::Binary<TypeCheckedCAst>,
        out_typ: SubObjType<ScalarType>,
    ) -> Value {
        let descr = match op {
            c::LogicBinaryOperator::And => "and",
            c::LogicBinaryOperator::Or => "or",
        };
        let [label_shortcirc, label_end] =
            JumpLabel::create(None, descr, ["shortcircuit", "end"]).map(Rc::new);

        let new_shortcirc_jump_instr = |condition: Value| {
            let jump_crit = match op {
                c::LogicBinaryOperator::And => JumpCriterion::JumpIfZero,
                c::LogicBinaryOperator::Or => JumpCriterion::JumpIfNotZero,
            };
            let lbl = Rc::clone(&label_shortcirc);
            Instruction::JumpIf(JumpIf { condition, jump_crit, lbl })
        };

        let new_out_const = |i: i32| Const::Int(i).cast_at_compile_time(&out_typ);
        let (shortcirc_val, fully_evald_val) = match op {
            c::LogicBinaryOperator::And => (new_out_const(0), new_out_const(1)),
            c::LogicBinaryOperator::Or => (new_out_const(1), new_out_const(0)),
        };

        let result = self.register_new_value(out_typ);

        /* Begin instructions */

        let lhs_val = self.gen_exp_and_get_value(*lhs);

        self.instrs.push(new_shortcirc_jump_instr(lhs_val));

        let rhs_val = self.gen_exp_and_get_value(*rhs);

        self.instrs.push(new_shortcirc_jump_instr(rhs_val));

        self.instrs.push(Instruction::Copy(SrcDst {
            src: Value::Constant(fully_evald_val),
            dst: result.clone(),
        }));

        self.instrs.push(Instruction::Jump(Rc::clone(&label_end)));

        self.instrs.push(Instruction::Label(label_shortcirc));

        self.instrs.push(Instruction::Copy(SrcDst {
            src: Value::Constant(shortcirc_val),
            dst: result.clone(),
        }));

        self.instrs.push(Instruction::Label(label_end));

        result
    }
}
