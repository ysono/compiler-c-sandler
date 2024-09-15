use super::FunInstrsGenerator;
use crate::{
    common::{
        identifier::{JumpLabel, UniqueId},
        types_frontend::{Const, VarType},
    },
    ds_n_a::singleton::Singleton,
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
};
use std::rc::Rc;

impl<'a> FunInstrsGenerator<'a> {
    /* C Unary */

    pub(super) fn gen_exp_unary(
        &mut self,
        c::Unary { op, sub_exp }: c::Unary<TypeCheckedCAst>,
        out_typ: Singleton<VarType>,
    ) -> ReadableValue {
        let op = convert_op_unary(op);
        let src = self.gen_exp(*sub_exp);
        let dst = self.symbol_table.declare_var_anon(out_typ);
        self.instrs
            .push(Instruction::Unary(Unary { op, src, dst: Rc::clone(&dst) }));
        ReadableValue::Variable(dst)
    }

    /* C Binary */

    pub(super) fn gen_exp_binary(
        &mut self,
        c_binary: c::Binary<TypeCheckedCAst>,
        out_typ: Singleton<VarType>,
    ) -> ReadableValue {
        match convert_op_binary(&c_binary.op) {
            BinaryOperatorType::EvaluateBothHands(t_op) => {
                self.gen_exp_binary_evalboth(t_op, c_binary, out_typ)
            }
            BinaryOperatorType::ShortCircuit(t_op) => {
                self.gen_exp_binary_shortcirc(t_op, c_binary, out_typ)
            }
        }
    }
    fn gen_exp_binary_evalboth(
        &mut self,
        op: BinaryOperator,
        c::Binary { op: _, lhs, rhs }: c::Binary<TypeCheckedCAst>,
        out_typ: Singleton<VarType>,
    ) -> ReadableValue {
        let lhs = self.gen_exp(*lhs);
        let rhs = self.gen_exp(*rhs);
        let dst = self.symbol_table.declare_var_anon(out_typ);
        self.instrs.push(Instruction::Binary(Binary {
            op,
            lhs,
            rhs,
            dst: Rc::clone(&dst),
        }));
        ReadableValue::Variable(dst)
    }
    fn gen_exp_binary_shortcirc(
        &mut self,
        op_type: ShortCircuitBOT,
        c::Binary { op: _, lhs, rhs }: c::Binary<TypeCheckedCAst>,
        out_typ: Singleton<VarType>,
    ) -> ReadableValue {
        let [label_shortcirc, label_end] =
            JumpLabel::create(UniqueId::new(), op_type.descr(), ["shortcircuit", "end"]);

        let new_shortcirc_jump_instr = |condition: ReadableValue| {
            let jump_crit = match op_type {
                ShortCircuitBOT::And => JumpCriterion::JumpIfZero,
                ShortCircuitBOT::Or => JumpCriterion::JumpIfNotZero,
            };
            let lbl = Rc::clone(&label_shortcirc);
            Instruction::JumpIf(JumpIf { condition, jump_crit, lbl })
        };

        let new_out_const = |i: i32| Const::Int(i).cast_to(&out_typ);
        let (shortcirc_val, fully_evald_val) = match op_type {
            ShortCircuitBOT::And => (new_out_const(0), new_out_const(1)),
            ShortCircuitBOT::Or => (new_out_const(1), new_out_const(0)),
        };

        let result = self.symbol_table.declare_var_anon(out_typ);

        /* Begin instructions */

        let lhs_val = self.gen_exp(*lhs);

        self.instrs.push(new_shortcirc_jump_instr(lhs_val));

        let rhs_val = self.gen_exp(*rhs);

        self.instrs.push(new_shortcirc_jump_instr(rhs_val));

        self.instrs.push(Instruction::Copy(SrcDst {
            src: ReadableValue::Constant(fully_evald_val),
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Jump(Rc::clone(&label_end)));

        self.instrs.push(Instruction::Label(label_shortcirc));

        self.instrs.push(Instruction::Copy(SrcDst {
            src: ReadableValue::Constant(shortcirc_val),
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Label(label_end));

        ReadableValue::Variable(result)
    }
}

enum BinaryOperatorType {
    EvaluateBothHands(BinaryOperator),
    ShortCircuit(ShortCircuitBOT),
}
enum ShortCircuitBOT {
    And,
    Or,
}
impl ShortCircuitBOT {
    pub fn descr(&self) -> &'static str {
        match self {
            Self::And => "and",
            Self::Or => "or",
        }
    }
}

fn convert_op_unary(c_unary_op: c::UnaryOperator) -> UnaryOperator {
    use c::UnaryOperator as CUO;
    use ComparisonUnaryOperator as TCUO;
    use NumericUnaryOperator as TNUO;
    use UnaryOperator as TUO;
    match c_unary_op {
        CUO::Complement => TUO::Numeric(TNUO::Complement),
        CUO::Negate => TUO::Numeric(TNUO::Negate),
        CUO::Not => TUO::Comparison(TCUO::Not),
    }
}
fn convert_op_binary(c_binary_op: &c::BinaryOperator) -> BinaryOperatorType {
    use c::BinaryOperator as CBO;
    use ArithmeticBinaryOperator as TBOA;
    use BinaryOperatorType as BOT;
    use ComparisonBinaryOperator as TBOC;
    use DivRemBinaryOperator as TBOD;
    use ShortCircuitBOT as SBOT;
    match c_binary_op {
        CBO::And => BOT::ShortCircuit(SBOT::And),
        CBO::Or => BOT::ShortCircuit(SBOT::Or),
        CBO::Sub => BOT::EvaluateBothHands(TBOA::Sub.into()),
        CBO::Add => BOT::EvaluateBothHands(TBOA::Add.into()),
        CBO::Mul => BOT::EvaluateBothHands(TBOA::Mul.into()),
        CBO::Div => BOT::EvaluateBothHands(TBOD::Div.into()),
        CBO::Rem => BOT::EvaluateBothHands(TBOD::Rem.into()),
        CBO::Eq => BOT::EvaluateBothHands(TBOC::Eq.into()),
        CBO::Neq => BOT::EvaluateBothHands(TBOC::Neq.into()),
        CBO::Lt => BOT::EvaluateBothHands(TBOC::Lt.into()),
        CBO::Lte => BOT::EvaluateBothHands(TBOC::Lte.into()),
        CBO::Gt => BOT::EvaluateBothHands(TBOC::Gt.into()),
        CBO::Gte => BOT::EvaluateBothHands(TBOC::Gte.into()),
    }
}
