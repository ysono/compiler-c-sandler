use super::FunInstrsGenerator;
use crate::{
    common::{identifier::JumpLabel, types_frontend::NonAggrType},
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
};
use std::rc::Rc;

impl FunInstrsGenerator<'_> {
    pub(super) fn gen_stmt_conditional(
        &mut self,
        c::If { condition, then, elze }: c::If<TypeCheckedCAst>,
    ) {
        match elze {
            None => {
                let [label_end] = JumpLabel::create(None, "stmt_cond", ["end"]).map(Rc::new);

                /* Begin instructions */

                let condition = self.gen_sca_exp_and_get_value(condition);

                self.instrs.push(Instruction::JumpIf(JumpIf {
                    condition,
                    jump_crit: JumpCriterion::JumpIfZero,
                    lbl: Rc::clone(&label_end),
                }));

                self.gen_stmt(*then);

                self.instrs.push(Instruction::Label(label_end));
            }
            Some(elze) => {
                let [label_else, label_end] =
                    JumpLabel::create(None, "stmt_cond", ["else", "end"]).map(Rc::new);

                /* Begin instructions */

                let condition = self.gen_sca_exp_and_get_value(condition);

                self.instrs.push(Instruction::JumpIf(JumpIf {
                    condition,
                    jump_crit: JumpCriterion::JumpIfZero,
                    lbl: Rc::clone(&label_else),
                }));

                self.gen_stmt(*then);

                self.instrs.push(Instruction::Jump(Rc::clone(&label_end)));

                self.instrs.push(Instruction::Label(label_else));

                self.gen_stmt(*elze);

                self.instrs.push(Instruction::Label(label_end));
            }
        }
    }
    pub(super) fn gen_exp_conditional(
        &mut self,
        c::Conditional { condition, then, elze }: c::Conditional<TypeCheckedCAst>,
        ifc_typ: NonAggrType,
    ) -> Option<Value> {
        let [label_else, label_end] =
            JumpLabel::create(None, "exp_cond", ["else", "end"]).map(Rc::new);

        let result = self.maybe_register_new_value(ifc_typ);

        /* Begin instructions */

        let condition = self.gen_sca_exp_and_get_value(*condition);

        self.instrs.push(Instruction::JumpIf(JumpIf {
            condition,
            jump_crit: JumpCriterion::JumpIfZero,
            lbl: Rc::clone(&label_else),
        }));

        let then = self.gen_exp_and_get_value(*then);

        if let Some(then) = then {
            self.instrs.push(Instruction::Copy(SrcDst {
                src: then,
                dst: result.clone().unwrap(),
            }));
        }

        self.instrs.push(Instruction::Jump(Rc::clone(&label_end)));

        self.instrs.push(Instruction::Label(label_else));

        let elze = self.gen_exp_and_get_value(*elze);

        if let Some(elze) = elze {
            self.instrs.push(Instruction::Copy(SrcDst {
                src: elze,
                dst: result.clone().unwrap(),
            }));
        }

        self.instrs.push(Instruction::Label(label_end));

        result
    }
}
