use super::FunInstrsGenerator;
use crate::{
    common::{
        identifier::{JumpLabel, UniqueId},
        types_frontend::{ScalarType, SubObjType},
    },
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
};
use std::rc::Rc;

impl<'a> FunInstrsGenerator<'a> {
    pub(super) fn gen_stmt_conditional(
        &mut self,
        c::If { condition, then, elze }: c::If<TypeCheckedCAst>,
    ) {
        match elze {
            None => {
                let [label_end] = JumpLabel::create(UniqueId::new(), "stmt_cond", ["end"]);

                /* Begin instructions */

                let condition = self.gen_exp_and_get_value(condition);

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
                    JumpLabel::create(UniqueId::new(), "stmt_cond", ["else", "end"]);

                /* Begin instructions */

                let condition = self.gen_exp_and_get_value(condition);

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
        out_typ: SubObjType<ScalarType>,
    ) -> Value {
        let [label_else, label_end] =
            JumpLabel::create(UniqueId::new(), "exp_cond", ["else", "end"]);

        let result = self.register_new_value(out_typ);

        /* Begin instructions */

        let condition = self.gen_exp_and_get_value(*condition);

        self.instrs.push(Instruction::JumpIf(JumpIf {
            condition,
            jump_crit: JumpCriterion::JumpIfZero,
            lbl: Rc::clone(&label_else),
        }));

        let then = self.gen_exp_and_get_value(*then);

        self.instrs
            .push(Instruction::Copy(SrcDst { src: then, dst: result.clone() }));

        self.instrs.push(Instruction::Jump(Rc::clone(&label_end)));

        self.instrs.push(Instruction::Label(label_else));

        let elze = self.gen_exp_and_get_value(*elze);

        self.instrs
            .push(Instruction::Copy(SrcDst { src: elze, dst: result.clone() }));

        self.instrs.push(Instruction::Label(label_end));

        result
    }
}
