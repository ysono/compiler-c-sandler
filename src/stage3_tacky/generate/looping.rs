use super::FunInstrsGenerator;
use crate::{
    common::identifier::JumpLabel,
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
    utils::noop,
};
use std::{collections::HashMap, rc::Rc};

impl FunInstrsGenerator<'_> {
    pub(super) fn gen_stmt_break(&mut self, loop_id: Rc<c::LoopId>) {
        let lbls = self.loop_id_to_labels.get_or_new_lbls(loop_id);
        let lbl_break = Rc::clone(&lbls.lbl_break);
        self.instrs.push(Instruction::Jump(lbl_break))
    }
    pub(super) fn gen_stmt_continue(&mut self, loop_id: Rc<c::LoopId>) {
        let lbls = self.loop_id_to_labels.get_or_new_lbls(loop_id);
        let lbl_cont = Rc::clone(&lbls.lbl_cont);
        self.instrs.push(Instruction::Jump(lbl_cont))
    }
    pub(super) fn gen_stmt_while(
        &mut self,
        loop_id: Rc<c::LoopId>,
        c::CondBody { condition, body }: c::CondBody<TypeCheckedCAst>,
    ) {
        let lbls = self.loop_id_to_labels.get_or_new_lbls(loop_id);
        let lbl_cont = Rc::clone(&lbls.lbl_cont);
        let lbl_break = Rc::clone(&lbls.lbl_break);

        /* Begin instructions */

        self.instrs.push(Instruction::Label(Rc::clone(&lbl_cont)));

        let condition = self.gen_exp_and_get_value(condition);

        self.instrs.push(Instruction::JumpIf(JumpIf {
            condition,
            jump_crit: JumpCriterion::JumpIfZero,
            lbl: Rc::clone(&lbl_break),
        }));

        self.gen_stmt(*body);

        self.instrs.push(Instruction::Jump(lbl_cont));

        self.instrs.push(Instruction::Label(lbl_break));
    }
    pub(super) fn gen_stmt_dowhile(
        &mut self,
        loop_id: Rc<c::LoopId>,
        c::CondBody { body, condition }: c::CondBody<TypeCheckedCAst>,
    ) {
        let lbl_start = Rc::new(LoopIdToLabels::new_lbl_start(&loop_id));
        let lbls = self.loop_id_to_labels.get_or_new_lbls(loop_id);
        let lbl_cont = Rc::clone(&lbls.lbl_cont);
        let lbl_break = Rc::clone(&lbls.lbl_break);

        /* Begin instructions */

        self.instrs.push(Instruction::Label(Rc::clone(&lbl_start)));

        self.gen_stmt(*body);

        self.instrs.push(Instruction::Label(lbl_cont));

        let condition = self.gen_exp_and_get_value(condition);

        self.instrs.push(Instruction::JumpIf(JumpIf {
            condition,
            jump_crit: JumpCriterion::JumpIfNotZero,
            lbl: lbl_start,
        }));

        self.instrs.push(Instruction::Label(lbl_break));
    }
    pub(super) fn gen_stmt_for(
        &mut self,
        loop_id: Rc<c::LoopId>,
        c::For { init, condition, post, body }: c::For<TypeCheckedCAst>,
    ) {
        let lbl_start = Rc::new(LoopIdToLabels::new_lbl_start(&loop_id));
        let lbls = self.loop_id_to_labels.get_or_new_lbls(loop_id);
        let lbl_cont = Rc::clone(&lbls.lbl_cont);
        let lbl_break = Rc::clone(&lbls.lbl_break);

        /* Begin instructions */

        match init {
            c::ForInit::Decl(c_var_defn) => self.gen_var_defn(c_var_defn),
            c::ForInit::Exp(c_exp) => {
                self.gen_exp(c_exp);
            }
            c::ForInit::None => noop!(),
        }

        self.instrs.push(Instruction::Label(Rc::clone(&lbl_start)));

        if let Some(c_cond) = condition {
            let condition = self.gen_exp_and_get_value(c_cond);

            self.instrs.push(Instruction::JumpIf(JumpIf {
                condition,
                jump_crit: JumpCriterion::JumpIfZero,
                lbl: Rc::clone(&lbl_break),
            }));
        }

        self.gen_stmt(*body);

        self.instrs.push(Instruction::Label(lbl_cont));

        if let Some(c_post) = post {
            self.gen_exp(c_post);
        }

        self.instrs.push(Instruction::Jump(lbl_start));

        self.instrs.push(Instruction::Label(lbl_break));
    }
}

#[derive(Default)]
pub struct LoopIdToLabels {
    loop_id_to_labels: HashMap<Rc<c::LoopId>, Labels>,
}
impl LoopIdToLabels {
    fn new_lbl_start(loop_id: &c::LoopId) -> JumpLabel {
        let [lbl_start] = JumpLabel::create(Some(&loop_id.id), loop_id.descr, ["start"]);
        lbl_start
    }
    fn get_or_new_lbls(&mut self, loop_id: Rc<c::LoopId>) -> &Labels {
        self.loop_id_to_labels
            .entry(loop_id)
            .or_insert_with_key(|loop_id| {
                let [lbl_break, lbl_cont] =
                    JumpLabel::create(Some(&loop_id.id), loop_id.descr, ["break", "cont"])
                        .map(Rc::new);
                Labels { lbl_break, lbl_cont }
            })
    }
}

struct Labels {
    lbl_break: Rc<JumpLabel>,
    lbl_cont: Rc<JumpLabel>,
}
