use super::FunInstrsGenerator;
use crate::{
    common::identifier::{JumpLabel, LoopId},
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
    utils::noop,
};
use std::{
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

impl FunInstrsGenerator<'_> {
    pub(super) fn gen_stmt_break(&mut self, loop_id: LoopId) {
        let lbls = self.loop_id_to_labels.get_lbls(&loop_id);
        let lbl_break = Rc::clone(&lbls.lbl_break);
        self.instrs.push(Instruction::Jump(lbl_break))
    }
    pub(super) fn gen_stmt_continue(&mut self, loop_id: LoopId) {
        let lbls = self.loop_id_to_labels.get_lbls(&loop_id);
        let lbl_cont = Rc::clone(&lbls.lbl_cont);
        self.instrs.push(Instruction::Jump(lbl_cont))
    }
    pub(super) fn gen_stmt_while(
        &mut self,
        c::CondBody { loop_id, condition, body }: c::CondBody<TypeCheckedCAst>,
    ) {
        let ([], lbls) = self
            .loop_id_to_labels
            .new_lbls(loop_id.clone(), "while", []);
        let lbl_cont = Rc::clone(&lbls.lbl_cont);
        let lbl_break = Rc::clone(&lbls.lbl_break);

        /* Begin instructions */

        self.instrs.push(Instruction::Label(Rc::clone(&lbl_cont)));

        let condition = self.gen_sca_exp_and_get_value(condition);

        self.instrs.push(Instruction::JumpIf(JumpIf {
            condition,
            jump_crit: JumpCriterion::JumpIfZero,
            lbl: Rc::clone(&lbl_break),
        }));

        self.gen_stmt(*body);

        self.instrs.push(Instruction::Jump(lbl_cont));

        self.instrs.push(Instruction::Label(lbl_break));

        /* End of instructions */

        self.loop_id_to_labels.remove_lbl(&loop_id);
    }
    pub(super) fn gen_stmt_dowhile(
        &mut self,
        c::CondBody { loop_id, body, condition }: c::CondBody<TypeCheckedCAst>,
    ) {
        let ([lbl_start], lbls) =
            self.loop_id_to_labels
                .new_lbls(loop_id.clone(), "dowhile", ["start"]);
        let lbl_start = Rc::new(lbl_start);
        let lbl_cont = Rc::clone(&lbls.lbl_cont);
        let lbl_break = Rc::clone(&lbls.lbl_break);

        /* Begin instructions */

        self.instrs.push(Instruction::Label(Rc::clone(&lbl_start)));

        self.gen_stmt(*body);

        self.instrs.push(Instruction::Label(lbl_cont));

        let condition = self.gen_sca_exp_and_get_value(condition);

        self.instrs.push(Instruction::JumpIf(JumpIf {
            condition,
            jump_crit: JumpCriterion::JumpIfNotZero,
            lbl: lbl_start,
        }));

        self.instrs.push(Instruction::Label(lbl_break));

        /* End of instructions */

        self.loop_id_to_labels.remove_lbl(&loop_id);
    }
    pub(super) fn gen_stmt_for(
        &mut self,
        c::For {
            loop_id,
            init,
            condition,
            post,
            body,
        }: c::For<TypeCheckedCAst>,
    ) {
        let ([lbl_start], lbls) =
            self.loop_id_to_labels
                .new_lbls(loop_id.clone(), "for", ["start"]);
        let lbl_start = Rc::new(lbl_start);
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
            let condition = self.gen_sca_exp_and_get_value(c_cond);

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

        /* End of instructions */

        self.loop_id_to_labels.remove_lbl(&loop_id);
    }
}

#[derive(Default)]
pub struct LoopIdToLabels {
    loop_id_to_labels: HashMap<LoopId, Labels>,
}
impl LoopIdToLabels {
    /// @arg `[extra_descr2s]` must be distinct from one another and distinct from "brk" and "cont".
    fn new_lbls<const CT: usize>(
        &mut self,
        loop_id: LoopId,
        descr: &'static str,
        extra_descr2s: [&'static str; CT],
    ) -> ([JumpLabel; CT], &Labels) {
        match self.loop_id_to_labels.entry(loop_id) {
            Entry::Vacant(entry) => {
                let loop_id = entry.key();

                let extra_lbls = JumpLabel::create(Some(loop_id.id()), descr, extra_descr2s);

                let [lbl_break, lbl_cont] =
                    JumpLabel::create(Some(loop_id.id()), descr, ["brk", "cont"]).map(Rc::new);
                let brk_cont_lbls = entry.insert(Labels { lbl_break, lbl_cont });

                (extra_lbls, brk_cont_lbls)
            }
            Entry::Occupied(_) => unreachable!(),
        }
    }
    fn get_lbls(&self, loop_id: &LoopId) -> &Labels {
        self.loop_id_to_labels.get(loop_id).unwrap()
    }
    fn remove_lbl(&mut self, loop_id: &LoopId) {
        self.loop_id_to_labels.remove(loop_id);
    }
}

struct Labels {
    lbl_break: Rc<JumpLabel>,
    lbl_cont: Rc<JumpLabel>,
}
