use super::FunInstrsGenerator;
use crate::{
    common::types_frontend::VarType,
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
                let label_end = Rc::new(LabelIdentifier::new("stmt_cond_end".to_string()));

                /* Begin instructions */

                let condition = self.gen_exp(condition);

                self.instrs.push(Instruction::JumpIf(JumpIf {
                    condition,
                    jump_crit: JumpCriterion::JumpIfZero,
                    lbl: Rc::clone(&label_end),
                }));

                self.gen_stmt(*then);

                self.instrs.push(Instruction::Label(label_end));
            }
            Some(elze) => {
                let name = &*then as *const c::Statement<TypeCheckedCAst> as usize;
                let label_else = Rc::new(LabelIdentifier::new(format!("stmt_cond.{name:x}.else")));
                let label_end = Rc::new(LabelIdentifier::new(format!("stmt_cond.{name:x}.end")));

                /* Begin instructions */

                let condition = self.gen_exp(condition);

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
        out_typ: VarType,
    ) -> ReadableValue {
        let result = self.symbol_table.declare_var_anon(out_typ);

        let name = result.id().unwrap().as_int();
        let label_else = Rc::new(LabelIdentifier::new(format!("exp_cond.{name:x}.else")));
        let label_end = Rc::new(LabelIdentifier::new(format!("exp_cond.{name:x}.end",)));

        /* Begin instructions */

        let condition = self.gen_exp(*condition);

        self.instrs.push(Instruction::JumpIf(JumpIf {
            condition,
            jump_crit: JumpCriterion::JumpIfZero,
            lbl: Rc::clone(&label_else),
        }));

        let then = self.gen_exp(*then);

        self.instrs.push(Instruction::Copy(SrcDst {
            src: then,
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Jump(Rc::clone(&label_end)));

        self.instrs.push(Instruction::Label(label_else));

        let elze = self.gen_exp(*elze);

        self.instrs.push(Instruction::Copy(SrcDst {
            src: elze,
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Label(label_end));

        ReadableValue::Variable(result)
    }
}
