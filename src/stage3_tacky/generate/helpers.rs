use crate::{stage2_parse::c_ast as c, stage3_tacky::tacky_ast::*};
use derive_more::Display;
use std::collections::HashMap;
use std::rc::Rc;

pub enum BinaryOperatorType {
    EvaluateBothHands(BinaryOperator),
    ShortCircuit(ShortCircuitBOT),
}
#[derive(Display)]
pub enum ShortCircuitBOT {
    And,
    Or,
}

pub fn convert_op_unary(c_unary_op: c::UnaryOperator) -> UnaryOperator {
    use c::UnaryOperator as CUO;
    match c_unary_op {
        CUO::Complement => UnaryOperator::Complement,
        CUO::Negate => UnaryOperator::Negate,
        CUO::Not => UnaryOperator::Not,
    }
}
pub fn convert_op_binary(c_binary_op: &c::BinaryOperator) -> BinaryOperatorType {
    use c::BinaryOperator as CBO;
    use BinaryOperator as TBO;
    use BinaryOperatorType as BOT;
    use ShortCircuitBOT as SBOT;
    match c_binary_op {
        CBO::And => BOT::ShortCircuit(SBOT::And),
        CBO::Or => BOT::ShortCircuit(SBOT::Or),
        CBO::Sub => BOT::EvaluateBothHands(TBO::Sub),
        CBO::Add => BOT::EvaluateBothHands(TBO::Add),
        CBO::Mul => BOT::EvaluateBothHands(TBO::Mul),
        CBO::Div => BOT::EvaluateBothHands(TBO::Div),
        CBO::Rem => BOT::EvaluateBothHands(TBO::Rem),
        CBO::Eq => BOT::EvaluateBothHands(TBO::Eq),
        CBO::Neq => BOT::EvaluateBothHands(TBO::Neq),
        CBO::Lt => BOT::EvaluateBothHands(TBO::Lt),
        CBO::Lte => BOT::EvaluateBothHands(TBO::Lte),
        CBO::Gt => BOT::EvaluateBothHands(TBO::Gt),
        CBO::Gte => BOT::EvaluateBothHands(TBO::Gte),
    }
}

#[derive(Default)]
pub struct LoopIdToLabels {
    loop_id_to_labels: HashMap<Rc<c::LoopId>, Labels>,
}
impl LoopIdToLabels {
    pub fn get_lbl_start(loop_id: &c::LoopId) -> LabelIdentifier {
        let name_start = format!("{}.{}.start", loop_id.descr(), loop_id.id());
        LabelIdentifier::new(name_start)
    }
    pub fn get_or_insert(&mut self, loop_id: Rc<c::LoopId>) -> &Labels {
        self.loop_id_to_labels
            .entry(loop_id)
            .or_insert_with_key(|loop_id| {
                let name_break = format!("{}.{}.break", loop_id.descr(), loop_id.id());
                let name_cont = format!("{}.{}.cont", loop_id.descr(), loop_id.id());
                let lbl_break = Rc::new(LabelIdentifier::new(name_break));
                let lbl_cont = Rc::new(LabelIdentifier::new(name_cont));
                Labels { lbl_break, lbl_cont }
            })
    }
}

pub struct Labels {
    pub lbl_break: Rc<LabelIdentifier>,
    pub lbl_cont: Rc<LabelIdentifier>,
}
