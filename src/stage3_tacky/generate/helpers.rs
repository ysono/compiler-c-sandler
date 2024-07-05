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
    use ComparisonUnaryOperator as TCUO;
    use NumericUnaryOperator as TNUO;
    use UnaryOperator as TUO;
    match c_unary_op {
        CUO::Complement => TUO::Numeric(TNUO::Complement),
        CUO::Negate => TUO::Numeric(TNUO::Negate),
        CUO::Not => TUO::Comparison(TCUO::Not),
    }
}
pub fn convert_op_binary(c_binary_op: &c::BinaryOperator) -> BinaryOperatorType {
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
