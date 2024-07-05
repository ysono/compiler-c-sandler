use crate::{
    stage4_asm_gen::asm_ast::StackPosition,
    symbol_table_frontend::ResolvedIdentifier,
    types_backend::{Alignment, AssemblyType, OperandByteLen},
};
use getset::Getters;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Getters)]
pub struct VarToStackPos {
    #[getset(get = "pub")]
    last_used_stack_pos: StackPosition,
    var_to_stack_pos: HashMap<Rc<ResolvedIdentifier>, StackPosition>,
}
impl Default for VarToStackPos {
    fn default() -> Self {
        Self {
            last_used_stack_pos: StackPosition(0),
            var_to_stack_pos: HashMap::new(),
        }
    }
}
impl VarToStackPos {
    pub fn var_to_stack_pos(
        &mut self,
        ident: Rc<ResolvedIdentifier>,
        asm_type: AssemblyType,
    ) -> StackPosition {
        let pos = self.var_to_stack_pos.entry(ident).or_insert_with(|| {
            let alloc = OperandByteLen::from(asm_type) as i64;
            self.last_used_stack_pos.0 -= alloc;

            let alignment = Alignment::from(asm_type) as i64;
            let rem = self.last_used_stack_pos.0 % alignment;
            if rem != 0 {
                self.last_used_stack_pos.0 -= alignment + rem;
            }

            self.last_used_stack_pos
        });
        *pos
    }
}
