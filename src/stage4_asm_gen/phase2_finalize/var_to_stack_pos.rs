use crate::{
    common::{
        identifier::SymbolIdentifier,
        types_backend::{Alignment, AssemblyType, OperandByteLen},
    },
    stage4_asm_gen::asm_ast::MemoryOffset,
};
use getset::Getters;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Getters)]
pub struct VarToStackPos {
    #[getset(get = "pub")]
    last_used_stack_pos: MemoryOffset,
    var_to_stack_pos: HashMap<Rc<SymbolIdentifier>, MemoryOffset>,
}
impl Default for VarToStackPos {
    fn default() -> Self {
        Self {
            last_used_stack_pos: MemoryOffset(0),
            var_to_stack_pos: HashMap::new(),
        }
    }
}
impl VarToStackPos {
    pub fn var_to_stack_pos(
        &mut self,
        ident: Rc<SymbolIdentifier>,
        asm_type: AssemblyType,
    ) -> MemoryOffset {
        let pos = self.var_to_stack_pos.entry(ident).or_insert_with(|| {
            let mut pos = self.last_used_stack_pos.0;

            let alloc = OperandByteLen::from(asm_type) as i64;
            pos -= alloc;

            let align = Alignment::default_of(asm_type) as i64;
            pos = ((pos - (align - 1)) / align) * align;

            self.last_used_stack_pos.0 = pos;
            self.last_used_stack_pos
        });
        *pos
    }
}
