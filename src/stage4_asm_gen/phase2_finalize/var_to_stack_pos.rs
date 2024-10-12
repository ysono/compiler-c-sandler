use crate::{
    common::{
        identifier::SymbolIdentifier,
        types_backend::{Alignment, AssemblyType, OperandByteLen},
    },
    stage4_asm_gen::asm_ast::MemoryOffset,
};
use std::collections::HashMap;
use std::rc::Rc;

pub struct VarToStackPos {
    last_used_stack_pos: MemoryOffset,
    var_to_stack_pos: HashMap<Rc<SymbolIdentifier>, MemoryOffset>,
}
impl Default for VarToStackPos {
    fn default() -> Self {
        Self {
            last_used_stack_pos: MemoryOffset::new(0),
            var_to_stack_pos: HashMap::new(),
        }
    }
}
impl VarToStackPos {
    pub fn last_used_stack_pos(self) -> MemoryOffset {
        self.last_used_stack_pos
    }

    pub fn resolve_stack_pos(
        &mut self,
        ident: Rc<SymbolIdentifier>,
        asm_typ: &AssemblyType,
    ) -> MemoryOffset {
        let pos = self.var_to_stack_pos.entry(ident).or_insert_with(|| {
            let AssemblyType::Scalar(sca_asm_typ) = asm_typ;

            let pos = self.last_used_stack_pos.as_mut();

            let alloc = OperandByteLen::from(*sca_asm_typ) as i64;
            *pos -= alloc;

            let align = Alignment::default_of(*sca_asm_typ) as i64;
            *pos = ((*pos - (align - 1)) / align) * align;

            self.last_used_stack_pos
        });
        *pos
    }
}
