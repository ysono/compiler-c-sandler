use crate::{
    common::{
        identifier::SymbolIdentifier,
        types_backend::{Alignment, AssemblyType, ByteArrayAssemblyType, OperandByteLen},
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
            let (alloc, align);
            match asm_typ {
                AssemblyType::Scalar(sca_asm_typ) => {
                    alloc = OperandByteLen::from(*sca_asm_typ) as i64;
                    align = Alignment::default_of_scalar(*sca_asm_typ) as i64;
                }
                AssemblyType::ByteArray(ByteArrayAssemblyType(bytelen, alignment)) => {
                    alloc = bytelen.as_int() as i64;
                    align = *alignment as i64;
                }
            }

            let pos = self.last_used_stack_pos.as_mut();

            *pos -= alloc;

            *pos = ((*pos - (align - 1)) / align) * align;

            self.last_used_stack_pos
        });
        *pos
    }
}
