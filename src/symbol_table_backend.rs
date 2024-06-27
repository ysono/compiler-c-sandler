use crate::{
    symbol_table_frontend::{ResolvedIdentifier, Symbol, SymbolTable, VarAttrs},
    types_backend::AssemblyType,
};
use derive_more::Deref;
use std::collections::HashMap;
use std::rc::Rc;

pub enum AsmEntry {
    Obj {
        asm_type: AssemblyType,
        storage_duration: StorageDuration,
    },
    Fun {
        is_defined: bool,
    },
}

pub enum StorageDuration {
    Automatic,
    Static,
}

#[derive(Deref)]
pub struct BackendSymbolTable {
    symbol_table: HashMap<Rc<ResolvedIdentifier>, AsmEntry>,
}
impl<'a> From<&'a SymbolTable> for BackendSymbolTable {
    fn from(c_table: &'a SymbolTable) -> Self {
        let mut asm_table = HashMap::new();
        for (ident, symbol) in c_table.iter() {
            let asm_entry = match symbol {
                Symbol::Var { typ, attrs } => {
                    let asm_type = AssemblyType::from(*typ);
                    let storage_duration = match attrs {
                        VarAttrs::AutomaticStorageDuration => StorageDuration::Automatic,
                        VarAttrs::StaticStorageDuration { .. } => StorageDuration::Static,
                    };
                    AsmEntry::Obj { asm_type, storage_duration }
                }
                Symbol::Fun { typ: _, attrs } => {
                    let is_defined = attrs.is_defined;
                    AsmEntry::Fun { is_defined }
                }
            };
            asm_table.insert(Rc::clone(ident), asm_entry);
        }
        Self { symbol_table: asm_table }
    }
}
