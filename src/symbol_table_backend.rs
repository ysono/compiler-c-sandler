use crate::{
    identifier::UniqueIdentifier,
    symbol_table_frontend::{Symbol, SymbolTable, VarAttrs},
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
    symbol_table: HashMap<Rc<UniqueIdentifier>, AsmEntry>,
}
impl From<SymbolTable> for BackendSymbolTable {
    fn from(c_table: SymbolTable) -> Self {
        let c_table: HashMap<_, _> = c_table.into();
        let asm_table = c_table
            .into_iter()
            .map(|(ident, symbol)| {
                let asm_entry = match symbol {
                    Symbol::Var { typ, attrs } => {
                        let asm_type = AssemblyType::from(typ);
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
                (ident, asm_entry)
            })
            .collect::<HashMap<_, _>>();
        Self { symbol_table: asm_table }
    }
}
