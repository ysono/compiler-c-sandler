use crate::common::{
    identifier::UniqueIdentifier,
    symbol_table_frontend::{Symbol, SymbolTable, VarAttrs},
    types_backend::AssemblyType,
};
use derive_more::{Deref, DerefMut};
use std::collections::HashMap;
use std::rc::Rc;

pub enum AsmEntry {
    Obj {
        asm_type: AssemblyType,
        loc: ObjLocation,
    },
    Fun {
        is_defined: bool,
    },
}
pub enum ObjLocation {
    Stack,
    StaticReadWrite,
    StaticReadonly,
}

#[derive(Deref, DerefMut)]
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
                        let loc = match attrs {
                            VarAttrs::AutomaticStorageDuration => ObjLocation::Stack,
                            VarAttrs::StaticStorageDuration { .. } => ObjLocation::StaticReadWrite,
                        };
                        AsmEntry::Obj { asm_type, loc }
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
