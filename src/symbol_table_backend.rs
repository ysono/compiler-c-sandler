use crate::symbol_table::{ResolvedIdentifier, Symbol, SymbolTable, VarAttrs, VarType};
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

#[derive(Clone, Copy, Debug)]
pub enum AssemblyType {
    Longword, // 32-bit. Aka doubleword.
    Quadword, // 64-bit
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
                    let asm_type = match typ {
                        VarType::Int => AssemblyType::Longword,
                        VarType::Long => AssemblyType::Quadword,
                        _ => todo!(),
                    };
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

#[derive(Debug)]
pub enum Alignment {
    B4 = 4,
    B8 = 8,
}
impl From<VarType> for Alignment {
    fn from(var_type: VarType) -> Self {
        match var_type {
            VarType::Int => Self::B4,
            VarType::Long => Self::B8,
            _ => todo!(),
        }
    }
}
impl From<AssemblyType> for Alignment {
    fn from(asm_type: AssemblyType) -> Self {
        match asm_type {
            AssemblyType::Longword => Self::B4,
            AssemblyType::Quadword => Self::B8,
        }
    }
}

#[derive(Clone, Copy)]
pub enum OperandByteLen {
    B1 = 1,
    B4 = 4,
    B8 = 8,
}
impl From<AssemblyType> for OperandByteLen {
    fn from(asm_type: AssemblyType) -> Self {
        match asm_type {
            AssemblyType::Longword => Self::B4,
            AssemblyType::Quadword => Self::B8,
        }
    }
}
