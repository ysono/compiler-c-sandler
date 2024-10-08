use crate::{
    common::{
        identifier::SymbolIdentifier,
        primitive::Const,
        types_frontend::{FunType, VarType},
    },
    ds_n_a::singleton::Singleton,
};
use anyhow::{anyhow, Result};
use derive_more::{AsMut, Deref, Into};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub enum Symbol {
    Var {
        typ: Singleton<VarType>,
        attrs: VarAttrs,
    },
    Fun {
        typ: Singleton<FunType>,
        attrs: FunAttrs,
    },
}

#[derive(Debug)]
pub enum VarAttrs {
    AutomaticStorageDuration,
    StaticStorageDuration {
        visibility: StaticVisibility,
        initial_value: StaticInitialValue,
    },
}
#[derive(Debug)]
pub enum StaticInitialValue {
    Initial(Const),
    Tentative,
    NoInitializer,
}

#[derive(Debug)]
pub struct FunAttrs {
    pub visibility: StaticVisibility,
    pub is_defined: bool,
}

#[derive(Clone, Copy, Debug)]
pub enum StaticVisibility {
    Global,    // Visible to other translation units.
    NonGlobal, // Visible within translation unit (eg within file; eg within block).
}

#[derive(Default, Into, Deref, AsMut, Debug)]
pub struct SymbolTable {
    symbol_table: HashMap<Rc<SymbolIdentifier>, Symbol>,
}
impl SymbolTable {
    pub fn get(&self, ident: &SymbolIdentifier) -> Result<&Symbol> {
        self.symbol_table
            .get(ident)
            .ok_or_else(|| anyhow!("Not declared. {ident:?}"))
    }
    pub fn get_var_type(&self, ident: &SymbolIdentifier) -> Result<&Singleton<VarType>> {
        let symbol = self.get(ident)?;
        match symbol {
            Symbol::Var { typ, .. } => Ok(typ),
            _ => Err(anyhow!("Not variable. {ident:?} {symbol:?}")),
        }
    }
    pub fn get_fun_type(&self, ident: &SymbolIdentifier) -> Result<&Singleton<FunType>> {
        let symbol = self.get(ident)?;
        match symbol {
            Symbol::Fun { typ, .. } => Ok(typ),
            _ => Err(anyhow!("Not function. {ident:?} {symbol:?}")),
        }
    }
}
