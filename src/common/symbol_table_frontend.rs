use crate::{
    common::{
        identifier::SymbolIdentifier,
        primitive::Const,
        types_backend::ByteLen,
        types_frontend::{ObjType, ScalarFunType},
    },
    ds_n_a::singleton::Singleton,
};
use anyhow::{Result, anyhow};
use derive_more::{AsMut, Deref, Into};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub enum Symbol {
    Obj {
        typ: Singleton<ObjType>,
        attrs: ObjAttrs,
    },
    Fun {
        typ: Singleton<ScalarFunType>,
        attrs: FunAttrs,
    },
}

#[derive(Debug)]
pub enum ObjAttrs {
    AutomaticStorageDuration,
    StaticReadWrite {
        visibility: StaticVisibility,
        initializer: StaticInitializer,
    },
    StaticReadonly {
        initializer: InitializerItem<Const>,
    },
}
#[derive(Debug)]
pub enum StaticInitializer {
    Concrete(Vec<InitializerItem<Const>>),
    Tentative,
    NoInitializer,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum InitializerItem<Sngl> {
    Single(Sngl),
    String {
        chars: Vec<u8>,
        zeros_sfx_bytelen: ByteLen, // Count of 0x00 bytes following the chars.
    },
    Pointer(Rc<SymbolIdentifier>),
    Zero(ByteLen),
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
    pub fn get_obj_type(&self, ident: &SymbolIdentifier) -> Result<&Singleton<ObjType>> {
        let symbol = self.get(ident)?;
        match symbol {
            Symbol::Obj { typ, .. } => Ok(typ),
            _ => Err(anyhow!("Not variable. {ident:?} {symbol:?}")),
        }
    }
    pub fn get_fun_type(&self, ident: &SymbolIdentifier) -> Result<&Singleton<ScalarFunType>> {
        let symbol = self.get(ident)?;
        match symbol {
            Symbol::Fun { typ, .. } => Ok(typ),
            _ => Err(anyhow!("Not function. {ident:?} {symbol:?}")),
        }
    }
}
