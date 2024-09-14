use crate::{
    common::{
        identifier::{IdentifierId, UniqueIdentifier},
        types_frontend::{Const, FunType, VarType},
    },
    stage2_parse::{c_ast::FunctionCall, phase2_resolve::ResolvedCAst},
};
use anyhow::{anyhow, Result};
use derive_more::{Deref, DerefMut, Into};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub enum Symbol {
    Var { typ: VarType, attrs: VarAttrs },
    Fun { typ: Rc<FunType>, attrs: FunAttrs },
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
    NonGlobal, // Visible within translation unit (eg within block).
}

#[derive(Default, Into, Deref, DerefMut, Debug)]
pub struct SymbolTable {
    symbol_table: HashMap<Rc<UniqueIdentifier>, Symbol>,
}
impl SymbolTable {
    pub fn declare_var_anon(&mut self, typ: VarType) -> Rc<UniqueIdentifier> {
        let ident = Rc::new(UniqueIdentifier::Generated {
            id: IdentifierId::new(),
            descr: None,
        });
        self.symbol_table.insert(
            Rc::clone(&ident),
            Symbol::Var {
                typ,
                attrs: VarAttrs::AutomaticStorageDuration,
            },
        );
        ident
    }

    pub fn use_var(&self, ident: &UniqueIdentifier) -> Result<VarType> {
        let prev_symbol = self
            .symbol_table
            .get(ident)
            .ok_or_else(|| anyhow!("Cannot use non-declared {ident:?}."))?;
        match prev_symbol {
            Symbol::Var { typ, .. } => Ok(*typ),
            Symbol::Fun { .. } => Err(anyhow!(
                "Cannot use {ident:?} typed {prev_symbol:?} as var."
            )),
        }
    }

    pub fn call_fun(
        &self,
        FunctionCall { ident, args }: &FunctionCall<ResolvedCAst>,
    ) -> Result<&Rc<FunType>> {
        let prev_symbol = self
            .symbol_table
            .get(ident)
            .ok_or_else(|| anyhow!("Cannot call non-declared {ident:?}."))?;
        match prev_symbol {
            Symbol::Fun { typ, .. } => {
                if typ.params.len() != args.len() {
                    return Err(anyhow!("Cannot call {ident:?} using mismatched signature. {prev_symbol:?} vs {args:?}"));
                }
                /* We don't compare arg or ret types, b/c all var types are convertible into one another. */
                Ok(typ)
            }
            _ => Err(anyhow!("Cannot call {ident:?}. {prev_symbol:?}")),
        }
    }
}
