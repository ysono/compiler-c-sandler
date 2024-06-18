use crate::{stage1_lex::tokens::Identifier, stage2_parse::c_ast_resolved::Expression};
use anyhow::{anyhow, Result};
use derivative::Derivative;
use derive_more::{Deref, DerefMut};
use std::collections::{hash_map::Entry, HashMap};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Derivative, Debug)]
#[derivative(PartialEq, Eq, Hash)]
pub enum ResolvedIdentifier {
    NoLinkage {
        id: IdentifierId,

        /// Used for debugging only.
        #[derivative(PartialEq = "ignore", Hash = "ignore")]
        orig: Option<Rc<Identifier>>,
    },
    ExternalLinkage(Rc<Identifier>),
}
impl ResolvedIdentifier {
    pub fn new_no_linkage(orig: Option<Rc<Identifier>>) -> Self {
        Self::NoLinkage {
            id: IdentifierId::new(),
            orig,
        }
    }
    pub fn id_int(&self) -> Option<usize> {
        match self {
            Self::NoLinkage { id, .. } => Some(id.as_int()),
            Self::ExternalLinkage(..) => None,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct IdentifierId(usize);
impl IdentifierId {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        static NEXT_ID: AtomicUsize = AtomicUsize::new(0);
        let curr_id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
        Self(curr_id)
    }
    pub fn as_int(&self) -> usize {
        self.0
    }
}

#[derive(Debug)]
pub enum SymbolType {
    Var,
    Function {
        params_count: usize,
        is_defined: bool,
    },
}

#[derive(Default, Deref, DerefMut, Debug)]
pub struct SymbolTable {
    symbol_table: HashMap<Rc<ResolvedIdentifier>, SymbolType>,
}
impl SymbolTable {
    pub fn declare_var(&mut self, ident: &Rc<ResolvedIdentifier>) -> Result<()> {
        match self.symbol_table.entry(Rc::clone(ident)) {
            Entry::Vacant(entry) => {
                entry.insert(SymbolType::Var);
                Ok(())
            }
            Entry::Occupied(_) => Err(anyhow!("Cannot declare {ident:?} 2+ times.")),
        }
    }
    pub fn use_var(&self, ident: &ResolvedIdentifier) -> Result<()> {
        let prior_typ = self
            .symbol_table
            .get(ident)
            .ok_or_else(|| anyhow!("Cannot use non-declared {ident:?}."))?;
        match prior_typ {
            SymbolType::Var => Ok(()),
            SymbolType::Function { .. } => {
                Err(anyhow!("Cannot use {ident:?} typed {prior_typ:?} as var."))
            }
        }
    }

    pub fn declare_or_define_fun(
        &mut self,
        ident: &Rc<ResolvedIdentifier>,
        params: &Vec<Rc<ResolvedIdentifier>>,
        newly_defined: bool,
    ) -> Result<()> {
        match self.symbol_table.entry(Rc::clone(ident)) {
            Entry::Vacant(entry) => {
                entry.insert(SymbolType::Function {
                    params_count: params.len(),
                    is_defined: newly_defined,
                });
                Ok(())
            }
            Entry::Occupied(mut entry) => {
                let prior_typ = entry.get_mut();
                match prior_typ {
                    SymbolType::Function {
                        params_count,
                        is_defined,
                    } => {
                        if *params_count != params.len() {
                            return Err(anyhow!("Cannot declare {ident:?} to have 2+ types: {prior_typ:?} and {params:?}."));
                        }
                        if *is_defined && newly_defined {
                            return Err(anyhow!(
                                "Cannot define {ident:?} typed {prior_typ:?} 2+ times."
                            ));
                        }
                        *is_defined |= newly_defined;
                        Ok(())
                    }
                    _ => Err(anyhow!(
                        "Cannot declare {ident:?} to have 2+ types: {prior_typ:?} and {params:?}."
                    )),
                }
            }
        }
    }
    pub fn call_fun(&self, ident: &ResolvedIdentifier, args: &Vec<Expression>) -> Result<()> {
        let prior_typ = self
            .symbol_table
            .get(ident)
            .ok_or_else(|| anyhow!("Cannot call non-declared {ident:?}."))?;
        match prior_typ {
            SymbolType::Function { params_count, .. } => {
                if *params_count != args.len() {
                    return Err(anyhow!("Cannot call {ident:?} typed {prior_typ:?} using a mismatched signature {args:?}."));
                }
                Ok(())
            }
            _ => Err(anyhow!(
                "Cannot call {ident:?} typed {prior_typ:?} as a function."
            )),
        }
    }
}
