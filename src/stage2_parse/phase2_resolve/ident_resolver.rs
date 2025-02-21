use crate::{
    common::identifier::{RawIdentifier, SymbolIdentifier},
    stage2_parse::c_ast::StorageClassSpecifier,
};
use anyhow::{Result, anyhow};
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

pub struct IdentResolver {
    /// This abstracts a copy-on-write dict.
    ident_to_resolved_idents: HashMap<Rc<RawIdentifier>, Vec<Rc<SymbolIdentifier>>>,

    /// This tracks each copy-on-write layer's keys.
    scope_to_idents: Vec<HashSet<Rc<RawIdentifier>>>,
}
impl Default for IdentResolver {
    fn default() -> Self {
        Self {
            ident_to_resolved_idents: HashMap::new(),
            scope_to_idents: vec![HashSet::new()], // The one file scope.
        }
    }
}
impl IdentResolver {
    fn is_file_scope(&self) -> bool {
        self.scope_to_idents.len() == 1
    }

    pub fn push_new_scope(&mut self) {
        self.scope_to_idents.push(HashSet::new());
    }
    pub fn pop_scope(&mut self) {
        let idents = self.scope_to_idents.pop().unwrap();
        for ident in idents {
            let resolved_idents = self.ident_to_resolved_idents.get_mut(&ident).unwrap();
            if resolved_idents.len() > 1 {
                resolved_idents.pop();
            } else {
                self.ident_to_resolved_idents.remove(&ident);
            }
        }
    }

    pub fn declare_var(
        &mut self,
        ident: RawIdentifier,
        storage_class: Option<&StorageClassSpecifier>,
    ) -> Result<Rc<SymbolIdentifier>> {
        let has_linkage = self.is_file_scope()
            || match storage_class {
                Some(StorageClassSpecifier::Extern) => true,
                Some(StorageClassSpecifier::Static) | None => false,
            };
        self.declare(ident, has_linkage)
    }
    pub fn declare_fun(&mut self, ident: RawIdentifier) -> Result<Rc<SymbolIdentifier>> {
        let has_linkage = true;
        self.declare(ident, has_linkage)
    }
    fn declare(
        &mut self,
        ident: RawIdentifier,
        new_has_linkage: bool,
    ) -> Result<Rc<SymbolIdentifier>> {
        let curr_scope = self.scope_to_idents.last_mut().unwrap();
        match curr_scope.contains(&ident) {
            false => {
                let ident = Rc::new(ident);

                curr_scope.insert(Rc::clone(&ident));

                let resolved_ident = if new_has_linkage {
                    SymbolIdentifier::Exact(Rc::clone(&ident))
                } else {
                    /* To help with debugging, we could provide a description here.
                    Eg concat(surrounding fun/block names, var name)
                    Eg tup(line number, column number) */
                    SymbolIdentifier::new_generated()
                };
                let resolved_ident = Rc::new(resolved_ident);

                self.ident_to_resolved_idents
                    .entry(ident)
                    .or_default()
                    .push(Rc::clone(&resolved_ident));

                Ok(resolved_ident)
            }
            true => {
                let resolved_idents = self.ident_to_resolved_idents.get(&ident).unwrap();
                let resolved_ident = resolved_idents.last().unwrap();

                let prev_has_linkage =
                    matches!(resolved_ident.as_ref(), SymbolIdentifier::Exact(_));
                if prev_has_linkage && new_has_linkage {
                    Ok(Rc::clone(resolved_ident))
                } else {
                    Err(anyhow!(
                        "In one scope, 2+ declarations of a same identifier must all refer to the same object or function, hence must all have some linkage. {ident:#?}: {resolved_ident:#?} vs {new_has_linkage}"
                    ))
                }
            }
        }
    }

    pub fn get(&self, ident: &RawIdentifier) -> Result<Rc<SymbolIdentifier>> {
        let resolved_idents = self
            .ident_to_resolved_idents
            .get(ident)
            .ok_or_else(|| anyhow!("Identifier wasn't declared in scope. {ident:#?}"))?;
        let resolved_ident = resolved_idents.last().unwrap();
        Ok(Rc::clone(resolved_ident))
    }
}
