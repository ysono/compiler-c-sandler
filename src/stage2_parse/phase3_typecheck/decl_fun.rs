use super::{TypeCheckedCAst, TypeChecker, VarDeclScope};
use crate::{
    common::{
        identifier::SymbolIdentifier,
        symbol_table_frontend::{FunAttrs, StaticVisibility, Symbol},
        types_frontend::FunType,
    },
    ds_n_a::singleton::Singleton,
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
    utils::noop,
};
use anyhow::{anyhow, Context, Result};
use std::collections::hash_map::Entry;
use std::rc::Rc;

impl TypeChecker {
    /// Resolve a function declaration, in the symbol table.
    ///
    /// @return `Some(_)` iff the function is defined.
    pub(super) fn typecheck_decl_fun(
        &mut self,
        decl: FunctionDeclaration<ResolvedCAst>,
        scope: FunDeclScope,
    ) -> Result<Option<FunctionDefinition<TypeCheckedCAst>>> {
        /* Visibility never changes following the first declaration. */
        let visibility = self.declare_fun(scope, &decl)?;

        let FunctionDeclaration {
            ident,
            typ,
            storage_class: _,
            param_idents,
            body,
        } = decl;

        for (param_ident, param_typ) in param_idents.iter().zip(typ.params.iter()) {
            let mock_var_decl = VariableDeclaration {
                ident: Rc::clone(param_ident),
                typ: param_typ.clone(),
                storage_class: None,
                init: None,
            };
            let param_defn = self.typecheck_decl_var(mock_var_decl, VarDeclScope::Paren)?;
            debug_assert!(param_defn.is_none());
        }

        let defn = match body {
            None => None,
            Some(body) => {
                self.curr_fun_type = Some(typ.clone());
                let body = self.typecheck_block(body)?;
                self.curr_fun_type = None;

                Some(FunctionDefinition {
                    ident,
                    visibility,
                    param_idents,
                    body,
                })
            }
        };
        Ok(defn)
    }

    fn declare_fun(
        &mut self,
        new_scope: FunDeclScope,
        new_decl @ FunctionDeclaration {
            ident,
            typ: new_typ,
            storage_class: new_sc,
            param_idents: _, // Params are not handled by this API.
            body: new_body,
        }: &FunctionDeclaration<ResolvedCAst>,
    ) -> Result<StaticVisibility> {
        let mut inner = || {
            let new_viz =
                Self::derive_fun_decl_visibility(new_scope, new_sc.as_ref(), new_body.as_ref())?;

            let newly_defined = new_body.is_some();

            self.insert_fun_decl(Rc::clone(ident), new_viz, newly_defined, new_typ)
        };
        inner().with_context(|| anyhow!("{new_scope:?}, {new_decl:?}"))
    }
    fn derive_fun_decl_visibility(
        new_scope: FunDeclScope,
        new_sc: Option<&StorageClassSpecifier>,
        new_body: Option<&Block<ResolvedCAst>>,
    ) -> Result<Viz> {
        use FunDeclScope as DS;
        use StorageClassSpecifier as SCS;

        let new_viz = match (new_scope, new_sc, new_body) {
            (DS::File, None | Some(SCS::Extern), _) => Viz::PrevOrGlobal,
            (DS::File, Some(SCS::Static), _) => Viz::TranslUnit,
            (DS::Block, None | Some(SCS::Extern), None) => Viz::PrevOrGlobal,
            (DS::Block, _, Some(_)) => {
                return Err(anyhow!(
                    "At scope=block, type=fun cannot have a definition."
                ))
            }
            (DS::Block, Some(SCS::Static), _) => {
                return Err(anyhow!(
                    "At scope=block, type=fun cannot specify storage duration `static`."
                ))
            }
        };
        Ok(new_viz)
    }
    fn insert_fun_decl(
        &mut self,
        ident: Rc<SymbolIdentifier>,
        new_viz: Viz,
        newly_defined: bool,
        new_typ: &Singleton<FunType>,
    ) -> Result<StaticVisibility> {
        match self.symbol_table.as_mut().entry(ident) {
            Entry::Vacant(entry) => {
                let visibility = match new_viz {
                    Viz::PrevOrGlobal => StaticVisibility::Global,
                    Viz::TranslUnit => StaticVisibility::NonGlobal,
                };
                entry.insert(Symbol::Fun {
                    typ: new_typ.clone(),
                    attrs: FunAttrs {
                        visibility,
                        is_defined: newly_defined,
                    },
                });
                Ok(visibility)
            }
            Entry::Occupied(mut entry) => {
                let prev_symbol = entry.get_mut();
                let inner = || match prev_symbol {
                    Symbol::Fun {
                        typ,
                        attrs: FunAttrs { visibility, is_defined },
                    } => {
                        if typ != new_typ {
                            return Err(anyhow!("Cannot declare with 2+ types."));
                        }
                        match (*visibility, new_viz) {
                            (_, Viz::PrevOrGlobal) => noop!(),
                            (StaticVisibility::NonGlobal, Viz::TranslUnit) => noop!(),
                            (StaticVisibility::Global, Viz::TranslUnit) => {
                                return Err(anyhow!("Cannot declare with 2+ visibilities."));
                            }
                        };
                        if *is_defined && newly_defined {
                            return Err(anyhow!("Cannot define 2+ times."));
                        }

                        *is_defined |= newly_defined;

                        return Ok(*visibility);
                    }
                    _ => return Err(anyhow!("Cannot declare with 2+ types.")),
                };
                inner().with_context(|| anyhow!("prev_symbol = {prev_symbol:?}"))
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(super) enum FunDeclScope {
    File,
    Block,
}

enum Viz {
    PrevOrGlobal,
    TranslUnit,
}
