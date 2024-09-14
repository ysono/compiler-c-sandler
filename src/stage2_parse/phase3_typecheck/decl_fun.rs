use super::{TypeCheckedCAst, TypeChecker, VarDeclScope};
use crate::{
    common::{
        identifier::UniqueIdentifier,
        symbol_table_frontend::{FunAttrs, StaticVisibility, Symbol},
        types_frontend::FunType,
    },
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
};
use anyhow::{anyhow, Context, Result};
use std::collections::hash_map::Entry;
use std::rc::Rc;

impl TypeChecker {
    pub(super) fn typecheck_decl_fundecl(
        &mut self,
        decl: FunctionDeclaration<ResolvedCAst>,
        scope: FunDeclScope,
    ) -> Result<FunctionDeclaration<TypeCheckedCAst>> {
        self.declare_fun(scope, &decl, None)?;

        let FunctionDeclaration {
            ident,
            param_idents,
            typ,
            storage_class,
        } = decl;
        let decl = FunctionDeclaration {
            ident,
            param_idents,
            typ,
            storage_class,
        };

        Ok(decl)
    }
    pub(super) fn typecheck_decl_fundefn(
        &mut self,
        FunctionDefinition { decl, body }: FunctionDefinition<ResolvedCAst>,
        scope: FunDeclScope,
    ) -> Result<FunctionDefinition<TypeCheckedCAst>> {
        self.declare_fun(scope, &decl, Some(&body))?;

        let FunctionDeclaration {
            ident,
            param_idents,
            typ,
            storage_class,
        } = decl;

        for (ident, typ) in param_idents.iter().zip(typ.params.iter()) {
            let mock_var_decl = VariableDeclaration {
                ident: Rc::clone(ident),
                init: None,
                typ: *typ,
                storage_class: None,
            };
            self.declare_var(VarDeclScope::Paren, &mock_var_decl)?;
        }

        self.curr_fun_type = Some(Rc::clone(&typ));
        let body = self.typecheck_block(body)?;
        self.curr_fun_type = None;

        let decl = FunctionDeclaration {
            ident,
            param_idents,
            typ,
            storage_class,
        };
        Ok(FunctionDefinition { decl, body })
    }

    fn declare_fun(
        &mut self,
        new_scope: FunDeclScope,
        new_decl @ FunctionDeclaration {
            ident,
            param_idents: _, // Params are not handled by this API.
            typ: new_typ,
            storage_class: new_sc,
        }: &FunctionDeclaration<ResolvedCAst>,
        new_body: Option<&Block<ResolvedCAst>>,
    ) -> Result<()> {
        let mut inner = || {
            let new_viz = Self::derive_fun_decl_visibility(new_scope, new_sc.as_ref(), new_body)?;

            let newly_defined = new_body.is_some();

            self.insert_fun_decl(Rc::clone(ident), new_viz, newly_defined, new_typ)
        };
        inner().with_context(|| anyhow!("{new_scope:?}, {new_decl:?}, {new_body:?}"))
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
            (DS::Block, None | Some(SCS::Extern), Some(_)) => {
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
        ident: Rc<UniqueIdentifier>,
        new_viz: Viz,
        newly_defined: bool,
        new_typ: &Rc<FunType>,
    ) -> Result<()> {
        match self.symbol_table.entry(ident) {
            Entry::Vacant(entry) => {
                let visibility = match new_viz {
                    Viz::PrevOrGlobal => StaticVisibility::Global,
                    Viz::TranslUnit => StaticVisibility::NonGlobal,
                };
                entry.insert(Symbol::Fun {
                    typ: Rc::clone(new_typ),
                    attrs: FunAttrs {
                        visibility,
                        is_defined: newly_defined,
                    },
                });
                Ok(())
            }
            Entry::Occupied(mut entry) => {
                let prev_symbol = entry.get_mut();
                let inner = || {
                    match prev_symbol {
                        Symbol::Fun {
                            typ,
                            attrs: FunAttrs { visibility, is_defined },
                        } => {
                            if typ != new_typ {
                                return Err(anyhow!("Cannot declare with 2+ types."));
                            }
                            match (visibility, new_viz) {
                                (_, Viz::PrevOrGlobal) => { /* No-op. */ }
                                (StaticVisibility::NonGlobal, Viz::TranslUnit) => { /* No-op. */ }
                                (StaticVisibility::Global, Viz::TranslUnit) => {
                                    return Err(anyhow!("Cannot declare with 2+ visibilities."));
                                }
                            };
                            if *is_defined && newly_defined {
                                return Err(anyhow!("Cannot define 2+ times."));
                            }

                            *is_defined |= newly_defined;

                            return Ok(());
                        }
                        _ => return Err(anyhow!("Cannot declare with 2+ types.")),
                    }
                };
                inner().with_context(|| anyhow!("prev_symbol = {prev_symbol:?}"))
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum FunDeclScope {
    File,
    Block,
}

enum Viz {
    PrevOrGlobal,
    TranslUnit,
}
