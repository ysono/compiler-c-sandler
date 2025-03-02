use super::{TypeCheckedCAst, TypeChecker, VarDeclScope};
use crate::{
    common::{
        identifier::SymbolIdentifier,
        symbol_table_frontend::{FunAttrs, StaticVisibility, Symbol},
        types_frontend::{ParsedFunType, ParsedObjType, ScalarFunType, ScalarType, SubObjType},
    },
    ds_n_a::singleton::Singleton,
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
    utils::noop,
};
use anyhow::{Context, Result, anyhow};
use std::{collections::hash_map::Entry, rc::Rc};

impl TypeChecker {
    /// Resolve a function declaration, in the frontend symbol table.
    ///
    /// @return `Some(_)` iff the function is defined.
    pub(super) fn typecheck_decl_fun(
        &mut self,
        FunctionDeclaration {
            ident,
            typ,
            storage_class,
            param_idents,
            body,
        }: FunctionDeclaration<ResolvedCAst>,
        scope: FunDeclScope,
    ) -> Result<Option<FunctionDefinition<TypeCheckedCAst>>> {
        let fun_typ = self.typecheck_fun_type(typ)?;

        /* Visibility never changes following the first declaration. */
        let visibility = self.declare_fun(scope, &ident, storage_class, body.as_ref(), &fun_typ)?;

        self.declare_params(&param_idents, &fun_typ.params)?;

        let defn = match body {
            None => None,
            Some(body) => {
                self.curr_fun_type = Some(fun_typ);
                let body = self.typecheck_block(body)?;
                let fun_typ = self.curr_fun_type.take().unwrap();

                Some(FunctionDefinition {
                    ident,
                    typ: fun_typ,
                    visibility,
                    param_idents,
                    body,
                })
            }
        };
        Ok(defn)
    }

    fn typecheck_fun_type(
        &mut self,
        in_fun_typ: Singleton<ParsedFunType>,
    ) -> Result<Singleton<ScalarFunType>> {
        let ParsedFunType { params, ret } = in_fun_typ.as_ref();

        let ret = ret.as_res()?;
        let ret = Self::extract_scalar_type(ret)
            .map_err(|typ| anyhow!("In C, a function can't return {typ:#?}"))?;

        let params = params
            .iter()
            .map(|param_typ| {
                let param_typ = param_typ.as_res()?;
                let param_typ = match Self::extract_scalar_type(param_typ.clone()) {
                    Ok(sca_typ) => sca_typ,
                    Err(arr_typ) => self.get_scalar_type(arr_typ.as_ptr_to_elem()),
                };
                Ok(param_typ)
            })
            .collect::<Result<Vec<_>>>()?;

        let out_fun_typ = self.fun_type_repo.get_or_new(ScalarFunType { params, ret });
        Ok(out_fun_typ)
    }

    fn declare_fun(
        &mut self,
        new_scope: FunDeclScope,
        ident: &Rc<SymbolIdentifier>,
        new_sc: Option<StorageClassSpecifier>,
        new_body: Option<&Block<ResolvedCAst>>,
        new_typ: &Singleton<ScalarFunType>,
    ) -> Result<StaticVisibility> {
        let mut inner = || {
            let new_viz = Self::derive_fun_decl_visibility(new_scope, new_sc, new_body)?;

            let newly_defined = new_body.is_some();

            self.insert_fun_decl(Rc::clone(ident), new_viz, newly_defined, new_typ)
        };
        inner().with_context(|| anyhow!("{new_scope:#?}, {ident:#?}"))
    }
    fn derive_fun_decl_visibility(
        new_scope: FunDeclScope,
        new_sc: Option<StorageClassSpecifier>,
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
                ));
            }
            (DS::Block, Some(SCS::Static), _) => {
                return Err(anyhow!(
                    "At scope=block, type=fun cannot specify storage duration `static`."
                ));
            }
        };
        Ok(new_viz)
    }
    fn insert_fun_decl(
        &mut self,
        ident: Rc<SymbolIdentifier>,
        new_viz: Viz,
        newly_defined: bool,
        new_typ: &Singleton<ScalarFunType>,
    ) -> Result<StaticVisibility> {
        match self.frontend_symtab.symtab_mut().entry(ident) {
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
                inner().with_context(|| anyhow!("prev_symbol = {prev_symbol:#?}"))
            }
        }
    }

    fn declare_params(
        &mut self,
        param_idents: &[Rc<SymbolIdentifier>],
        param_types: &[SubObjType<ScalarType>],
    ) -> Result<()> {
        for (param_ident, param_typ) in param_idents.iter().zip(param_types.iter()) {
            let mock_var_decl = VariableDeclaration {
                ident: Rc::clone(param_ident),
                typ: ParsedObjType(Ok(param_typ.as_owner().clone())),
                storage_class: None,
                init: None,
            };
            let param_defn = self.typecheck_decl_var(mock_var_decl, VarDeclScope::Paren)?;
            debug_assert!(param_defn.is_none());
        }
        Ok(())
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
