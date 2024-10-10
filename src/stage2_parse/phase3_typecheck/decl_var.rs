use super::{TypeCheckedCAst, TypeChecker};
use crate::{
    common::{
        identifier::SymbolIdentifier,
        symbol_table_frontend::{StaticInitialValue, StaticVisibility, Symbol, VarAttrs},
        types_frontend::ObjType,
    },
    ds_n_a::singleton::Singleton,
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
    utils::noop,
};
use anyhow::{anyhow, Context, Result};
use derive_more::From;
use std::collections::hash_map::Entry;
use std::rc::Rc;

impl TypeChecker {
    /// Resolve a variable declaration, in the symbol table.
    ///
    /// @return `Some(_)` iff the variable is initialized at run-time, rather than at compile-time.
    pub(super) fn typecheck_decl_var(
        &mut self,
        VariableDeclaration { ident, typ, storage_class, init }: VariableDeclaration<ResolvedCAst>,
        scope: VarDeclScope,
    ) -> Result<Option<VariableDefinition<TypeCheckedCAst>>> {
        let decl_summary = self.derive_var_decl_summary(scope, storage_class, init, &typ)?;

        let (decl_summary, run_time_init) = match decl_summary {
            Decl::AutoStorDur(run_time_init) => (Decl::AutoStorDur(()), run_time_init),
            Decl::StaticStorDur(viz, siv) => (Decl::StaticStorDur(viz, siv), None),
        };

        self.insert_var_decl(Rc::clone(&ident), decl_summary, &typ)?;

        run_time_init
            .map(|init| {
                let init = self.typecheck_initializer_runtime(&typ, init)?;
                Ok(VariableDefinition { ident, typ, init })
            })
            .transpose()
    }

    fn derive_var_decl_summary(
        &mut self,
        new_scope: VarDeclScope,
        new_sc: Option<StorageClassSpecifier>,
        new_init: Option<VariableInitializer<ResolvedCAst>>,
        new_typ: &Singleton<ObjType>,
    ) -> Result<Decl<Option<VariableInitializer<ResolvedCAst>>>> {
        use StaticInitialValue as SIV;
        use StorageClassSpecifier as SCS;
        use VarDeclScope as DS;

        let mut siv = |init: VariableInitializer<ResolvedCAst>| {
            self.typecheck_initializer_static(new_typ, init)
                .map(StaticInitialValue::Initial)
        };
        let siv_zero = || Self::generate_zero_static_initial_value(new_typ);

        #[rustfmt::skip]
        let new_decl_summary = match (new_scope, new_sc, new_init) {
            (DS::File, None, Some(init))              => Decl::StaticStorDur(LViz::Global.into(), siv(init)?),
            (DS::File, None, None)                    => Decl::StaticStorDur(LViz::Global.into(), SIV::Tentative),
            (DS::File, Some(SCS::Static), Some(init)) => Decl::StaticStorDur(LViz::TranslUnit.into(), siv(init)?),
            (DS::File, Some(SCS::Static), None)       => Decl::StaticStorDur(LViz::TranslUnit.into(), SIV::Tentative),
            (DS::File, Some(SCS::Extern), Some(init)) => Decl::StaticStorDur(LViz::PrevOrGlobal.into(), siv(init)?),
            (DS::File, Some(SCS::Extern), None)       => Decl::StaticStorDur(LViz::PrevOrGlobal.into(), SIV::NoInitializer),
            (DS::Block, None, init)                    => Decl::AutoStorDur(init),
            (DS::Block, Some(SCS::Static), Some(init)) => Decl::StaticStorDur(Viz::Local, siv(init)?),
            (DS::Block, Some(SCS::Static), None)       => Decl::StaticStorDur(Viz::Local, siv_zero()),
            (DS::Block, Some(SCS::Extern), Some(_)) => return Err(anyhow!(
                "At scope=block, type=var w/ `extern` cannot have an initializer.")),
            (DS::Block, Some(SCS::Extern), None) => {
                /* Technically, the way we will determine this declaration's visibility is non-conformant to the C standard.
                For explanation, see the book page 218 and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=90472#c3
                The only case we don't handle correctly causes the compiler to exhibit undefined behavior, according to the C standard;
                    hence our overall resolution of this identifier's declaration is conformant to the C standard. */
                Decl::StaticStorDur(LViz::PrevOrGlobal.into(), SIV::NoInitializer)
            }
            (DS::Paren, None, init) => Decl::AutoStorDur(init),
            (DS::Paren, Some(_), _) => return Err(anyhow!(
                "Inside parens, type=var must not have any storage class specifier.")),
        };
        Ok(new_decl_summary)
    }
    fn insert_var_decl(
        &mut self,
        ident: Rc<SymbolIdentifier>,
        new_decl_summary: Decl<()>,
        new_typ: &Singleton<ObjType>,
    ) -> Result<()> {
        use StaticInitialValue as SIV;

        let entry = self.symbol_table.as_mut().entry(ident);
        match (entry, new_decl_summary) {
            (Entry::Vacant(entry), new_decl_summary) => {
                let typ = new_typ.clone();
                let attrs = match new_decl_summary {
                    Decl::AutoStorDur(()) => VarAttrs::AutomaticStorageDuration,
                    Decl::StaticStorDur(viz, initial_value) => {
                        let visibility = match viz {
                            Viz::Lkg(LViz::Global | LViz::PrevOrGlobal) => StaticVisibility::Global,
                            Viz::Lkg(LViz::TranslUnit) | Viz::Local => StaticVisibility::NonGlobal,
                        };
                        VarAttrs::StaticStorageDuration { visibility, initial_value }
                    }
                };
                entry.insert(Symbol::Var { typ, attrs });
                Ok(())
            }
            (Entry::Occupied(_), Decl::AutoStorDur(()) | Decl::StaticStorDur(Viz::Local, _)) => {
                Err(anyhow!("Cannot declare same ident 2+ times."))
            }
            (Entry::Occupied(mut entry), Decl::StaticStorDur(Viz::Lkg(new_viz), new_init_val)) => {
                let prev_symbol = entry.get_mut();
                let inner = || match prev_symbol {
                    Symbol::Var { typ, attrs } => {
                        if typ != new_typ {
                            return Err(anyhow!("Cannot declare with 2+ types."));
                        }
                        match attrs {
                            #[rustfmt::skip]
                            VarAttrs::StaticStorageDuration { visibility, initial_value } => {
                                match (visibility, new_viz) {
                                    (_, LViz::PrevOrGlobal) => noop!(),
                                    (StaticVisibility::NonGlobal, LViz::TranslUnit) => noop!(),
                                    (StaticVisibility::NonGlobal, LViz::Global) => return Err(anyhow!("Cannot declare with 2+ visibilities.")),
                                    (StaticVisibility::Global, LViz::Global) => noop!(),
                                    (StaticVisibility::Global, LViz::TranslUnit) => return Err(anyhow!("Cannot declare with 2+ visibilities.")),
                                }
                                match (&initial_value, &new_init_val) {
                                    (SIV::NoInitializer, SIV::NoInitializer) => noop!(),
                                    (SIV::NoInitializer, SIV::Tentative | SIV::Initial(_)) => { *initial_value = new_init_val; }
                                    (SIV::Tentative, SIV::NoInitializer | SIV::Tentative) => noop!(),
                                    (SIV::Tentative, SIV::Initial(_)) => { *initial_value = new_init_val; }
                                    (SIV::Initial(_), SIV::NoInitializer | SIV::Tentative) => noop!(),
                                    (SIV::Initial(_), SIV::Initial(_)) => return Err(anyhow!("Cannot initialize 2+ times.")),
                                }
                            }
                            VarAttrs::AutomaticStorageDuration => {
                                return Err(anyhow!("Cannot declare with 2+ storage durations."))
                            }
                        }
                        Ok(())
                    }
                    _ => return Err(anyhow!("Cannot declare with 2+ types.")),
                };
                inner().with_context(|| anyhow!("prev_symbol = {prev_symbol:?}"))
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(super) enum VarDeclScope {
    File,
    Block,
    Paren,
}

enum Decl<AutoInit> {
    AutoStorDur(AutoInit),
    StaticStorDur(Viz, StaticInitialValue),
}
#[derive(From)]
enum Viz {
    Local,     // No linkage
    Lkg(LViz), // Some linkage
}
enum LViz {
    TranslUnit,
    Global,
    PrevOrGlobal,
}
