use super::{TypeCheckedCAst, TypeChecker};
use crate::{
    common::{
        identifier::SymbolIdentifier,
        symbol_table_frontend::{ObjAttrs, StaticInitializer, StaticVisibility, Symbol},
        types_frontend::NonVoidType,
    },
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
    utils::noop,
};
use anyhow::{Context, Result, anyhow};
use derive_more::From;
use std::{collections::hash_map::Entry, rc::Rc};

impl TypeChecker {
    /// Resolve a variable declaration, in the frontend symbol table.
    ///
    /// @return `Some(_)` iff the variable is initialized at run-time, rather than at compile-time.
    pub(super) fn typecheck_decl_var(
        &mut self,
        VariableDeclaration { ident, typ, storage_class, init }: VariableDeclaration<ResolvedCAst>,
        scope: VarDeclScope,
    ) -> Result<Option<VariableDefinition<TypeCheckedCAst>>> {
        let typ = typ.into_res()?;
        let typ = NonVoidType::try_from(typ)
            .map_err(|typ| anyhow!("Var decl must have a complete type. {typ:#?}"))?;

        let decl_summary = self.derive_var_decl_summary(scope, storage_class, init, &typ)?;

        let (decl_summary, run_time_init) = match decl_summary {
            Decl::AutoStorDur(run_time_init) => (Decl::AutoStorDur(()), run_time_init),
            Decl::StaticStorDur(viz, si) => (Decl::StaticStorDur(viz, si), None),
        };

        self.insert_var_decl(Rc::clone(&ident), decl_summary, &typ)?;

        run_time_init
            .map(|init| {
                let init = self.typecheck_initializer_runtime(&typ, init)?;
                Ok(VariableDefinition { ident, init })
            })
            .transpose()
    }

    fn derive_var_decl_summary(
        &mut self,
        new_scope: VarDeclScope,
        new_sc: Option<StorageClassSpecifier>,
        new_init: Option<VariableInitializer<ResolvedCAst>>,
        new_typ: &NonVoidType,
    ) -> Result<Decl<Option<VariableInitializer<ResolvedCAst>>>> {
        use StaticInitializer as SI;
        use StorageClassSpecifier as SCS;
        use VarDeclScope as DS;

        let mut si = |init: VariableInitializer<ResolvedCAst>| {
            self.typecheck_initializer_static(new_typ, init)
                .map(StaticInitializer::Concrete)
        };
        let si_zero = || Self::generate_zero_static_initializer(new_typ);

        #[rustfmt::skip]
        let new_decl_summary = match (new_scope, new_sc, new_init) {
            (DS::File, None, Some(init))              => Decl::StaticStorDur(LViz::Global.into(), si(init)?),
            (DS::File, None, None)                    => Decl::StaticStorDur(LViz::Global.into(), SI::Tentative),
            (DS::File, Some(SCS::Static), Some(init)) => Decl::StaticStorDur(LViz::TranslUnit.into(), si(init)?),
            (DS::File, Some(SCS::Static), None)       => Decl::StaticStorDur(LViz::TranslUnit.into(), SI::Tentative),
            (DS::File, Some(SCS::Extern), Some(init)) => Decl::StaticStorDur(LViz::PrevOrGlobal.into(), si(init)?),
            (DS::File, Some(SCS::Extern), None)       => Decl::StaticStorDur(LViz::PrevOrGlobal.into(), SI::NoInitializer),
            (DS::Block, None, init)                    => Decl::AutoStorDur(init),
            (DS::Block, Some(SCS::Static), Some(init)) => Decl::StaticStorDur(Viz::Local, si(init)?),
            (DS::Block, Some(SCS::Static), None)       => Decl::StaticStorDur(Viz::Local, si_zero()),
            (DS::Block, Some(SCS::Extern), Some(_)) => return Err(anyhow!(
                "At scope=block, type=var w/ `extern` cannot have an initializer.")),
            (DS::Block, Some(SCS::Extern), None) => {
                /* Technically, the way we will determine this declaration's visibility is non-conformant to the C standard.
                For explanation, see the book page 218 and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=90472#c3
                The only case we don't handle correctly causes the compiler to exhibit undefined behavior, according to the C standard;
                    hence our overall resolution of this identifier's declaration is conformant to the C standard. */
                Decl::StaticStorDur(LViz::PrevOrGlobal.into(), SI::NoInitializer)
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
        new_typ: &NonVoidType,
    ) -> Result<()> {
        use StaticInitializer as SI;

        let entry = self.frontend_symtab.symtab_mut().entry(ident);
        match (entry, new_decl_summary) {
            (Entry::Vacant(entry), new_decl_summary) => {
                let typ = new_typ.clone();
                let attrs = match new_decl_summary {
                    Decl::AutoStorDur(()) => ObjAttrs::AutomaticStorageDuration,
                    Decl::StaticStorDur(viz, initializer) => {
                        let visibility = match viz {
                            Viz::Lkg(LViz::Global | LViz::PrevOrGlobal) => StaticVisibility::Global,
                            Viz::Lkg(LViz::TranslUnit) | Viz::Local => StaticVisibility::NonGlobal,
                        };
                        ObjAttrs::StaticReadWrite { visibility, initializer }
                    }
                };
                entry.insert(Symbol::Obj { typ, attrs });
                Ok(())
            }
            (Entry::Occupied(_), Decl::AutoStorDur(()) | Decl::StaticStorDur(Viz::Local, _)) => {
                Err(anyhow!("Cannot declare same ident 2+ times."))
            }
            (Entry::Occupied(mut entry), Decl::StaticStorDur(Viz::Lkg(new_viz), new_init)) => {
                let prev_symbol = entry.get_mut();
                let inner = || match prev_symbol {
                    Symbol::Obj { typ, attrs } => {
                        if typ != new_typ {
                            return Err(anyhow!("Cannot declare with 2+ types."));
                        }
                        match attrs {
                            ObjAttrs::AutomaticStorageDuration => {
                                return Err(anyhow!("Cannot declare with 2+ storage durations."));
                            }
                            #[rustfmt::skip]
                            ObjAttrs::StaticReadWrite { visibility, initializer } => {
                                match (visibility, new_viz) {
                                    (_, LViz::PrevOrGlobal) => noop!(),
                                    (StaticVisibility::NonGlobal, LViz::TranslUnit) => noop!(),
                                    (StaticVisibility::NonGlobal, LViz::Global) => return Err(anyhow!("Cannot declare with 2+ visibilities.")),
                                    (StaticVisibility::Global, LViz::Global) => noop!(),
                                    (StaticVisibility::Global, LViz::TranslUnit) => return Err(anyhow!("Cannot declare with 2+ visibilities.")),
                                }
                                match (&initializer, &new_init) {
                                    (SI::NoInitializer, SI::NoInitializer) => noop!(),
                                    (SI::NoInitializer, SI::Tentative | SI::Concrete(_)) => { *initializer = new_init; }
                                    (SI::Tentative, SI::NoInitializer | SI::Tentative) => noop!(),
                                    (SI::Tentative, SI::Concrete(_)) => { *initializer = new_init; }
                                    (SI::Concrete(_), SI::NoInitializer | SI::Tentative) => noop!(),
                                    (SI::Concrete(_), SI::Concrete(_)) => return Err(anyhow!("Cannot initialize 2+ times.")),
                                }
                            }
                            ObjAttrs::StaticReadonly { .. } => {
                                unreachable!(
                                    "Declarations of static readonly objs within C src code (using the `const` keyword) aren't supported."
                                )
                            }
                        }
                        Ok(())
                    }
                    _ => return Err(anyhow!("Cannot declare with 2+ types.")),
                };
                inner().with_context(|| anyhow!("prev_symbol = {prev_symbol:#?}"))
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
    StaticStorDur(Viz, StaticInitializer),
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
