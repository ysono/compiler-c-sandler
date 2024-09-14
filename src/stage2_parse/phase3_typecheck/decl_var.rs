use super::{TypeCheckedCAst, TypeChecker};
use crate::{
    common::{
        identifier::UniqueIdentifier,
        symbol_table_frontend::{StaticInitialValue, StaticVisibility, Symbol, VarAttrs},
        types_frontend::{Const, VarType},
    },
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
};
use anyhow::{anyhow, Context, Result};
use derive_more::From;
use std::collections::hash_map::Entry;
use std::rc::Rc;

impl TypeChecker {
    pub(super) fn typecheck_decl_var(
        &mut self,
        decl: VariableDeclaration<ResolvedCAst>,
        scope: VarDeclScope,
    ) -> Result<VariableDeclaration<TypeCheckedCAst>> {
        self.declare_var(scope, &decl)?;

        let VariableDeclaration { ident, init, typ, storage_class } = decl;

        let init = init
            .map(|exp| -> Result<_> {
                let exp = self.typecheck_exp(exp)?;
                let exp = Self::maybe_cast_exp(exp, typ);
                Ok(exp)
            })
            .transpose()?;

        Ok(VariableDeclaration { ident, init, typ, storage_class })
    }

    pub(super) fn declare_var(
        &mut self,
        new_scope: VarDeclScope,
        new_decl @ VariableDeclaration {
            ident,
            init: new_init,
            typ: new_typ,
            storage_class: new_sc,
        }: &VariableDeclaration<ResolvedCAst>,
    ) -> Result<()> {
        let mut inner = || {
            let new_decl_summary = Self::derive_var_decl_summary(
                new_scope,
                new_sc.as_ref(),
                new_init.as_ref(),
                *new_typ,
            )?;

            self.insert_var_decl(Rc::clone(ident), new_decl_summary, *new_typ)
        };
        inner().with_context(|| anyhow!("{new_scope:?}, {new_decl:?}"))
    }
    fn derive_var_decl_summary(
        new_scope: VarDeclScope,
        new_sc: Option<&StorageClassSpecifier>,
        new_init: Option<&Expression<ResolvedCAst>>,
        new_typ: VarType,
    ) -> Result<Decl> {
        use StaticInitialValue as SIV;
        use StorageClassSpecifier as SCS;
        use VarDeclScope as DS;

        let siv = |init_exp: &Expression<ResolvedCAst>| {
            match init_exp {
                Expression::Const(konst) => {
                    let konst = konst.cast_to(new_typ);
                    Ok(StaticInitialValue::Initial(konst))
                }
                _ => Err(anyhow!("On type=var w/ storage_duration=static, initializer, if present, must be constexpr. Only a simple const is supported."))
            }
        };
        let siv_zero = || StaticInitialValue::Initial(Const::new_zero_bits(new_typ));

        #[rustfmt::skip]
        let new_decl_summary = match (new_scope, new_sc, new_init) {
            (DS::File, None, Some(init))              => Decl::StaticStorDur(LViz::Global.into(), siv(init)?),
            (DS::File, None, None)                    => Decl::StaticStorDur(LViz::Global.into(), SIV::Tentative),
            (DS::File, Some(SCS::Static), Some(init)) => Decl::StaticStorDur(LViz::TranslUnit.into(), siv(init)?),
            (DS::File, Some(SCS::Static), None)       => Decl::StaticStorDur(LViz::TranslUnit.into(), SIV::Tentative),
            (DS::File, Some(SCS::Extern), Some(init)) => Decl::StaticStorDur(LViz::PrevOrGlobal.into(), siv(init)?),
            (DS::File, Some(SCS::Extern), None)       => Decl::StaticStorDur(LViz::PrevOrGlobal.into(), SIV::NoInitializer),
            (DS::Block, None, _)                       => Decl::AutoStorDur,
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
            (DS::Paren, None, _) => Decl::AutoStorDur,
            (DS::Paren, Some(_), _) => return Err(anyhow!(
                "Inside parens, type=var must not have any storage class specifier.")),
        };
        Ok(new_decl_summary)
    }
    fn insert_var_decl(
        &mut self,
        ident: Rc<UniqueIdentifier>,
        new_decl_summary: Decl,
        new_typ: VarType,
    ) -> Result<()> {
        use StaticInitialValue as SIV;

        let entry = self.symbol_table.entry(ident);
        match (entry, new_decl_summary) {
            (Entry::Vacant(entry), new_decl_summary) => {
                let typ = new_typ;
                let attrs = match new_decl_summary {
                    Decl::AutoStorDur => VarAttrs::AutomaticStorageDuration,
                    Decl::StaticStorDur(viz, initial_value) => {
                        let visibility = match viz {
                            Viz::L(LViz::Global) | Viz::L(LViz::PrevOrGlobal) => {
                                StaticVisibility::Global
                            }
                            Viz::L(LViz::TranslUnit) | Viz::Local => StaticVisibility::NonGlobal,
                        };
                        VarAttrs::StaticStorageDuration { visibility, initial_value }
                    }
                };
                entry.insert(Symbol::Var { typ, attrs });
                Ok(())
            }
            (Entry::Occupied(_), Decl::AutoStorDur | Decl::StaticStorDur(Viz::Local, _)) => {
                Err(anyhow!("Cannot declare same ident 2+ times."))
            }
            (Entry::Occupied(mut entry), Decl::StaticStorDur(Viz::L(new_viz), new_init_val)) => {
                let prev_symbol = entry.get_mut();
                let inner = || {
                    match prev_symbol {
                        Symbol::Var { typ, attrs } => {
                            if typ != &new_typ {
                                return Err(anyhow!("Cannot declare with 2+ types."));
                            }
                            match attrs {
                                #[rustfmt::skip]
                                VarAttrs::StaticStorageDuration { visibility, initial_value } => {
                                    match (visibility, new_viz) {
                                        (StaticVisibility::NonGlobal, LViz::TranslUnit | LViz::PrevOrGlobal) => { /* No-op. */ }
                                        (StaticVisibility::NonGlobal, LViz::Global) => return Err(anyhow!("Cannot declare with 2+ visibilities.")),
                                        (StaticVisibility::Global, LViz::Global | LViz::PrevOrGlobal) => { /* No-op. */ }
                                        (StaticVisibility::Global, LViz::TranslUnit) => return Err(anyhow!("Cannot declare with 2+ visibilities.")),
                                    }
                                    match (&initial_value, &new_init_val) {
                                        (SIV::NoInitializer, SIV::NoInitializer) => { /* No-op. */ }
                                        (SIV::NoInitializer, SIV::Tentative | SIV::Initial(_)) => { *initial_value = new_init_val }
                                        (SIV::Tentative, SIV::NoInitializer | SIV::Tentative) => { /* No-op. */ }
                                        (SIV::Tentative, SIV::Initial(_)) => { *initial_value = new_init_val }
                                        (SIV::Initial(_), SIV::NoInitializer | SIV::Tentative) => { /* No-op. */ }
                                        (SIV::Initial(_), SIV::Initial(_)) => return Err(anyhow!("Cannot initialize 2+ times.")),
                                    };
                                }
                                VarAttrs::AutomaticStorageDuration { .. } => {
                                    return Err(anyhow!(
                                        "Cannot declare with 2+ storage durations."
                                    ))
                                }
                            }
                            Ok(())
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
pub enum VarDeclScope {
    File,
    Block,
    Paren,
}

enum Decl {
    AutoStorDur,
    StaticStorDur(Viz, StaticInitialValue),
}
#[derive(From)]
enum Viz {
    Local,   // No linkage
    L(LViz), // Some linkage
}
enum LViz {
    TranslUnit,
    Global,
    PrevOrGlobal,
}
