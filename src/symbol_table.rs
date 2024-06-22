use crate::stage2_parse::{
    c_ast::Identifier,
    c_ast_resolved::{
        Block, Const, Expression, FunctionCall, FunctionDeclaration, StorageClassSpecifier,
        VariableDeclaration,
    },
    phase2_resolve::ResolvedCAst,
};
use anyhow::{anyhow, Context, Result};
use derivative::Derivative;
use derive_more::{Deref, DerefMut, From};
use std::collections::{hash_map::Entry, HashMap};
use std::fmt::Display;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Derivative, Debug)]
#[derivative(PartialEq, Eq, Hash)]
pub enum ResolvedIdentifier {
    NoLinkage {
        id: IdentifierId,

        #[derivative(PartialEq = "ignore", Hash = "ignore")]
        orig: Option<Rc<Identifier>>,
    },
    SomeLinkage(Rc<Identifier>),
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
            Self::SomeLinkage(..) => None,
        }
    }
}
impl Display for ResolvedIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoLinkage { id, orig } => {
                /* DELIM and ORIG_DEFAULT must each be a non-empty str that cannot be a substring within any original identifier string. */
                const DELIM: char = '.';
                const ORIG_DEFAULT: &str = "tmp.";
                let name = orig
                    .as_ref()
                    .map(|ident| ident as &str)
                    .unwrap_or(ORIG_DEFAULT);
                let id = id.as_int();
                write!(f, "{name}{DELIM}{id:x}")
            }
            Self::SomeLinkage(ident) => {
                let name = ident as &str;
                write!(f, "{name}")
            }
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
pub enum Symbol {
    Var { typ: VarType, attrs: VarAttrs },
    Fun { typ: Rc<FunType>, attrs: FunAttrs },
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum VarType {
    Int,
    Long,
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

#[derive(PartialEq, Eq, Debug)]
pub struct FunType {
    pub params: Vec<VarType>,
    pub ret: VarType,
}

#[derive(Debug)]
pub struct FunAttrs {
    pub visibility: StaticVisibility,
    pub is_defined: bool,
}

#[derive(Clone, Copy, Debug)]
pub enum StaticVisibility {
    Global,    // Visible to other translation units.
    NonGlobal, // Visible either in translation unit or in block.
}

#[derive(Default, Deref, DerefMut, Debug)]
pub struct SymbolTable {
    symbol_table: HashMap<Rc<ResolvedIdentifier>, Symbol>,
}
impl SymbolTable {
    pub fn declare_var(
        &mut self,
        new_scope: VarDeclScope,
        new_decl @ VariableDeclaration {
            ident,
            init: new_init,
            typ: new_typ,
            storage_class: new_sc,
        }: &VariableDeclaration<ResolvedCAst>,
    ) -> Result<()> {
        use StaticInitialValue as SIV;
        use StorageClassSpecifier as SCS;
        use VarDeclScope as DS;

        enum Decl {
            AutoStorDur,
            StaticStorDur(Viz, SIV),
        }
        #[derive(From)]
        enum Viz {
            Block,   // No linkage
            L(LViz), // Some linkage
        }
        enum LViz {
            TranslUnit,
            Global,
            PrevOrGlobal,
        }

        let init_cast_const = |konst: &Const| -> Const {
            match (konst, new_typ) {
                (k @ Const::Int(_), VarType::Int) => *k,
                (Const::Int(i), VarType::Long) => Const::Long(*i as i64),
                (Const::Long(i), VarType::Int) => Const::Int(*i as i32), // Truncate most significant bytes.
                (k @ Const::Long(_), VarType::Long) => *k,
            }
        };
        let init_const = |exp: &Expression<ResolvedCAst>| -> Result<SIV> {
            match exp {
                Expression::Const(konst) => {
                    let konst = init_cast_const(konst);
                    Ok(SIV::Initial(konst))
                }
                _ => Err(anyhow!("On type=var w/ storage_duration=static, initializer, if present, must be constexpr, but found {new_init:?}"))
            }
        };
        #[rustfmt::skip]
        let new_decl_summary = match (new_scope, new_sc, new_init) {
            (DS::File, None, Some(init))              => Decl::StaticStorDur(LViz::Global.into(), init_const(init)?),
            (DS::File, None, None)                    => Decl::StaticStorDur(LViz::Global.into(), SIV::Tentative),
            (DS::File, Some(SCS::Static), Some(init)) => Decl::StaticStorDur(LViz::TranslUnit.into(), init_const(init)?),
            (DS::File, Some(SCS::Static), None)       => Decl::StaticStorDur(LViz::TranslUnit.into(), SIV::Tentative),
            (DS::File, Some(SCS::Extern), Some(init)) => Decl::StaticStorDur(LViz::PrevOrGlobal.into(), init_const(init)?),
            (DS::File, Some(SCS::Extern), None)       => Decl::StaticStorDur(LViz::PrevOrGlobal.into(), SIV::NoInitializer),
            (DS::Block, None, _)                       => Decl::AutoStorDur,
            (DS::Block, Some(SCS::Static), Some(init)) => Decl::StaticStorDur(Viz::Block, init_const(init)?),
            (DS::Block, Some(SCS::Static), None)       => Decl::StaticStorDur(Viz::Block, SIV::Initial(init_cast_const(&Const::Int(0)))),
            (DS::Block, Some(SCS::Extern), Some(_)) => return Err(anyhow!(
                "At scope=block, on type=var w/ `extern`, we cannot have an initializer. {new_decl:?}")),
            (DS::Block, Some(SCS::Extern), None) => {
                /* Technically, the way we will determine this declaration's visibility is non-conformant to the C standard.
                For explanation, see the book page 218 and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=90472#c3
                The only case we don't handle correctly causes the compiler to exhibit undefined behavior, according to the C standard;
                    hence our overall resolution of this identifier's declaration is conformant to the C standard. */
                Decl::StaticStorDur(LViz::PrevOrGlobal.into(), SIV::NoInitializer)
            }
            (DS::ForInit, None, _) => Decl::AutoStorDur,
            (DS::ForInit, Some(_), _) => return Err(anyhow!(
                "For-loop init declaration must not have any storage class specifier. {new_decl:?}")),
        };

        let entry = self.symbol_table.entry(Rc::clone(ident));
        match (entry, new_decl_summary) {
            (Entry::Vacant(entry), new_decl_summary) => {
                let typ = *new_typ;
                let attrs = match new_decl_summary {
                    Decl::AutoStorDur => VarAttrs::AutomaticStorageDuration,
                    Decl::StaticStorDur(viz, initial_value) => {
                        let visibility = match viz {
                            Viz::L(LViz::Global) | Viz::L(LViz::PrevOrGlobal) => {
                                StaticVisibility::Global
                            }
                            Viz::L(LViz::TranslUnit) | Viz::Block => StaticVisibility::NonGlobal,
                        };
                        VarAttrs::StaticStorageDuration {
                            visibility,
                            initial_value,
                        }
                    }
                };
                entry.insert(Symbol::Var { typ, attrs });
                Ok(())
            }
            (Entry::Occupied(_), Decl::AutoStorDur | Decl::StaticStorDur(Viz::Block, _)) => {
                Err(anyhow!("Cannot declare {ident:?} 2+ times."))
            }
            (Entry::Occupied(mut entry), Decl::StaticStorDur(Viz::L(new_viz), new_init_val)) => {
                let prev_symbol = entry.get_mut();
                let inner = || {
                    match prev_symbol {
                        Symbol::Var { typ, attrs } => {
                            if typ != new_typ {
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
                inner().with_context(|| anyhow!("{ident:?}: {prev_symbol:?} vs {new_decl:?}"))
            }
        }
    }
    pub fn use_var(&self, ident: &ResolvedIdentifier) -> Result<VarType> {
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

    pub fn declare_fun(
        &mut self,
        new_scope: FunDeclScope,
        new_decl @ FunctionDeclaration {
            ident,
            param_idents: _,
            typ: new_typ,
            storage_class: new_sc,
        }: &FunctionDeclaration,
        new_body: Option<&Block<ResolvedCAst>>,
    ) -> Result<()> {
        use FunDeclScope as DS;
        use StorageClassSpecifier as SCS;
        enum Viz {
            PrevOrGlobal,
            TranslUnit,
        }

        let new_viz = match (new_scope, new_sc, new_body) {
            (DS::File, None | Some(SCS::Extern), _) =>  Viz::PrevOrGlobal,
            (DS::File, Some(SCS::Static), _) =>  Viz::TranslUnit,
            (DS::Block, None | Some(SCS::Extern), None) =>  Viz::PrevOrGlobal,
            (DS::Block, None | Some(SCS::Extern), Some(_)) => {
                return Err(anyhow!(
                    "At scope=block, on type=fun, we cannot have a definition. {new_decl:?}"
                ))
            }
            (DS::Block, Some(SCS::Static), _) => {
                return Err(anyhow!(
                    "At scope=block, on type=fun, we cannot specify storage duration `static`. {new_decl:?}"
                ))
            }
        };

        let newly_defined = new_body.is_some();

        match self.symbol_table.entry(Rc::clone(ident)) {
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
                            attrs:
                                FunAttrs {
                                    visibility,
                                    is_defined,
                                },
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
                inner().with_context(|| anyhow!("{ident:?}: {prev_symbol:?} vs {new_decl:?}"))
            }
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

pub enum VarDeclScope {
    File,
    Block,
    ForInit,
}
pub enum FunDeclScope {
    File,
    Block,
}
