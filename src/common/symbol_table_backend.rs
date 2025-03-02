use crate::common::{
    identifier::SymbolIdentifier,
    primitive::Const,
    symbol_table_frontend::{
        FrontendSymbolTable, ObjAttrs, StaticInitializer, StaticInitializerItem, StaticVisibility,
        Symbol,
    },
    types_backend::{Alignment, AssemblyType, ScalarAssemblyType},
};
use derive_more::From;
use getset::{Getters, MutGetters};
use std::{
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

#[derive(Debug)]
pub struct AsmObj {
    pub asm_type: AssemblyType,
    pub asm_attrs: AsmObjAttrs,
}
#[derive(From, Debug)]
pub enum AsmObjAttrs {
    Stack,
    StaticRW(StaticReadWriteAsmObjAttrs),
    StaticRO(StaticReadonlyAsmObjAttrs),
}
#[derive(Debug)]
pub struct StaticReadWriteAsmObjAttrs {
    pub visibility: StaticVisibility,
    pub alignment: Alignment,
    pub initializer: Option<Vec<StaticInitializerItem>>,
}
#[derive(Debug)]
pub struct StaticReadonlyAsmObjAttrs {
    pub alignment: Alignment,
    pub initializer: StaticInitializerItem,
}

#[derive(Debug)]
pub struct AsmFun {
    pub is_defined: bool,
}

#[derive(Default, Getters, MutGetters, Debug)]
#[cfg_attr(test, derive(derive_more::Into))]
#[getset(get = "pub", get_mut = "pub")]
pub struct BackendSymbolTable {
    ident_to_obj: HashMap<Rc<SymbolIdentifier>, AsmObj>,
    ident_to_fun: HashMap<Rc<SymbolIdentifier>, AsmFun>,
}
impl BackendSymbolTable {
    pub fn merge_symbols_from(&mut self, fe_symtab: FrontendSymbolTable) {
        let ident_to_symbol: HashMap<_, _> = fe_symtab.into();
        for (ident, symbol) in ident_to_symbol.into_iter() {
            match symbol {
                Symbol::Obj { typ, attrs } => {
                    let asm_type = AssemblyType::from(&typ);
                    let asm_attrs = match attrs {
                        ObjAttrs::AutomaticStorageDuration => AsmObjAttrs::Stack,
                        ObjAttrs::StaticReadWrite { visibility, initializer } => {
                            let alignment = Alignment::default_of_nonvoid_type(&typ);
                            let initializer = match initializer {
                                StaticInitializer::Concrete(inits) => Some(inits),
                                StaticInitializer::Tentative => {
                                    let bytelen = typ.bytelen();
                                    let init = StaticInitializerItem::Zero(bytelen);
                                    Some(vec![init])
                                }
                                StaticInitializer::NoInitializer => None,
                            };
                            StaticReadWriteAsmObjAttrs {
                                visibility,
                                alignment,
                                initializer,
                            }
                            .into()
                        }
                        ObjAttrs::StaticReadonly { initializer } => {
                            /* In practice, gcc and clang emit asm s.t. each ro string doesn't specify any alignment.
                            I'm not sure whether
                                each ro string obj has alignment=1 unconditionally,
                                or its alignment depends on whether its length >= 16
                                    as implicitly specified by `.ascii` or `.cstring` directive.
                            */
                            let alignment = Alignment::B1;
                            let initializer = *initializer;
                            let initializer = StaticInitializerItem::String(initializer);
                            StaticReadonlyAsmObjAttrs { alignment, initializer }.into()
                        }
                    };
                    self.ident_to_obj
                        .insert(ident, AsmObj { asm_type, asm_attrs });
                }
                Symbol::Fun { typ: _, attrs } => {
                    let is_defined = attrs.is_defined;
                    self.ident_to_fun.insert(ident, AsmFun { is_defined });
                }
            }
        }
    }
}

#[derive(Default)]
pub struct BackendSymbolTableWithDeduper {
    backend_symtab: BackendSymbolTable,

    static_ro_scalar_to_ident: HashMap<(Alignment, Const), Rc<SymbolIdentifier>>,
}
impl BackendSymbolTableWithDeduper {
    pub fn get_or_new_static_readonly_scalar(
        &mut self,
        alignment: Alignment,
        konst: Const,
    ) -> Rc<SymbolIdentifier> {
        match self.static_ro_scalar_to_ident.entry((alignment, konst)) {
            Entry::Vacant(entry) => {
                let ident = Rc::new(SymbolIdentifier::new_generated());

                entry.insert(Rc::clone(&ident));

                self.backend_symtab.ident_to_obj.insert(
                    Rc::clone(&ident),
                    AsmObj {
                        asm_type: ScalarAssemblyType::from(konst.arithmetic_type()).into(),
                        asm_attrs: StaticReadonlyAsmObjAttrs {
                            alignment,
                            initializer: StaticInitializerItem::Single(konst),
                        }
                        .into(),
                    },
                );

                ident
            }
            Entry::Occupied(entry) => Rc::clone(entry.get()),
        }
    }

    pub fn drop_deduper(self) -> BackendSymbolTable {
        drop(self.static_ro_scalar_to_ident);
        self.backend_symtab
    }
}
