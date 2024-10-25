use crate::common::{
    identifier::SymbolIdentifier,
    primitive::Const,
    symbol_table_frontend::{Symbol, SymbolTable, VarAttrs},
    types_backend::{Alignment, AssemblyType, ScalarAssemblyType},
};
use getset::{Getters, MutGetters};
use std::collections::{hash_map::Entry, HashMap};
use std::rc::Rc;

#[derive(Debug)]
pub struct AsmObj {
    pub asm_type: AssemblyType,
    pub loc: ObjLocation,
}
#[derive(Debug)]
pub enum ObjLocation {
    Stack,
    StaticReadWrite,
    StaticReadonly,
}

#[derive(Debug)]
pub struct AsmFun {
    pub is_defined: bool,
}

#[derive(Default, Getters, MutGetters, Debug)]
#[getset(get = "pub", get_mut = "pub")]
pub struct BackendSymbolTable {
    ident_to_obj: HashMap<Rc<SymbolIdentifier>, AsmObj>,
    ident_to_fun: HashMap<Rc<SymbolIdentifier>, AsmFun>,

    static_readonly_to_ident: HashMap<(Alignment, Const), Rc<SymbolIdentifier>>,
}
impl BackendSymbolTable {
    pub fn get_or_new_static_readonly(
        &mut self,
        alignment: Alignment,
        konst: Const,
    ) -> Rc<SymbolIdentifier> {
        match self.static_readonly_to_ident.entry((alignment, konst)) {
            Entry::Vacant(entry) => {
                let ident = Rc::new(SymbolIdentifier::new_generated());

                entry.insert(Rc::clone(&ident));

                self.ident_to_obj.insert(
                    Rc::clone(&ident),
                    AsmObj {
                        asm_type: ScalarAssemblyType::from(konst.arithmetic_type()).into(),
                        loc: ObjLocation::StaticReadonly,
                    },
                );

                ident
            }
            Entry::Occupied(entry) => Rc::clone(entry.get()),
        }
    }

    pub fn merge_symbols_from(&mut self, c_table: SymbolTable) {
        let c_table: HashMap<_, _> = c_table.into();
        for (ident, symbol) in c_table.into_iter() {
            match symbol {
                Symbol::Var { typ, attrs } => {
                    let asm_type = AssemblyType::from(typ.as_ref());
                    let loc = match attrs {
                        VarAttrs::AutomaticStorageDuration => ObjLocation::Stack,
                        VarAttrs::StaticStorageDuration { .. } => ObjLocation::StaticReadWrite,
                        /* I wonder whether, if type==Double, we should be locating it on a readonly section. */
                    };
                    self.ident_to_obj.insert(ident, AsmObj { asm_type, loc });
                }
                Symbol::Fun { typ: _, attrs } => {
                    let is_defined = attrs.is_defined;
                    self.ident_to_fun.insert(ident, AsmFun { is_defined });
                }
            }
        }
    }
}
