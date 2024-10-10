use crate::common::{
    identifier::SymbolIdentifier,
    symbol_table_frontend::{Symbol, SymbolTable, VarAttrs},
    types_backend::AssemblyType,
};
use getset::{Getters, MutGetters};
use std::collections::HashMap;
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

#[derive(Getters, MutGetters, Debug)]
#[getset(get = "pub", get_mut = "pub")]
pub struct BackendSymbolTable {
    objs: HashMap<Rc<SymbolIdentifier>, AsmObj>,
    funs: HashMap<Rc<SymbolIdentifier>, AsmFun>,
}
impl From<SymbolTable> for BackendSymbolTable {
    fn from(c_table: SymbolTable) -> Self {
        let mut objs = HashMap::default();
        let mut funs = HashMap::default();

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
                    objs.insert(ident, AsmObj { asm_type, loc });
                }
                Symbol::Fun { typ: _, attrs } => {
                    let is_defined = attrs.is_defined;
                    funs.insert(ident, AsmFun { is_defined });
                }
            }
        }
        Self { objs, funs }
    }
}
