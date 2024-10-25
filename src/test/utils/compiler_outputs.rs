//! Inspect or manipulate compiler outputs: ASTs and symtabs.

use crate::{
    common::{
        identifier::SymbolIdentifier,
        primitive::Const,
        symbol_table_backend::{
            AsmObj, AsmObjAttrs, BackendSymbolTable, StaticReadWriteAsmObjAttrs,
            StaticReadonlyAsmObjAttrs,
        },
    },
    stage3_tacky::tacky_ast,
    utils::noop,
};
use std::rc::Rc;

pub fn expect_tacky_implicit_return_instr(mut t_prog: tacky_ast::Program) -> tacky_ast::Program {
    for fun in t_prog.funs.iter_mut() {
        let last_instr = fun.instrs.pop();
        assert!(matches!(
            last_instr,
            Some(tacky_ast::Instruction::Return(tacky_ast::Value::Constant(
                Const::Int(0)
            )))
        ));
    }
    t_prog
}

pub fn be_symtab_into_static_objs(
    be_symtab: BackendSymbolTable,
) -> (
    Vec<(Rc<SymbolIdentifier>, StaticReadWriteAsmObjAttrs)>,
    Vec<(Rc<SymbolIdentifier>, StaticReadonlyAsmObjAttrs)>,
) {
    let mut static_rw_objs = vec![];
    let mut static_ro_objs = vec![];
    let (ident_to_obj, _, _) = be_symtab.into();
    for (ident, obj) in ident_to_obj.into_iter() {
        let AsmObj { asm_type: _, asm_attrs } = obj;
        match asm_attrs {
            AsmObjAttrs::Stack => noop!(),
            AsmObjAttrs::StaticRW(attrs) => {
                let obj = (ident, attrs);
                static_rw_objs.push(obj);
            }
            AsmObjAttrs::StaticRO(attrs) => {
                let obj = (ident, attrs);
                static_ro_objs.push(obj);
            }
        }
    }

    static_rw_objs.sort_by(|(l_ident, _), (r_ident, _)| l_ident.cmp(r_ident));
    static_ro_objs.sort_by(|(l_ident, _), (r_ident, _)| l_ident.cmp(r_ident));

    (static_rw_objs, static_ro_objs)
}
