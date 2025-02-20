//! Inspect or manipulate compiler outputs: ASTs and symtabs.

use crate::{
    common::{
        identifier::{RawIdentifier, SymbolIdentifier},
        primitive::Const,
        symbol_table_backend::{
            AsmObj, AsmObjAttrs, BackendSymbolTable, StaticReadWriteAsmObjAttrs,
            StaticReadonlyAsmObjAttrs,
        },
        types_backend::ScalarAssemblyType,
    },
    stage3_tacky::tacky_ast,
    stage4_asm_gen::{FinalizedAsmAst, asm_ast},
    test::utils::fail,
    utils::noop,
};
use owning_ref::OwningRef;
use std::{collections::HashMap, rc::Rc};

pub fn remove_tacky_implicit_return_instr(mut t_prog: tacky_ast::Program) -> tacky_ast::Program {
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

pub fn asm_prog_into_fun_by_ident(
    a_prog: asm_ast::Program<FinalizedAsmAst>,
) -> HashMap<OwningRef<Rc<RawIdentifier>, str>, asm_ast::Function<FinalizedAsmAst>> {
    a_prog
        .funs
        .into_iter()
        .map(|a_fun| {
            let ident = match &a_fun.ident.as_ref() {
                SymbolIdentifier::Exact(raw_ident) => {
                    /* We want HashMap key type to be `Borrow<str>`, for syntactical convenience. */
                    OwningRef::new(Rc::clone(raw_ident)).map(|raw_ident| &raw_ident as &str)
                }
                SymbolIdentifier::Generated { .. } => fail!(),
            };
            (ident, a_fun)
        })
        .collect()
}

pub fn remove_asm_implicit_return_instr(a_fun: &mut asm_ast::Function<FinalizedAsmAst>) {
    let instr_2 = a_fun.instrs.pop_back();
    let instr_1 = a_fun.instrs.pop_back();
    assert!(matches!(
        instr_1,
        Some(asm_ast::Instruction::Mov {
            asm_type: ScalarAssemblyType::Longword,
            src: asm_ast::Operand::ImmediateValue(0),
            dst: asm_ast::Operand::Register(asm_ast::Register::AX),
        })
    ));
    assert!(matches!(instr_2, Some(asm_ast::Instruction::Ret)));
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
