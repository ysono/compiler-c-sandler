use crate::{
    common::types_frontend::{ArithmeticType, VarType},
    stage2_parse::{c_ast as c, phase1_parse::ParsedCAst},
    test::utils::{
        compile_parsed_c_prog, decompose_var_type, fail, ProtoType, TestDeclaratorItem as Dec,
    },
};
use anyhow::Result;

fn extract_first_decl(c_prog: &c::Program<ParsedCAst>) -> &VarType {
    match &c_prog.decls[0] {
        c::Declaration::Var(c::VariableDeclaration { typ, .. }) => typ,
        _ => fail!("{c_prog:?}"),
    }
}

#[test]
fn arithmetic_type() -> Result<()> {
    let pp = "unsigned int my_int;";

    let c_prog = compile_parsed_c_prog(pp)?;
    let typ = extract_first_decl(&c_prog);
    assert_eq!(
        decompose_var_type(typ),
        ProtoType {
            items_baseward: vec![],
            base_type: ArithmeticType::UInt
        }
    );

    Ok(())
}

#[test]
fn ptr_type() -> Result<()> {
    let pp = "extern double *my_ptr;";

    let c_prog = compile_parsed_c_prog(pp)?;
    let typ = extract_first_decl(&c_prog);
    assert_eq!(
        decompose_var_type(typ),
        ProtoType {
            items_baseward: vec![Dec::Ptr],
            base_type: ArithmeticType::Double
        }
    );

    Ok(())
}

#[test]
fn nested_ptr_type() -> Result<()> {
    let pp = "unsigned static long **my_ptr;";

    let c_prog = compile_parsed_c_prog(pp)?;
    let typ = extract_first_decl(&c_prog);
    assert_eq!(
        decompose_var_type(typ),
        ProtoType {
            items_baseward: vec![Dec::Ptr, Dec::Ptr],
            base_type: ArithmeticType::ULong
        }
    );

    Ok(())
}
