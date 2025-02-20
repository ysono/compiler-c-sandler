use crate::{
    common::types_frontend::ArithmeticType,
    stage2_parse::c_ast as c,
    test::utils::{self, ProtoType, TestDeclaratorItem as Dec, fail},
};
use anyhow::Result;

fn do_test(pp: &'static str, expected_type: ProtoType) -> Result<()> {
    let c_prog = utils::compile_until_parser(pp)?;
    let typ = match &c_prog.decls[0] {
        c::Declaration::Var(c::VariableDeclaration { typ, .. }) => typ,
        _ => fail!("{c_prog:?}"),
    };
    assert_eq!(typ.as_ref(), expected_type);
    Ok(())
}

#[test]
fn ari_type() -> Result<()> {
    do_test(
        "unsigned int my_int;",
        ProtoType {
            items_baseward: vec![],
            base_type: ArithmeticType::UInt,
        },
    )?;
    Ok(())
}

#[test]
fn ptr_type() -> Result<()> {
    do_test(
        "extern double *my_ptr;",
        ProtoType {
            items_baseward: vec![Dec::Ptr],
            base_type: ArithmeticType::Double,
        },
    )?;
    do_test(
        "unsigned static long **my_ptr;",
        ProtoType {
            items_baseward: vec![Dec::Ptr, Dec::Ptr],
            base_type: ArithmeticType::ULong,
        },
    )?;
    Ok(())
}

#[test]
fn arr_type() -> Result<()> {
    do_test(
        "extern long my_arr[17];",
        ProtoType {
            items_baseward: vec![Dec::Arr(17)],
            base_type: ArithmeticType::Long,
        },
    )?;
    do_test(
        "static unsigned my_arr[17][13][11];",
        ProtoType {
            items_baseward: vec![Dec::Arr(17), Dec::Arr(13), Dec::Arr(11)],
            base_type: ArithmeticType::UInt,
        },
    )?;
    Ok(())
}

#[test]
fn arr_and_ptr_types() -> Result<()> {
    do_test(
        "int *my_arr[17];",
        ProtoType {
            items_baseward: vec![Dec::Arr(17), Dec::Ptr],
            base_type: ArithmeticType::Int,
        },
    )?;
    do_test(
        "int (*my_ptr)[17];",
        ProtoType {
            items_baseward: vec![Dec::Ptr, Dec::Arr(17)],
            base_type: ArithmeticType::Int,
        },
    )?;
    do_test(
        "int **(**my_arr[17][13])[11][7];",
        ProtoType {
            items_baseward: vec![
                Dec::Arr(17),
                Dec::Arr(13),
                Dec::Ptr,
                Dec::Ptr,
                Dec::Arr(11),
                Dec::Arr(7),
                Dec::Ptr,
                Dec::Ptr,
            ],
            base_type: ArithmeticType::Int,
        },
    )?;
    do_test(
        "int *(*(**my_ptr)[17][13])[11][7];",
        ProtoType {
            items_baseward: vec![
                Dec::Ptr,
                Dec::Ptr,
                Dec::Arr(17),
                Dec::Arr(13),
                Dec::Ptr,
                Dec::Arr(11),
                Dec::Arr(7),
                Dec::Ptr,
            ],
            base_type: ArithmeticType::Int,
        },
    )?;
    Ok(())
}
