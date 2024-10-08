use crate::{
    common::{primitive::Const, types_frontend::ArithmeticType},
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    test::utils::{compile_typechecked_c_prog, decompose_var_type, fail, ProtoType},
};
use anyhow::Result;

fn get_block_items(pp: &'static str) -> Result<Vec<c::BlockItem<TypeCheckedCAst>>> {
    let mut c_prog = compile_typechecked_c_prog(pp)?;
    let fun = c_prog.decls.pop().unwrap();
    Ok(fun.body.items)
}

#[test]
fn return_const() -> Result<()> {
    let items = get_block_items(
        "int main(void) {
            return 42;
        }",
    )?;
    match &items[..] {
        // No redundant casting "as if by assignment".
        [c::BlockItem::Statement(c::Statement::Return(c::TypedExpression {
            exp: c::Expression::Const(Const::Int(42)),
            typ,
        }))] => {
            assert_eq!(
                decompose_var_type(&typ),
                ProtoType {
                    items_baseward: vec![],
                    base_type: ArithmeticType::Int,
                }
            );
        }
        _ => fail!("{items:?}"),
    }
    Ok(())
}
