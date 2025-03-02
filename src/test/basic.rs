use crate::{
    common::{primitive::Const, types_frontend::ArithmeticType},
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    test::utils::{self, ProtoType, fail},
};
use anyhow::Result;

fn get_block_items(pp: &'static str) -> Result<Vec<c::BlockItem<TypeCheckedCAst>>> {
    let (mut c_prog, _) = utils::compile_until_typechecker(pp)?;
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
        [
            c::BlockItem::Statement(c::Statement::Return(Some(c::TypedExp::R(c::TypedRExp {
                exp: c::RExp::Const(Const::Int(42)),
                typ,
            })))),
        ] => {
            assert_eq!(
                typ.as_owner().as_ref(),
                ProtoType {
                    items_baseward: vec![],
                    base_type: ArithmeticType::Int,
                }
            );
        }
        _ => fail!("{items:#?}"),
    }
    Ok(())
}
