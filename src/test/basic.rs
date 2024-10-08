use crate::{
    common::{primitive::Const, types_frontend::ArithmeticType},
    stage2_parse::c_ast as c,
    test::utils::{compile_typechecked_c_prog, decompose_var_type, fail, ProtoType},
};
use anyhow::Result;

#[test]
fn return_const() -> Result<()> {
    let pp = "int main(void) {
            return 42;
        }";

    let mut c_prog = compile_typechecked_c_prog(pp)?;
    let fun = c_prog.decls.pop().unwrap();
    match &fun.body.items[..] {
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
        _ => fail!("{fun:?}"),
    }

    Ok(())
}
