use crate::{
    stage3_tacky::tacky_ast as t,
    test::utils::{compile_tacky_prog, expect_tacky_implicit_return_instr, fail},
    utils::noop,
};
use anyhow::Result;

#[test]
fn deref() -> Result<()> {
    let pp = "int main(void) {
            int* my_ptr;
            *my_ptr;
        }";

    let t_prog = compile_tacky_prog(pp)?;
    let t_prog = expect_tacky_implicit_return_instr(t_prog);
    assert!(t_prog.funs[0].instrs.is_empty());

    Ok(())
}

#[test]
fn deref_then_get_value() -> Result<()> {
    let pp = "int main(void) {
            int* my_ptr;
            return *my_ptr;
        }";

    let t_prog = compile_tacky_prog(pp)?;
    let t_prog = expect_tacky_implicit_return_instr(t_prog);
    match &t_prog.funs[0].instrs[..] {
        [t::Instruction::Load(_), t::Instruction::Return(_)] => noop!(),
        _ => fail!(),
    }

    Ok(())
}

#[test]
fn deref_then_assign() -> Result<()> {
    let pp = "int main(void) {
            int* my_ptr;
            *my_ptr = 42;
        }";

    let t_prog = compile_tacky_prog(pp)?;
    let t_prog = expect_tacky_implicit_return_instr(t_prog);
    match &t_prog.funs[0].instrs[..] {
        [t::Instruction::Store(_)] => noop!(),
        _ => fail!(),
    }

    Ok(())
}

#[test]
fn deref_then_addrof() -> Result<()> {
    let pp = "int main(void) {
            int* my_ptr;
            &*my_ptr;
        }";

    let t_prog = compile_tacky_prog(pp)?;
    let t_prog = expect_tacky_implicit_return_instr(t_prog);
    assert!(t_prog.funs[0].instrs.is_empty());
    /* The pointer-typed value can have any bit-sequence, even nullptr;
    regardles, the typechecker doesn't fail, and tacky doesn't produce any Load or GetAddress instructions. */

    Ok(())
}

#[test]
fn addrof() -> Result<()> {
    let pp = "int main(void) {
            int my_int;
            &my_int;
        }";

    let t_prog = compile_tacky_prog(pp)?;
    let t_prog = expect_tacky_implicit_return_instr(t_prog);
    match &t_prog.funs[0].instrs[..] {
        [t::Instruction::GetAddress(_)] => noop!(),
        _ => fail!(),
    }

    Ok(())
}

#[test]
fn addrof_then_deref() -> Result<()> {
    let pp = "int main(void) {
            int my_int;
            *&my_int;
        }";

    let t_prog = compile_tacky_prog(pp)?;
    let t_prog = expect_tacky_implicit_return_instr(t_prog);
    match &t_prog.funs[0].instrs[..] {
        [t::Instruction::GetAddress(_)] => noop!(),
        _ => fail!(),
    }

    Ok(())
}
