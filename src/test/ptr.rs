use crate::{
    common::primitive::Const,
    stage3_tacky::tacky_ast as t,
    test::utils::{compile_tacky_prog, expect_tacky_implicit_return_instr, fail},
    utils::noop,
};
use anyhow::Result;

fn compile_tacky_instrs(pp: &'static str) -> Result<Vec<t::Instruction>> {
    let t_prog = compile_tacky_prog(pp)?;
    let mut t_prog = expect_tacky_implicit_return_instr(t_prog);
    let instrs = t_prog.funs.pop().unwrap().instrs;
    Ok(instrs)
}

#[test]
fn deref() -> Result<()> {
    let pp = "int main(void) {
            int* my_ptr;
            *my_ptr;
        }";
    let instrs = compile_tacky_instrs(pp)?;
    assert!(instrs.is_empty());
    Ok(())
}

#[test]
fn deref_then_get_value() -> Result<()> {
    let pp = "int main(void) {
            int* my_ptr;
            return *my_ptr;
        }";
    let instrs = compile_tacky_instrs(pp)?;
    match &instrs[..] {
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

    let instrs = compile_tacky_instrs(pp)?;
    match &instrs[..] {
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

    let instrs = compile_tacky_instrs(pp)?;
    assert!(instrs.is_empty());
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

    let instrs = compile_tacky_instrs(pp)?;
    match &instrs[..] {
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

    let instrs = compile_tacky_instrs(pp)?;
    match &instrs[..] {
        [t::Instruction::GetAddress(_)] => noop!(),
        _ => fail!(),
    }
    Ok(())
}

#[test]
fn ptr_plus_integer() -> Result<()> {
    let pp = "int main(void) {
            int* my_ptr;
            my_ptr + 7l;
        }";

    let instrs = compile_tacky_instrs(pp)?;
    match &instrs[..] {
        [t::Instruction::AddPtr(t::AddPtr { idx, scale, .. })] => {
            assert_eq!(idx, &t::Value::Constant(Const::Long(7)));
            assert_eq!(scale.as_int(), 4);
        }
        _ => fail!(),
    }
    Ok(())
}

#[test]
fn ptr_minus_integer() -> Result<()> {
    let pp = "int main(void) {
            int* my_ptr;
            my_ptr - 7l;
        }";

    let instrs = compile_tacky_instrs(pp)?;
    match &instrs[..] {
        [t::Instruction::Unary(t::Unary {
            op: t::UnaryOperator::Numeric(t::NumericUnaryOperator::Negate),
            ..
        }), t::Instruction::AddPtr(t::AddPtr { scale, .. })] => {
            assert_eq!(scale.as_int(), 4);
        }
        _ => fail!(),
    }
    Ok(())
}

#[test]
fn ptr_minus_ptr() -> Result<()> {
    {
        let pp = "int main(void) {
                int* my_ptr_1;
                int* my_ptr_2;
                my_ptr_2 - my_ptr_1;
            }";

        let instrs = compile_tacky_instrs(pp)?;
        assert_sub_then_div(&instrs, 4);
    }
    {
        let pp = "int main(void) {
                int** my_ptr_1;
                int** my_ptr_2;
                my_ptr_2 - my_ptr_1;
            }";

        let instrs = compile_tacky_instrs(pp)?;
        assert_sub_then_div(&instrs, 8);
    }
    {
        let pp = "int main(void) {
                int (*my_ptr_1)[17];
                int (*my_ptr_2)[17];
                my_ptr_2 - my_ptr_1;
            }";

        let instrs = compile_tacky_instrs(pp)?;
        assert_sub_then_div(&instrs, 4 * 17);
    }
    {
        let pp = "int main(void) {
                int my_arr_1[17];
                int my_arr_2[17];
                my_arr_2 - my_arr_1;
            }";

        let instrs = compile_tacky_instrs(pp)?;
        match &instrs[..] {
            [t::Instruction::GetAddress(_), t::Instruction::GetAddress(_), tail_instrs @ ..] => {
                assert_sub_then_div(tail_instrs, 4);
            }
            _ => fail!(),
        }
    }
    {
        let pp = "int main(void) {
                int my_arr_1[17][13];
                int my_arr_2[17][13];
                my_arr_2 - my_arr_1;
            }";

        let instrs = compile_tacky_instrs(pp)?;
        match &instrs[..] {
            [t::Instruction::GetAddress(_), t::Instruction::GetAddress(_), tail_instrs @ ..] => {
                assert_sub_then_div(tail_instrs, 4 * 13);
            }
            _ => fail!(),
        }
    }

    fn assert_sub_then_div(instrs: &[t::Instruction], expected_scale: u64) {
        match instrs {
            [t::Instruction::Binary(t::Binary {
                op: t::BinaryOperator::Arithmetic(t::ArithmeticBinaryOperator::Sub),
                ..
            }), t::Instruction::Binary(t::Binary {
                op: t::BinaryOperator::DivRem(t::DivRemBinaryOperator::Div),
                rhs,
                ..
            })] => {
                assert_eq!(rhs, &t::Value::Constant(Const::ULong(expected_scale)));
            }
            _ => fail!(),
        }
    }

    Ok(())
}

#[test]
fn subscript() -> Result<()> {
    let ptr_pps = [
        "int main(void) {
            int* my_ptr;
            my_ptr[13L];
        }",
        "int main(void) {
            int* my_ptr;
            13L[my_ptr];
        }",
    ];
    let ptr_to_ptr_pps = [
        "int main(void) {
            int** my_ptr;
            my_ptr[13L][3L];
        }",
        "int main(void) {
            int** my_ptr;
            13L[my_ptr][3L];
        }",
    ];
    let arr_pps = [
        "int main(void) {
            int my_arr[17L][7L];
            my_arr[13L][3L];
        }",
        "int main(void) {
            int my_arr[17L][7L];
            13L[my_arr][3L];
        }",
    ];
    let err_pps = [
        "int main(void) {
            int* my_ptr;
            my_ptr[13L][3L];
        }", // Cannot subscript integer expr and integer expr.
        "int main(void) {
            int 17L[my_arr][7L];
            my_arr[13L][3L];
        }", // Invalid decl.
        "int main(void) {
            int my_arr[17L][7L];
            13L[3L][my_arr];
        }", // Cannot subscript integer expr and integer expr.
    ];

    for pp in ptr_pps {
        let instrs = compile_tacky_instrs(pp)?;
        match &instrs[..] {
            [addptr] => {
                assert_addptr(addptr, 13, 4); // Scale is bytelen of `int`.
            }
            _ => fail!(),
        }
    }
    for pp in ptr_to_ptr_pps {
        let instrs = compile_tacky_instrs(pp)?;
        match &instrs[..] {
            [addptr_dim0, t::Instruction::Load(_), addptr_dim1] => {
                assert_addptr(addptr_dim0, 13, 8); // Scale is bytelen of `int*`.
                assert_addptr(addptr_dim1, 3, 4); // Scale is bytelen of `int`.
            }
            _ => fail!(),
        }
    }
    for pp in arr_pps {
        let instrs = compile_tacky_instrs(pp)?;
        match &instrs[..] {
            [t::Instruction::GetAddress(_), addptr_dim0, addptr_dim1] => {
                assert_addptr(addptr_dim0, 13, 4 * 7); // Scale is bytelen of `int[7]`.
                assert_addptr(addptr_dim1, 3, 4); // Scale is bytelen of `int`.
            }
            _ => fail!(),
        }
    }
    fn assert_addptr(instr: &t::Instruction, exp_idx: i64, exp_scale: u64) {
        match instr {
            t::Instruction::AddPtr(t::AddPtr { idx, scale, .. }) => {
                assert_eq!(idx, &t::Value::Constant(Const::Long(exp_idx)));
                assert_eq!(scale.as_int(), exp_scale);
            }
            _ => fail!(),
        }
    }

    for pp in err_pps {
        assert!(compile_tacky_instrs(pp).is_err());
    }

    Ok(())
}
