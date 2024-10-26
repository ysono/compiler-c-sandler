use crate::{
    common::{identifier::RawIdentifier, types_backend::ScalarAssemblyType},
    stage4_asm_gen::{asm_ast::*, FinalizedAsmAst},
    test::utils,
};
use anyhow::Result;
use owning_ref::OwningRef;
use std::{collections::HashMap, rc::Rc};

fn compile_into_asm_funs(
    pp: &'static str,
) -> Result<HashMap<OwningRef<Rc<RawIdentifier>, str>, Function<FinalizedAsmAst>>> {
    let (prog, _) = utils::compile_until_asm_gen(pp)?;

    let mut funs = utils::asm_prog_into_fun_by_ident(prog);

    for (_ident, fun) in funs.iter_mut() {
        utils::remove_asm_implicit_return_instr(fun);
    }

    Ok(funs)
}

#[test]
fn narrow_arg() -> Result<()> {
    let pp = "
        int accepts_int(int arg) {}
        int accepts_char(char arg) {
            accepts_int(arg);
        }
        int main(void) {
            char my_char;
            accepts_char(my_char);
        }
    ";

    let mut funs = compile_into_asm_funs(pp)?;

    let main_fun = funs.remove("main").unwrap();
    let main_instrs = main_fun.instrs.into_iter().collect::<Vec<_>>();
    assert!(matches!(
        &main_instrs[..],
        [
            Instruction::Binary {
                op: BinaryOperator::Sub,
                asm_type: ScalarAssemblyType::Quadword,
                tgt: Operand::Register(Register::SP),
                arg: _,
            }, // Allocate the local stack frame.
            /* (End of prologue; Beginning of fun-call.) */
            Instruction::Mov {
                asm_type: ScalarAssemblyType::Byte,
                src: Operand::Memory(Register::BP, _),
                dst: Operand::Register(Register::DI),
            }, // Initialize the outgoing arg at %dil.
            Instruction::Movsx {
                src_asm_type: ScalarAssemblyType::Byte,
                dst_asm_type: ScalarAssemblyType::Longword,
                src: Operand::Register(Register::DI),
                dst: Operand::Register(Register::DI),
            }, // Widen the outgoing arg at %dil into covering %edi.
            Instruction::Call(_),
            Instruction::Mov {
                src: Operand::Register(Register::AX),
                ..
            }, // Copy incoming return value.
        ]
    ));

    let accepts_char_fun = funs.remove("accepts_char").unwrap();
    let accepts_char_instrs = accepts_char_fun.instrs.into_iter().collect::<Vec<_>>();
    assert!(matches!(
        &accepts_char_instrs[..],
        [
            Instruction::Binary {
                op: BinaryOperator::Sub,
                asm_type: ScalarAssemblyType::Quadword,
                tgt: Operand::Register(Register::SP),
                arg: _,
            }, // Allocate the local stack frame.
            Instruction::Mov {
                asm_type: ScalarAssemblyType::Byte,
                src: Operand::Register(Register::DI),
                dst: Operand::Memory(Register::BP, _),
            }, // Copy the incoming arg to inside the stack frame.
            /* (End of prologue; Beginning of fun-call.) */
            Instruction::Movsx {
                src_asm_type: ScalarAssemblyType::Byte,
                dst_asm_type: ScalarAssemblyType::Longword,
                src: Operand::Memory(Register::BP, _),
                dst: Operand::Register(Register::R11),
            }, // Implicitly "cast as if by assignment" the outgoing arg. Instr 1 of 2.
            Instruction::Mov {
                asm_type: ScalarAssemblyType::Longword,
                src: Operand::Register(Register::R11),
                dst: Operand::Memory(Register::BP, _),
            }, // Implicitly "cast as if by assignment" the outgoing arg. Instr 2 of 2.
            Instruction::Mov {
                asm_type: ScalarAssemblyType::Longword,
                src: Operand::Memory(Register::BP, _),
                dst: Operand::Register(Register::DI),
            }, // Initialize the outgoing arg at %edi.
            Instruction::Call(_),
            Instruction::Mov {
                src: Operand::Register(Register::AX),
                ..
            }, // Copy incoming return value.
        ]
    ));

    Ok(())
}
