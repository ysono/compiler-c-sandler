# Functions

The stack:
```txt
    (larger memory address)
    |      ...
    | 24(%rbp) -- Incoming arg #7 (zero-indexed).
a   | 16(%rbp) -- Incoming arg #6 (zero-indexed).
b   |  8(%rbp) -- The instruction pointer to be restored to RIP.
c   |  0(%rbp) -- The previous stack frame's base address, to be restored to RBP.
    |      ... -+ Maybe incoming args; maybe local variables; maybe padding.
    |      ...  |
    |      ...  |
d   |   0x???0 /
    |   0x???8 \
    |   0x???0  |
    |      ...  |
e   |   0x???0 -+ For one function call at a time: maybe padding; maybe outgoing on-stack args.
    (smaller memory address)
```

Chronologically ordered events:
1. The previous function initializes args that are incoming to the current function.
    + RSP points to "a", and is guaranteed to be 16-byte aligned.
1. The `call` asm instruction pushes the to-be-restored instruction pointer.
    + RSP points to "b".
1. The current function's prologue:
    1. pushes the previous stack frame's RBP.
        + RBP and RSP are callee-saved registers.
        + Emitted by [`crate::stage5_asm_emit::emit::AsmCodeEmitter::write_fun()`].
        + RSP points to "c", and is guaranteed to be 16-byte aligned.
        + RBP points to "c".
    1. allocates the local stack frame.
        + The local variables/values' locations are translated from abstract locations into concrete stack positions by
            [`crate::stage4_asm_gen::phase2_finalize::var_to_stack_pos::VarToStackPos::resolve_stack_pos()`].
        + The stack frame's length is determined by
            [`crate::stage4_asm_gen::phase2_finalize::InstrsFinalizer::finalize_instrs()`].
        + RSP points to "d", and is guaranteed to be 16-byte aligned, possibly using padding.
    1. copies incoming args into the local stack frame.
        + Incoming args may be on registers and on stack at `16(%rbp)`, `24(%rbp)`, etc.
        + Emitted by [`crate::stage4_asm_gen::phase1_generate::InstrsGenerator::gen_fun_copy_incoming_args()`].
1. The current function operates on variables/values within its local stack frame.
    + Emitted by [`crate::stage4_asm_gen::phase1_generate::InstrsGenerator::gen_instructions()`].
1. Whenever the current function calls another function:
    1. The current function must save any caller-saved registers that the current function will need to read after the callee returns.
        But, whenever the current function's local logic receives data at registers, we always immediately copy them into the local stack frame.
        Eg we always copy incoming args.
        Eg we always copy incoming return values.
        Eg we always copy the output of `idiv` at AX or DX.
        Hence, we always omit saving caller-saved registers just before calling another function.
    1. The current function possibly pushes one 8-byte padding item onto the stack.
    1. The current function initializes outgoing args.
        + Each on-stack arg is 8-byte aligned.
        + RSP points to "e", and is guaranteed to be 16-byte aligned, possibly using the aforementioned padding.
    1. The `call` asm instruction is executed.
        + Just before each `call` instruction, the RSP is required to be 16-byte aligned.
    1. The child function returns.
        + RSP points to "e".
    1. The current function pops the stack, undoing the prior preparation for the function-call.
        + RSP points to "d".
    1. The current function copies the incoming return value into the local stack frame.
    + Emitted by [`crate::stage4_asm_gen::phase1_generate::InstrsGenerator::gen_funcall()`].
1. The current function returns.
    1. The current function initializes the outgoing return value.
        + Emitted by [`crate::stage4_asm_gen::phase1_generate::InstrsGenerator::gen_return()`].
    1. The current function's epilogue:
        1. points RSP to "c".
        1. pops the previous stack frame's RBP value, and restores this value into the RBP register.
            + RSP points to "b".
        1. executes the `ret` asm instruction, which pops the next RIP value and restores this value into the RIP register.
            + RSP points to "a".
        + Emitted by [`crate::stage5_asm_emit::emit::AsmCodeEmitter::write_instr()`]
            at the case that handles [`crate::stage4_asm_gen::asm_ast::Instruction::Ret`].
    + [`crate::stage3_tacky::generate::FunInstrsGenerator::tackify_fun_defn()`]
        ensures the presence of at least one instance of this return sequence, per function.
