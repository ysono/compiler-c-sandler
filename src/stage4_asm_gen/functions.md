# Functions

The stack:
```txt
    (larger memory address)
    |      ...
    | 24(%rbp) -- Arg #7 (zero-indexed).
a   | 16(%rbp) -- Arg #6 (zero-indexed).
b   |  8(%rbp) -- The instruction pointer to be restored to RIP.
c   |  0(%rbp) -- The previous stack frame's base address, to be restored to RBP.
    |      ... -+ Incoming on-stack args; local variables; maybe padding.
    |      ...  |
    |      ...  |
d   |   0x???0 /
    |   0x???8 \
    |   0x???0  |
    |      ...  |
e   |   0x???0 -+ For one function call at a time: maybe padding; outgoing args.
    (smaller memory address)
```

1. The previous function pushes args incoming to the current function.
    + RSP points to "a", and is guaranteed to be 16-byte aligned.
1. The `call` asm instruction pushes the to-be-restored instruction pointer.
    + RSP points to "b".
1. The current function's prologue pushes the previous stack frame's RBP.
    + Emitted by [`crate::stage5_asm_emit::emit::AsmCodeEmitter::write_fun()`].
    + RSP points to "c", and is guaranteed to be 16-byte aligned.
    + RBP points to "c".
1. The current function's prologue allocates and initializes the current function's incoming arguments and local variables.
    1. Allocates and possibly pads the stack to be 16-byte aligned.
        Emitted by [`crate::stage4_asm_gen::phase2_finalize::InstrsFinalizer::finalize_instrs()`].
    1. Copies incoming on-stack arguments, from `16(%rbp)`, `24(%rbp)`, etc, into the current function's stack frame.
        (Presumably this can be optimized away in the future.)
        Emitted by [`crate::stage4_asm_gen::phase1_generate::InstrsGenerator::convert_fun()`].
    1. Uses local variables within the allocated stack frame.
        Emitted by [`crate::stage4_asm_gen::phase1_generate::InstrsGenerator::convert_fun()`],
        using all the various emitters in [`crate::stage4_asm_gen`], and
        converting abstract variables to on-stack positions at [`crate::stage4_asm_gen::phase2_finalize::InstrsFinalizer::var_to_stack_pos()`].
    + RSP points to "d", and is guaranteed to be 16-byte aligned.
1. Whenever the current function calls another function,
    1. The current function must save any caller-saved registers that the current function will need to read after the callee returns.
        But, all our usages elsewhere of these registers entail immediately returning them or saving them to a memory location.
        Eg we always copy function args. Eg we always copy outputs of operations to the memory.
        Hence, we always omit saving caller-saved registers before calling another function.
    1. The current function pushes outgoing args.
        + RSP points to "e", and is guaranteed to be 16-byte aligned, possibly using padding.
            + The args must be aligned to "e"; hence padding (if any) must be pushed before args (if any).
            + Each `pushq` asm instruction pushes one 8-byte item. Hence, while the stack grows from "d" to "e", RSP is guaranteed to be 8-byte aligned. Hence, we must pad zero-or-one unit of 8 bytes.
    1. The `call` asm instruction is executed.
        + Just before each `call` instruction, the RSP is required to be 16-byte aligned.
    1. The current function pops the stack, undoing the prior preparation for the function-call.
        + RSP points to "d".
    + Emitted by [`crate::stage4_asm_gen::phase1_generate::InstrsGenerator::gen_funcall_instrs()`].
1. The current function's epilogue restores the previous stack frame and instruction pointer.
    1. Pops the stack, s.t. RSP points to "c".
    1. Pops the previous stack frame's RBP.
    1. The `ret` asm instruction pops the next RIP.
    + Emitted by [`crate::stage5_asm_emit::emit::AsmCodeEmitter::write_instr()`] on [`Instruction::Ret`](crate::stage4_asm_gen::asm_ast::Instruction::Ret).
