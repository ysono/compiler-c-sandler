This project follows ["Writing a C Compiler" by Nora Sandler](https://nostarch.com/writing-c-compiler), in building a compiler for a subset of the C language.

## Design

![](./doc/stages.drawio.svg)

This solution diverges from the book's pseudocode in the following notable ways.

The flow across stages:

+ The API between Typechecker -> Tacky stages is lean.
    + The Typechecker removes all definition-less declarations and all static object definitions from the C AST;
        hence retains solely function definitions, which contain runtime object definitions.
    + (Relevant mostly since ch10. Commit `88e8eb5`.)
+ Static objects' resolved definitions exist solely in the symtabs, not in any of the ASTs.
    1. The Typechecker resolves static object definitions using the FE symtab as the staging area; and
        does _not_ move or clone any symbol from the FE symtab to the C AST.
        (This logic is exactly as the book prescribes.)
    1. The Tacky generator
        does _not_ move or clone any symbol from the FE symtab to the Tacky AST.
    1. The Assembly generator translates the FE symtab into the BE symtab; and
        does _not_ move or clone any symbol from the BE symtab to the asm AST.
    + (Relevant since ch10. Commit `b54d6e0`.)
+ Static readonly string objects are defined solely during the Typechecker stage, not Tacky.
    + (Relevant since ch16. Commit `0430f66`.)
+ The API between Assembly Generator -> Assembly Emitter stages is more explicit about operands.
    + The Assembly Generator stage resolves static object operands as read-write or read-only.
        The Assembly Emission stage does not need to look up this distinction in the BE symtab.
    + (Relevant since ch13. Commit `7f290b2`.)

Implementation details on data structures & algorithms:

+ Lexer stage
    + The regexes in use are few and simple.
+ Parser stage
    + The temporary structure representing declarators is not a linked-list but a vector. (Relevant since ch14.)
    + The types are encoded in a trie containing non-redundant nodes. (Relevant since ch14.)
+ Identifier resolver stage
    + The dictionaries containing scoped identifiers simulate copy-on-write. (Relevant since ch5.)
+ Typechecker stage
    + The typechecked C AST encodes constraints on Expression nodes, by using Rust generics. (Relevant mostly since ch15.)
    + All C AST variants, across [Parser, Identifier resolver, Typechecker] stages, share Rust structs, distinguished by generics.
+ Tacky stage
    + Using Rust's type system, it is demonstrated that all Tacky Values are scalar-typed, even after the introduction of an aggregate type. (Relevant since ch15.)
+ Initializers are represented uniformly and compactly.
    + The Typechecker translates each initializer from the parsed recursive structure to a linear series of items, for static and runtime definitions alike, rather than just static.
    + The initializer representation compacts neighboring fill-in 0x00 bytes, including string initializers' trailing '\0' bytes.
    + The Tacky generator emits runtime initialization instructions to copy bytes at greedily wide intervals. It does so on 0x00 initializers, ignoring the object's type, as well as on string initializers.
    + (Relevant since ch15.)

## Development cheatsheet

```sh
cargo build && RUST_LOG=info ./target/debug/compiler_driver ./data/sample.c --<flag> >> ./data/debug.txt ; echo $?

cargo build && RUST_LOG=info ./target/debug/compiler_driver ./data/sample.c >> ./data/debug.txt ; echo $? ; ./data/sample ; echo $?
```

## Testing cheatsheet

```sh
tester_dir='/path/to/writing-a-c-compiler-tests'
tester_dir_mem='/dev/shm/writing-a-c-compiler-tests' # Use `df` to see which of your mounts are in-memory.
git clone https://github.com/nlsandler/writing-a-c-compiler-tests/ "${tester_dir}"
cd "${tester_dir}"
git worktree add "${tester_dir_mem}"

soln_dir='/path/to/this_repo'
cd "${soln_dir}"
cargo build --release
export RUST_LOG=off
"${tester_dir_mem}/test_compiler" ./target/release/compiler_driver --chapter 1 --stage lex
```

This solution has been tested on Linux and Intel Mac.
