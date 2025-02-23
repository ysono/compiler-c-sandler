This project follows ["Writing a C Compiler" by Nora Sandler](https://nostarch.com/writing-c-compiler), in building a compiler for a subset of the C language.

## Design

![](./doc/stages.drawio.svg)

The flow across stages diverges from the book's pseudocode in the following notable ways:

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
    + The Tacky stage adds symbols for local values only.
    + (Relevant since ch16. Commit `0430f66`.)

Implementation details on data structures & algorithms:

+ Lexer
    + The regexes in use are few and simple.
+ Frontend types
    + The parser's temporary structure representing declarators is not a linked-list but a vector. (Relevant since ch14.)
    + The frontend types are encoded in a trie containing non-redundant nodes.
        + When hashing a type or comparing two types, the algorithm evaluates immediate node(s) without recursing across nodes towards the base type(s).
        + (Relevant since ch14.)
+ Identifier resolver
    + The dictionaries containing scoped identifiers simulate copy-on-write. (Relevant since ch5.)
+ C AST
    + All C AST variants, the outputs of [Parser, Identifier resolver, Typechecker] stages, share Rust structs, distinguished by generics.
+ Expression type
    + The typechecked Expression nodes encode their types as constrained more specifically than "any object type". (Relevant mostly since ch15.)
    + The type constraints demonstrate that all Tacky Values are scalar-typed. (Relevant since ch15.)
+ Object initializer
    + The Typechecker translates each initializer from the parsed recursive structure to a linear series of items, for static and runtime definitions alike, rather than just static.
    + The initializer representation compacts neighboring fill-in 0x00 bytes, including string initializers' trailing '\0' bytes.
    + The Tacky generator emits runtime initialization instructions to copy bytes at greedily wide intervals.
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

cd '/path/to/this_solution_repo'
export RUST_LOG=off
cargo build --release && "${tester_dir_mem}/test_compiler" ./target/release/compiler_driver --chapter 1 --stage lex
```

This solution has been tested on Linux and Intel Mac.
