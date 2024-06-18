This project follows ["Writing a C Compiler" by Nora Sandler](https://nostarch.com/writing-c-compiler), in building a compiler for a subset of the C language.

Development cheatsheet:

```sh
cargo build && RUST_LOG=info ./target/debug/compiler_driver ./data/sample.c --<flag> >> ./data/debug.txt ; echo $?
cargo build && RUST_LOG=info ./target/debug/compiler_driver ./data/sample.c >> ./data/debug.txt ; echo $? ; ./data/sample ; echo $?

cargo build --release
cd ../compiler-c-sandler-tester/
./test_compiler ../compiler-c-sandler/target/release/compiler_driver --chapter 1 --stage lex
```
