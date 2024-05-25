```sh
cargo build && ./target/debug/compiler_driver ./data/return_2.c

cd ../compiler-c-sandler-tester/
./test_compiler ../compiler-c-sandler/target/debug/compiler_driver --chapter 1 --stage lex
```
