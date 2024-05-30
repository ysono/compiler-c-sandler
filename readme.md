```sh
cargo build && RUST_LOG=info ./target/debug/compiler_driver ./data/sample.c --<flag> >> ./data/debug.txt ; echo $?
cargo build && RUST_LOG=info ./target/debug/compiler_driver ./data/sample.c >> ./data/debug.txt ; echo $? ; ./data/sample ; echo $?

cd ../compiler-c-sandler-tester/
./test_compiler ../compiler-c-sandler/target/debug/compiler_driver --chapter 1 --stage lex
```
