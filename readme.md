This project follows ["Writing a C Compiler" by Nora Sandler](https://nostarch.com/writing-c-compiler), in building a compiler for a subset of the C language.

![](./doc/stages.drawio.svg)

### Development cheatsheet:

```sh
cargo build && RUST_LOG=info ./target/debug/compiler_driver ./data/sample.c --<flag> >> ./data/debug.txt ; echo $?
cargo build && RUST_LOG=info ./target/debug/compiler_driver ./data/sample.c >> ./data/debug.txt ; echo $? ; ./data/sample ; echo $?
```

### Testing cheatsheet:

```sh
soln_dir='/path/to/this_repo'
cd "${soln_dir}"
cargo build --release

tester_dir='/path/to/writing-a-c-compiler-tests/'
tester_dir_mem='/dev/shm/writing-a-c-compiler-tests' # Use `df` to see which of your mounts are in-memory.
git clone https://github.com/nlsandler/writing-a-c-compiler-tests/ "${tester_dir}"
cd "${tester_dir}"
git worktree add "${tester_dir_mem}"
cd "${tester_dir_mem}"
export RUST_LOG=off
./test_compiler "${soln_dir}/target/release/compiler_driver" --chapter 1 --stage lex
```
