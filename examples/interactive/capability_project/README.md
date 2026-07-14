# C* Capability Project

Interactive demo for current language features:

- `include ... as ...` module aliases
- CRT import through a module
- `std/core.cstar` print/read helpers
- public module functions and static module state
- `struct`, `constructor`, `destructor`, `drop`
- `trait` conformance
- value operator overload
- unique and shared ownership pointers
- reference parameters and pointer writes
- interactive `core_read_string()` / `core_read_i64()` / `core_print*()` through the terminal module

Run from the repository root:

```sh
./build/cstar examples/interactive/capability_project/main.cstar --run
```
