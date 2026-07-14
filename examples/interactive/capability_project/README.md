# C* Capability Project

Interactive demo for current language features:

- `include ... as ...` module aliases
- CRT import through a module
- public module functions and static module state
- `struct`, `constructor`, `destructor`, `drop`
- `trait` conformance
- value operator overload
- unique and shared ownership pointers
- reference parameters and pointer writes
- interactive `input_string()` / `input_int()` / `print(...)`

Run from the repository root:

```sh
./build/cstar examples/interactive/capability_project/main.cstar --run
```
