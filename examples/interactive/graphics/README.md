# C* OpenGL Snake

This interactive example is a real GLFW/OpenGL C* program. It opens a native
window, reads shader files through `std/fs.cstar`, compiles an OpenGL program,
streams a CPU-built vertex buffer every frame, and runs a Snake game fully in
C*.

macOS setup:

```sh
brew install glfw
./build/cstar examples/interactive/graphics/main.cstar --run
```

Controls:

- `WASD` or arrow keys: turn the snake
- `R` or `Space`: restart after game over
- `Esc`: quit

The example intentionally uses the language surface as a stress-style showcase:

- module aliases for GLFW/OpenGL/shader/std/fs/game/render layers
- `struct` lifecycle and pointer receiver calls
- `SnakeRenderer` stack/by-value lifecycle with borrowed render calls
- `dynamic GameRead&` for a read-only HUD/render view
- enums, `option`, value operators, writable array parameters and loops
- caller-owned fixed buffers for both shader source and render vertices

`modules/opengl.cstar` requests the platform OpenGL library with
`import from "OpenGL"` and `modules/glfw.cstar` requests GLFW with
`import from "glfw"`. The compiler maps those logical library names to target
linker arguments. If GLFW is not installed, C* compilation succeeds and the
native linker reports the missing development library.
