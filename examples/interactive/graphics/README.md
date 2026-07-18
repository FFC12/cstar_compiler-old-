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

The example is organized as a small production-style C* program:

- `main.cstar`: native window/resource setup, shader loading, frame loop
- `modules/glfw.cstar`: GLFW C ABI declarations and platform constants
- `modules/opengl.cstar`: logical OpenGL import and typed GL constants
- `modules/shader.cstar`: shader resource struct, trait conformance, compile/link helpers
- `modules/snake_types.cstar`: domain constants, `GridPoint`, `SnakeGame`, `GameRead`
- `modules/snake_input.cstar`: hot-path keyboard input adapter
- `modules/snake_model.cstar`: deterministic game state transitions
- `modules/snake_render.cstar`: streaming quad renderer and HUD drawing

The program intentionally uses the language surface as a stress-style showcase:

- module aliases for platform, shader, stdlib, model, input and render layers
- `struct` lifecycle, destructor cleanup, public/default-private API boundaries
- value operators on `GridPoint` for collision logic
- `SnakeRenderer` stack/by-value lifecycle with explicit `ref renderer` borrowed calls
- `dynamic GameRead&` for a read-only HUD/render view
- fixed `T[N]` array parameters for size-safe, kopyasız model updates
- `T[]` span parameters for renderer read-only ranges
- caller-owned fixed buffers for shader source, snake body storage and streaming vertices

`modules/opengl.cstar` requests the platform OpenGL library with
`import from "OpenGL"` and `modules/glfw.cstar` requests GLFW with
`import from "glfw"`. The compiler maps those logical library names to target
linker arguments. If GLFW is not installed, C* compilation succeeds and the
native linker reports the missing development library.
