# C* OpenGL Pastel Triangle

This example opens a GLFW window on macOS, reads vertex/fragment shaders from
disk through `std/fs.cstar`, compiles a shader program, and draws a pastel
triangle with OpenGL.

macOS setup:

```sh
brew install glfw
./build/cstar examples/interactive/graphics/main.cstar --run
```

`modules/opengl.cstar` macOS OpenGL framework'ünü `import from "OpenGL"`
ile ister; compiler backend bunu target platforma uygun linker argümanına
çevirir. GLFW de cross-platform pencere/input yüzeyi için `import from "glfw"`
ile istenir; sistemde GLFW development library yoksa derleme C* aşamasını
geçer ve linker `library 'glfw' not found` mesajıyla durur.

The example intentionally uses C* modules, public stdlib APIs, traits,
attributes, caller-owned file buffers, explicit CRT/OpenGL imports, and shader
source files instead of embedding shader strings in the program.
