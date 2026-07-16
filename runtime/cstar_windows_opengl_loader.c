#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <stddef.h>
#include <stdint.h>

typedef unsigned int GLenum;
typedef unsigned int GLuint;
typedef int GLint;
typedef int GLsizei;
typedef ptrdiff_t GLsizeiptr;
typedef unsigned char GLboolean;
typedef char GLchar;

static void* cstar_gl_proc(const char* name) {
  static HMODULE opengl32_module = NULL;
  void* proc = (void*)wglGetProcAddress(name);
  if (proc == NULL || proc == (void*)0x1 || proc == (void*)0x2 ||
      proc == (void*)0x3 || proc == (void*)-1) {
    if (opengl32_module == NULL) {
      opengl32_module = LoadLibraryA("opengl32.dll");
    }
    proc = opengl32_module == NULL ? NULL
                                   : (void*)GetProcAddress(opengl32_module,
                                                           name);
  }
  return proc;
}

#define CSTAR_GL_PROC(type, name)                    \
  static type proc = NULL;                           \
  if (proc == NULL) {                                \
    proc = (type)cstar_gl_proc(name);                \
  }

GLuint APIENTRY glCreateShader(GLenum shader_type) {
  typedef GLuint(APIENTRY * Proc)(GLenum);
  CSTAR_GL_PROC(Proc, "glCreateShader");
  return proc == NULL ? 0 : proc(shader_type);
}

void APIENTRY glShaderSource(GLuint shader, GLsizei count,
                             const GLchar* const* string,
                             const GLint* length) {
  typedef void(APIENTRY * Proc)(GLuint, GLsizei, const GLchar* const*,
                                const GLint*);
  CSTAR_GL_PROC(Proc, "glShaderSource");
  if (proc != NULL) {
    proc(shader, count, string, length);
  }
}

void APIENTRY glCompileShader(GLuint shader) {
  typedef void(APIENTRY * Proc)(GLuint);
  CSTAR_GL_PROC(Proc, "glCompileShader");
  if (proc != NULL) {
    proc(shader);
  }
}

void APIENTRY glGetShaderiv(GLuint shader, GLenum pname, GLint* params) {
  typedef void(APIENTRY * Proc)(GLuint, GLenum, GLint*);
  CSTAR_GL_PROC(Proc, "glGetShaderiv");
  if (proc != NULL) {
    proc(shader, pname, params);
  }
}

void APIENTRY glGetShaderInfoLog(GLuint shader, GLsizei buf_size,
                                 GLsizei* length, GLchar* info_log) {
  typedef void(APIENTRY * Proc)(GLuint, GLsizei, GLsizei*, GLchar*);
  CSTAR_GL_PROC(Proc, "glGetShaderInfoLog");
  if (proc != NULL) {
    proc(shader, buf_size, length, info_log);
  }
}

GLuint APIENTRY glCreateProgram(void) {
  typedef GLuint(APIENTRY * Proc)(void);
  CSTAR_GL_PROC(Proc, "glCreateProgram");
  return proc == NULL ? 0 : proc();
}

void APIENTRY glAttachShader(GLuint program, GLuint shader) {
  typedef void(APIENTRY * Proc)(GLuint, GLuint);
  CSTAR_GL_PROC(Proc, "glAttachShader");
  if (proc != NULL) {
    proc(program, shader);
  }
}

void APIENTRY glBindAttribLocation(GLuint program, GLuint index,
                                   const GLchar* name) {
  typedef void(APIENTRY * Proc)(GLuint, GLuint, const GLchar*);
  CSTAR_GL_PROC(Proc, "glBindAttribLocation");
  if (proc != NULL) {
    proc(program, index, name);
  }
}

void APIENTRY glBindFragDataLocation(GLuint program, GLuint color_number,
                                     const GLchar* name) {
  typedef void(APIENTRY * Proc)(GLuint, GLuint, const GLchar*);
  CSTAR_GL_PROC(Proc, "glBindFragDataLocation");
  if (proc != NULL) {
    proc(program, color_number, name);
  }
}

void APIENTRY glLinkProgram(GLuint program) {
  typedef void(APIENTRY * Proc)(GLuint);
  CSTAR_GL_PROC(Proc, "glLinkProgram");
  if (proc != NULL) {
    proc(program);
  }
}

void APIENTRY glGetProgramiv(GLuint program, GLenum pname, GLint* params) {
  typedef void(APIENTRY * Proc)(GLuint, GLenum, GLint*);
  CSTAR_GL_PROC(Proc, "glGetProgramiv");
  if (proc != NULL) {
    proc(program, pname, params);
  }
}

void APIENTRY glGetProgramInfoLog(GLuint program, GLsizei buf_size,
                                  GLsizei* length, GLchar* info_log) {
  typedef void(APIENTRY * Proc)(GLuint, GLsizei, GLsizei*, GLchar*);
  CSTAR_GL_PROC(Proc, "glGetProgramInfoLog");
  if (proc != NULL) {
    proc(program, buf_size, length, info_log);
  }
}

void APIENTRY glUseProgram(GLuint program) {
  typedef void(APIENTRY * Proc)(GLuint);
  CSTAR_GL_PROC(Proc, "glUseProgram");
  if (proc != NULL) {
    proc(program);
  }
}

void APIENTRY glDeleteShader(GLuint shader) {
  typedef void(APIENTRY * Proc)(GLuint);
  CSTAR_GL_PROC(Proc, "glDeleteShader");
  if (proc != NULL) {
    proc(shader);
  }
}

void APIENTRY glDeleteProgram(GLuint program) {
  typedef void(APIENTRY * Proc)(GLuint);
  CSTAR_GL_PROC(Proc, "glDeleteProgram");
  if (proc != NULL) {
    proc(program);
  }
}

void APIENTRY glGenVertexArrays(GLsizei n, GLuint* arrays) {
  typedef void(APIENTRY * Proc)(GLsizei, GLuint*);
  CSTAR_GL_PROC(Proc, "glGenVertexArrays");
  if (proc != NULL) {
    proc(n, arrays);
  }
}

void APIENTRY glBindVertexArray(GLuint array) {
  typedef void(APIENTRY * Proc)(GLuint);
  CSTAR_GL_PROC(Proc, "glBindVertexArray");
  if (proc != NULL) {
    proc(array);
  }
}

void APIENTRY glGenBuffers(GLsizei n, GLuint* buffers) {
  typedef void(APIENTRY * Proc)(GLsizei, GLuint*);
  CSTAR_GL_PROC(Proc, "glGenBuffers");
  if (proc != NULL) {
    proc(n, buffers);
  }
}

void APIENTRY glBindBuffer(GLenum target, GLuint buffer) {
  typedef void(APIENTRY * Proc)(GLenum, GLuint);
  CSTAR_GL_PROC(Proc, "glBindBuffer");
  if (proc != NULL) {
    proc(target, buffer);
  }
}

void APIENTRY glBufferData(GLenum target, GLsizeiptr size, const void* data,
                           GLenum usage) {
  typedef void(APIENTRY * Proc)(GLenum, GLsizeiptr, const void*, GLenum);
  CSTAR_GL_PROC(Proc, "glBufferData");
  if (proc != NULL) {
    proc(target, size, data, usage);
  }
}

GLint APIENTRY glGetAttribLocation(GLuint program, const GLchar* name) {
  typedef GLint(APIENTRY * Proc)(GLuint, const GLchar*);
  CSTAR_GL_PROC(Proc, "glGetAttribLocation");
  return proc == NULL ? -1 : proc(program, name);
}

void APIENTRY glEnableVertexAttribArray(GLuint index) {
  typedef void(APIENTRY * Proc)(GLuint);
  CSTAR_GL_PROC(Proc, "glEnableVertexAttribArray");
  if (proc != NULL) {
    proc(index);
  }
}

void APIENTRY glVertexAttribPointer(GLuint index, GLint size, GLenum type,
                                    GLboolean normalized, GLsizei stride,
                                    const void* pointer) {
  typedef void(APIENTRY * Proc)(GLuint, GLint, GLenum, GLboolean, GLsizei,
                                const void*);
  CSTAR_GL_PROC(Proc, "glVertexAttribPointer");
  if (proc != NULL) {
    proc(index, size, type, normalized, stride, pointer);
  }
}

#endif
