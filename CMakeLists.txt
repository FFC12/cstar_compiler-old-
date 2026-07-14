cmake_minimum_required(VERSION 3.20)
project(cstar)

file(GLOB_RECURSE SOURCES CONFIGURE_DEPENDS "src/*.cpp")

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

find_program(CSTAR_BACKEND_CLANG
  NAMES clang clang.exe
  HINTS "${LLVM_TOOLS_BINARY_DIR}"
)

if(CSTAR_BACKEND_CLANG)
  message(STATUS "Using backend clang: ${CSTAR_BACKEND_CLANG}")
  set(CSTAR_TARGET_PROBE_SOURCE "${CMAKE_CURRENT_BINARY_DIR}/cstar_target_probe.c")
  file(WRITE "${CSTAR_TARGET_PROBE_SOURCE}" "int main(void) { return 0; }\n")
  execute_process(
    COMMAND "${CSTAR_BACKEND_CLANG}" "-###" -x c "${CSTAR_TARGET_PROBE_SOURCE}" -c
    ERROR_VARIABLE CSTAR_CLANG_DRIVER_TRACE
    OUTPUT_QUIET
    ERROR_STRIP_TRAILING_WHITESPACE
  )
  string(REGEX MATCH "\\\"-triple\\\"[ \t]+\\\"([^\\\"]+)\\\"" CSTAR_TRIPLE_MATCH "${CSTAR_CLANG_DRIVER_TRACE}")
  if(CSTAR_TRIPLE_MATCH)
    set(CSTAR_TARGET_TRIPLE "${CMAKE_MATCH_1}")
  endif()

  execute_process(
    COMMAND "${CSTAR_BACKEND_CLANG}" -dumpmachine
    OUTPUT_VARIABLE CSTAR_DUMPMACHINE_TRIPLE
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
  )
  if(NOT CSTAR_TARGET_TRIPLE)
    set(CSTAR_TARGET_TRIPLE "${CSTAR_DUMPMACHINE_TRIPLE}")
  endif()

  set(CSTAR_DATALAYOUT_ARGS -S -emit-llvm -x c "${CSTAR_TARGET_PROBE_SOURCE}" -o -)
  if(CSTAR_TARGET_TRIPLE)
    list(PREPEND CSTAR_DATALAYOUT_ARGS -target "${CSTAR_TARGET_TRIPLE}")
  endif()
  execute_process(
    COMMAND "${CSTAR_BACKEND_CLANG}" ${CSTAR_DATALAYOUT_ARGS}
    OUTPUT_VARIABLE CSTAR_TARGET_IR
    ERROR_QUIET
  )
  string(REGEX MATCH "target datalayout = \"([^\"]+)\"" CSTAR_DATALAYOUT_MATCH "${CSTAR_TARGET_IR}")
  if(CSTAR_DATALAYOUT_MATCH)
    set(CSTAR_TARGET_DATA_LAYOUT "${CMAKE_MATCH_1}")
  else()
    set(CSTAR_TARGET_DATA_LAYOUT "")
  endif()
else()
  message(STATUS "Backend clang was not found at configure time; runtime will use CSTAR_CLANG or PATH")
  set(CSTAR_BACKEND_CLANG "")
  set(CSTAR_TARGET_DATA_LAYOUT "")
endif()

if(CSTAR_TARGET_TRIPLE)
  message(STATUS "Using target triple: ${CSTAR_TARGET_TRIPLE}")
else()
  message(STATUS "Target triple was not detected; backend clang may choose a default")
  set(CSTAR_TARGET_TRIPLE "")
endif()

if(CSTAR_TARGET_DATA_LAYOUT)
  message(STATUS "Using target data layout: ${CSTAR_TARGET_DATA_LAYOUT}")
else()
  message(STATUS "Target data layout was not detected; LLVM will use its default")
endif()

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/include/cstar_config.hpp.in"
  "${CMAKE_CURRENT_BINARY_DIR}/generated/cstar_config.hpp"
  @ONLY
)

add_executable(${PROJECT_NAME} main.cpp ${SOURCES})

target_compile_features(${PROJECT_NAME} PRIVATE cxx_std_17)

target_include_directories(${PROJECT_NAME}
  PRIVATE
    include
    "${CMAKE_CURRENT_BINARY_DIR}/generated"
    ${LLVM_INCLUDE_DIRS}
)

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND "${LLVM_DEFINITIONS}")
target_compile_options(${PROJECT_NAME} PRIVATE ${LLVM_DEFINITIONS_LIST})

if(MSVC)
  target_compile_options(${PROJECT_NAME} PRIVATE /permissive- /Zc:__cplusplus)
endif()

if(WIN32 AND MINGW)
  get_filename_component(CSTAR_TOOLCHAIN_BIN_DIR "${CMAKE_CXX_COMPILER}" DIRECTORY)
  foreach(CSTAR_RUNTIME_DLL IN ITEMS
    libgcc_s_seh-1.dll
    libstdc++-6.dll
    libwinpthread-1.dll
    libffi-8.dll
    libxml2-16.dll
    zlib1.dll
    libzstd.dll
    libiconv-2.dll
  )
    unset(CSTAR_RUNTIME_DLL_PATH CACHE)
    find_file(CSTAR_RUNTIME_DLL_PATH
      NAMES ${CSTAR_RUNTIME_DLL}
      HINTS "${CSTAR_TOOLCHAIN_BIN_DIR}"
      NO_DEFAULT_PATH
    )
    if(CSTAR_RUNTIME_DLL_PATH)
      add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
          "${CSTAR_RUNTIME_DLL_PATH}"
          "$<TARGET_FILE_DIR:${PROJECT_NAME}>"
      )
    endif()
  endforeach()

  unset(CSTAR_LLVM_DLL_PATH CACHE)
  find_file(CSTAR_LLVM_DLL_PATH
    NAMES
      "libLLVM-${LLVM_VERSION_MAJOR}.dll"
      "LLVM-${LLVM_VERSION_MAJOR}.dll"
      libLLVM.dll
      LLVM.dll
    HINTS
      "${LLVM_TOOLS_BINARY_DIR}"
      "${CSTAR_TOOLCHAIN_BIN_DIR}"
    NO_DEFAULT_PATH
  )
  if(CSTAR_LLVM_DLL_PATH)
    add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "${CSTAR_LLVM_DLL_PATH}"
        "$<TARGET_FILE_DIR:${PROJECT_NAME}>"
    )
  endif()
endif()

if(TARGET LLVM)
  target_link_libraries(${PROJECT_NAME} PRIVATE LLVM)
else()
  llvm_map_components_to_libnames(llvm_libs support core irreader)
  list(REMOVE_DUPLICATES llvm_libs)
  target_link_libraries(${PROJECT_NAME} PRIVATE ${llvm_libs})
endif()
