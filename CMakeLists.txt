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
  execute_process(
    COMMAND "${CSTAR_BACKEND_CLANG}" -dumpmachine
    OUTPUT_VARIABLE CSTAR_TARGET_TRIPLE
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
  )
else()
  message(STATUS "Backend clang was not found at configure time; runtime will use CSTAR_CLANG or PATH")
  set(CSTAR_BACKEND_CLANG "")
endif()

if(CSTAR_TARGET_TRIPLE)
  message(STATUS "Using target triple: ${CSTAR_TARGET_TRIPLE}")
else()
  message(STATUS "Target triple was not detected; backend clang may choose a default")
  set(CSTAR_TARGET_TRIPLE "")
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
  foreach(CSTAR_RUNTIME_DLL IN ITEMS libgcc_s_seh-1.dll libstdc++-6.dll libwinpthread-1.dll)
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
endif()

llvm_map_components_to_libnames(llvm_libs support core irreader)
target_link_libraries(${PROJECT_NAME} ${llvm_libs})



