#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="${BUILD_DIR:-"$ROOT/build"}"
BUILD_TYPE="${CMAKE_BUILD_TYPE:-Debug}"

configure_args=(
  -S "$ROOT"
  -B "$BUILD_DIR"
  -DCMAKE_BUILD_TYPE="$BUILD_TYPE"
)

if [[ -n "${CMAKE_GENERATOR:-}" ]]; then
  configure_args+=(-G "$CMAKE_GENERATOR")
fi

if [[ -n "${LLVM_DIR:-}" ]]; then
  configure_args+=(-DLLVM_DIR="$LLVM_DIR")
elif [[ -f /ucrt64/lib/cmake/llvm/LLVMConfig.cmake ]]; then
  configure_args+=(-DLLVM_DIR=/ucrt64/lib/cmake/llvm)
elif command -v brew >/dev/null 2>&1; then
  BREW_LLVM_PREFIX="$(brew --prefix llvm 2>/dev/null || true)"
  if [[ -n "$BREW_LLVM_PREFIX" && -f "$BREW_LLVM_PREFIX/lib/cmake/llvm/LLVMConfig.cmake" ]]; then
    configure_args+=(-DLLVM_DIR="$BREW_LLVM_PREFIX/lib/cmake/llvm")
  fi
fi

if [[ -n "${CC:-}" ]]; then
  configure_args+=(-DCMAKE_C_COMPILER="$CC")
fi

if [[ -n "${CXX:-}" ]]; then
  configure_args+=(-DCMAKE_CXX_COMPILER="$CXX")
fi

if [[ -n "${CMAKE_MAKE_PROGRAM:-}" ]]; then
  configure_args+=(-DCMAKE_MAKE_PROGRAM="$CMAKE_MAKE_PROGRAM")
fi

cmake "${configure_args[@]}"
cmake --build "$BUILD_DIR" --config "$BUILD_TYPE"

if [[ "${1:-}" == "--run" ]]; then
  CSTAR="$BUILD_DIR/cstar"
  if [[ -x "$BUILD_DIR/$BUILD_TYPE/cstar" ]]; then
    CSTAR="$BUILD_DIR/$BUILD_TYPE/cstar"
  elif [[ -x "$BUILD_DIR/cstar.exe" ]]; then
    CSTAR="$BUILD_DIR/cstar.exe"
  elif [[ -x "$BUILD_DIR/$BUILD_TYPE/cstar.exe" ]]; then
    CSTAR="$BUILD_DIR/$BUILD_TYPE/cstar.exe"
  fi

  "$CSTAR" "$ROOT/examples/smoke/minimal.cstar"
fi
