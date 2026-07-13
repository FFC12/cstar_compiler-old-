@echo off
setlocal

set "ROOT=%~dp0."
set "BUILD_DIR_WAS_SET=1"
set "BUILD_DIR=%BUILD_DIR%"
if "%BUILD_DIR%"=="" (
  set "BUILD_DIR_WAS_SET="
  set "BUILD_DIR=%ROOT%\build"
)

set "CONFIG=%CMAKE_CONFIG%"
if "%CONFIG%"=="" set "CONFIG=Debug"

set "CMAKE_EXE=%CMAKE_EXE%"
if "%CMAKE_EXE%"=="" set "CMAKE_EXE=cmake"

where "%CMAKE_EXE%" >nul 2>nul
if errorlevel 1 (
  if exist "C:\msys64\ucrt64\bin\cmake.exe" (
    set "CMAKE_EXE=C:\msys64\ucrt64\bin\cmake.exe"
  )
)

set "USING_MSYS_LLVM="
if "%LLVM_DIR%"=="" (
  if exist "%ProgramFiles%\LLVM\lib\cmake\llvm\LLVMConfig.cmake" (
    set "LLVM_DIR=%ProgramFiles%\LLVM\lib\cmake\llvm"
  ) else if exist "%ProgramFiles(x86)%\LLVM\lib\cmake\llvm\LLVMConfig.cmake" (
    set "LLVM_DIR=%ProgramFiles(x86)%\LLVM\lib\cmake\llvm"
  ) else if exist "C:\msys64\ucrt64\lib\cmake\llvm\LLVMConfig.cmake" (
    set "LLVM_DIR=C:\msys64\ucrt64\lib\cmake\llvm"
    set "USING_MSYS_LLVM=1"
  )
)

if "%CMAKE_GENERATOR%"=="" (
  if "%USING_MSYS_LLVM%"=="1" (
    if exist "C:\msys64\ucrt64\bin\ninja.exe" set "CMAKE_GENERATOR=Ninja"
    if "%CMAKE_C_COMPILER%"=="" if exist "C:\msys64\ucrt64\bin\clang.exe" set "CMAKE_C_COMPILER=C:\msys64\ucrt64\bin\clang.exe"
    if "%CMAKE_CXX_COMPILER%"=="" if exist "C:\msys64\ucrt64\bin\clang++.exe" set "CMAKE_CXX_COMPILER=C:\msys64\ucrt64\bin\clang++.exe"
    if "%CMAKE_MAKE_PROGRAM%"=="" if exist "C:\msys64\ucrt64\bin\ninja.exe" set "CMAKE_MAKE_PROGRAM=C:\msys64\ucrt64\bin\ninja.exe"
    if "%BUILD_DIR_WAS_SET%"=="" set "BUILD_DIR=%ROOT%\build-ucrt"
  )
)

set "GENERATOR_ARGS="
if not "%CMAKE_GENERATOR%"=="" set "GENERATOR_ARGS=-G "%CMAKE_GENERATOR%""

set "TOOLCHAIN_ARGS="
if not "%CMAKE_C_COMPILER%"=="" set "TOOLCHAIN_ARGS=%TOOLCHAIN_ARGS% -DCMAKE_C_COMPILER=%CMAKE_C_COMPILER%"
if not "%CMAKE_CXX_COMPILER%"=="" set "TOOLCHAIN_ARGS=%TOOLCHAIN_ARGS% -DCMAKE_CXX_COMPILER=%CMAKE_CXX_COMPILER%"
if not "%CMAKE_MAKE_PROGRAM%"=="" set "TOOLCHAIN_ARGS=%TOOLCHAIN_ARGS% -DCMAKE_MAKE_PROGRAM=%CMAKE_MAKE_PROGRAM%"

set "LLVM_ARGS="
if not "%LLVM_DIR%"=="" set "LLVM_ARGS=-DLLVM_DIR=%LLVM_DIR%"

"%CMAKE_EXE%" -S "%ROOT%" -B "%BUILD_DIR%" %GENERATOR_ARGS% -DCMAKE_BUILD_TYPE=%CONFIG% %LLVM_ARGS% %TOOLCHAIN_ARGS%
if errorlevel 1 exit /b %errorlevel%

"%CMAKE_EXE%" --build "%BUILD_DIR%" --config "%CONFIG%"
if errorlevel 1 exit /b %errorlevel%

if "%~1"=="--run" (
  if exist "%BUILD_DIR%\%CONFIG%\cstar.exe" (
    "%BUILD_DIR%\%CONFIG%\cstar.exe" "%ROOT%\examples\smoke\minimal.cstar"
  ) else (
    "%BUILD_DIR%\cstar.exe" "%ROOT%\examples\smoke\minimal.cstar"
  )
  exit /b %errorlevel%
)
