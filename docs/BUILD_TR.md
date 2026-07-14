# C* Build Notları

Bu proje CMake üzerinden build edilir. Amaç tek bir platforma veya tek bir Windows toolchain'ine kilitlenmemektir.

## Varsayılan Windows Akışı

```powershell
.\build.bat
```

Script önce CMake ve LLVM kurulumunu arar. Bu makinedeki gibi sadece MSYS2/UCRT64 LLVM bulunursa otomatik olarak şu yolu seçer:

- Generator: Ninja
- Compiler: `C:\msys64\ucrt64\bin\clang.exe`
- C++ compiler: `C:\msys64\ucrt64\bin\clang++.exe`
- Build klasörü: `build-ucrt`

Bu ayrım önemli: MSYS2 LLVM paketleri Visual Studio/MSVC linker'ı ile karıştırılmamalıdır.

MSYS2/UCRT64 build'inde `cstar.exe` yanına gerekli runtime DLL'ler otomatik kopyalanır:

- `libgcc_s_seh-1.dll`
- `libstdc++-6.dll`
- `libwinpthread-1.dll`

Bu sayede `build-ucrt\cstar.exe` Windows Explorer'dan çift tıklanınca da loader aşamasında kapanmaz. Argümansız çift tıklamada kullanım mesajı gösterilir ve pencere Enter bekler.

## Visual Studio / MSVC

Visual Studio ile build almak için MSVC uyumlu LLVM kurulumu gerekir. Tipik yol:

```powershell
$env:LLVM_DIR = "C:\Program Files\LLVM\lib\cmake\llvm"
.\build.bat
```

Bu akışta CMake varsayılan Visual Studio generator'ını kullanabilir ve çıktı genellikle `build\Debug\cstar.exe` altında olur.

Visual Studio config'i ile F5 kullanırken VSCode'da şu konfigürasyon seçilir:

```text
Debug cstar: Visual Studio
```

## MSYS2 / UCRT64

MSYS2 toolchain'i açıkça seçmek için:

```powershell
$env:CMAKE_GENERATOR = "Ninja"
$env:CMAKE_C_COMPILER = "C:\msys64\ucrt64\bin\clang.exe"
$env:CMAKE_CXX_COMPILER = "C:\msys64\ucrt64\bin\clang++.exe"
$env:CMAKE_MAKE_PROGRAM = "C:\msys64\ucrt64\bin\ninja.exe"
$env:LLVM_DIR = "C:\msys64\ucrt64\lib\cmake\llvm"
$env:BUILD_DIR = "$PWD\build-ucrt"
.\build.bat
```

Varsayılan F5 konfigürasyonu bunu hedefler:

```text
Debug cstar: Ninja/Clang
```

## Linux / macOS

```bash
LLVM_DIR=/path/to/llvm/lib/cmake/llvm ./build.sh
```

Gerekirse generator ve compiler değiştirilebilir:

```bash
CMAKE_GENERATOR=Ninja CC=clang CXX=clang++ ./build.sh
```

## Backend Clang

Compiler şu an LLVM IR üretip backend olarak Clang çağırır. Backend yolu CMake configure zamanında bulunur. Çalıştırma anında override etmek için:

```powershell
$env:CSTAR_CLANG = "C:\Path\To\clang.exe"
```

Target triple CMake configure sırasında `clang -dumpmachine` ile üretilir ve LLVM module'a yazılır.

## Emit Modları

```bash
./build/cstar file.cstar --emit=ir
./build/cstar file.cstar --emit=asm
./build/cstar file.cstar --emit=obj
./build/cstar file.cstar --emit=staticlib
./build/cstar file.cstar --emit=dynamiclib
./build/cstar file.cstar --emit=exe --run
```

Kısa alias'lar:

- `--emit-llvm`
- `--emit-asm`
- `--emit-staticlib`
- `--emit-dynamiclib`
- `--build-exe`

Static library üretimi object dosyasını `ar rcs` ile paketler. `CSTAR_AR` ortam değişkeni ile archive tool override edilebilir. Dynamic library üretimi backend clang `-shared` yolunu kullanır; macOS'ta `.dylib`, Linux'ta `.so`, Windows'ta `.dll` hedeflenir.

## Example Runner

Varsayılan smoke suite:

```powershell
.\tools\run_examples.ps1
```

Belirli suite çalıştırma için üç biçim de desteklenir:

```powershell
.\tools\run_examples.ps1 type_checker --expect-diagnostics
.\tools\run_examples.ps1 --suite type_checker --expect-diagnostics
.\tools\run_examples.ps1 -Suite type_checker -ExpectDiagnostics
.\tools\run_examples.bat --suite type_checker --expect-diagnostics
```

Tüm suite'ler:

```powershell
.\tools\run_examples.ps1 --all --expect-diagnostics
```

Diagnostic bekleyen örneklerde `// expected-code: CSTNNNN` etiketi varsa runner compiler çıktısında o hata kodunu da arar. Bu sayede type-checker suite'i yalnızca "hata verdi" durumunu değil, beklenen semantic sınıfını da korur.

Yardım:

```powershell
.\tools\run_examples.ps1 --help
```
