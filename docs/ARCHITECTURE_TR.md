# C* Derleyici Mimarisi

Bu belge mevcut kod tabanını hızlı okuyabilmek için hazırlanmıştır. Amaç, projeyi devam ettirirken "hangi dosya ne yapıyor?" sorusuna kısa yoldan cevap vermek.

## Ana Akış

```text
main.cpp
  -> cstar::driver::parseArgs()
  -> CStarLexer
  -> CStarParser
  -> CStarCodegen::build()
       -> pass0()  Symbol Analysis
       -> pass1()  Type Checking
       -> codegen() LLVM IR
       -> clang ile assembly/exe üretimi
       -> üretilen exe'yi çalıştırma
```

## Klasör Yapısı

```text
.
├── main.cpp
├── CMakeLists.txt
├── build.bat
├── build.sh
├── grammar.cfg
├── precedence_table
├── .vscode/
│   ├── launch.json
│   ├── tasks.json
│   └── extensions.json
├── docs/
│   ├── ARCHITECTURE_TR.md
│   └── TODO_TR.md
├── tools/
│   └── run_examples.ps1
├── examples/
│   ├── smoke/          # recursive smoke categories: core, arrays, pointers, structs, ...
│   ├── functions/
│   ├── type_checker/   # recursive diagnostic categories: core, casts, imports, traits, ...
│   ├── variables/
│   └── papers/
├── include/
│   ├── base.hpp
│   ├── driver/
│   ├── lexer/
│   ├── parser/
│   ├── ast/
│   ├── visitor/
│   └── codegen/
└── src/
    ├── driver/
    ├── parser/
    ├── visitor/
    └── codegen/
```

## Ana Bileşenler

### `main.cpp`

CLI girişidir. Banner basar, driver argümanlarını parse eder, kaynak dosyayı okur ve lexer/parser/codegen zincirini başlatır. Argüman ayrıştırma artık `main.cpp` içinde değildir.

Dosya yolu artık `std::filesystem::absolute` ile normalize ediliyor; eski POSIX `realpath` bağımlılığı kaldırıldı.

### Driver

Dosyalar:

```text
include/driver/driver.hpp
src/driver/driver.cpp
```

Görevleri:

- CLI kullanım metnini üretmek.
- `--emit`, `--run`, `--output-dir`, `--verbose`, `--stats` gibi seçenekleri `CStarCodegenOptions` yapısına çevirmek.
- `main.cpp`'i sadece compiler pipeline orkestrasyonuna odaklı tutmak.

### Lexer

Dosya:

```text
include/lexer/lexer.hpp
```

Görevleri:

- Source buffer üzerinden token stream üretmek.
- Keyword classification yapmak.
- Scalar, string literal, char literal ve yorumları okumak.

Not:

- Lexer halen header içinde.
- EOF sınır kontrolü kritik; daha önce out-of-bounds hataları vardı.

### Parser

Dosyalar:

```text
src/parser/parser.cpp
src/parser/variable.cpp
src/parser/function.cpp
src/parser/expr.cpp
src/parser/branch.cpp
src/parser/loop.cpp
```

Parser AST üretir. Hata durumunda ağırlıklı olarak `ParserError(...); exit(1);` modeli kullanıyor. Daha sonra error recovery ve diagnostic ayrımı temizlenmeli.

### AST

Dosyalar:

```text
include/ast/*.hpp
```

Temel model:

```cpp
class IAST {
  virtual SymbolInfo acceptBefore(Visitor& visitor) = 0;
  virtual ValuePtr accept(Visitor& visitor) = 0;
};
```

İki visitor yolu var:

- `acceptBefore`: symbol/type-check pass'leri.
- `accept`: LLVM codegen.

### Semantic / Previsit

Dosyalar:

```text
include/visitor/visitor.hpp
include/visitor/symbols.hpp
src/visitor/previsit.cpp
src/codegen/pass0.cpp
src/codegen/pass1.cpp
```

Önemli tablolar:

- `Visitor::GlobalSymbolTable`
- `Visitor::LocalSymbolTable`
- `SymbolInfo`
- `TypeCheckerInfo`

`pass0` sembolleri toplar, `pass1` type-check yapar.

### LLVM Codegen

Dosya:

```text
src/visitor/visitor.cpp
```

`visit(...)` fonksiyonları LLVM IR üretir.

Şu an çalışan önemli parçalar:

- Fonksiyon iskeleti üretimi.
- Basit scalar literal üretimi.
- Basit `ret` üretimi.
- Lokal primitive değişken üretimi.
- Basit arithmetic expression üretimi.
- Lokal scalar assignment üretimi.
- Dereference assignment, tek boyutlu array element assignment ve shortcut assignment.
- Primitive function call, forward declaration call, pointer parametre ve primitive reference parametre.
- Fonksiyon sonunda default return üretimi.

Eksik ana parçalar:

```cpp
visit(ParamAST&)
visit(TypeAST&)
visit(FixAST&)
```

Not: `Visitor::buildFunctionParamLayout(...)` fonksiyon parametrelerinin LLVM IR tipi, value tipi, isim ve reference bilgisini tek yerden çıkarır. `declareFunction(...)` ve `visit(FuncAST&)` aynı layout bilgisini kullanır; bu, referans parametrelerde imza/body uyuşmazlığını önlemek için bilinçli olarak tekleştirilmiştir.

### Codegen Orchestrator

Dosyalar:

```text
include/codegen/codegen.hpp
src/codegen/codegen.cpp
```

`CStarCodegen::build()` sırası:

1. Parser çalışır.
2. AST ownership alınır.
3. `pass0()` çalışır.
4. `pass1()` çalışır.
5. Semantic failure kontrol edilir.
6. LLVM module/codegen çalışır.
7. `.ll` yazılır.
8. `clang` ile seçilen `--emit` hedefine göre `.s`, object veya executable üretilir.
9. Sadece `--run` verilirse üretilen executable çalıştırılır.

## Build ve Çalıştırma

Windows için önerilen hızlı yol:

```powershell
.\build.bat
.\build.bat --run
```

MSYS/bash için:

```bash
./build.sh
./build.sh --run
```

Smoke örneklerini çalıştırmak için:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\tools\run_examples.ps1
```

Tüm `examples/` ağacını denemek için:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\tools\run_examples.ps1 -All
```

Not: `-All` şu an birçok eski/deneysel örnekte fail verebilir. Güvenilir kabul edilen set şimdilik `examples/smoke/`.

## VSCode F5

Eklenen dosyalar:

```text
.vscode/tasks.json
.vscode/launch.json
.vscode/extensions.json
```

F5 konfigürasyonu:

```text
Debug cstar: smoke minimal
```

Bu akış:

1. `Build cstar (UCRT64)` task'ini çalıştırır.
2. `build-ucrt/cstar.exe` dosyasını GDB ile açar.
3. Argüman olarak `examples/smoke/core/minimal.cstar` verir.

Gereken VSCode eklentisi:

```text
ms-vscode.cpptools
```

## Kurulu Araçlar

Bu makinede doğrulanan ortam:

```text
MSYS2 UCRT64
LLVM 22.1.8
Clang 22.1.8
GCC 16.1.0
CMake 4.4.0
Ninja 1.13.2
GDB 17.2
```

## Güncel Sağlık Durumu

Derleme başarılı.

Çalışan smoke:

```text
examples/smoke/core/minimal.cstar
examples/smoke/core/local_int.cstar
examples/smoke/core/binary_expr.cstar
examples/smoke/core/assignment.cstar
```

Smoke örnekleri şu hattı geçiyor:

```text
Lexical Analysis
Syntantic Analysis
Pass 0
Pass 1
Codegen
clang assembly
clang exe
generated exe run
```

Bilinen uyarı:

```text
warning: overriding the module target triple with x86_64-w64-windows-gnu
```

Bu uyarı LLVM module target triple/data layout açıkça set edilmediği için geliyor. Çalışmayı engellemiyor, ama LLVM 22 uyumluluk temizliğinde ele alınmalı.
