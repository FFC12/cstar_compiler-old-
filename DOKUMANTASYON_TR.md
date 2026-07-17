# C* Eski Derleyici Projesi - Türkçe İnceleme ve Dil Dokümantasyonu

Bu belge, `FFC12/cstar_compiler-old-` deposunun kaynak kodu, örnekleri ve tasarım notları incelenerek hazırlanmıştır.

Önemli kapsam notu: Proje README'de de söylendiği gibi pre-alpha/dağınık eski bir uygulama. Bu yüzden aşağıda iki ayrı katman kullanıyorum:

- **Mevcut çekirdek:** Lexer/parser/semantic/codegen içinde fiilen uygulanmış ya da uygulanmaya başlanmış özellikler.
- **Tasarım niyeti:** `examples/papers/*.cstar`, `grammar.cfg`, TODO'lar ve yarım AST/codegen parçalarından anlaşılan ama mevcut derleyicide tamamlanmamış özellikler.

## 1. Proje Özeti

C* yeni bir programlama dili denemesi. Dilin çekirdeği C/C++ benzeri statik tipler, pointer/reference kavramları, fonksiyonlar, blok scope, ifade önceliği, basit kontrol akışı ve LLVM IR üretimi üzerine kurulmuş.

Projede öne çıkan fikirler:

- C benzeri primitive tipler: `int8`, `int16`, `int32`, `int64`, `int`, `uint8`, `uint16`, `uint32`, `uint64`, `uint128`, `uint`, `isize`, `usize`, `float32`, `float64`, `float`, `char`, `uchar`, `bool`, `void`.
- Pointer/reference modeli:
  - `*` shareable pointer gibi düşünülmüş.
  - `^` unique/ownership pointer fikri taşıyor.
  - `&`, `ref`, `deref` ile referans alma/dereference desteği var.
- Qualifier sistemi:
  - `const`
  - `constptr`
  - `constref`
  - `readonly`
- Fonksiyon dönüş tipi için `::` kullanımı:
  - `main() :: int32 { ... }`
  - dönüş tipi yoksa varsayılan `void`.
- `if / elif / else`
- `loop(...) { ... }` ile hem while benzeri hem iterable/range benzeri döngü fikri.
- `cast<T>(expr)` ve `unsafe_cast<T>(expr)` ayrımı.
- `move`, `typeof`, `sizeof` gibi unary operator fikirleri.
- `struct`, compile-time `trait`, attribute metadata MVP'si ve macro/directive MVP'si başlamıştır. `attribute` tanımı/annotation compiler pipeline'ında kabul edilir; expression/statement/item/type macro çağrıları parse öncesi token expansion ile çalışır; `#warning`/`#error` compile-time diagnostic üretir; temel `#if/#else` block selection çalışır. `protocol`, `dynamic protocol` gibi daha ileri fikirler tasarlanmış, fakat mevcut compiler bunları henüz gerçek lowering olarak derleyemiyor.

## 2. Depo Yapısı

Ana dosyalar:

- `main.cpp`: CLI girişi. Kaynak dosyayı okur, lexer/parser/codegen zincirini başlatır.
- `include/lexer/lexer.hpp`: Lexer tamamen header içinde.
- `include/parser/*.hpp`, `src/parser/*.cpp`: Parser ve expression parser.
- `include/ast/*.hpp`: AST düğümleri.
- `include/visitor/*.hpp`, `src/visitor/*.cpp`: Semantic pass ve LLVM codegen visitor'ları.
- `include/codegen/codegen.hpp`, `src/codegen/*.cpp`: Pass sıralaması ve LLVM modül üretimi.
- `examples/`: Dil örnekleri ve type-checker testleri.
- `examples/papers/`: Uygulanmış dil değil, daha çok tasarım belgesi/proposal.
- `grammar.cfg`: Eski/yarım gramer taslağı.
- `precedence_table`: Expression parser tasarım notları.

Derleme akışı:

1. Lexer token stream üretir.
2. Parser AST üretir.
3. `pass0`: sembol analizi, scope ve redefinition kontrolleri.
4. `pass1`: type checking.
5. Codegen: LLVM IR üretimi, `.ll`/`.s`/executable çıktıları ve istenirse generated program çalıştırma.

## 3. Build Durumu

Proje CMake ve LLVM gerektiriyor; artık eski sabit path'lere bağlı build akışı yerine daha taşınabilir bir düzen var.

Windows tarafında iki ana yol destekleniyor:

- MSYS2/UCRT64 LLVM bulunursa `build-ucrt` klasöründe Ninja + Clang ile build.
- Visual Studio/MSVC için ayrı `build` klasörü ve MSVC uyumlu LLVM kurulumu.

Pratik komutlar:

```powershell
.\build.bat
.\tools\run_examples.bat --suite smoke
.\tools\run_examples.bat --suite type_checker --expect-diagnostics
```

Linux/macOS tarafında `build.sh` aynı CMake akışını kullanır.

Backend Clang yolu CMake'den gelir; gerekirse `CSTAR_CLANG` ortam değişkeni ile override edilebilir. LLVM module target triple configure zamanında `clang -dumpmachine` ile set edilir. Generated `.ll`, `.s` ve executable çıktıları root'a değil varsayılan olarak `.cstar-out/` altına yazılır. Generated program'ın non-zero exit code'u artık compiler hatası sayılmaz; runner bunu ayrı raporlar.

VSCode F5/debug akışı için `.vscode` yapılandırmaları da eklendi.

### 3.1 Güncel doğrulama setleri

`examples` altındaki `.cstar` dosyalarının başında artık beklenti etiketi bulunur:

```cstar
// expected: pass
// expected-exit: 7
// expected: diagnostic
// expected-code: CST2100
// expected: diagnostic (proposal/stress)
```

`expected-exit`, generated executable'ın process exit status değeridir. Yani `ret 7;` console'a `7` yazdırmaz; programın exit code'unu `7` yapar. Terminalde doğrudan `.exe` çalıştırıldığında Windows bu değeri ekrana basmaz, PowerShell tarafında `$LASTEXITCODE` ile görülür. Smoke runner bu değeri otomatik yakalar ve `[OK] ... (exit N)` şeklinde doğrular.

Smoke ve type-checker suite'lerine ek olarak seed'li stress/fuzz runner da vardır:

```sh
python3 tools/stress_fuzz.py --cases 200 --mutations 80 --seed 12666
```

Bu runner geçerli/geçersiz C* programları üretir, `examples/smoke` ve `std` corpus'unu mutate eder ve her case'i compile-only modda dener. Rastgele input diagnostic üretebilir; bu normaldir. Bug sayılan durumlar compiler crash'i, assert/segfault, sanitizer/traceback çıktısı ve timeout/hang'dir. Reproducer dosyaları, loglar ve en yavaş case listesi `tests/stress/out/` altında tutulur. Test stratejisi `tests/stress/README_TR.md` içinde detaylıdır.

Performans baseline için C/C++ karşılaştırmalı runner vardır:

```sh
python3 tools/perf_benchmark.py --work 1000000 --compile-iters 5 --run-iters 15
```

Bu runner aynı workload'u C*, C ve C++ kaynak olarak üretir; compile wall time, executable size, runtime wall time ve return code ölçer. Varsayılan C/C++ profili `-O0`'dır; C* bugünkü backend hattıyla açık optimizer profili taşımadığı için bu debug baseline daha okunaklıdır. Release karşılaştırması için `--c-opt -O2 --cpp-opt -O2` kullanılabilir. Çıktılar `tests/performance/out/results.csv` ve `tests/performance/out/REPORT.md` altındadır; plan `tests/performance/README_TR.md` içinde tutulur.

Güncel küçük çalışan çekirdek `examples/smoke/` altındadır. Smoke dosyaları artık tek klasörde yığılmaz; konuya göre `core`, `casts`, `arrays`, `control_flow`, `functions`, `imports`, `pointers`, `ownership`, `runtime`, `enums` ve `structs` alt klasörlerine ayrılır. `examples/smoke/modules/` yalnızca include helper dosyaları içindir ve runner tarafından bilinçli skip edilir. Bu set şu anda runner'ın skip ettiği module helper dosyaları hariç 134/134 başarılıdır; toplam 137 smoke dosyasının 3 tanesi bilinçli skip edilir:

- minimal program ve `ret expr`
- `ret;` kullanan void fonksiyon çağrısı
- local primitive variable declaration
- global mutable primitive variable
- uninitialized local primitive zero-init
- `char`, `float32`, `float64` primitive smoke'ları
- fractional float literal: `0.5`, `4.5`
- bool literal
- scalar enum MVP: `enum Color : uint8 { Red, Green, Blue = 7 }`, `Color.Green`
- flags enum MVP: `flags enum FileMode : uint32 { Read = 1, Write = 2 }`, bitwise `|`, `&`, `^`, unary `~`
- integer ve floating point arithmetic
- comparison expression: `<`, `<=`, `>`, `>=`, `==`, `!=`
- logical expression: `&&`, `||`
- prefix/postfix increment-decrement statement: `++x;`, `x++;`, `--x;`, `x--;`
- scalar assignment
- `const` scalar read ve `const int32*` target/value ayrımı
- assignment type cast ve data-loss warning
- explicit `cast<T>(expr)` numerik dönüşüm
- `expr as T` safe cast syntax'ı
- explicit cast edilmiş function argument
- parametre seviyesinde implicit cast izni ayrımı:
  - `bool y` cast edilebilir primitive parametredir.
  - `x bool` aynı primitive tipi taşır fakat sembolün scope içinde implicit cast edilmesini yasaklar.
- `unsafe_cast<T>(expr)` ile integer -> pointer MVP
- tek boyutlu array element read/write: `arr[0]`, `arr[0] = value`, `arr[0] += value`
- negatif array index normalizasyonu: `arr[-1]` son eleman, `arr[-2]` sondan ikinci eleman olur; sabit taşmalar diagnostic üretir, dinamik negatif indexler codegen'de boyut ile normalize edilir
- tek elemanlı local/global array initializer: `int32 values[1] = (7);`
- tek boyutlu array parametre read/write: `read_second(int32[2] arr)`, `write_first(int32[2] arr)`
- çok boyutlu array read/write/shortcut assignment: `int32 matrix[2:3] = ((1, 2, 3), (4, 5, 6))`, `matrix[1:2]`, `matrix[row:col]`, `matrix[-1:-1]`
- çok boyutlu array parametre read/write: `read_mid(int32[2:3] matrix)`, `write_mid(int32[2:3] matrix)`
- newline continuation: array initializer, binary expression, function call ve multidim subscript satır bölünce bozulmaz
- function call, forward function call, call statement
- symbol argümanlı function call
- tek seviyeli pointer argümanlı function call: `read_ptr(ref x)` ve callee içinde `deref p`
- primitive reference parametre: `write_ref(ref value)`
- primitive `constref` parametre: `read_ref(ref value)` ve callee içinde salt-okunur değer okuma
- primitive `constptr` parametre: callee içinde pointer adresi sabit, target write serbest
- primitive `readonly` parametre: callee içinde pointer adresi ve target salt-okunur
- tek seviyeli pointer variable initializer: `int32* p = ref x`
- shared pointer atomic strong-count: `int32* q = p`, `q = p`, `q := p`, `strong_count(q)`, by-value function arg retain
- unique pointer move-only semantik: `int32^ p = ref x`, `int32^ q := p`, `q := p`, `take(move p)`, `ret move p`, `nomove int32^ p`, `deref q = value`
- `constptr` pointer initializer ve dereference read/write: `constptr int32* p = ref x`, `deref p = value`
- `readonly` pointer initializer ve dereference read: `readonly int32* p = ref x`, `ret deref p`
- pointer return: `identity(int32* p) :: int32*`
- dereference assignment: `deref p = value`, `*p = value`
- dereference shortcut assignment: `deref p += value`
- çok seviyeli dereference read/write: `**pp`, `**pp = value`
- pointer'dan pointer okuma: `int32* q = deref pp`
- geçici `print(...)` builtin
- geçici `input_int()` builtin
- geçici `input_string()` builtin
- string literal / `const char*` ABI: literal doğrudan `const char*` parametre/değişkene ve `readonly char*` değişkene geçebilir
- geçici `clear_screen()`, `flush_output()`, `sleep_ms(ms)`, `enable_raw_input()`, `disable_raw_input()` ve `read_key()` konsol builtin'leri
- native import/export:
  - `import abs(int32) :: int32;`
  - `import abs(int32 value) :: int32;`
  - `import abs(int32 value) :: int32 from "std:crt";`
  - `import { ... }`
  - `import from "std:crt" { ... }`
  - `export from "std:math" { ... }`
- local module include, `public` visibility ve alias function lookup: `include "modules/math_module.cstar" as math`, `math.add_from_module(2, 5)`
- public static module state/function smoke: `examples/smoke/imports/include_module_public_static.cstar`
- `if`, `if/else`, `if/elif/else`, nested if ve branch fallthrough
- `int`, `float` ve pointer condition conversion
- while-style `loop (condition)`, nested `if` içinde loop ve pointer condition loop
- `break` / `continue` statement'ları ve loop dışı kullanım diagnostic'i
- range loop: `loop(i in [min, max])`
- array iterable loop: `loop(value in arr)` ve `loop(index, value in arr)`
- struct MVP:
  - `struct Name { field; ... }`
  - zero-init local/global struct storage
  - `value.field` read
  - `value.field = expr` ve `value.field += expr`
  - by-value struct function parametre/return
  - nested by-value struct field ve chained field access: `line.start.x`
  - struct method ve `self` MVP: `value.method(args)`, implicit `self&`, `self.field` read/write
  - parantezsiz no-param method tanımı: `read :: int32 { ... }`
  - constructor initializer MVP: `constructor(args) { ... }`, `Point p = Point(args)`
  - unique/shared struct pointer receiver MVP: `owned.field`, `shared.field`, `owned.method(args)`, `shared.method(args)`
  - static struct method MVP: `static name :: int32 { ... }`, `Type::name()`
  - `new` allocation operator MVP: `new Type(args)`, `shared new Type(args)`, `new(allocator) Type(args)`
  - lifecycle MVP: `destructor() { ... }`, `drop value;`, by-value local struct scope-exit destructor lowering
  - trait MVP: `trait Name { ... }`, `struct T with Trait`, compile-time conformance check
  - value operator MVP: struct içinde `operator +(T rhs) :: T` benzeri methodlar

`examples/type_checker/` seti kontrollü diagnostic üretir; crash/assert beklenmez. Bu set de `arrays`, `casts`, `control_flow`, `core`, `enums`, `functions`, `imports`, `ownership`, `pointers`, `proposals`, `runtime`, `structs` ve `traits` alt klasörlerine ayrılır. `examples/type_checker/modules/` yalnızca include helper dosyası taşır ve runner tarafından skip edilir. `// expected-code: CSTNNNN` etiketi varsa runner beklenen diagnostic kodunu da doğrular. Güncel suite 117 dosyada 113 controlled diagnostic, 2 positive/pass ve 2 module helper skip ile geçer. Yeni negatif çekirdek testleri `const`/`readonly` assignment reddini, safe cast pointer/value kategori reddini, safe cast qualifier stripping reddini, user-defined cast controlled diagnostic'ini, çıplak value ile reference parametre çağrısı reddini, `constref` parametreye assignment reddini, `constptr` parametre/pointer adresi reassignment reddini, `readonly` parametre/pointer address/value assignment reddini, string literal'ın mutable `char*`, hedefi mutable bırakan `constptr char*` ve ownership iddiası taşıyan `char^` bağlamlarına implicit geçişinin reddini, array parametreye scalar/farklı boyutlu array geçişi reddini, sabit array index out-of-bounds reddini, array initializer arity reddini, runtime-sized declaration/parameter array dimension reddini, local stack array büyüklük guard'ını, private module type alias ve private module field erişimi reddini, instance olmadan `StructType.field` erişimi reddini, instance method'un `StructType.method()` şeklinde çağrılamamasını, `const int32*` target assignment reddini, çok seviyeli qualifier pointer reddini, invalid qualifier/type kombinasyonunu, `*`/`^` pointer marker karışımını reddini, unique pointer copy reddini, primitive `:=` reddini, function arg/return ownership transfer ihlallerini, `nomove` ownership-flow ihlallerini, `async`/`await` proposal diagnostic'ini, moved-after-use reddini, dropped-after-use reddini, direct destructor call reddini, `.=` protocol proposal diagnostic'ini, loop dışı `break`/`continue` reddini, literal `option` pattern diagnostic'ini, enum `option` exhaustiveness/type mismatch/duplicate branch diagnostic'lerini, include edilen module içindeki private function erişimi reddini, `static` function içinden non-static global/function erişimi reddini, struct duplicate/unknown field diagnostic'lerini, direct self-by-value struct field reddini, unknown struct method reddini, constructor olmayan type için constructor initializer reddini, instance method'un `::` ile çağrılamamasını, non-static/by-value `new` method formlarının reddini, user-defined lifecycle operator reddini, local/static data member reddini, eksik trait conformance reddini, allocator olmayan değerle `new(allocator)` kullanımını, unknown enum member reddini, enum type mismatch reddini, scalar enum bitwise/unary `~` reddini, flags enum power-of-two/explicit value zorunluluğunu, enum repr overflow diagnostic'ini, duplicate enum value diagnostic'ini ve `void` fonksiyonda değer döndürme reddini kapsar.

`examples/functions/`, `examples/variables/` ve `examples/papers/` dizinleri hâlâ daha çok proposal/stres örnekleridir. Runner ile ayrı çalıştırılır; amaç hepsini bugün yeşil yapmak değil, dil geliştikçe buradan küçük MVP smoke'lar çıkarmaktır. `examples/interactive/` ise input, terminal kontrolü, raw input, frame render ve ownership stresini daha büyük programlarla dener.

## 4. Lexer: Tanınan Token ve Anahtar Kelimeler

Lexer `include/lexer/lexer.hpp` içinde. Tek dosyada token enum'u, keyword classification ve tokenization bulunuyor.

### 4.1 Fiilen tanınan keyword'ler

`classifyIdents` içinde keyword'e dönüştürülen identifier'lar:

```text
ret, in, as,
if, elif, else,
ref, deref,
include, involved, option, loop, default,
from, import, export, public, private, static,
cast, unsafe_cast, sizeof, typeof, move, drop, async, await,
dynamic, protocol, state,
with, struct, trait, macro, constructor, destructor, allocator,
except, throw, defer, self, is,
const, constptr, constref, readonly, nomove,
int8, int16, int32, int64, int,
uint8, uint16, uint32, uint64, uint128, uint,
isize, usize,
float, float32, float64,
uchar, char, bool,
vec2, vec3, vec4,
void, any,
attribute, prototype, enum, flags,
for, break, continue,
nil, true, false
```

Lexer enum'unda `NATIVE`, `MATRIX`, `VECTOR`, `EXTERN` gibi token'lar da var; ancak bunların keyword classification tarafında tam karşılığı yok ya da parser tarafından kullanılmıyor.

Yeni proposal/lifecycle keyword'leri (`protocol`, `dynamic`, `state`, `struct`, `trait`, `attribute`, `macro`, `with`, `constructor`, `destructor`, `drop`, `new`, `shared`, `operator`, `allocator`, `except`, `throw`, `defer`, `self`, `is`, `dyn`) lexer tarafından tanınır. Parser `struct Name { field; ... }`, `trait Name { ... }`, `struct T with Trait`, `enum Name : repr { Member }`, `flags enum Name : repr { Member = bit }`, `new Type(args)`, `shared new Type(args)`, `drop value;`, `attribute Name for struct { ... }`, `@Name(args)`, `macro name($arg: kind) -> kind { ... }` ve `#directive ...` yüzeylerini gerçek grammar olarak işler. Macro expansion, `#warning/#error` compile-time diagnostic ve temel `#if/#else` evaluation MVP'si çalışır; richer `#if` expression grammar, source-map/hygiene ve diğer ileri proposal keyword'leri controlled proposal diagnostic veya explicit unsupported diagnostic üretir. Eski `policy` kelimesi yeni canonical grammar'da reserved değildir.

### 4.2 Literal ve scalar desteği

Desteklenen atomlar:

- Integer scalar: `10`, `123`
- Float scalar: `12.34`
- String literal: `"Hello"`
- Char/letter literal: `'A'`
- Boolean: `true`, `false`

Dikkat:

- Hex literal niyeti örnek notlarda görünse de (`0xFF`), lexer bunu gerçek hex sayı olarak parse etmiyor.
- Sayı parser'ı temel olarak digit ve `.` üzerinden çalışıyor.

### 4.3 Yorumlar

Destekleniyor:

```c
// tek satır yorum
/* çok satır yorum */
```

Lexer yorum token'larını üretse de `perform()` içinde yorumları token stream'e eklemiyor.

### 4.4 Operator ve noktalama token'ları

Lexer düzeyinde tanınanlar:

```text
.  ..  ...  .=
,  ;  :  ::  :=
$  #  _  ?  =>  ->
+  +=  ++
-  -=  --
*  *=
/  /=
%  %=
<< <<= >> >>=
=  ==
!  !=
~  ~=
&& & &=
|| | |=
^  ^=
< <= > >=
{ } ( ) [ ]
```

`:=` token olarak `TYPEINF`, `.=` token olarak `POLICY_ASSIGN` şeklinde tanınıyor. C* tasarımında `:=` type inference değildir; unique ownership transfer intent'i için kullanılır. `.=` dynamic protocol/provability-gap assignment proposal'ıdır ve protocol runtime lowering netleşene kadar kontrollü parser diagnostic üretir.

## 5. Tip Sistemi

### 5.1 Primitive tipler

Parser'ın type olarak kabul ettiği primitive tipler:

```text
void
int8 int16 int32 int64 int
uint8 uint16 uint32 uint64 uint128 uint
isize usize
float32 float64 float
uchar char bool
vec2 vec3 vec4
```

`any` lexer keyword'ü var ama parser `isType()` içinde `any` kabul etmiyor. Yani mevcut compiler çekirdeğinde `any` tamamlanmış değil.

`vec2`, `vec3`, `vec4` parser düzeyinde type olarak kabul ediliyor; semantic/codegen tarafında ise birçok yerde `Not implemented yet!` durumunda.

### 5.2 Mimariye bağlı alias'lar

Semantic/codegen tarafındaki niyet:

- `int`: x64'te 64 bit, aksi halde 32 bit.
- `uint`: x64'te 128 bit, aksi halde 64 bit.
- `isize`: x64'te 64 bit, aksi halde 32 bit.
- `usize`: x64'te 128 bit, aksi halde 64 bit.
- `float`: x64'te `float64`, aksi halde `float32`.

Bu kararlar ilginç: `uint` ve `usize` için x64'te 128 bit seçimi alışılmış dillerden farklı.

### 5.3 User-defined type

Parser `IDENT` ile başlayan type isimlerini `SPEC_DEFINED` olarak temsil edebiliyor:

```cstar
Shape^ shape = Triangle();
Triangle^ triangle = cast<Triangle^>(shape);
```

Mevcut semantic pass'te `struct Name { field; ... }` MVP'si için user-defined type table doldurulur. Primitive ve by-value user-defined field layout LLVM `StructType` olarak üretilir, zero-init struct variable oluşturulur, `value.field` read/write syntax'ı çalışır, nested field chain `line.start.x` GEP zinciriyle iner ve by-value struct parametre/return desteklenir. Struct method MVP'sinde methodlar internal olarak `StructName.method(self&, ...)` fonksiyonuna iner; `value.method(args)` receiver'ı implicit `self` argümanı yapar ve `self.field` read/write referans üzerinden caller storage'ına iner. `Point^ owned = ref p; owned.x` ve `Counter* sharedValue = ref c; sharedValue.value` formları pointee struct alanına auto-deref edilir; `owned.method(args)` ve `sharedValue.method(args)` çağrıları implicit `self&` için pointee adresini geçirir. `static` struct method self almaz ve `Type::method(args)` ile çağrılır; `::` instance receiver için kullanılmaz. Constructor MVP'sinde `constructor(args) { ... }` internal `StructName.constructor(self&, ...)` fonksiyonuna iner ve `StructName value = StructName(args);` local by-value initializer storage'ı zero-init edip constructor'ı çağırır. Constructor/destructor explicit return type kabul etmez; ikisi de lifecycle olarak `void` sayılır ve gövde sonunda `ret;` yazmak zorunlu değildir. Destructor canonical olarak `destructor() { ... }` şeklindedir ve user parametre alamaz. Value operator MVP'sinde `operator +`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `<=`, `>` ve `>=` struct method ABI'siyle çalışır; lifecycle/allocation operator'ları compiler-reserved kalır. `new` static method değildir; `new Type(args)` unique heap storage, `shared new Type(args)` shared handle/control-count üretir, constructor'ı çağırır ve drop/scope-exit release hattına bağlanır. Tasarım yönü olarak `new` yalnız struct/resource type'lara hapsedilmez; `new int32(7)` gibi sized primitive storage allocation da C* vizyonuna uygundur, fakat mevcut MVP'nin primitive `new` yüzeyi ayrıca uygulanmalıdır. `new(allocator) Type(args)` için allocator değerinin `#[lang(allocator)]` ile işaretlenmiş canonical allocator trait'ine conform etmesi beklenir; trait adı tek başına özel değildir. Normal kullanıcı çağrısı `value.destructor()` reddedilir; erken release `drop value;` ile yapılır ve by-value local struct'lar scope/return çıkışında otomatik destructor çağırır. Direct self-by-value field reddedilir. Trait MVP'si `trait Name { requirement(...) :: Type; }` ve `struct T with Trait` compile-time conformance check'i yapar. `protocol`, `dynamic protocol` ve `dynamic Trait` runtime/flow lowering'i Aşama 8+ ileri proposal olarak kalır ve bugün controlled diagnostic üretir.

Scalar enum ve flags enum MVP'si user-defined type table'a girer fakat struct layout üretmez. `enum Color : uint8 { Red, Green, Blue = 7 }` formunda explicit repr zorunludur; `Color.Green` member lookup'u enum type'ı olarak type-check edilir ve codegen'de underlying integer constant'a iner. Enum local variable, parametre ve return storage'ı repr type'ıdır. Unknown member, farklı enum type ataması, repr overflow ve duplicate enum value diagnostic üretir. `flags enum FileMode : uint32 { Read = 1, Write = 2 }` bitmask yüzeyi olarak çalışır; flags member değerleri explicit ve `0`/power-of-two olmak zorundadır. Scalar enum bitwise operatörleri ve unary `~` kullanımını reddeder. Flags enum `|`, `&`, `^`, unary `~` ve shortcut assignment formlarını kabul eder. `~` repr-width complement üretir; örneğin `uint32` flags enum'da 32-bit storage'ın tüm bitleri terslenir. Declared flag set'e geri sıkıştırmak isteyen kod `& KnownMask` kullanmalıdır. `option(enum_value)` statement MVP'si `Enum.Member` pattern'leri ve `_` default ile çalışır; `_` yoksa bütün enum üyeleri exhaustive olmak zorundadır. Tagged payload layout ve tagged payload destructuring hâlâ Aşama 8 proposal/hardening işidir.

## 6. Pointer, Reference ve Ownership Modeli

C*'ın en karakteristik tarafı pointer/reference tarafı.

### 6.1 Pointer işaretleri

```cstar
int* p;
int** pp;
int^ uniquePtr;
int^^^^ deepUnique;
```

- `*`: shareable pointer. Codegen'de raw pointer ABI değildir; compiler-owned shared handle `{ data: ptr, strong: i64* }` olarak taşınır. Copy/assignment atomic strong-count artırır, overwrite release eder, `:=`/`move` ownership transfer yapar.
- `^`: unique/ownership pointer. Doğrudan copy reddedilir; ownership transfer için `:=` veya `move` gerekir. By-value `^` function parametre ve return da aynı kuralı izler: `take(move p)` ve `ret move p;` açık transferdir. `nomove` parametre modifier'ı by-value pointer parametrenin fonksiyon gövdesinde yeniden taşınmasını engeller. Move edilen `*` ve `^` source yeniden initialize edilmeden kullanılamaz.
- Aynı type içinde `*` ve `^` karıştırılamıyor. Parser bunu hata sayıyor:

```cstar
int*^ bad;  // desteklenmiyor
int^* bad;  // desteklenmiyor
```

### 6.2 Reference

```cstar
int& r = x;
int32* p = ref x;
int64 x = deref p;
```

Desteklenen biçimler:

- Type sonrasında `&`: reference type.
- Unary operator olarak `ref`.
- Unary operator olarak `deref`.
- `*` expression içinde dereference olarak da kullanılabiliyor.

Güncel codegen notu: `ref x` shared pointer beklenen yerde `{ data=&x, strong=new atomic i64(1) }` handle'ı üretir. `int32* q = p` ve `q = p` atomic retain yapar; `q := p` ve `q = move p` transfer yapar ve source moved kabul edilir. Shared pointer by-value function argument plain symbol ile retain/copy yapar; local shared alias'lar ve by-value shared parametreler scope/return çıkışında atomic release üretir. `move` argument/return source'u null'a çekilir. `strong_count(p)` compiler builtin'i atomic strong-count değerini döndürür. Primitive pointer parametre/return ABI'si de bu shared handle'ı taşır; `read_ptr(ref x)`, `identity(int32* p) :: int32*`, `int32* q = deref pp`, `deref p = value`, `deref p += value`, `nomove int32^ p`, `**pp` ve `**pp = value` smoke setinde çalışır. CRT/string interop için `char*` ve `const char*` raw C string pointer ABI'sinde kalır; string literal `const char*` parametreye/değişkene ve `readonly char*` bağlamına doğrudan geçebilir. Mutable `char*`, yalnız pointer adresini sabitleyen `constptr char*` ve unique ownership iddiası taşıyan `char^` bağlamlarına implicit decay etmez; bu durum `CST2100` qualifier diagnostic'i üretir. Owned mutable string gerekiyorsa ileride explicit copy/allocator API'si kullanılmalıdır. Primitive reference parametreler `int32& x` syntax'ı ile çağıranın storage'ına alias olur; çağrı tarafında açık `ref value` gerekir, fonksiyon gövdesinde `x` normal değer gibi okunur ve `x = value;` çağıranın değerini günceller. `const`/`constref`/`constptr`/`readonly` qualifier kontrolleri semantic pass'te korunur.

### 6.3 Nullability Tasarım Yönü

Runtime/IR seviyesinde null pointer gerçekliği vardır; özellikle C ABI, allocator failure ve move sonrası source temizleme gibi alanlar bunu kullanabilir. Fakat C* yüzeyinde normal pointer'ların default nullable olması hedeflenmez. Canonical yön:

```cstar
int32* p = ref value;        // non-null shared pointer
int32^ owned = new int32(7); // non-null unique pointer proposal

int32*? maybe = nil;         // nullable shared pointer
int32^? maybe_owned = nil;   // nullable unique pointer
```

`T*`, `T^` ve `T&` non-null kabul edilir. `T&` nullable olmaz; reference var olan storage'a alias demektir. Null gerekliyse `T*?` veya `T^?` gibi açık nullable pointer yazılır. `nil` yalnız nullable pointer bağlamında legal olur. `deref` nullable pointer üzerinde non-null proof olmadan `CST2100` üretir; `if (p)` true branch'i içinde pointer non-null'a narrow edilir. Nullable pointer non-null pointer'a ancak bu proof scope'unda aktarılabilir. Move edilen pointer'ın runtime iç temsilde null'a çekilmesi kullanıcı açısından nullable pointer değildir; semantic state `moved` kalır ve yeniden initialize edilmeden kullanım diagnostic üretir.

Fallible allocation yüzeyi `new?` / `shared new?` olarak parse edilir ve sonuç type'ı nullable ownership handle olur: `new? T(args)` -> `T^?`, `shared new? T(args)` -> `T*?`. Infallible `new` ve `shared new` non-null kabul edilir; allocator failure'ın runtime policy'si görünür hata/abort veya ileride effect/result modeliyle sıkılaştırılacaktır. Non-null `int32* p = nil;`, `int32^ p = nil;` ve `p = nil;` gibi kullanımlar `CST2100` üretir.

Allocator-backed ownership testlerinde `drop` ve scope-exit release hattı önce destructor'ı, ardından allocator'ın `free(ptr, bytes, align)` method'unu çağırır. Heap-backed allocator bu method içinde gerçek CRT `free(ptr)` çağırmalıdır; arena/region allocator ise no-op free davranışını ancak kendi contract'ı olarak açıkça seçebilir.

### 6.4 Qualifier'lar

```cstar
const int x = 10;
constptr int* p = &x;
constref int& r = x;
readonly char* str = "Hello";
```

Niyet:

- `const`: değerin değişmemesi.
- `constptr`: pointer adresinin sabit olması, işaret edilen değerin değişebilir olması.
- `constref`: reference'ın const davranması.
- `readonly`: hem adres hem değer tarafında daha katı salt-okunur davranış.

Semantic pass bu qualifier'lar için bazı kontroller yapıyor:

- `constptr` pointer olmayan değerle uyumsuz; pointer adresi sonradan değiştirilemez.
- `constptr` pointer'ın hedef değeri `deref` ile okunup yazılabilir.
- Bu yüzden `constptr char*` string literal için güvenli hedef değildir; literal immutable storage kabul edildiği için hedef değeri salt-okunur yapan `const char*` veya `readonly char*` gerekir.
- `char^`, `const char^` ve `readonly char^` da string literal için güvenli değildir; literal storage'ı caller-owned heap allocation değildir.
- `constref` reference olmayan değerle uyumsuz; mutable storage'a read-only reference olarak bağlanabilir.
- `constref` parametre okunabilir fakat assignment target olamaz.
- `readonly` pointer adresi ve hedef değeri salt-okunur kabul ediliyor.
- `const` scalar değer tekrar assign edilemez.
- `const int32*` pointer'ın hedef değeri salt-okunurdur; pointer adresi değiştirilebilir.
- `readonly int32**` çok seviyeli pointer read desteklenir; hedef yazma reddedilir.
- `const int32**` mutable pointer zincirine bağlanmaz.

Qualifier diagnostic kodları:

- `CST2100`: qualifier mismatch.
- `CST2101`: invalid qualifier/type combination.
- `CST2102`: const assignment.
- `CST2103`: constptr pointer reassignment.
- `CST2104`: readonly assignment.

Qualifier negatif örneklerinin kritik kısmı `// expected-code: CST21xx` ile sabitlenir. Runner, diagnostic suite'inde bu kodların çıktıda gerçekten bulunduğunu kontrol eder. Safe cast qualifier stripping de `CST2100` sınıfına girer.

Compiler iç temsili per-level qualifier metadata taşır. `SymbolInfo::qualifierLevels` içinde index `0` final target value, index `N` ise pointer object level `N` anlamına gelir. Bugünkü prefix syntax bu yapıya conservative biçimde map edilir: `const` target value, `constptr` en dış pointer object, `constref` reference target value, `readonly` ise tüm mevcut seviyeler olarak işlenir. Pointer seviyesine özel yeni surface syntax henüz proposal aşamasındadır.

## 7. Değişken Deklarasyonu

Genel form:

```cstar
[visibility] [qualifier] type [pointer/ref] name [array_dims] [= expr];
```

Örnekler:

```cstar
int x = 10;
float y = 12.24;
const int a = 10 + 30, b = 20;
readonly char* str = "Hello world";
int32 arr[2:2] = ((0, 1), (2, 3));
Shape^ shape = Triangle();
```

Visibility/storage/linkage:

```cstar
public int32 moduleVisible = 10;
import externalFunc(int x) :: void;
export main() :: int32 { ret 0; }
static int globalCounter = 0;
```

Mevcut parser module-level function, variable, struct, trait ve enum deklarasyonlarında `public`/`private` erişim bilgisini parse eder. Function ve variable deklarasyonlarında buna ek olarak `static`, `import` ve `export` modifier'ları da desteklenir. Struct/trait/enum için `import`/`export` ve top-level `static` bugün geçerli surface değildir.

Visibility/linkage ayrımı:

- `public`, include edilen local module'den dışarı açılan declaration'ı seçer.
- Modifier yazılmayan declaration default olarak private kabul edilir.
- `private` açıkça yazılabilir ama default ile aynıdır.
- `import`/`export` visibility değildir; native/linkage ABI sınırı için kullanılır.
- Top-level `public struct`, `public trait` ve `public enum` hedef kural olarak aynı global declaration havuzuna açılır; yani module API'si yalnız fonksiyonlardan oluşmaz.
- Struct body içinde field/method visibility de aynı prensibi izler: member default private, `public` member module dışından erişilebilir olmalıdır.
- Bugünkü compiler'da struct field metadata'sında `isPublic` tutulur, fakat module sınırı üzerinden field visibility enforcement henüz tamamlanmamıştır. Bu yüzden dokümandaki kural canonical hedeftir; negatif/pozitif testleri TODO'dadır.

`static` kararı:

- Global scope'ta `static`, internal linkage/storage duration anlamı taşır ve tek başına visibility açmaz.
- `public static`, C* source include yüzeyinde public API'ye açılan ama native/linkage tarafında static/internal kalan module-level state/function anlamına gelir.
- Sadece `static` yazılan declaration private kabul edilir.
- Static function LLVM tarafında internal linkage alır.
- Static function yalnızca static global state'e ve static function'lara erişebilir; non-static global symbol/function kullanımı semantic diagnostic üretir.
- Local static ve static data member bu modelde kapalıdır; parser module-level static state'e yönlendiren diagnostic üretir. Thread-safe one-time local initialization gerekiyorsa ayrı proposal olarak ele alınacaktır.
- `protocol` tarafında ayrıca `static` yazmaya gerek yoktur; protocol default olarak static/provable kabul edilir. Runtime state isteyen açıkça `dynamic protocol` yazmalıdır.

### 7.1 Çoklu deklarasyon

Parser comma ile aynı type üzerinden devam eden deklarasyonu desteklemeye çalışıyor:

```cstar
const int a = 10, b = 20;
int *p, *q;
```

Kodda bu bölüm `goto not_needed_type` ile yürütülüyor. Eski ama anlaşılır bir yaklaşım.

### 7.2 Array deklarasyonu

Array dimension syntax'ı:

```cstar
int arr[4];
int mat[2:2];
int cube[2:2:2];
```

Mevcut parser dimension elemanı olarak scalar integer veya identifier kabul ediyor:

```cstar
int arr[n:m];
```

Initializer örnekleri:

```cstar
int values[2:2] = ((0,1),(2,3));
int32 arr[4] = (0,1,2,3);
```

Not: `grammar.cfg` içinde `{ ... }` initializer list fikri var; mevcut parser expression tabanlı `(...)` comma tree yapısını kullanıyor.

Güncel çalışan MVP:

- Tek boyutlu array initializer ve element read smoke ile doğrulanmıştır.
- Tek boyutlu element assignment ve shortcut assignment çalışır:
- Çok boyutlu array read/write ve parametre geçişi row-major flattening ile çalışır.

```cstar
int32 arr[2] = (1, 2);
arr[0] = 9;
arr[1] += 5;
ret arr[1];

int32 matrix[2:3] = ((1, 2, 3), (4, 5, 6));
matrix[1:1] = 9;
ret matrix[1:2];
```

Çok boyutlu initializer kaynakta katmanlı yazılır; codegen bunu C benzeri row-major flat belleğe indirir. `[rows:cols]` için lineer index `row * cols + col`; daha yüksek boyutta soldan sağa `linear = linear * next_dim + index` kuralı uygulanır. Her eksende negatif sabit index boyut içinde kaldığı sürece sondan indeksleme olarak normalize edilir: `arr[-1]` son elemana, `matrix[-1:-1]` son satır son kolona iner. Initializer eleman sayısı `product(dimensions)` ile eşleşmek zorundadır; eksik/fazla initializer controlled diagnostic üretir. Sabit index pozitif veya negatif tarafta array sınırını aşarsa compile-time error üretir; dinamik index için runtime bounds check henüz üretilmez. MVP'de array dimension'ları compile-time positive integer literal olmalıdır. Local primitive/pointer array storage 1 MiB üstünü aşarsa stack overflow riskine karşı diagnostic üretilir; büyük buffer için ileride heap/allocator-backed array/slice modeli kullanılmalıdır.

Expression parser delimiter içinde veya operator/comma sonrasında gelen satır sonlarını expression devamı kabul eder. Bu yüzden çok boyutlu initializer, function argümanları, binary expression ve subscript indexleri okunabilir biçimde alt satıra taşınabilir.

## 8. Fonksiyonlar

### 8.1 Temel syntax

```cstar
main() :: void {
    ret;
}

sum(int a, int b) :: int32 {
    ret a + b;
}
```

Dönüş tipi belirtilmezse `void` kabul ediliyor:

```cstar
func2() {
    int a = 10;
}
```

### 8.2 Forward/import deklarasyon

```cstar
import printf(char*) :: void;
import forwardDecl(const int*, constptr float*) :: void;
import abs(int32 value) :: int32;
import printf(const char* fmt, ...) :: int32;
```

`import` fonksiyonlar forward declaration gibi ele alınıyor ve body yerine `;` bekleniyor. Parametre adı ABI için opsiyoneldir; `import abs(int32) :: int32;` ve `import abs(int32 value) :: int32;` aynı external imzaya iner. C ABI variadic deklarasyon için `...` parametre listesinin sonunda kullanılabilir: `printf(const char* fmt, ...) :: int32;`. Fixed parametreler normal type-check alır; `...` tarafındaki argümanlar C ABI çağıran sözleşmesine bağlıdır. C* içinde variadic function body yazmak için `va_list`/`va_arg` modeli henüz ayrı tasarımdır. Basit native import çağrısı smoke suite içinde gerçek libc `abs` ve variadic `printf` çağrılarıyla doğrulanır. `from "lib"`, `import { ... }` ve `import from "lib" { ... }` formları parser/codegen akışına bağlıdır.

### 8.3 Parametre syntax'ı

Parser birkaç farklı biçimi kabul etmeye çalışıyor:

```cstar
func(int a, float b) :: void { }
func(a int, b float) :: void { }
func(constref x int&, int* p, float^ s, p int*, k int) :: int32& { }
func(mainArg Type*) :: void { }
func(int[2:2] arr, b int, c int[2:2]) :: void { }
```

Parametre parser'ı C-style ve alternatif `name type` style arasında ayrım yapmaya çalışıyor. User-defined type belirsizliğini semantic pass'te çözme fikri var.

Primitive parametrelerde sıranın semantik anlamı vardır:

```cstar
cast_is_not_possible(x bool, bool y) :: void {
    int32 a = x; // diagnostic: x implicit cast edilemez
    int32 b = y; // legal: bool y implicit cast edilebilir
}
```

Bu davranış `examples/type_checker/functions/076.cstar` ile negatif diagnostic, `examples/smoke/casts/castable_param.cstar` ile pozitif smoke olarak doğrulanır.

## 9. Statement'lar

Fonksiyon scope içinde parser'ın işlediği ana statement türleri:

- Local variable declaration
- Assignment
- Array element assignment
- Prefix/postfix increment/decrement
- `ret`
- `if / elif / else`
- `loop`

### 9.1 Return

```cstar
ret;
ret 0;
ret x + y;
```

Parser `RetAST` oluşturuyor ve codegen tarafında `ret;` ile `ret expr;` için temel LLVM return üretimi artık çalışıyor. Primitive return expression'lar smoke setinde doğrulanmıştır.

Semantic tarafında return expression tipi, fonksiyonun dönüş tipiyle karşılaştırılır. Örneğin `main() :: int32 { ret returns_bool(); }` gibi bool dönen call'lar artık codegen'e kaçmadan diagnostic üretir.

### 9.2 Assignment

Desteklenen operator seti:

```text
= := -= *= /= %= >>= <<= &= |= ^= =
.=
```

Örnek:

```cstar
x = 20;
r += 1;
r >>= 1;
r := otherUnique;
r .= a;  // proposal: dynamic protocol/provability-gap assignment, henüz codegen yok
```

Dereference assignment:

```cstar
*p = 10;
deref p = 10;
**pp = 20;
```

Array assignment:

```cstar
arr[1] = 2;
arr[2] += 3;
arr[a:b] = 3;
```

Not: Scalar local assignment, dereference assignment, tek boyutlu array element assignment ve `arr[a:b]` formundaki çok boyutlu/colon index assignment smoke setinde doğrulanıyor.

### 9.3 Increment/decrement

```cstar
x++;
++x;
x--;
--x;
```

Parser `FixAST` oluşturuyor. Semantic tarafında kontrol izi var, ancak codegen `visit(FixAST&)` boş.

## 10. Kontrol Akışı

### 10.1 If / elif / else

Syntax:

```cstar
if (x == 10) {
    int a = 1;
} elif (x > 10) {
    int b = 2;
} else {
    int c = 3;
}
```

Parser `IfStmtAST` içinde condition block, elif block'ları ve else scope'u tutuyor.

Codegen:

- LLVM basic block üretimi var.
- `if`, `if/else`, `if/elif/else`, nested if ve branch içinde fallthrough assignment smoke setinde doğrulanıyor.
- Branch condition değerleri `int`, `float`, `bool/i1` ve pointer için zero/null karşılaştırmasına indirgeniyor.
- `ret` üreten branch bloklarının arkasına fazladan `br merge` basılmaması için terminator guard eklendi.
- PHI node hâlâ yalnızca ileride `if` expression olarak istenirse tasarlanmalı.

### 10.2 Loop

Tek keyword: `loop`.

#### While benzeri

```cstar
loop (x < 20) {
    x += 1;
}

loop (true) {
}
```

#### Iterable/range benzeri

```cstar
loop(data in arr) {
}

loop(index, data in arr) {
}

loop(i in [0,100]) {
}
```

Semantic niyeti:

- `data in arr`: `arr` iterable olmalı. Şimdilik array için düşünülmüş.
- `index, data in arr`: index ve value birlikte.
- `[min,max]`: numeric range.

Codegen:

- While-style conditional loop `cond -> body -> cond -> after` basic block akışıyla çalışır.
- Loop condition değerleri `if` ile aynı şekilde bool'a indirilir; scalar, bool ve pointer condition desteklenir.
- `break` loop çıkış bloğuna, `continue` loop condition bloğuna branch üretir.
- `break` / `continue` yalnızca loop içinde geçerlidir; loop dışı kullanım semantic diagnostic üretir.
- Range loop `loop(i in [min, max])` min inclusive, max exclusive çalışır.
- Array iterable loop `loop(value in arr)` ve indexed form `loop(index, value in arr)` local array ve array parametre için çalışır.
- Daha genel sequence/trait tabanlı iterable, reverse/step range ve bounds politikası ayrı stdlib/trait aşamasıdır.

## 11. Expression Sistemi

Expression parser en yoğun bölüm. Recursive descent ve operator precedence bucket yaklaşımı karışık kullanılmış.

Desteklenen atomlar:

```text
identifier
integer scalar
float scalar
string literal
char literal
true / false
type atomları
```

### 11.1 Binary operator'lar

Parser/AST düzeyinde ana binary operator'lar:

```text
+  -  *  /  %
&  &&  |  ||  ^
>  >=  <  <=  ==
<< >>
.  ->  ::
,
? :
[]
[:]
```

Comparison operatorleri (`<`, `<=`, `>`, `>=`, `==`, `!=`) codegen'de `bool/i1` sonuç üretir. Bu sonuç `ret` veya variable initializer hedefinde integer/bool tipe `0/1` olarak cast edilebilir.

Arithmetic codegen notu: integer `+`, `-`, `*` signed değerlerde `nsw`, unsigned değerlerde `nuw` LLVM flag'iyle üretilir. `float32`/`float64` için `+`, `-`, `*`, `/`, `%` ilgili floating point LLVM instruction'larına iner.

### 11.2 Unary operator'lar

```text
sizeof
typeof
move
++ --
+ -
!
~
deref / *
ref / &
```

Örnekler:

```cstar
int s = ~b;
int v = sizeof(float) / 2;
int32* p = ref x;
int64 x = deref p;
int z = move y + x; // type checker bazı bağlamlarda bunu reddediyor
```

### 11.3 Cast operator'ları

```cstar
cast<int>(x)
unsafe_cast<int32>(a)
unsafe_cast<int32*>(10 + 20)
expr as Type
```

Parser `cast`, `unsafe_cast` ve `as` için altyapı içeriyor.

Semantic/codegen durumu:

- `cast<T>(expr)` ve `expr as T` safe cast olarak aynı semantic/codegen yolunu kullanır.
- Safe cast için primitive numerik dönüşüm ve pointer -> pointer MVP çalışır.
- Safe cast pointer/value kategori geçişini reddeder; örneğin pointer'ı safe cast ile integer'a çevirmek diagnostic üretir.
- Safe cast pointer qualifier stripping'i reddeder.
- `unsafe_cast<T>(expr)` integer -> pointer, pointer -> integer ve pointer -> pointer gibi daha gevşek dönüşümleri LLVM IR'a indirir.
- User-defined type cast/conversion overload semantic'i henüz yoktur; `struct`/defined type hedefleri kontrollü diagnostic üretir.

### 11.4 Function call ve generic/type attribute

Örnekler:

```cstar
func(10,20)
func<int>(10,20)
Triangle<int>()
int32 x = add(1, 2);
foo();
```

Parser function call için `FuncCallAST` oluşturabiliyor. `<TYPE>` biçimi function call generic/type attribute gibi yorumlanıyor.

Güncel durum:

- Primitive argümanlı ve primitive return değerli function call MVP çalışıyor.
- Call sonucu `ret add(1, 2);` ve `int32 x = add(1, 2);` içinde kullanılabiliyor.
- Parametresiz call statement (`foo();`) destekleniyor.
- Primitive pointer argümanı için `read_ptr(ref x)` ve callee içinde `deref p` smoke'u çalışıyor.
- Primitive reference parametresi için `write_ref(int32& x)` ve çağrı tarafında `write_ref(ref value)` smoke'u çalışıyor.
- Primitive `constref` parametresi için `read_ref(constref int32& x)` ve çağrı tarafında `read_ref(ref value)` smoke'u çalışıyor; gövde içi assignment diagnostic üretir.
- Primitive `constptr` parametresi için `write_through(constptr int32* p)` callee içinde `deref p = value` yazabilir; `p = ref other` diagnostic üretir.
- Primitive `readonly` parametresi için `read_through(readonly int32* p)` callee içinde target read yapabilir; `deref p = value` diagnostic üretir.
- Tek ve çok boyutlu array parametreleri `int32[2] arr` / `int32[2:3] arr` syntax'ı ile ABI'de array storage adresi olarak taşınır; callee içinde `arr[i]` veya `arr[i:j]` read/write caller array'ine erişir.
- Pointer variable initializer için `int32* p = ref x;` smoke'u çalışıyor.
- Pointer return için `identity(int32* p) :: int32*` smoke'u çalışıyor.
- Dereference assignment ve shortcut assignment için `deref p = value;`, `*p = value;`, `deref p += value;` smoke'ları çalışıyor.
- Çok seviyeli dereference assignment ve pointer'dan pointer okuma için `**pp = value;` ve `int32* q = deref pp;` smoke'ları çalışıyor.
- Fonksiyon prototipleri body codegen başlamadan önce module'a declare ediliyor; bu yüzden çağrılan fonksiyon kaynakta sonra tanımlansa da forward call smoke çalışıyor.
- Pass0 içinde function signature table tutuluyor.
- Pass1 içinde bilinmeyen fonksiyon, argüman sayısı, temel scalar argüman tipi, primitive symbol argüman tipi ve call result return type diagnostic'leri üretiliyor.
- `<TYPE>` parser yolu `func<A****>()` ve `func<int32*>()` gibi pointer seviyeli tip argümanlarını syntax olarak kabul ediyor. Generic semantics/runtime davranışı ise henüz tasarlanmadı.

Geçici builtin:

```cstar
print("Hello\n");
print(42);
int64 value = input_int();
```

`print(...)`, `input_int()`, `input_string()`, `clear_screen()`, `flush_output()`, `sleep_ms(ms)`, `enable_raw_input()`, `disable_raw_input()` ve `read_key()` şu an compiler içindeki `NativeRuntime` katmanı üzerinden CRT/POSIX çağrılarına indiriliyor. `print(...)` `printf` kullanır. `input_int()` önce `scanf("%255s", ...)` ile token okur, sonra `atoll` ile `int64` üretir; sayı olmayan token consume edilir ve `0` değerine dönüşür. `input_string()` `scanf("%255s", ...)` ile whitespace'e kadar kısa string okur ve doğrudan `print(input_string())` gibi kullanılabilir. `clear_screen()` ANSI terminal temizleme dizisini yazar ve hemen flush eder; `flush_output()` frame tabanlı console renderlarında `fflush(NULL)` ile buffer'ı boşaltır. `sleep_ms(ms)` aktif konsol demoları için milisaniyeyi mikro saniyeye çevirip `usleep` çağırır. `enable_raw_input()` ve `disable_raw_input()` POSIX terminali non-canonical, non-blocking moda alıp geri toparlar; `read_key()` tuş yokken `-1`, tuş varken ASCII/escape byte değerini `int32` döndürür. String literal kaçışları için temel `\n`, `\t`, `\"`, `\\` decode ediliyor. Gerçek stdlib/native interop tasarımı büyüdükçe bu katman ABI bağlantı noktası olarak genişletilecek.

Stdlib MVP artık modül bazlı küçük bir framework olarak ele alınır. `std/core.cstar` eski `core_*` wrapper'ları için geriye uyumluluk dosyası olarak kalır; canonical yeni yüzeyler `std/print.cstar`, `std/math.cstar`, `std/time.cstar`, `std/network.cstar` ve `std/fs.cstar` dosyalarıdır. Bu dosyalar compiler içi özel binding eklemeden, normal C* `include ... as ...`, public module API, `struct`, `trait`, value operator, `attribute`, `macro`, `enum` ve CRT import kurallarıyla çalışır.

Print modülü:

```cstar
include "../../std/print.cstar" as io

main() :: int32 {
  io.write("value: ");
  io.write_i32(42);
  io.write(" ");
  io.write_f64(3.5);
  io.line();

  ret 0;
}
```

`std/print.cstar` metni `%s`, sayıları `%d`/`%lld`/`%f` formatlarıyla CRT `printf`'e geçirir. `flush()` CRT `fflush(NULL)` çağırır. `read_i32` ve `read_i64` whitespace atlayıp signed decimal integer parse eder. `read_token` şimdilik `public static char[256]` buffer ve `scanf("%255s", ...)` kullanır.

Print modülü ayrıca `TextWriter` trait'i ve `ConsoleWriter` struct'ını sağlar. `ConsoleWriter` trait conformance, constructor, method receiver, public field ve CRT flush yolunu birlikte doğrulayan canonical writer yüzeyidir. `writeln` implementation'ı modül içi `print_line` macro'sunu kullanır.

`public macro` declaration'ları source include alias üzerinden stabil compile-time API olarak kullanılabilir. Örneğin `include "../../std/print.cstar" as io` sonrası `io.print_line("text")`, `include "../../std/math.cstar" as math` sonrası `math.math_clamp_i64(value, min, max)` parse öncesi expand edilir. Public macro export runtime/linkage ABI değildir; include edilen `.cstar` kaynak dosyadan compile-time olarak toplanır ve `alias.macro(...)` qualified call yüzeyiyle kullanılır.

Math/time/network modülleri:

```cstar
include "../../std/math.cstar" as math
include "../../std/time.cstar" as time
include "../../std/network.cstar" as net

main() :: int32 {
  math.Vec2i velocity = Vec2i(6, 8);
  int64 distance = velocity.manhattan();

  time.Instant start = time.from_ticks(10);
  time.Instant finish = time.from_ticks(17);
  int64 elapsed = time.elapsed_ticks(start, finish);

  net.Endpoint endpoint = Endpoint(12, net.default_port(Protocol.Https),
                                   Protocol.Https);

  if (net.endpoint_is_secure(endpoint)) {
    ret cast<int32>(distance + elapsed + 50);
  }

  ret 1;
}
```

`std/math.cstar` temel integer yardımcıları, `Metric64` trait'i, `Vec2i` value type'ı, `+`, `-`, `==` value operator'ları, `StdMathValue` attribute'u ve modül içi clamp/abs macro'ları sağlar. `Vec2i` hem method receiver hem by-value struct parametre hem de trait conformance için test edilen temel stdlib value type'tır.

`std/time.cstar` `Instant` ve `Duration` value type'larını, `TickValue` trait'ini, `Duration + Duration` ve `Duration - Duration` operator'larını, tick/millisecond helper'larını ve `StdTimeValue` attribute'unu sağlar. `now()` CRT `clock()` import'una bağlıdır, deterministic testler `from_ticks` ve `duration_from_ticks` kullanır.

`std/network.cstar` gerçek socket açmaz; protokol, endpoint descriptor, status classification, retry backoff ve bağlantı öncesi policy/utility yüzeyini sağlar. `Protocol` enum, `Endpoint` struct, `SecureEndpoint` trait, `Endpoint == Endpoint` operator'ı, endpoint method'ları ve `StdNetworkValue` attribute'u bu modülde birlikte kullanılır. Host string modeli gerçek string/dynamic buffer tamamlanana kadar `host_id` descriptor olarak tutulur.

Dosya sistemi modülü:

```cstar
include "../../std/fs.cstar" as fs

main() :: int32 {
  char[4096] buffer;
  fs.FileReadResult result = fs.read_text_4096("data/input.txt", buffer);
  if (result.success()) {
    ret cast<int32>(result.length);
  }
  ret result.kind;
}
```

`std/fs.cstar` şimdilik güvenli ve predictable bir MVP yüzeyi verir: dosya içeriği caller-owned `char[4096]` buffer'a okunur, sonuç `FileReadResult` value type'ı ile döner. Bu model dynamic string/allocator tamamlanmadan heap ownership gizlemez; `ReadableFile` trait'i, `StdFileValue` attribute'u, `fs_ok(...)` public macro'su ve CRT `fopen/fgetc/fclose` import'larıyla standart kütüphane mimarisine bağlı kalır. C ABI uyumu için `char*` string pointer'ları ve `void*` opaque native handle'ları raw pointer olarak iner; C* managed/shared pointer modeli typed object pointer'larda korunur.

Stdlib smoke kapsamı sadece “derleniyor mu?” değildir. `examples/smoke/stdlib/std_public_macro_alias.cstar` public macro export'u doğrudan doğrular. `std_math_advanced.cstar`, `std_time_duration.cstar`, `std_network_endpoint.cstar`, `std_print_writer.cstar`, `std_fs_read.cstar` ve `std_comprehensive_framework.cstar` dosyaları trait conformance, attribute expansion, public macro export, value operator, method receiver, include alias, enum, CRT import, dosya okuma ve modüller arası kullanımın beraber çalıştığını doğrular.

Doğrudan native variadic import da desteklenir:

```cstar
import from "std:crt" {
  printf(const char* fmt, ...) :: int32;
}

main() :: int32 {
  printf("value %s %d %lld %f\n", "x", cast<int32>(7), cast<int64>(99), 2.5);
  ret 0;
}
```

Native import link metadata'sı linker flag değil, logical/native library adı taşır. Örneğin `import from "OpenGL" { ... }` macOS tarafında compiler backend tarafından `-framework OpenGL` olarak çözülür; `import from "glfw" { ... }` Unix/macOS tarafında `-lglfw`, Windows tarafında `glfw.lib` olur. C* kaynaklarında `-lglfw`, `-framework OpenGL` gibi doğrudan linker flag yazılmamalıdır; bu mapping compiler/linker backend sorumluluğudur.

Interactive örnek:

```text
examples/interactive/calculator.cstar
```

Bu dosya `// expected: interactive` ile işaretlidir; normal runner suite'lerinde skip edilir, doğrudan çalıştırıldığında kullanıcıdan sayı ve işlem kodu bekler.

### 11.5 Subscript / array indexing

```cstar
arr[1]
arr[1:0]
abc[a[10] + f(20)]
```

Expression parser `[]` ve `:` ile çok boyutlu index ifadelerini parse etmeye çalışıyor. AST'de `B_ARRS` ve `B_MARRS` ayrımı var.
Codegen bugün sabit boyutlu array'lerde `arr[i]` ve `arr[i:j:k]` formunu row-major lineer index'e indirir. Sabit index expression'ı array sınırını aşarsa semantic pass codegen'e geçmeden error üretir. Negatif indexler her boyutta sondan indeksleme olarak ele alınır; `arr[-1]` son elemana, `matrix[-1:-1]` son satır/son kolona gider. Dinamik negatif index expression'ları codegen sırasında `index < 0 ? index + dimension : index` formunda normalize edilir; runtime bounds check henüz yoktur.

## 12. Operator Önceliği

Parser constructor içinde oluşturulan fiili öncelik tablosu, yüksekten düşüğe yaklaşık şöyle:

| Öncelik | Operator |
|---:|---|
| 16 | `::` |
| 15 | function call `()`, subscript `[]`, `cast`, `unsafe_cast`, type attribute `<T>`, postfix `++`, postfix `--` |
| 14 | `.`, `->` |
| 13 | `sizeof`, `typeof`, `move`, `deref`, `ref`, `as`, unary `+`, unary `-`, `!`, `~`, unary `*`, unary `&`, prefix `++`, prefix `--` |
| 11 | `*`, `/`, `%` |
| 10 | `+`, `-` |
| 9 | `<<`, `>>` |
| 8 | `<`, `<=`, `>`, `>=` |
| 7 | `==`, `!=` |
| 6 | `&` |
| 5 | `^` |
| 4 | `|` |
| 3 | `&&` |
| 2 | `||` |
| 1 | `,`, `?` |
| 0 | `:` |

Notlar:

- `=` ve shortcut assignment operator'ları normal expression precedence içinde değil, statement parser içinde özel ele alınıyor.
- `? :` ternary operator `BinaryOpAST` içinde üçüncü `extra` child ile temsil ediliyor.
- `,` initializer list / arg list / expression list için binary operator gibi kullanılıyor.

### 12.1 Ternary Expression

`cond ? a : b` C* içinde statement değil expression'dır. Bugünkü MVP üç ana bağlamda testlidir:

```cstar
int32 x = flag ? left : right;
ret enabled ? 7 : 3;
ret add_one(use_big ? 8 : 2);
```

Koşul tarafı `if` ile aynı truthiness kuralını kullanır: `bool`, integer, float ve pointer değerler condition'a çevrilebilir. Pointer condition zero/null karşılaştırmasına iner; örneğin `p ? 1 : 0` non-null pointer için `1` döndürür.

Branch'ler değer üretmek zorundadır. `void` branch kabul edilmez. Numeric primitive branch'ler beklenen hedef tipe cast edilebilir; pointer branch'lerde pointee, ownership ve qualifier şekli korunmalıdır. Qualifier stripping güvenli olmadığı için reddedilir.

MVP codegen side-effect-free branch'leri LLVM `select` ile üretir. Bu yüzden branch içinde function call, allocation veya assignment varsa şu an diagnostic verilir; bu form ileride basic block + PHI lowering ile güvenli şekilde açılmalıdır.

```cstar
main() :: int32 {
  bool flag = false;

  ret flag ?
      2 :
      5;
}
```

## 13. Semantic Analiz

Semantic yapı `Visitor::preVisit(...)` fonksiyonları üzerinden yürüyor.

### 13.1 Pass 0: Symbol Analysis

Amaç:

- Global symbol table oluşturmak.
- Fonksiyon scope'larındaki local symbol'ları toplamak.
- Redefinition kontrolü yapmak.
- User-defined type belirsizlikleri için ön bilgi toplamak.

Kontroller:

- Global symbol redefinition.
- Fonksiyon redefinition.
- Local redefinition.
- Global shadowing bazı durumlarda hata.
- Parametre sembolleri scope'a ekleniyor.

### 13.2 Pass 1: Type Checking

Amaç:

- Atama uyumluluğu.
- Assignment RHS symbol lookup, statement'ın bulunduğu scope/sıra konumuyla yapılır; `x = y;` içinde `y`, sol taraf `x`'in eski declaration id'sine göre yanlış reddedilmez.
- Pointer/reference uyumluluğu.
- Qualifier kısıtları.
- Array index/dimension kontrolleri.
- Scope içinde kullanımdan önce deklarasyon.
- Return expression type check.
- Function call signature check:
  - bilinmeyen fonksiyon
  - argüman sayısı
  - temel scalar/symbol argüman tipi
  - call result return type
- Condition expression kontrolleri.
- Cast expression kontrolleri:
  - explicit hedef tip zorunluluğu
  - plain `void` hedef reddi
  - safe cast pointer/value kategori reddi
  - safe cast qualifier stripping reddi
- Ternary expression kontrolleri:
  - condition `if` ile aynı bool/integer/float/pointer dönüşüm yüzeyini kullanır
  - branch'ler value-producing olmalıdır
  - primitive numeric branch'ler beklenen hedef tipe indirilebilir
  - pointer branch'lerde pointee/ownership/qualifier şekli korunur
  - side-effect branch'leri MVP'de reddedilir; PHI lowering ileri iştir

Örnek type-checker testleri:

```cstar
int s = t;       // t yoksa hata
int z = &t;      // t yoksa hata
int* b = &a;
int* x = a + b; // pointer/non-pointer uyumsuzluğu
const int r = 20;
r += 1;         // const assign hatası
readonly char* str = "Hello";
```

### 13.3 Eksik/yarım semantic alanlar

- User-defined type table dolmuyor.
- `struct Name { field; ... }` MVP grammar'ı çalışır; `trait`, `protocol`, `dynamic protocol`, `attribute`, `macro/directive`, `except/throw` yüzeyi lexer tarafından tanınır ve henüz gerçek grammar/lowering olmayan alanlarda controlled proposal diagnostic verir.
- User-defined type cast/conversion overload semantic'i yok; `struct`/defined type sistemi gelene kadar controlled diagnostic üretilir.
- Safe cast tarafında pointer qualifier stripping reddedilir; daha zengin qualifier/cast kuralları `struct`/`trait` ve çok seviyeli pointer syntax'ı olgunlaşınca genişletilecek.
- `typeof` ve `move` bazı binary operation bağlamlarında özellikle reddediliyor ama genel davranış tamamlanmamış.
 - Array initializer arity validation MVP'si çalışır; shape-preserving destructuring/slice validation hâlâ ileri safety aşamasıdır.

## 14. LLVM Codegen Durumu

Codegen iki parçalı:

- `Visitor::visit(...)` fonksiyonları LLVM IR üretir.
- `CStarCodegen::build()` modülü `.ll`, `.s` ve executable üretimi için CMake'in bulduğu Clang backend'ini kullanır.

### 14.1 Uygulanmış görünen codegen parçaları

- LLVM `Module`, `IRBuilder`, `Function` oluşturma.
- Global variable oluşturma.
- Local variable alloca/store.
- Scalar constant üretimi.
- String global üretimi.
- `ret;` ve `ret expr;`.
- Scalar local assignment.
- Primitive function call ve forward function call.
- Primitive pointer/ref expression codegen'i:
  - `ref x`
  - `deref p`
  - `int32* p = ref x;`
  - `int32* q = deref pp;`
  - `deref p = value;`
  - `*p = value;`
  - `**pp = value;`
  - `identity(int32* p) :: int32*`
- Native runtime builtin `print(...)` -> CRT `printf`.
- Native runtime builtin `input_int()` -> CRT `scanf("%255s")` + `atoll`.
- Native runtime builtin `input_string()` -> CRT `scanf("%255s")`.
- Native runtime builtin `clear_screen()` -> ANSI clear/home escape.
- Native runtime builtin `flush_output()` -> CRT `fflush(NULL)`.
- Native runtime builtin `sleep_ms(ms)` -> POSIX `usleep(ms * 1000)`.
- Native runtime builtin `enable_raw_input()` / `disable_raw_input()` -> POSIX `stty` non-canonical/sane köprüsü.
- Native runtime builtin `read_key()` -> CRT `getchar()` ile non-blocking key polling.
- Stdlib MVP:
  - `std/print.cstar`
  - `std/math.cstar`
  - `std/time.cstar`
  - `std/network.cstar`
  - `std/fs.cstar`
  - `std/core.cstar` legacy `core_*` wrapper'ları için korunur.
  - `examples/smoke/stdlib/*.cstar` yeni modüler stdlib yüzeyini doğrular.
- Native variadic import: `printf(const char* fmt, ...) :: int32` gibi C ABI vararg fonksiyonları LLVM vararg declaration/call olarak üretilir.
- Binary arithmetic:
  - add/sub/mul/div/mod
  - bitwise and/or/xor
  - logical and/or (`&&`, `||`)
  - shift
  - comparison (`<`, `<=`, `>`, `>=`, `==`, `!=`) sonucu `bool/i1`
  - ternary için `select`; condition `if` helper'ı ile bool'a çevrilir ve branch'ler tek LLVM tipe cast edilir
- Array initializer için kısmi constant array / memcpy yaklaşımı.
- Tek ve çok boyutlu local/global array element read/write.
- `if/elif/else` basic block üretimi; nested/fallthrough senaryoları smoke ile doğrulanıyor.
- While-style `loop`, `break` ve `continue` basic block üretimi smoke ile doğrulanıyor.
- `cast<T>(expr)`, `expr as T` ve `unsafe_cast<T>(expr)` MVP IR üretimi.
- Iterable array loop için kısmi basic block ve GEP denemesi.
- Scalar enum storage ve member constant lowering:
  - `enum Color : uint8 { Red, Green }`
  - `Color.Green` underlying integer constant'a iner.
  - enum variable/param/return repr type storage kullanır.

### 14.2 Boş veya tamamlanmamış codegen parçaları

Aşağıdaki codegen alanları hâlâ boş veya tamamlanmamış kabul edilmelidir:

```cpp
ValuePtr Visitor::visit(TypeAST &typeAst) {}
ValuePtr Visitor::visit(FixAST &fixAst) {}
```

MVP executable davranışı artık küçük smoke seti için vardır; fakat proposal'ın tamamı hâlâ çok daha büyüktür.

Özellikle:

- User-defined type cast/conversion overload IR üretimi tamamlanmadı; bugün controlled diagnostic ile durdurulur.
- Pointer/ref/qualifier parametre codegen'i genişletilmeli; primitive pointer argüman, pointer variable initializer, pointer return ve dereference assignment smoke'ları çalışıyor.
- `++` / `--` IR üretmiyor.

## 15. Tasarım Notlarındaki Daha Büyük Dil Vizyonu

`examples/papers/*.cstar` mevcut compiler'dan daha ileri C* vizyonunu concept bazlı proposal dosyaları olarak gösteriyor. `policy.cstar` artık eski tek-hook policy fikrinin hangi ayrı kavramlara bölündüğünü anlatan kısa concept map dosyasıdır; canonical tasarım örnekleri kendi dosyalarındadır:

- `struct.cstar`: struct layout, constructor/destructor, drop ve value operator.
- `allocator.cstar`: allocator capability ve `new(allocator) Type(args)` lowering modeli.
- `trait.cstar`: static trait conformance ve açık `dynamic Trait` object modeli.
- `protocol.cstar`: typestate, `dynamic protocol`, `.=` ve scope-exit cleanup modeli.
- `enum.cstar`: scalar enum, flags enum ve explicit tagged layout.
- `nullability.cstar`: non-null default pointer, nullable `T*?`/`T^?` ve `nil` modeli.
- `metaprogramming.cstar`: macro, directive, attribute ve compile-time reflection.
- `concurrency.cstar`: async/task boundary, `Send`/`Sync` ve `nomove`.

Tasarlanan ama mevcut compiler'da uygulanmayan/çok yarım özellikler:

### 15.1 Package/import sistemi

Örnek tasarım:

```cstar
include involved {
    'ML',
    'X',
    'Y',
}

include {
    "std:math"
    "std:math:cos"
}

include "std:math:PI" as PI
```

Compiler artık `include involved { ... }`, `include { ... }` ve `include "module" as alias` formlarını gerçek grammar olarak parse eder. `include` hedefi `.cstar` ile biten yerel bir dosyaysa dosya ana compilation unit'e parse/merge edilir; böylece başka dosyadaki public global deklarasyonlar hedeflenebilir. `std:math`, `std:print`, `std:fs` gibi logical std package hedefleri de repo `std/` köküne çözülür; `std:math:abs_i64` gibi member hedefleri bugünkü MVP'de `std/math.cstar` dosyasını dahil eder ve sembol filtreleme/re-export aşamasına hazır canonical syntax olarak kabul edilir. Bugün function/variable yolu smoke testlerle sabitlenmiştir. Top-level `public struct`, `public trait` ve `public enum` için de `mod.Type` alias type syntax'ı çalışır; bugünkü source-merge mimarisinde bu isimler semantic olarak public type'ın gerçek adına iner. Include edilen public struct field'ları default private kabul edilir; module dışından yalnız `public` field erişilebilir. Struct'ın kendi method'ları private field'lara erişebilir. Gerçek namespace/type identity hâlâ ayrı tamamlanacak iştir.

```cstar
include "modules/math_module.cstar" as math

main() :: int32 {
    ret add_from_module(2, 5);
}
```

`as` alias'ı function call lookup için çalışır; `math.add_from_module(...)` gibi alias member çağrısı include edilen module içindeki `public add_from_module` imzasına çözülür. Type pozisyonlarında `geo.Point`, `geo.Color` ve `geo.Drawable` gibi aliaslı public type isimleri kullanılabilir. Include edilen local module yalnızca `public` deklarasyonları ve gerekli native `import` forward deklarasyonlarını dışarı açar. Modifier yazılmayan declaration private kabul edilir ve alias üzerinden çağrılamaz; `examples/type_checker/imports/052.cstar` private function erişimini, `examples/type_checker/imports/private_module_type_alias.cstar` private type erişimini doğrular.

Std package include yüzeyi:

```cstar
include involved {
    "std:math"
}

include {
    "std:math:abs_i64"
}

include "std:math" as math

main() :: int32 {
    int64 a = abs_i64(0 - 5);
    int64 b = math.abs_i64(0 - 7);
    int64 c = math.math_abs_i64(a);
    ret cast<int32>(a + b + c);
}
```

Aliaslı include public macro export'u da toplar; `math.math_abs_i64(...)` gibi qualified compile-time macro çağrıları source include preprocess aşamasında expand edilir. Bu yüzey `examples/smoke/imports/include_std_package.cstar` ile doğrulanır.

Module API yüzeyi için hedef kural:

```cstar
// modules/geometry.cstar
public struct Point {
    public int32 x;
    public int32 y;
    private int32 cachedHash;
}

public trait Drawable {
    draw(self) :: void;
}

public static int32 version = 1; // include API'sinde public, native/linkage tarafında internal
static int32 cacheHits = 0;      // private module state
```

```cstar
include "modules/geometry.cstar" as geo

main() :: int32 {
    geo.Point p = Point(3, 4);
    geo.Color c = Color.Green;
    // geo.Drawable trait'i `struct Local with geo.Drawable` formunda kullanılabilir.
    // Public field'a instance üzerinden erişilir; Point.cachedHash gibi private
    // field'lar module dışından diagnostic üretir.
    // Point.x gibi instance olmadan field erişimi de geçersizdir.
    ret 0;
}
```

### 15.2 Import/export from library

Örnek tasarım:

```cstar
import glClearColor(float r,float g,float b,float a) :: void from "OpenGL";
import from "myLibrary" {
    printf(char*) :: void;
}

export sin(float64 x) :: float64 from "std:math";
export from "std:math" {
    abs(int32 x) :: int32;
}
```

Mevcut compiler `import func(...) :: type;`, `import func(...) :: type from "lib";`, `import { ... }`, `import from "lib" { ... }`, `export func(...) :: type from "module";` ve `export from "module" { ... }` formlarını parser, semantic signature ve LLVM external call codegen boyunca destekler. Parametre adı ABI için opsiyoneldir; `import abs(int32) :: int32;` ve `import abs(int32 value) :: int32;` aynı external imzaya iner.

Native linker davranışı:

- `"m"` gibi kısa isimler Unix/macOS tarafında `-lm` olur.
- `"OpenGL"` gibi platform framework/system library adları backend tarafından target platforma uygun şekilde çözülür; macOS için `-framework OpenGL`, Windows için `opengl32.lib` gibi.
- `.a`, `.so`, `.dylib`, `.lib` ve path içeren düşük seviye değerler doğrudan linker argümanı olarak geçebilir; bu escape hatch normal std/proposal örneklerinde tercih edilmez.
- `"std:math"` ve `"std:crt"` gibi `:` içeren logical module adları linker argümanına çevrilmez; bunlar package/module namespace tasarımı için saklanır.

Executable üretirken ana dosyada `export` deklarasyonu varsa compiler warning üretir; çünkü `export` library ABI görünürlüğüdür. Dış modüller için symbol üretilecekse `--emit=staticlib` veya `--emit=dynamiclib` kullanılmalıdır.

### 15.3 Attribute/metaprogramming

Canonical proposal artık `examples/papers/metaprogramming.cstar` içindedir. `syntax.cstar` eski metaprogramming denemelerini taşımaz; genel syntax vitrini olarak kalır.

Önerilen yön:

```cstar
attribute ExportJson for struct {
    $emit function to_json($item& value, StringBuilder& out) :: void {
        #for field in fields($item) {
            out.write(field.name);
            out.write(":");
            out.write(value.$field);
        }
        ret;
    }
}

@ExportJson
struct PlayerProfile {
    int32 id;
    int32 score;
}
```

Karar notu:

- `attribute`, trait/protocol yerine geçmez.
- Mevcut compiler `attribute Name for struct { ... }` ve `@Name(args)` formlarını parse eder ve metadata MVP'si olarak kabul eder. Duplicate attribute definition ve bilinmeyen annotation diagnostic üretir. Generated item emission henüz açılmadığı için `$emit` body şimdilik yürütülmez.
- Trait type capability contract, protocol typestate/state contract, attribute ise compile-time transformation/reflection alanıdır.
- Attribute item/type üzerinde metadata veya checked transform üretir.
- Reflection ilk aşamada sınırlıdır: `name($item)`, `fields($item)`, `methods($item)`, `has_attribute(...)`, `attribute_args(...)`, `sizeof(Type)` ve `alignof(Type)`.
- Field metadata `name`, `type`, `is_public`, qualifier bilgisi ve compile-time biliniyorsa `offset` taşır; method metadata ad, parametre tipleri, dönüş tipi ve `static` bilgisini taşır.
- Attribute private field'ı public yapmaz, layout'u gizlice değiştirmez, runtime metadata tablosu üretmez ve trait/protocol conformance'ı bypass etmez.
- Üretilen declaration'lar normal semantic/type-check pass'lerinden geçmelidir.
- İlk uygulanacak adım expansion değil, parser + kontrollü diagnostic olmalıdır.

### 15.4 Protocol / Trait / Struct

```cstar
protocol FileState for FileHandle {
    state closed, opened;

    default closed;

    closed -> opened :: open();
    opened -> closed :: close();

    read() :: !closed;
    write(const char* data) :: !closed;
    open(const char* path) :: !opened;

    scope_exit opened -> closed :: close();
}

dynamic protocol RetryState for Connection {
    state idle, retrying, failed;

    default idle;

    idle -> retrying :: send();
    retrying -> idle :: ack();
    retrying -> failed :: timeout();

    send(const char* packet) :: !failed;
}

trait CustomAlloc {}
struct String : Allocator with CustomAlloc {
}
```

Bu bölümün bir kısmı hâlâ tasarım/proposal seviyesindedir. Canonical örnekler `examples/papers/struct.cstar`, `allocator.cstar`, `nullability.cstar`, `trait.cstar` ve `protocol.cstar` dosyalarına ayrılmıştır. Mevcut parser `struct Name { field; ... }` MVP'sini, method/`self` MVP'sini, static struct method MVP'sini, local by-value constructor initializer MVP'sini, destructor/drop/scope-exit MVP'sini, `new`/`shared new` allocation operator MVP'sini, `new?`/`shared new?` nullable ownership yüzeyini, `T*?`/`T^?` nullability semantic'ini, `trait Name { ... }` ve `struct T with Trait` compile-time conformance yüzeyini ve unique/shared struct pointer üzerinden `ptr.field`, `ptr.method(args)` erişimini işler; `protocol`, `dynamic protocol`, `dynamic Trait` ve tam allocator failure/effect policy yüzeyi henüz gerçek lowering değildir.

Karar:

- `protocol` ana typestate/state-contract yönüdür.
- `protocol` default olarak static/provable kabul edilir; `static protocol` gereksizdir ve önerilen canonical syntax değildir.
- Runtime maliyeti isteniyorsa açıkça `dynamic protocol` yazılır.
- Eski `policy for T { on_null_reference() ... }` ve `policy protocol` yaklaşımları superseded kabul edilir. Bu modeller gizli runtime hook/dispatch/side-table çağrışımı veya gereksiz çift isim taşıdığı için ana grammar'a alınmayacak.
- `.=` yalnızca `dynamic protocol` veya flow analysis'in kanıtlayamadığı açık provability-gap durumları için anlamlıdır. Her protocol olayına gizli hook gibi davranmamalıdır.
- `.=` bugün token olarak tanınır ama codegen yoktur; controlled proposal diagnostic üretir.
- `--show-desugar`, `.=` dynamic state check'lerini ve protocol kaynaklı scope-exit cleanup edge'lerini gösterebilmelidir; protocol davranışı inspect edilemeyen hook table'a saklanmamalıdır.

Protocol amacı:

- User-defined state/typestate üretmek.
- `opened FileHandle^` ve `closed FileHandle^` gibi state'i type'ın parçası yapmak.
- Hataları mümkünse pass1'de compile-time yakalamak.
- Runtime fallback gerektiğinde maliyeti görünür ve desugar edilebilir tutmak.
- `scope_exit A -> B :: method();` normal `ret`, `throw`, `break`, `continue` gibi scope çıkışlarında aynı cleanup edge'i üretir. Fallible cleanup açık user code ile yapılmalı; scope-exit transition'ları effect sistemi geldiğinde cleanup-safe/noexcept kabul edilmelidir.
- Bir type birden fazla protocol state'i taşıyabilir; her protocol kendi named state slot'una sahiptir. Örneğin `opened locked FileHandle^` tek birleşik enum değil, `FileState == opened` ve `LockState == locked` bilgisidir.

Struct yönü:

- İlk MVP `struct Name { field; ... }` olarak başladı: primitive/by-value field layout, zero-init storage, `value.field` read/write, by-value parametre/return, nested field chain, method/`self` lowering ve local constructor initializer çalışır.
- Unique `^` ve shared `*` struct handle'larında `ptr.field` pointee alanına, `ptr.method(args)` implicit `self&` pointee adresine iner. `::` yalnız static type method çağrısıdır: `Type::method(args)`.
- By-value initialization için constructor ana yoldur: `Type value = Type(args);`. `new` heap allocation/control-block ve allocator seçimi için allocation operator'dır; by-value `new` method/factory kullanımı reddedilir.
- `destructor()` tanımlanabilir; user parametre ve explicit return type kabul etmez. Normal kullanımda by-value local struct için compiler return/scope-exit öncesi otomatik çağırır. Erken release için hedef yüzey `drop value;` olur; `drop` destructor'ı çağırıp değeri dropped state'e çeker ve sonraki kullanımı diagnostic yapar. `value.destructor()` normal kullanıcı yüzeyi değildir ve diagnostic üretir.
- Field layout doğrudan LLVM `StructType` ile temsil edilmeli; gizli reflection/layout metadata ilk MVP'ye girmemelidir.
- Method syntax `self` lowering ile çözüldü: `value.method(args)`, `owned.method(args)` ve `shared.method(args)` internal `StructName.method(self&, ...)` call olur. Static method self almaz ve `Type::method(args)` ile çağrılır.
- `constructor` local by-value initializer için çalışır. `new` allocation entry olarak unique/shared heap allocation, constructor call ve drop/scope-exit release hattına bağlanır. Explicit allocator değeri `Allocator` conformance ister; shared metadata/failure policy runtime contract'ı ileride sıkılaştırılacaktır.
- `new` yalnız struct/resource allocation değildir; tasarım yönü sized concrete storage allocation'dır. Bu yüzden primitive heap allocation C* için legal olmalıdır: `int32^ n = new int32(7);`, `float64* f = shared new float64(1.0);`. Primitive `new` constructor/destructor çağırmaz; allocate + initializer store + drop/release free hattına iner. Struct `new` ise constructor/destructor edge'lerini de üretir.
- `new T(args)` ve `shared new T(args)` default olarak non-null/infallible kabul edilmelidir. Allocation failure gizli null pointer üretmemelidir. Fallible allocation ayrı yüzey olmalıdır: önerilen form `new? T(args)` -> `T^?`, `shared new? T(args)` -> `T*?`. Bu yüzey `except`/`throw` veya result-like model olgunlaşınca ona bağlanabilir.
- `syntax.cstar` içindeki eski attribute/prototype denemeleri canonical proposal olmaktan çıkarıldı. Yeni yön `struct T with Trait` static conformance, attribute için ise `attribute Name for <kind>` compile-time transform modelidir.

Trait yönü:

- `trait`, runtime interface/vtable değildir; varsayılan yön compile-time capability contract ve static/monomorphized dispatch'tir.
- `struct T with TraitA, TraitB` conformance check üretir; eksik method controlled semantic diagnostic'tir.
- Trait, protocol ile karıştırılmamalıdır: trait “bu type ne yapabilir?”, protocol “bu değer hangi state içinde güvenli?” sorusunu cevaplar.

Allocator ve hata yönetimi:

- Custom allocator eski `policy for T` hook modeliyle değil, explicit language-item trait ve `constructor` yüzeyiyle ele alınmalıdır.
- Allocator capability isme göre seçilmez: compiler `#[lang(allocator)]` ile kayıtlı trait'i bulur, `new(allocator)` expression type'ının bu trait'e conform olmasını ister. `trait Allocator` adı tek başına özel değildir; `trait HeapDomain` gibi farklı isimli canonical allocator trait'i de `#[lang(allocator)]` ile mümkündür.
- `shared new(allocator) Type(args)` explicit allocator domain'ini hem payload storage hem de strong-count metadata için kullanır. Current MVP metadata'yı ayrı `alloc(sizeof(i64), alignof(i64))` ile alır ve son release'te payload ile birlikte allocator'a geri verir; compiler ileride aynı contract altında fused control-block layout üretebilir.
- Allocation failure için gizli policy hook veya sessiz null yerine `new?`, `except`/`throw` veya explicit result-like dönüş modeli tercih edilmelidir.
- Sistem programlama hedefi nedeniyle allocator, destructor ve shared control-block entegrasyonu thread-safe ve görünür maliyetli olmalıdır.

### 15.5 Macro / preprocess sistemi

Macro sistemi iki işi hedefler:

- Güvenli AST substitution/generation.
- Compile-time config, target ve feature flag'leriyle kod seçimi.

Önerilen yön:

```cstar
macro clamp($value: expr, $min: expr, $max: expr) -> expr {
    (($value) < ($min) ? ($min) : (($value) > ($max) ? ($max) : ($value)))
}

#if target.os == "windows" {
    import Sleep(uint32 ms) :: void from "kernel32";
} else {
    import usleep(uint32 usec) :: int32 from "c";
}

#if !feature("simd") {
    #warning "SIMD kapalı; scalar fallback kullanılacak"
}
```

Karar notu:

- Macro text replacement değil, typed AST substitution olmalıdır.
- Macro parametre kind'ları `expr`, `stmt`, `item`, `type`, `ident`, gerekirse `tokens` olarak ayrılmalıdır.
- Mevcut compiler macro declaration'larını parse öncesi toplar, `macroName(args)` çağrılarını token düzeyinde expand eder ve normal parser/type-check/codegen hattına verir. `expr`, `stmt`, `item` ve `type` macro return kind'ları smoke seviyesinde çalışır.
- Source include ile gelen `public macro` declaration'ları preprocess öncesi taranır ve include alias'ı üzerinden `alias.macroName(args)` olarak export edilir. Bu yüzey namespace/linkage runtime ABI değil, compile-time kaynak API'sidir.
- `#warning "message"` compile-time warning üretir; `#error "message"` derlemeyi durdurur.
- `#if true/false { ... } else { ... }`, `target.os` ve `target.arch` equality/inequality koşulları çalışır. `feature("x")` ve `cfg("x")` şimdilik false kabul edilir.
- Hijyenli source-map expansion, richer `#if` expression grammar ve compile-time config'in CLI/build sistemine bağlanması sonraki metaprogramming hardening işidir.
- Expansion hygienic olmalı; macro local isimleri caller scope'una sızmamalıdır.
- Diagnostic source-map ile çağrı yerini göstermelidir.
- Protocol gizli hook zinciri değil, typestate/state analizi olarak kalmalıdır.

### 15.6 Enum / explicit tagged layout

Enum inheritance yapmaz ve struct/prototype field'ı çekmez. C* için ana enum yönü C ABI'ye yakın, repr'ı açık scalar enum'dır. Scalar enum ve flags enum alt kümesi artık compiler MVP'si olarak çalışır:

```cstar
enum Color : uint8 {
    Red,
    Green,
    Blue,
}
```

Çalışan MVP'de repr zorunludur, member erişimi `Color.Green` formundadır, enum değerleri local/param/return tarafında repr integer storage'a iner. Scalar enum member'ları implicit incremental veya explicit integer literal alabilir. Repr sınırını aşan değerler, duplicate value ve unknown member controlled diagnostic üretir. Scalar enum'lar bitmask değildir; bu yüzden `Color.Red | Color.Blue` gibi bitwise kullanım reddedilir. Unqualified member erişimi henüz açılmadı; canonical form açık `Enum.Member` yazımıdır.

Flags enum scalar enum üzerine kurulan explicit bitmask yüzeyidir:

```cstar
flags enum FileMode : uint32 {
    None   = 0,
    Read   = 1,
    Write  = 2,
    Execute = 4,
    Create = 8,
}

open_flags() :: FileMode {
    ret FileMode.Read | FileMode.Create;
}

main() :: int32 {
    FileMode mode = open_flags();
    mode |= FileMode.Write;

    FileMode readable = mode & FileMode.Read;
    FileMode toggled = mode ^ FileMode.Write;

    ret 0;
}
```

Flags enum kuralları:

- Her member explicit değer alır; implicit incremental değer flags enum'da kapalıdır.
- Değer `0` veya power-of-two olmalıdır. `ReadWrite = 3` gibi birleşik alias ilk MVP'de diagnostic üretir; ileride açık alias policy'si ayrı tasarlanabilir.
- Duplicate member value reddedilir.
- `|`, `&`, `^`, `|=`, `&=`, `^=` legal; arithmetic operatörler legal değildir.
- Unary `~` repr-width complement üretir; declared flag union'a otomatik clamp yapmaz.
- `uint128` gibi full-width literal sınırları mevcut numeric literal storage genişletilince tekrar sıkılaştırılacaktır; bugünkü MVP member literal değerlerini `uint64_t` sınırında tutar.

Enum branch kontrolü için canonical yüzey `option` statement'ıdır. Bu yapı bugün expression değeri üretmez; branch gövdeleri normal statement scope'u olarak çalışır:

```cstar
enum TokenKind : uint8 {
    Identifier,
    Integer,
    End,
}

score(TokenKind kind) :: int32 {
    option(kind) {
        TokenKind.Identifier: {
            ret 1;
        },
        TokenKind.Integer: {
            ret 2;
        },
        TokenKind.End: {
            ret 0;
        }
    }
}
```

Kurallar:

- Pattern MVP yalnızca `Enum.Member` ve `_` default kabul eder.
- `_` yoksa bütün enum üyeleri yazılmalıdır; eksik üye diagnostic üretir.
- Aynı enum üyesi iki kez yazılamaz.
- Pattern enum type'ı, `option(...)` içindeki değerin enum type'ı ile aynı olmalıdır.
- Literal `0:`, range, guard ve payload destructuring henüz açılmadı; bu alanlar tagged/layout aşamasına bağlıdır.

Payload gereken durumlarda canonical yüzey implicit payload enum değil, explicit tagged layout'tur:

```cstar
enum TokenKind : uint8 {
    Identifier,
    Integer,
    End,
}

struct Token {
    TokenKind kind;
    const char* text;
    usize text_len;
    int64 int_value;
}
```

`tagged` syntax'ı bu explicit modeli okunabilir hale getiren sugar'dır; yine aynı `tag + explicit storage` layout'una inmeli, heap allocation, destructor veya generic result-like stdlib soyutlamalarını dil merkezine gizlice koymamalıdır:

```cstar
tagged Packet : uint8 {
    Ping {
        uint32 nonce;
    }

    Data {
        const char* bytes;
        usize len;
    }

    Close
}

packet_size(Packet& packet) :: usize {
    option(packet.tag) {
        Packet.Ping: {
            ret sizeof(uint32);
        },
        Packet.Data: {
            ret packet.Data.len;
        },
        Packet.Close: {
            ret 0;
        }
    }
}
```

### 15.7 Dynamic trait object proposal

`with Trait` static conformance'dır ve vtable üretmez. Runtime dispatch isteyen kod bunu açık ve uzun `dynamic Trait` ile ister. Kısa `dyn` alias'ı canonical değildir; sistem programlama dili için runtime maliyetin syntax'ta görünmesi tercih edilir. Mevcut compiler `dynamic Trait&`, `dynamic Trait*`, `dynamic Trait^` type grammar'ını ve named-value erase syntax'ını tanır; trait hedefini ve kaynak struct conformance'ını semantic olarak doğrular. Vtable ABI/dispatch lowering henüz uygulanmadığı için valid kullanım controlled `CST2001` diagnostic üretir:

```cstar
log_dynamic(dynamic Logger& logger, const char* message) :: void {
    logger.write(message);
    ret;
}

ConsoleLogger logger = ConsoleLogger();
dynamic Logger& erased = dynamic ref logger as Logger;
```

Representation contract:

- `dynamic Trait&`: borrowed trait object.
- `dynamic Trait*`: shared trait object; C* shared handle retain/release eder.
- `dynamic Trait^`: unique trait object; move-only sahiplik taşır.
- Temel ABI `{ data: void*, vtable: constptr TraitVTable* }`.
- `dynamic ref value as Trait`: value'nun type'ı `Trait` sağlıyorsa borrowed trait object üretir.
- `dynamic move value as Trait`: value'nun type'ı `Trait` sağlıyorsa unique trait object üretir ve source moved state'e geçmelidir.
- `unsafe_cast` trait object üretmez; vtable doğruluğu compiler sorumluluğudur.
- Public/export ABI'de repr explicit olmalıdır; C boundary için `void* data` + generated vtable pointer struct'ı ayrı yazılır.

### 15.8 Async/await/except/throw

Örnek tasarımda geçiyor:

```cstar
example6(readonly char* str) async :: any {
    bool success = await a_function_must_be_awaited();
}

example7(bool a_param) except :: int32 {
    throw;
}
```

Güncel karar:

- `async` function effect ve `await` expression lexer tarafından reserved keyword olarak tanınır.
- Parser bu yüzeyi bugün `CST1001` controlled proposal diagnostic ile durdurur; runtime scheduler/coroutine lowering henüz yoktur.
- Async/task boundary ownership kuralı function boundary ile aynı yöndedir: by-value `^` yalnızca açık `move` ile taşınabilir, plain copy yasaktır.
- Shared `*` task boundary'de atomic retain/copy veya açık `move` transfer kullanır.
- `Send`/`Sync` runtime vtable değildir; trait/capability marker olarak düşünülür ve scheduler lowering geldiğinde boundary check üretir.

Mevcut lexer/parser bu sistemi uygulamıyor.

### 15.9 Option/match/case

```cstar
enum Status : uint8 {
    Ready,
    Busy,
    Failed,
}

classify(Status s) :: int32 {
    option(s) {
        Status.Ready: {
            ret 1;
        },
        _: {
            ret 0;
        }
    }
}
```

Karar: canonical yüzey `option` statement'ıdır; ayrı `match` keyword'ü şimdilik eklenmez. `_` default branch'tir. İlk MVP expression döndürmez, yalnızca statement scope'ları çalıştırır. Çalışan MVP enum odaklıdır: pattern tarafı `Enum.Member` veya `_` olmalıdır. `_` kullanılmıyorsa enum exhaustive olmak zorundadır.

Bugünkü compiler parser/AST/semantic/codegen hattında `option(enum_value)` lowering üretir. Literal scalar/char/bool pattern, range pattern, guard, destructuring ve expression-valued option henüz yoktur. `examples/smoke/enums/option_enum_exhaustive.cstar` ve `examples/smoke/enums/option_enum_default.cstar` pozitif örneklerdir; `examples/type_checker/enums/085.cstar`, `086.cstar`, `087.cstar` exhaustiveness, type mismatch ve duplicate branch diagnostic'lerini doğrular.

## 16. Mevcut Dil İçin Kısa Cheat Sheet

Aşağıdaki syntax mevcut parser çekirdeğine en yakın güvenli alt kümedir:

```cstar
import puts(constptr char*) :: int;
include "modules/math_module.cstar" as math;

static int globalCounter = 0;

add(int a, int b) :: int {
    int result = a + b;
    ret result;
}

main(int argc, char** argv) :: int {
    int x = 10;
    int y = 20;
    int z = x + y * 2;

    int values[2:2] = ((0,1),(2,3));
    int32 item = math.add_from_module(2, 5);

    if (z > 10) {
        z += 1;
    } elif (z == 10) {
        z = 0;
    } else {
        z -= 1;
    }

    loop(v in values) {
        int copy = v;
    }

    ret z;
}
```

Codegen notu: Bu cheat sheet proposal tarafına biraz yakın durur. Bugün güvenle çalıştığı smoke ile doğrulanan alt küme; primitive local/global değişkenler, char/float primitive'leri, integer/float arithmetic, comparison/logical expression, prefix/postfix increment-decrement statement, scalar/dereference/tek ve çok boyutlu array assignment, çok boyutlu dynamic index, scalar enum repr storage/member constant, flags enum bitwise/unary `~` storage/member constant, enum `option(Enum.Member/_ )` statement ve exhaustiveness MVP'si, `ret expr`, primitive function call, `import/export/from` native/module declaration, local `.cstar` include, `public`/default-private module visibility MVP'si, module-level `static` function/variable MVP'si ve alias function lookup, struct declaration/zero-init/field read-write/nested field/by-value param-return/method-self/local-constructor/drop/by-value scope-exit destructor/unique-shared-pointer-field-method/instance-scope-method-alias/unique-shared-new-operator/value-operator MVP'si, trait declaration ve `struct with Trait` conformance MVP'si, explicit cast, unsafe integer/pointer cast MVP, pointer argümanı, primitive reference parametresi, pointer variable initializer, pointer return, pointer'dan pointer okuma, `print(...)`, `input_int()`, `input_string()`, `clear_screen()`, `flush_output()`, `sleep_ms(ms)`, `enable_raw_input()`, `disable_raw_input()`, `read_key()`, temel `if/elif/else`, while-style `loop`, range loop, array iterable loop, `break` ve `continue` akışıdır. Genel sequence iterable, gerçek namespace/type module sistemi, tagged enum payload layout/destructuring, operator index/generic overload resolution ve protocol/dynamic trait-object lowering hâlâ ayrı aşama gerektirir.

## 17. Bilinen Sorunlar ve Teknik Riskler

### 17.1 Parser bug/riskleri

- `include` local `.cstar` dosyalarını public declaration olarak merge eder ve alias function lookup çalışır. Gerçek namespace/type module sistemi ve public function body içinde private helper lowering'i henüz tamamlanmadı.
- Expression parser karmaşık state değişkenleriyle çalışıyor; nested generic/call/subscript/ternary kombinasyonları kırılgan.
- Parser hata durumlarında hâlâ çoğunlukla tek diagnostic sonrası çıkıyor; recoverable diagnostics ayrı tasarlanmalı.

### 17.2 Semantic riskleri

- User-defined type sistemi genişlemeye devam ediyor; struct/trait ve temel value operator MVP çalışır, operator index, generic trait bound, protocol flow analysis ve dynamic trait object ABI hâlâ eksiktir.
- Scalar enum ve flags enum MVP çalışır; repr overflow, duplicate value, scalar/flags bitwise ayrımı, flags enum unary `~` semantic'i ve `option(enum_value)` exhaustiveness diagnostic/smoke ile doğrulanır. Tagged payload layout/destructuring ve full-width `uint128` enum literal modeli hâlâ eksiktir.
- Array validation MVP'si boyut içindeki negatif index'i sondan indeksleme olarak normalize eder, sabit out-of-bounds index'i error yapar, initializer arity'yi doğrular ve aşırı büyük local stack array'leri reddeder; runtime bounds check ve slice doğrulaması sonraki safety/stdlib aşamasındadır.
- Scope ve symbol validation elle yönetilen id/level mekanizmasına bağlı.
- `move`/ownership modeli semantic pass ve shared handle codegen içinde çalışır; by-value function argument/return transfer MVP'si, `nomove` parametre kısıtı, drop/scope-exit destructor ve unique/shared `new` release hattı vardır. Explicit allocator kullanılan shared allocation'da payload ve strong-count metadata aynı allocator domain'indedir. Kalan büyük eksik gerçek async lowering ve allocation failure policy'sidir.
- `const`, `readonly`, primitive `constref` assignment reddi, primitive `constptr` pointer adresi reassignment reddi, primitive `readonly` pointer address/value assignment reddi, primitive `const` pointer target assignment reddi ve çok seviyeli qualifier pointer reddi type-checker negatif testleriyle doğrulanır. Ownership tarafı için davranış hâlâ ayrıntılı tasarım/uygulama ister.

### 17.3 Codegen riskleri

- User-defined/qualifier-aware cast ve fix/increment yolları tamamlanmadı.
- Pointer/ref/qualifier parametre codegen'i genişletilmeli; primitive pointer call, primitive reference parametresi, pointer initializer, pointer return ve dereference assignment MVP dışında genel model tamamlanmadı.
- Generated program çalıştırma artık CLI moduna ayrıldı. Varsayılan `cstar file.cstar` executable üretip durur; `--run` verilirse generated program çalıştırılır. Pass süreleri, Total LoC, output path ve generated exit code normal modda gizlenir; `--stats` ile gösterilir. Backend/link komutları normal modda gizlenir, `--verbose` ile gösterilir.

### 17.4 Build/taşınabilirlik riskleri

- MSVC tarafında gerçek LLVM kurulumu ile düzenli doğrulama yapılmalı.
- MSYS2 LLVM ile Visual Studio generator'ı karıştırılmamalı.
- CLI modları ayrıştırılıyor: `--emit=<ir|asm|obj|staticlib|dynamiclib|exe>`, `--emit-llvm`, `--emit-asm`, `--emit-staticlib`, `--emit-dynamiclib`, `--build-exe`, `--run`, `--no-run`, `--output-dir`, `--verbose` ve `--stats` desteklenir. Static library için object + `ar rcs`, dynamic library için backend clang `-shared` yolu kullanılır. İleri adım: object yolunu clang driver yerine LLVM `TargetMachine` üzerinden üretmek.

## 18. Devam Etmek İçin En Mantıklı Yol Haritası

Bu belge sadece inceleme amaçlıdır; yine de projeye devam edilecekse teknik olarak en güvenli sıra şu olur:

1. Minimum çalışan dil çekirdeğini smoke setiyle yeşil tut:
   - primitive declaration
   - arithmetic expression
   - `ret expr`
   - scalar assignment
   - function call
   - if/elif/else

2. Function/parameter sistemini genişlet:
   - pointer/ref/qualifier parametreler
   - import/native function call
   - array parametreleri

3. Memory model MVP'sini netleştir:
   - `ref x` / `deref p` smoke tamamlandı
   - pointer variable initializer tamamlandı
   - pointer return tamamlandı
   - dereference assignment tamamlandı
   - çok seviyeli dereference assignment smoke tamamlandı
   - `^` ownership pointer için karar belgesi

4. Cast ve array codegen boşluklarını kapat:
   - user-defined cast/conversion overload tasarımı
   - zengin qualifier-aware cast kuralları
   - runtime checked array/slice modeli

5. Büyük vizyonu sonra ele al:
   - struct/trait/protocol
   - package/import sistemi
   - attribute/directive/macro sistemi
   - static/local-static/member-static semantiği.

## 19. Sonuç

Bu eski proje tamamlanmamış olsa da ciddi bir dil tasarım enerjisi taşıyor. Özellikle pointer/reference/ownership qualifier tarafı, expression parser denemesi, type checker pass'leri ve LLVM hedefi net bir “C/C++ sonrası sistem dili” arayışını gösteriyor.

Bugünkü haliyle proje:

- Bir **lexer + parser + partial semantic analyzer + partial LLVM codegen** prototipi.
- Dilin gerçek çalışan kısmı küçük.
- Tasarım vizyonu mevcut implementation'dan çok daha büyük.
- Devam etmek için en kritik adım, hayal edilen tüm özelliklere dönmeden önce küçük ama uçtan uca çalışan bir C* çekirdeği çıkarmak.
