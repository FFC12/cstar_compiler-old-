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
- `struct` MVP'si başlamıştır; `attribute`, `protocol`, `trait`, directive/macro gibi daha ileri fikirler tasarlanmış, fakat mevcut compiler bunları henüz gerçek lowering olarak derleyemiyor.

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

Güncel küçük çalışan çekirdek `examples/smoke/` altındadır. Bu set şu anda module helper dosyaları hariç 98/98 başarılıdır:

- minimal program ve `ret expr`
- `ret;` kullanan void fonksiyon çağrısı
- local primitive variable declaration
- global mutable primitive variable
- uninitialized local primitive zero-init
- `char`, `float32`, `float64` primitive smoke'ları
- fractional float literal: `0.5`, `4.5`
- bool literal
- integer ve floating point arithmetic
- comparison expression: `<`, `<=`, `>`, `>=`, `==`, `!=`
- logical expression: `&&`, `||`
- scalar assignment
- `const` scalar read ve `const int32*` target/value ayrımı
- assignment type cast ve data-loss warning
- explicit `cast<T>(expr)` numerik dönüşüm
- `expr as T` safe cast syntax'ı
- explicit cast edilmiş function argument
- `unsafe_cast<T>(expr)` ile integer -> pointer MVP
- tek boyutlu array element read/write: `arr[0]`, `arr[0] = value`, `arr[0] += value`
- tek boyutlu array parametre read/write: `read_second(int32[2] arr)`, `write_first(int32[2] arr)`
- çok boyutlu array read/write/shortcut assignment: `int32 matrix[2:3] = ((1, 2, 3), (4, 5, 6))`, `matrix[1:2]`, `matrix[row:col]`
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
- geçici `clear_screen()`, `flush_output()`, `sleep_ms(ms)`, `enable_raw_input()`, `disable_raw_input()` ve `read_key()` konsol builtin'leri
- native import/export:
  - `import abs(int32) :: int32;`
  - `import abs(int32 value) :: int32;`
  - `import abs(int32 value) :: int32 from "std:crt";`
  - `import { ... }`
  - `import from "std:crt" { ... }`
  - `export from "std:math" { ... }`
- local module include, `public` visibility ve alias function lookup: `include "modules/math_module.cstar" as math`, `math.add_from_module(2, 5)`
- public static module state/function smoke: `examples/smoke/include_module_public_static.cstar`
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

`examples/type_checker/` seti şu anda 59/59 kontrollü diagnostic üretir; crash/assert beklenmez. `// expected-code: CSTNNNN` etiketi varsa runner beklenen diagnostic kodunu da doğrular. Yeni negatif çekirdek testleri `const`/`readonly` assignment reddini, safe cast pointer/value kategori reddini, safe cast qualifier stripping reddini, user-defined cast controlled diagnostic'ini, çıplak value ile reference parametre çağrısı reddini, `constref` parametreye assignment reddini, `constptr` parametre/pointer adresi reassignment reddini, `readonly` parametre/pointer address/value assignment reddini, array parametreye scalar/farklı boyutlu array geçişi reddini, `const int32*` target assignment reddini, çok seviyeli qualifier pointer reddini, invalid qualifier/type kombinasyonunu, `*`/`^` pointer marker karışımı reddini, unique pointer copy reddini, primitive `:=` reddini, function arg/return ownership transfer ihlallerini, `nomove` ownership-flow ihlallerini, `async`/`await` proposal diagnostic'ini, moved-after-use reddini, `.=` protocol proposal diagnostic'ini, loop dışı `break`/`continue` reddini, `option` proposal diagnostic'ini, include edilen module içindeki private function erişimi reddini, `static` function içinden non-static global/function erişimi reddini, struct duplicate/unknown field diagnostic'lerini, direct self-by-value struct field reddini ve unknown struct method reddini kapsar.

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
cast, unsafe_cast, sizeof, typeof, move, async, await,
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
attribute, prototype, enum,
break, continue,
nil, true, false
```

Lexer enum'unda `NATIVE`, `MATRIX`, `VECTOR`, `EXTERN`, `FOR` gibi token'lar da var; ancak bunların keyword classification tarafında tam karşılığı yok ya da parser tarafından kullanılmıyor.

Yeni proposal keyword'leri (`protocol`, `dynamic`, `state`, `struct`, `trait`, `attribute`, `macro`, `with`, `constructor`, `destructor`, `allocator`, `except`, `throw`, `defer`, `self`, `is`) lexer tarafından tanınır. Parser `struct Name { field; ... }` MVP'sini gerçek grammar olarak işler; diğer ileri proposal keyword'leri top-level kullanımlarda controlled proposal diagnostic üretir. Eski `policy` kelimesi yeni canonical grammar'da reserved değildir.

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

Mevcut semantic pass'te `struct Name { field; ... }` MVP'si için user-defined type table doldurulur. Primitive ve by-value user-defined field layout LLVM `StructType` olarak üretilir, zero-init struct variable oluşturulur, `value.field` read/write syntax'ı çalışır, nested field chain `line.start.x` GEP zinciriyle iner ve by-value struct parametre/return desteklenir. Struct method MVP'sinde methodlar internal olarak `StructName.method(self&, ...)` fonksiyonuna iner; `value.method(args)` receiver'ı implicit `self` argümanı yapar ve `self.field` read/write referans üzerinden caller storage'ına iner. Direct self-by-value field reddedilir. `trait`, `protocol`, constructor/destructor ve lifetime lowering henüz tamamlanmış değildir.

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

Güncel codegen notu: `ref x` shared pointer beklenen yerde `{ data=&x, strong=new atomic i64(1) }` handle'ı üretir. `int32* q = p` ve `q = p` atomic retain yapar; `q := p` ve `q = move p` transfer yapar ve source moved kabul edilir. Shared pointer by-value function argument plain symbol ile retain/copy yapar; `move` argument/return source'u null'a çekilir. `strong_count(p)` compiler builtin'i atomic strong-count değerini döndürür. Primitive pointer parametre/return ABI'si de bu shared handle'ı taşır; `read_ptr(ref x)`, `identity(int32* p) :: int32*`, `int32* q = deref pp`, `deref p = value`, `deref p += value`, `nomove int32^ p`, `**pp` ve `**pp = value` smoke setinde çalışır. Primitive reference parametreler `int32& x` syntax'ı ile çağıranın storage'ına alias olur; çağrı tarafında açık `ref value` gerekir, fonksiyon gövdesinde `x` normal değer gibi okunur ve `x = value;` çağıranın değerini günceller. `const`/`constref`/`constptr`/`readonly` qualifier kontrolleri semantic pass'te korunur.

### 6.3 Qualifier'lar

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

Mevcut parser module-level function ve variable deklarasyonlarında `public`, `private`, `static`, `import` ve `export` modifier'larını kabul eder.

Visibility/linkage ayrımı:

- `public`, include edilen local module'den dışarı açılan declaration'ı seçer.
- Modifier yazılmayan declaration default olarak private kabul edilir.
- `private` açıkça yazılabilir ama default ile aynıdır.
- `import`/`export` visibility değildir; native/linkage ABI sınırı için kullanılır.

`static` kararı:

- Global scope'ta `static`, internal linkage/storage duration anlamı taşır.
- Static function LLVM tarafında internal linkage alır.
- Static function yalnızca static global state'e ve static function'lara erişebilir; non-static global symbol/function kullanımı semantic diagnostic üretir.
- Local static, static member, init-order ve thread-safe one-time initialization henüz yoktur.
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

Çok boyutlu initializer kaynakta katmanlı yazılır; codegen bunu C benzeri row-major flat belleğe indirir. `[rows:cols]` için lineer index `row * cols + col`; daha yüksek boyutta soldan sağa `linear = linear * next_dim + index` kuralı uygulanır. Sabit out-of-range index compile-time warning üretir; dinamik index için runtime bounds check henüz üretilmez.

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
```

`import` fonksiyonlar forward declaration gibi ele alınıyor ve body yerine `;` bekleniyor. Parametre adı ABI için opsiyoneldir; `import abs(int32) :: int32;` ve `import abs(int32 value) :: int32;` aynı external imzaya iner. Basit native import çağrısı smoke suite içinde gerçek libc `abs` çağrısıyla doğrulanır. `from "lib"`, `import { ... }` ve `import from "lib" { ... }` formları parser/codegen akışına bağlıdır.

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
Codegen bugün sabit boyutlu array'lerde `arr[i]` ve `arr[i:j:k]` formunu row-major lineer index'e indirir. Dinamik index expression'ları da desteklenir; runtime bounds check henüz yoktur.

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
 - Array initializer validation için çok boyutlu tarafta FIXME var.

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
- Binary arithmetic:
  - add/sub/mul/div/mod
  - bitwise and/or/xor
  - logical and/or (`&&`, `||`)
  - shift
  - comparison (`<`, `<=`, `>`, `>=`, `==`, `!=`) sonucu `bool/i1`
  - ternary için `select`
- Array initializer için kısmi constant array / memcpy yaklaşımı.
- Tek ve çok boyutlu local/global array element read/write.
- `if/elif/else` basic block üretimi; nested/fallthrough senaryoları smoke ile doğrulanıyor.
- While-style `loop`, `break` ve `continue` basic block üretimi smoke ile doğrulanıyor.
- `cast<T>(expr)`, `expr as T` ve `unsafe_cast<T>(expr)` MVP IR üretimi.
- Iterable array loop için kısmi basic block ve GEP denemesi.

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

`examples/papers/syntax.cstar` ve `policy.cstar` mevcut compiler'dan daha ileri bir C* vizyonu gösteriyor.

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

Compiler artık `include involved { ... }`, `include { ... }` ve `include "module" as alias` formlarını gerçek grammar olarak parse eder. `include` hedefi `.cstar` ile biten yerel bir dosyaysa dosya ana compilation unit'e parse/merge edilir; böylece başka dosyadaki function deklarasyonları çağrılabilir.

```cstar
include "modules/math_module.cstar" as math

main() :: int32 {
    ret add_from_module(2, 5);
}
```

`as` alias'ı function call lookup için çalışır; `math.add_from_module(...)` gibi alias member çağrısı include edilen module içindeki `public add_from_module` imzasına çözülür. Include edilen local module yalnızca `public` deklarasyonları ve gerekli native `import` forward deklarasyonlarını dışarı açar. Modifier yazılmayan declaration private kabul edilir ve alias üzerinden çağrılamaz; `examples/type_checker/052.cstar` bunu doğrular.

### 15.2 Import/export from library

Örnek tasarım:

```cstar
import glClearColor(float r,float g,float b,float a) :: void from "opengl32.lib";
import from "myLibrary.lib" {
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
- `"opengl32.lib"`, `.a`, `.so`, `.dylib` ve path içeren değerler doğrudan linker argümanı olarak geçer.
- `"std:math"` ve `"std:crt"` gibi `:` içeren logical module adları linker argümanına çevrilmez; bunlar package/module namespace tasarımı için saklanır.

Executable üretirken ana dosyada `export` deklarasyonu varsa compiler warning üretir; çünkü `export` library ABI görünürlüğüdür. Dış modüller için symbol üretilecekse `--emit=staticlib` veya `--emit=dynamiclib` kullanılmalıdır.

### 15.3 Attribute/metaprogramming

```cstar
attribute Area<T> {
    area($0,$1) -> T match {
        'Triangle': $0 * $1 / 2,
        'Square': $0 * $1,
        _: cterror("no this is not valid type")
    }
}
```

Lexer `attribute` tanıyor. Parser bu yüzeyi henüz gerçek grammar olarak işlemez; top-level kullanımlarda controlled proposal diagnostic üretir.

Karar notu:

- `attribute`, trait/protocol yerine geçmez.
- Trait type capability contract, protocol typestate/state contract, attribute ise compile-time transformation/reflection alanı olarak düşünülmelidir.
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

    scope_exit :: closed;
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

Bu bölümün büyük kısmı hâlâ tasarım/proposal seviyesindedir. Mevcut parser `struct Name { field; ... }` MVP'sini işler; `protocol`, `trait`, `dynamic protocol`, constructor/destructor ve allocator yüzeyi henüz gerçek lowering değildir.

Karar:

- `protocol` ana typestate/state-contract yönüdür.
- `protocol` default olarak static/provable kabul edilir; `static protocol` gereksizdir ve önerilen canonical syntax değildir.
- Runtime maliyeti isteniyorsa açıkça `dynamic protocol` yazılır.
- Eski `policy for T { on_null_reference() ... }` ve `policy protocol` yaklaşımları superseded kabul edilir. Bu modeller gizli runtime hook/dispatch/side-table çağrışımı veya gereksiz çift isim taşıdığı için ana grammar'a alınmayacak.
- `.=` yalnızca `dynamic protocol` veya flow analysis'in kanıtlayamadığı açık provability-gap durumları için anlamlıdır. Her protocol olayına gizli hook gibi davranmamalıdır.
- `.=` bugün token olarak tanınır ama codegen yoktur; controlled proposal diagnostic üretir.

Protocol amacı:

- User-defined state/typestate üretmek.
- `opened FileHandle^` ve `closed FileHandle^` gibi state'i type'ın parçası yapmak.
- Hataları mümkünse pass1'de compile-time yakalamak.
- Runtime fallback gerektiğinde maliyeti görünür ve desugar edilebilir tutmak.

Struct yönü:

- İlk MVP `struct Name { field; ... }` olarak başladı: primitive/by-value field layout, zero-init storage, `value.field` read/write, by-value parametre/return, nested field chain ve method/`self` lowering çalışır.
- Constructor/destructor ve pointer/shared field access sonraki struct adımıdır.
- Field layout doğrudan LLVM `StructType` ile temsil edilmeli; gizli reflection/layout metadata ilk MVP'ye girmemelidir.
- Method syntax `self` lowering ile çözüldü: `value.method(args)` internal `StructName.method(self&, ...)` call olur. İlk MVP sadece value receiver içindir; pointer/shared receiver kararları ayrıca yapılacaktır.
- `constructor` ve `destructor` explicit lifetime noktalarıdır; shared `*`, unique `^`, allocator ve scope-exit release ile aynı modelde düşünülmelidir.
- `syntax.cstar` içindeki `struct Shape<T> from Area<T>` fikri attribute/type capability bağlama proposal'ıdır. İlk parser MVP'si bunu controlled diagnostic olarak tutmalı; gerçek field/method struct desteği önce gelmelidir.

Trait yönü:

- `trait`, runtime interface/vtable değildir; varsayılan yön compile-time capability contract ve static/monomorphized dispatch'tir.
- `struct T with TraitA, TraitB` gibi bir yüzey conformance check üretmelidir.
- Trait, protocol ile karıştırılmamalıdır: trait “bu type ne yapabilir?”, protocol “bu değer hangi state içinde güvenli?” sorusunu cevaplar.

Allocator ve hata yönetimi:

- Custom allocator eski `policy for T` hook modeliyle değil, explicit `allocator`/`trait`/`constructor` yüzeyiyle ele alınmalıdır.
- Allocation failure için gizli policy hook yerine `except`/`throw` veya explicit result-like dönüş modeli tercih edilmelidir.
- Sistem programlama hedefi nedeniyle allocator, destructor ve shared control-block entegrasyonu thread-safe ve görünür maliyetli olmalıdır.

### 15.5 Directive/macro sistemi

Örnekler:

```cstar
@compiler_hook()
__greater_than_hook() :: void {
}

#this_function_may_throw_an_error(bool a_param) {
    $func(a_param)::onexcept {
    };
}
```

Lexer `#`, `$` ve `macro` keyword'ünü tanıyor; `@` unhandled. Parser directive/macro tarafında tamamlanmamış.

Karar notu:

- Macro/directive sistemi eski `policy for T` runtime hook modelinin yerine geçirilmemeli.
- Macro/directive expansion görünür, kaynak konumu izlenebilir ve debug edilebilir olmalı.
- Protocol gizli hook zinciri değil, typestate/state analizi olarak kalmalı.

### 15.6 Async/await/except/throw

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

### 15.7 Option/match/case

```cstar
option (k) {
    0: { ret 0; },
    1: { ret 1; },
    _: { ret -1; }
}
```

Karar: canonical yüzey `option` statement'ıdır; ayrı `match` keyword'ü şimdilik eklenmez. `_` default branch'tir. İlk MVP expression döndürmez, yalnızca statement scope'ları çalıştırır. Pattern tarafı önce scalar literal/char/bool ve `_` ile sınırlı tutulmalıdır; destructuring, range pattern ve exhaustiveness daha sonra `enum`/`struct` sistemiyle birlikte ele alınır.

Mevcut compiler `option` keyword'ünü function body içinde görürse controlled `CST1001` proposal diagnostic üretir. Böylece parser takılmaz; gerçek AST/codegen henüz yoktur.

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

Codegen notu: Bu cheat sheet proposal tarafına biraz yakın durur. Bugün güvenle çalıştığı smoke ile doğrulanan alt küme; primitive local/global değişkenler, char/float primitive'leri, integer/float arithmetic, comparison/logical expression, scalar/dereference/tek ve çok boyutlu array assignment, çok boyutlu dynamic index, `ret expr`, primitive function call, `import/export/from` native/module declaration, local `.cstar` include, `public`/default-private module visibility MVP'si, module-level `static` function/variable MVP'si ve alias function lookup, struct declaration/zero-init/field read-write/nested field/by-value param-return/method-self MVP'si, explicit cast, unsafe integer/pointer cast MVP, pointer argümanı, primitive reference parametresi, pointer variable initializer, pointer return, pointer'dan pointer okuma, `print(...)`, `input_int()`, `input_string()`, `clear_screen()`, `flush_output()`, `sleep_ms(ms)`, `enable_raw_input()`, `disable_raw_input()`, `read_key()`, temel `if/elif/else`, while-style `loop`, range loop, array iterable loop, `break` ve `continue` akışıdır. Genel sequence iterable, gerçek namespace/type module sistemi ve trait/protocol lowering hâlâ ayrı aşama gerektirir.

## 17. Bilinen Sorunlar ve Teknik Riskler

### 17.1 Parser bug/riskleri

- `include` local `.cstar` dosyalarını public declaration olarak merge eder ve alias function lookup çalışır. Gerçek namespace/type module sistemi ve public function body içinde private helper lowering'i henüz tamamlanmadı.
- Expression parser karmaşık state değişkenleriyle çalışıyor; nested generic/call/subscript/ternary kombinasyonları kırılgan.
- Parser hata durumlarında hâlâ çoğunlukla tek diagnostic sonrası çıkıyor; recoverable diagnostics ayrı tasarlanmalı.

### 17.2 Semantic riskleri

- User-defined type sistemi tamamlanmamış.
- Array validation MVP'si sabit index warning'i üretir; runtime bounds check ve slice doğrulaması sonraki safety/stdlib aşamasındadır.
- Scope ve symbol validation elle yönetilen id/level mekanizmasına bağlı.
- `move`/ownership modeli semantic pass ve shared handle codegen içinde çalışır; by-value function argument/return transfer MVP'si, `nomove` parametre kısıtı ve async/task boundary tasarım kararı vardır. Kalan büyük eksik scope çıkışı/destructor lowering, gerçek async lowering ve allocator/new control-block entegrasyonudur.
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
