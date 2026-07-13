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
- `attribute`, `policy`, `trait`, directive/macro gibi daha ileri fikirler tasarlanmış, fakat mevcut compiler bunları derleyemiyor.

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
// expected: diagnostic (proposal/stress)
```

`expected-exit`, generated executable'ın process exit status değeridir. Yani `ret 7;` console'a `7` yazdırmaz; programın exit code'unu `7` yapar. Terminalde doğrudan `.exe` çalıştırıldığında Windows bu değeri ekrana basmaz, PowerShell tarafında `$LASTEXITCODE` ile görülür. Smoke runner bu değeri otomatik yakalar ve `[OK] ... (exit N)` şeklinde doğrular.

Güncel küçük çalışan çekirdek `examples/smoke/` altındadır. Bu set şu anda 55/55 başarılıdır:

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
- explicit cast edilmiş function argument
- `unsafe_cast<T>(expr)` ile integer -> pointer MVP
- tek boyutlu array element read/write: `arr[0]`, `arr[0] = value`, `arr[0] += value`
- function call, forward function call, call statement
- symbol argümanlı function call
- tek seviyeli pointer argümanlı function call: `read_ptr(ref x)` ve callee içinde `deref p`
- primitive reference parametre: `write_ref(ref value)`
- primitive `constref` parametre: `read_ref(ref value)` ve callee içinde salt-okunur değer okuma
- tek seviyeli pointer variable initializer: `int32* p = ref x`
- shared pointer atomic strong-count: `int32* q = p`, `q = p`, `q := p`, `strong_count(q)`
- unique pointer move-only semantik: `int32^ p = ref x`, `int32^ q := p`, `q := p`, `deref q = value`
- `constptr` pointer initializer ve dereference read/write: `constptr int32* p = ref x`, `deref p = value`
- `readonly` pointer initializer ve dereference read: `readonly int32* p = ref x`, `ret deref p`
- pointer return: `identity(int32* p) :: int32*`
- dereference assignment: `deref p = value`, `*p = value`
- dereference shortcut assignment: `deref p += value`
- çok seviyeli dereference read/write: `**pp`, `**pp = value`
- pointer'dan pointer okuma: `int32* q = deref pp`
- geçici `print(...)` builtin
- geçici `input_int()` builtin
- `if`, `if/else`, `if/elif/else`, nested if ve branch fallthrough
- `int`, `float` ve pointer condition conversion

`examples/type_checker/` seti şu anda 34/34 kontrollü diagnostic üretir; crash/assert beklenmez. Yeni negatif çekirdek testleri `const`/`readonly` assignment reddini, safe cast pointer/value kategori reddini, çıplak value ile reference parametre çağrısı reddini, `constref` parametreye assignment reddini, `constptr` pointer adresi reassignment reddini, `readonly` pointer address/value assignment reddini, `const int32*` target assignment reddini, çok seviyeli qualifier pointer reddini, invalid qualifier/type kombinasyonunu, `*`/`^` pointer marker karışımı reddini, unique pointer copy reddini, primitive `:=` reddini, moved-after-use reddini ve `.=` policy proposal diagnostic'ini kapsar.

`examples/functions/`, `examples/variables/` ve `examples/papers/` dizinleri hâlâ daha çok proposal/stres örnekleridir. Runner ile ayrı çalıştırılır; amaç hepsini bugün yeşil yapmak değil, dil geliştikçe buradan küçük MVP smoke'lar çıkarmaktır.

## 4. Lexer: Tanınan Token ve Anahtar Kelimeler

Lexer `include/lexer/lexer.hpp` içinde. Tek dosyada token enum'u, keyword classification ve tokenization bulunuyor.

### 4.1 Fiilen tanınan keyword'ler

`classifyIdents` içinde keyword'e dönüştürülen identifier'lar:

```text
ret, in, as,
if, elif, else,
ref, deref,
include, involved, option, loop, default,
from, import, export, static,
cast, unsafe_cast, sizeof, typeof, move,
const, constptr, constref, readonly,
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

`:=` token olarak `TYPEINF`, `.=` token olarak `POLICY_ASSIGN` şeklinde tanınıyor. C* tasarımında `:=` type inference değildir; unique ownership transfer intent'i için kullanılır. `.=` policy/member-safe assignment proposal'ıdır ve policy runtime semantics gelene kadar kontrollü parser diagnostic üretir.

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

Ancak mevcut semantic pass'te user-defined type table fiilen doldurulmuyor. `struct`, `trait`, `policy` gibi yapılar parse edilmediği için gerçek user-defined type desteği tamamlanmış değil.

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
- `^`: unique/ownership pointer. Doğrudan copy reddedilir; ownership transfer için `:=` veya `move` gerekir. Move edilen `*` ve `^` source yeniden initialize edilmeden kullanılamaz.
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

Güncel codegen notu: `ref x` shared pointer beklenen yerde `{ data=&x, strong=new atomic i64(1) }` handle'ı üretir. `int32* q = p` ve `q = p` atomic retain yapar; `q := p` ve `q = move p` transfer yapar ve source moved kabul edilir. `strong_count(p)` compiler builtin'i atomic strong-count değerini döndürür. Primitive pointer parametre/return ABI'si de bu shared handle'ı taşır; `read_ptr(ref x)`, `identity(int32* p) :: int32*`, `int32* q = deref pp`, `deref p = value`, `deref p += value`, `**pp` ve `**pp = value` smoke setinde çalışır. Primitive reference parametreler `int32& x` syntax'ı ile çağıranın storage'ına alias olur; çağrı tarafında açık `ref value` gerekir, fonksiyon gövdesinde `x` normal değer gibi okunur ve `x = value;` çağıranın değerini günceller. `const`/`constref`/`constptr`/`readonly` qualifier kontrolleri semantic pass'te korunur.

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

Visibility/storage:

```cstar
import externalFunc(int x) :: void;
export main() :: int32 { ret 0; }
static int globalCounter = 0;
```

Mevcut parser global değişkenlerde `import`, `export`, `static` işaretlerini kabul ediyor.

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

```cstar
int32 arr[2] = (1, 2);
arr[0] = 9;
arr[1] += 5;
ret arr[1];
```

Çok boyutlu array flatten/index semantics proposal tarafında durur; MVP'de ayrı aşama olarak ele alınacaktır.

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
```

`import` fonksiyonlar forward declaration gibi ele alınıyor ve body yerine `;` bekleniyor.

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
r .= a;  // proposal: policy/member-safe assignment, henüz codegen yok
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

Not: Scalar local assignment, dereference assignment ve tek boyutlu array element assignment artık smoke setinde doğrulanıyor. `arr[a:b]` gibi çok boyutlu/colon index assignment proposal tarafındadır.

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

- Range/iterable loop için bazı alloca ve array boyutu hesaplama denemeleri var.
- While-style conditional loop codegen'i tamamlanmış görünmüyor.
- `break` / `continue` lexer'da var ama parser statement olarak işlemiyor.

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

- `cast<T>(expr)` için primitive numerik dönüşüm ve pointer -> pointer MVP çalışır.
- `cast<T>(expr)` pointer/value kategori geçişini reddeder; örneğin pointer'ı safe cast ile integer'a çevirmek diagnostic üretir.
- `unsafe_cast<T>(expr)` integer -> pointer, pointer -> integer ve pointer -> pointer gibi daha gevşek dönüşümleri LLVM IR'a indirir.
- `expr as Type` syntax kararı ve codegen yolu henüz sonraki adımdadır.

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

`print(...)` şu an dirty MVP olarak CRT `printf` çağrısına indiriliyor. `input_int()` de aynı şekilde CRT `scanf("%lld", ...)` çağrısına indiriliyor ve `int64` döndürüyor. String literal kaçışları için temel `\n`, `\t`, `\"`, `\\` decode ediliyor. Gerçek stdlib/native interop tasarımı gelince bu bölüm yeniden ele alınmalı.

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
- `struct`, `trait`, `policy` parse edilmediği için gerçek custom type sistemi yok.
- User-defined type cast semantic'i yok.
- Qualifier-aware cast kuralları henüz yok.
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
- Dirty builtin `print(...)` -> CRT `printf`.
- Dirty builtin `input_int()` -> CRT `scanf`.
- Binary arithmetic:
  - add/sub/mul/div/mod
  - bitwise and/or/xor
  - logical and/or (`&&`, `||`)
  - shift
  - comparison (`<`, `<=`, `>`, `>=`, `==`, `!=`) sonucu `bool/i1`
  - ternary için `select`
- Array initializer için kısmi constant array / memcpy yaklaşımı.
- Tek boyutlu local/global array element read/write.
- `if/elif/else` basic block üretimi; nested/fallthrough senaryoları smoke ile doğrulanıyor.
- `cast<T>(expr)` ve `unsafe_cast<T>(expr)` MVP IR üretimi.
- Iterable array loop için kısmi basic block ve GEP denemesi.

### 14.2 Boş veya tamamlanmamış codegen parçaları

Aşağıdaki codegen alanları hâlâ boş veya tamamlanmamış kabul edilmelidir:

```cpp
ValuePtr Visitor::visit(TypeAST &typeAst) {}
ValuePtr Visitor::visit(FixAST &fixAst) {}
```

MVP executable davranışı artık küçük smoke seti için vardır; fakat proposal'ın tamamı hâlâ çok daha büyüktür.

Özellikle:

- User-defined type cast IR üretimi tamamlanmadı.
- Çok boyutlu array element assignment tamamlanmadı.
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

Mevcut parser `include` token'ını package mark olarak görüyor ve gerçek include/package grammar'ı henüz uygulanmadığı için kontrollü diagnostic üretiyor. Eski infinite loop riski giderildi; `examples/papers/syntax.cstar` bu davranışı doğrulayan proposal örneği olarak kalıyor.

### 15.2 Import/export from library

Örnek tasarım:

```cstar
import glClearColor(float r,float g,float b,float a) :: void from "opengl32.lib";
import from "myLibrary.lib" {
    printf(char*) :: void;
}

export sin(float64 x) :: float64 from "std:math";
```

Mevcut parser yalnızca basit `import func(...) :: type;` formuna yakın.

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

Lexer `attribute` tanıyor, parser `ATTRIB` için TODO bırakıyor.

### 15.4 Policy/trait/struct

```cstar
policy for String {
    on_null_reference() :: void {
        rterror("...");
    }
}

trait CustomAlloc {}
struct String : Allocator with CustomAlloc {
}
```

Bunlar tasarım/proposal seviyesinde. Mevcut parser `policy`, `trait`, `struct` keyword'lerini bile gerçek grammar olarak işlemiyor. `.=` policy/member-safe assignment token'ı tanınır, fakat policy runtime ve thread-safe hook modeli netleşene kadar kontrollü proposal diagnostic üretir.

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

Lexer `#`, `$` token'larını tanıyor; `@` unhandled. Parser directive/macro tarafında tamamlanmamış.

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

Mevcut lexer/parser bu sistemi uygulamıyor.

### 15.7 Option/match/case

```cstar
option(k) {
    0: { },
    1: { },
    _: { }
}
```

`option` ve `_` token düzeyinde var, parser statement olarak işlemiyor.

## 16. Mevcut Dil İçin Kısa Cheat Sheet

Aşağıdaki syntax mevcut parser çekirdeğine en yakın güvenli alt kümedir:

```cstar
import puts(constptr char*) :: int;

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

Codegen notu: Bu cheat sheet proposal tarafına biraz yakın durur. Bugün güvenle çalıştığı smoke ile doğrulanan alt küme; primitive local/global değişkenler, char/float primitive'leri, integer/float arithmetic, comparison/logical expression, scalar/dereference/tek boyutlu array assignment, `ret expr`, primitive function call, explicit cast, unsafe integer/pointer cast MVP, pointer argümanı, primitive reference parametresi, pointer variable initializer, pointer return, pointer'dan pointer okuma, `print(...)`, `input_int()` ve temel `if/elif/else` akışıdır. Loop, çok boyutlu array ve qualifier-heavy bölümler hâlâ ayrı MVP adımı gerektirir.

## 17. Bilinen Sorunlar ve Teknik Riskler

### 17.1 Parser bug/riskleri

- `include` gerçek package sistemi olarak uygulanmadı; ancak parser artık bu syntax için kontrollü diagnostic üretir.
- `initializerList()` fonksiyonu dönüş tipi `ASTNode` olmasına rağmen bazı yollarda return etmiyor.
- Expression parser karmaşık state değişkenleriyle çalışıyor; nested generic/call/subscript/ternary kombinasyonları kırılgan.
- Parser hata durumlarında hâlâ çoğunlukla tek diagnostic sonrası çıkıyor; recoverable diagnostics ayrı tasarlanmalı.

### 17.2 Semantic riskleri

- User-defined type sistemi tamamlanmamış.
- Array validation içinde FIXME var.
- Scope ve symbol validation elle yönetilen id/level mekanizmasına bağlı.
- `move`/ownership modeli semantic pass ve shared handle codegen içinde çalışır; kalan büyük eksik scope çıkışı/destructor lowering ve allocator/new control-block entegrasyonudur.
- `const`, `readonly`, primitive `constref` assignment reddi, primitive `constptr` pointer adresi reassignment reddi, primitive `readonly` pointer address/value assignment reddi, primitive `const` pointer target assignment reddi ve çok seviyeli qualifier pointer reddi type-checker negatif testleriyle doğrulanır. Ownership tarafı için davranış hâlâ ayrıntılı tasarım/uygulama ister.

### 17.3 Codegen riskleri

- User-defined/qualifier-aware cast, fix/increment ve çok boyutlu array assignment yolları tamamlanmadı.
- Pointer/ref/qualifier parametre codegen'i genişletilmeli; primitive pointer call, primitive reference parametresi, pointer initializer, pointer return ve dereference assignment MVP dışında genel model tamamlanmadı.
- Generated program çalıştırma artık CLI moduna ayrıldı. Varsayılan `cstar file.cstar` executable üretip durur; `--run` verilirse generated program çalıştırılır. Pass süreleri, Total LoC, output path ve generated exit code normal modda gizlenir; `--stats` ile gösterilir. Backend/link komutları normal modda gizlenir, `--verbose` ile gösterilir.

### 17.4 Build/taşınabilirlik riskleri

- MSVC tarafında gerçek LLVM kurulumu ile düzenli doğrulama yapılmalı.
- MSYS2 LLVM ile Visual Studio generator'ı karıştırılmamalı.
- CLI modları ayrıştırılıyor: `--emit=<ir|asm|obj|exe>`, `--emit-llvm`, `--emit-asm`, `--build-exe`, `--run`, `--no-run`, `--output-dir`, `--verbose` ve `--stats` desteklenir. İleri adım: `--emit=obj` yolunu clang driver yerine LLVM `TargetMachine` üzerinden üretmek.

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
   - `expr as T`
   - user-defined/qualifier-aware cast
   - çok boyutlu array indexing/read/write

5. Büyük vizyonu sonra ele al:
   - struct/trait/policy
   - package/import sistemi
   - attribute/directive/macro sistemi
   - ownership/move modelinin gerçek kuralları.

## 19. Sonuç

Bu eski proje tamamlanmamış olsa da ciddi bir dil tasarım enerjisi taşıyor. Özellikle pointer/reference/ownership qualifier tarafı, expression parser denemesi, type checker pass'leri ve LLVM hedefi net bir “C/C++ sonrası sistem dili” arayışını gösteriyor.

Bugünkü haliyle proje:

- Bir **lexer + parser + partial semantic analyzer + partial LLVM codegen** prototipi.
- Dilin gerçek çalışan kısmı küçük.
- Tasarım vizyonu mevcut implementation'dan çok daha büyük.
- Devam etmek için en kritik adım, hayal edilen tüm özelliklere dönmeden önce küçük ama uçtan uca çalışan bir C* çekirdeği çıkarmak.
