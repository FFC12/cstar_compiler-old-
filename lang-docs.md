# C* Dil Kitabı ve Cookbook

Bu belge C* dilinin mevcut compiler yüzeyini, örneklerini, negatif testlerini ve proposal dosyalarını birlikte okuyarak hazırlanmış Türkçe bir başvuru dokümanıdır.

Kapsam iki ayrı katmanda anlatılır:

- **Bugün çalışan C\***: `examples/smoke/`, `examples/type_checker/`, `std/core.cstar` ve compiler kaynak kodunda doğrulanan özellikler.
- **Proposal / gelecek tasarımı**: `examples/papers/*.cstar` altında tasarlanan ama henüz tam lowering veya semantic desteği olmayan özellikler.

C* şu anda pre-alpha bir sistem programlama dili denemesidir. Dilin ana karakteri C benzeri düşük seviye kontrol, LLVM tabanlı native codegen, explicit pointer/ownership modeli, değer odaklı `struct`, static trait conformance ve güvenli semantic diagnostic üretme hedefidir.

## İçindekiler

- [1. Dil Felsefesi](#1-dil-felsefesi)
- [2. Dosya ve Çalıştırma Modeli](#2-dosya-ve-çalıştırma-modeli)
- [3. Program Yapısı](#3-program-yapısı)
- [4. Tip Sistemi](#4-tip-sistemi)
- [5. Değişkenler ve Atama](#5-değişkenler-ve-atama)
- [6. İfadeler ve Operatörler](#6-ifadeler-ve-operatörler)
- [7. Fonksiyonlar](#7-fonksiyonlar)
- [8. Kontrol Akışı](#8-kontrol-akışı)
- [9. Diziler](#9-diziler)
- [10. Pointer, Ref, Deref ve Qualifier Modeli](#10-pointer-ref-deref-ve-qualifier-modeli)
- [11. Ownership: Shared ve Unique Pointer](#11-ownership-shared-ve-unique-pointer)
- [12. String Literal ve C ABI](#12-string-literal-ve-c-abi)
- [13. Native Import, Include ve Modül Görünürlüğü](#13-native-import-include-ve-modül-görünürlüğü)
- [14. Runtime ve `std/core.cstar`](#14-runtime-ve-stdcorecstar)
- [15. Enum, Flags Enum ve Option](#15-enum-flags-enum-ve-option)
- [16. Struct, Constructor, Destructor ve Drop](#16-struct-constructor-destructor-ve-drop)
- [17. Trait ve Value Operator](#17-trait-ve-value-operator)
- [18. Diagnostic Rehberi](#18-diagnostic-rehberi)
- [19. Cookbook](#19-cookbook)
- [20. Proposal Haritası](#20-proposal-haritası)
- [21. Test ve Geliştirme Akışı](#21-test-ve-geliştirme-akışı)

## 1. Dil Felsefesi

C* sistem programlama tarafında konumlanır. Amaç, C/C++ ailesindeki düşük seviye kontrolü korurken bazı maliyetli hataları dil yüzeyinde görünür yapmaktır.

Ana kararlar:

- Bellek ve lifetime açık olmalıdır.
- Pointer kullanımı gizlenmez, ama qualifier ve ownership kurallarıyla daha denetlenebilir hale getirilir.
- `struct` gerçek layout sahibidir; inheritance yoktur, composition vardır.
- `constructor` allocation değildir; var olan storage'ı initialize eder.
- `destructor` free değildir; object lifetime kapanışıdır.
- `new` compiler-reserved allocation operator'dür; kullanıcı `operator new` yazmaz.
- Trait varsayılan olarak static conformance'tır; runtime dispatch ancak future `dynamic Trait` yüzeyiyle explicit olacaktır.
- Proposal özellikler compiler'da sessizce yanlış kabul edilmez; çoğu kontrollü diagnostic üretir.

Bu nedenle C*'ta "sihirli runtime hook" fikri yerine, mümkün olduğunca ABI-visible ve inspect edilebilir lowering tercih edilir.

## 2. Dosya ve Çalıştırma Modeli

Compiler CMake ile build edilir. Windows tarafında `build-ucrt/cstar.exe`, Linux/macOS tarafında CMake build output'u kullanılır.

Temel komutlar:

```powershell
.\build.bat
.\tools\run_examples.bat --suite smoke
.\tools\run_examples.bat --suite type_checker --expect-diagnostics
```

Tek dosya çalıştırma:

```powershell
.\build-ucrt\cstar.exe .\examples\smoke\core\minimal.cstar --run
```

`--run` derlenen programı çalıştırır. `ret 7;` console'a `7` yazdırmaz; process exit code'u `7` olur. PowerShell'de doğrudan görmek için:

```powershell
$LASTEXITCODE
```

`--stats` verilirse pass süreleri, LOC ve backend aşamaları görünür:

```powershell
.\build-ucrt\cstar.exe .\examples\smoke\core\operator_precedence_stress.cstar --run --stats
```

Test dosyalarının başında beklenti etiketi bulunur:

```cstar
// expected: pass
// expected-exit: 42
```

Diagnostic beklenen dosyalarda:

```cstar
// expected: diagnostic
// expected-code: CST2001
```

## 3. Program Yapısı

En küçük C* programı:

```cstar
// expected: pass
// expected-exit: 0

main() :: int32 {
  ret 0;
}
```

Fonksiyon dönüş tipi `::` ile yazılır. Dönüş tipi yazılmazsa dil yüzeyi bunu `void` fonksiyon gibi ele alır:

```cstar
log_done() :: void {
  ret;
}
```

`void` fonksiyonda değer döndürmek diagnostic üretir:

```cstar
main() :: void {
  ret 0; // hata: void function cannot return a value
}
```

Top-level sıralama:

- Global değişkenler tanımlanabilir.
- Fonksiyonlar tanımlanabilir.
- `struct`, `trait`, `enum` tanımları kullanılabilir.
- `include`, `import`, `export` ifadeleri top-level'da kullanılır.

## 4. Tip Sistemi

### 4.1 Primitive Tipler

Çalışan primitive tip yüzeyi:

```text
int8   int16   int32   int64   int
uint8  uint16  uint32  uint64  uint128  uint
isize  usize
float32  float64  float
char  uchar
bool
void
```

Örnek:

```cstar
main() :: int32 {
  int32 a = 5;
  int64 b = 123456789;
  float32 x = 1.5;
  float64 y = x + 2.25;
  char c = 'A';
  bool ok = true;

  if (ok && y > 3.0) {
    ret cast<int32>(c);
  }

  ret a;
}
```

### 4.2 Tanımlı Tipler

`struct`, `enum` ve `trait` üzerinden tanımlı tipler oluşur.

```cstar
enum Color : uint8 {
  Red,
  Green,
  Blue = 7,
}

struct Point {
  int32 x;
  int32 y;
}
```

### 4.3 Cast Kuralları

C* iki cast yüzeyi taşır:

```cstar
int32 a = cast<int32>(4.9);
int32 b = 5 as int32;
```

`cast<T>(expr)` ve `expr as T` safe cast kategorisindedir. Pointer ve value kategorilerini geçmek safe cast ile yasaktır:

```cstar
main() :: int32 {
  int32 value = 7;
  int32* p = ref value;
  int64 raw = cast<int64>(p); // diagnostic

  ret 0;
}
```

Bu tür kategori geçişleri için `unsafe_cast<T>` gerekir:

```cstar
main() :: int32 {
  int64 raw = 0;
  int32* p = unsafe_cast<int32*>(raw);
  ret 0;
}
```

`unsafe_cast` adı bilinçlidir: ABI, layout ve geçerlilik sorumluluğu programcıdadır.

## 5. Değişkenler ve Atama

### 5.1 Local Değişkenler

```cstar
main() :: int32 {
  int32 x = 7;
  int32 y;

  y = x;
  ret y;
}
```

Uninitialized local primitive değerler bugün MVP'de zero-init davranışı alır. Bu karar ileride daha sıkı bir definite-assignment analizine dönüşebilir.

### 5.2 Global Değişkenler

```cstar
int32 global_count = 7;

main() :: int32 {
  ret global_count;
}
```

### 5.3 Assignment Operatörleri

Desteklenen scalar assignment yüzeyi:

```cstar
x = 1;
x += 2;
x -= 1;
x *= 3;
x /= 2;
x %= 5;
x <<= 1;
x >>= 1;
x &= 3;
x |= 4;
x ^= 2;
```

Unique pointer move assignment için `:=` kullanılır:

```cstar
main() :: int32 {
  int32 first = 3;
  int32 second = 9;
  int32^ target = ref first;
  int32^ source = ref second;

  target := source;

  ret deref target;
}
```

Primitive scalar için `:=` kullanılmaz; bu ownership transfer operatörüdür.

## 6. İfadeler ve Operatörler

C* expression parser precedence tablosuna dayanır. Aşağıdaki örnek geniş operatör yüzeyini aynı dosyada test eder:

```cstar
main() :: int32 {
  int32 a = 5;
  int32 b = 3;
  int32 c = 2;

  int32 tick = 1;
  tick++;
  ++tick;
  tick--;
  --tick;

  int32 arith = ((+a + b * c) - ((a - c) / b) + (a % c));
  int32 bitmix = ((5 & 3) | (8 ^ 2)) + ((16 >> 2) + (1 << 3));
  int32 inverted = ~0;
  int32 narrowed = cast<int32>(4.9) + (5 as int32);

  bool logic = !false && ((arith == 11) && (bitmix >= 23)) && !(a < b) &&
               ((a != c) || false);

  int32 selected = logic ? (arith + bitmix + narrowed - tick) : 100;
  int32* pointer = ref selected;
  int32 through_pointer = pointer ? deref pointer : 0;

  ret through_pointer;
}
```

### 6.1 Aritmetik

```cstar
int32 x = 10 + 3 * 2;
int32 y = (10 - 4) / 3;
int32 z = 10 % 3;
```

Integer ve float arithmetic codegen desteklenir.

### 6.2 Unary Operatörler

```cstar
int32 a = +5;
int32 b = -a;
bool c = !false;
int32 d = ~0;
```

Kurallar:

- Unary `+` yalnız numeric operand kabul eder.
- Unary `-` yalnız numeric operand kabul eder.
- `!` condition-like değerleri bool'a indirir.
- `~` yalnız integer veya flags enum üzerinde geçerlidir.
- `~1.5` diagnostic üretir.
- `+pointer` diagnostic üretir.

### 6.3 Karşılaştırma ve Logical

```cstar
bool a = 5 < 7;
bool b = 5 <= 5;
bool c = 7 > 3;
bool d = 7 >= 7;
bool e = 7 == 7;
bool f = 7 != 8;
bool ok = a && b || !f;
```

Pointer karşılaştırmaları equality ile sınırlıdır. Ordered comparison (`<`, `>`, `<=`, `>=`) pointer için reddedilir. Bool değerler de ordered comparison'da numeric gibi kabul edilmez:

```cstar
bool a = true;
bool b = false;
bool c = a > b; // diagnostic
```

### 6.4 Ternary

```cstar
main() :: int32 {
  bool pick_left = false;
  int32 left = 7;
  int32 right = 12;
  int32 result = pick_left ? left : right;

  ret result;
}
```

Ternary kuralları:

- Condition bool/int/float/pointer condition olarak değerlendirilebilir.
- Branch'ler value üretmelidir.
- Branch tipleri compatible olmalıdır.
- Bugünkü MVP'de ternary branch expression side-effect-free olmalıdır; function call, assignment veya allocation gibi yan etkili branch'ler için `if/else` kullanılır.

Pointer condition ve deref:

```cstar
main() :: int32 {
  int32 value = 9;
  int32* p = ref value;
  int32 out = p ? deref p : 0;
  ret out;
}
```

## 7. Fonksiyonlar

### 7.1 Basit Fonksiyon

```cstar
add(int32 a, int32 b) :: int32 {
  ret a + b;
}

main() :: int32 {
  ret add(2, 5);
}
```

Forward function call smoke'ları çalışır; compiler pass'leri fonksiyon imzalarını önceden görebilir.

### 7.2 Call Statement

```cstar
touch() :: void {
  ret;
}

main() :: int32 {
  touch();
  ret 0;
}
```

### 7.3 Parametre Cast Politikası

C* parametre declarator sırasını semantic niyet için kullanır.

```cstar
castable(bool y) :: int32 {
  int32 b = y; // legal implicit primitive conversion
  ret b;
}

non_castable(x bool) :: int32 {
  int32 a = x; // diagnostic: symbol castable değil
  ret a;
}
```

`x bool` biçimi aynı primitive tipi taşır, fakat sembolün scope içinde implicit cast edilmesini yasaklayan bir kısıt olarak kullanılır.

## 8. Kontrol Akışı

### 8.1 If / Elif / Else

```cstar
main() :: int32 {
  int32 x = 7;

  if (x < 3) {
    ret 1;
  } elif (x < 10) {
    ret 2;
  } else {
    ret 3;
  }
}
```

Condition dönüşümleri:

- `bool`: doğrudan.
- Integer: sıfır değilse true.
- Float: sıfır değilse true.
- Pointer: null-like değilse true.

### 8.2 Loop

While-style loop:

```cstar
main() :: int32 {
  int32 i = 0;
  int32 sum = 0;

  loop (i < 5) {
    sum += i;
    i++;
  }

  ret sum;
}
```

Range loop:

```cstar
main() :: int32 {
  int32 sum = 0;

  loop (i in [1, 4]) {
    sum += i;
  }

  ret sum;
}
```

Array iterable loop:

```cstar
main() :: int32 {
  int32 values[3] = (2, 4, 6);
  int32 sum = 0;

  loop (index, value in values) {
    sum += value + index;
  }

  ret sum;
}
```

`break` ve `continue` sadece loop içinde geçerlidir; loop dışında diagnostic üretir.

## 9. Diziler

### 9.1 Tek Boyutlu Array

```cstar
main() :: int32 {
  int32 values[3] = (1, 2, 3);
  values[0] = 5;
  values[1] += 2;
  ret values[0] + values[1] + values[2];
}
```

Initializer arity kontrol edilir:

```cstar
int32 values[3] = (1, 2);    // diagnostic
int32 other[2] = (1, 2, 3);  // diagnostic
```

### 9.2 Çok Boyutlu Array

Çok boyutlu array dimension ayırıcı olarak `:` kullanır:

```cstar
main() :: int32 {
  int32 matrix[2:3] = ((1, 2, 3),
                       (4, 5, 6));
  ret matrix[1:2];
}
```

C/C++ tarzı `matrix[1][2]` yerine C* canonical notasyonu `matrix[1:2]` şeklindedir.

### 9.3 Negatif Index

C* subscript içinde negatif index'i sondan erişim olarak destekler:

```cstar
main() :: int32 {
  int32 values[3] = (10, 20, 30);
  ret values[-1]; // 30
}
```

Çok boyutlu array:

```cstar
main() :: int32 {
  int32 matrix[2:3] = ((1, 2, 3),
                       (4, 5, 6));
  ret matrix[-1:-1]; // 6
}
```

Sabit out-of-bounds index diagnostic üretir:

```cstar
int32 values[2] = (1, 2);
ret values[-3]; // diagnostic
```

Array dimension içinde negatif değer kullanımı ayrı konudur; dimension compile-time pozitif integer literal olmalıdır.

### 9.4 Array Parametreleri

```cstar
read_second(int32[2] values) :: int32 {
  ret values[1];
}

main() :: int32 {
  int32 local[2] = (4, 9);
  ret read_second(local);
}
```

Parametre array boyutu imzanın parçasıdır. Farklı boyutlu array geçirmek diagnostic üretir.

### 9.5 Stack Guard

Büyük local array'ler stack taşmasını önlemek için diagnostic üretir:

```cstar
main() :: int32 {
  int64 big[200000] = (0); // diagnostic
  ret 0;
}
```

Bu alan future allocator-backed buffer modeliyle genişleyecektir.

## 10. Pointer, Ref, Deref ve Qualifier Modeli

C* pointer yüzeyi:

- `T*`: shared/non-owning ABI pointer çizgisi, compiler tarafında shared count MVP'si de kullanılır.
- `T^`: unique pointer, move-only ownership.
- `T&`: reference/borrow.
- `ref x`: adres/referans alma.
- `deref p` veya `*p`: dereference.

### 10.1 Basit Pointer

```cstar
main() :: int32 {
  int32 value = 7;
  int32* pointer = ref value;
  ret deref pointer;
}
```

Assignment:

```cstar
main() :: int32 {
  int32 value = 1;
  int32* pointer = ref value;

  deref pointer = 9;
  deref pointer += 1;

  ret value;
}
```

Çok seviyeli pointer:

```cstar
main() :: int32 {
  int32 value = 3;
  int32* p = ref value;
  int32** pp = ref p;

  **pp = 7;
  ret deref p;
}
```

### 10.2 Reference Parametre

```cstar
write_ref(int32& value) :: void {
  value = 9;
  ret;
}

main() :: int32 {
  int32 x = 1;
  write_ref(ref x);
  ret x;
}
```

### 10.3 Qualifier'lar

`const`:

```cstar
main() :: int32 {
  const int32 value = 7;
  ret value;
}
```

`constref`:

```cstar
read_ref(constref int32& value) :: int32 {
  ret value;
}

main() :: int32 {
  int32 x = 7;
  ret read_ref(ref x);
}
```

`constptr` pointer adresini sabitler; target write serbest olabilir:

```cstar
write_target(constptr int32* p) :: void {
  deref p = 9;
  ret;
}
```

`readonly` pointer ve target üzerinde salt-okunur akış ister:

```cstar
main() :: int32 {
  int32 value = 7;
  readonly int32* pointer = ref value;
  ret deref pointer;
}
```

Readonly target'a yazmak diagnostic üretir.

## 11. Ownership: Shared ve Unique Pointer

### 11.1 Shared Pointer `*`

Bugünkü compiler `T*` için bazı smoke'larda strong count davranışını takip eder.

```cstar
main() :: int32 {
  int32 value = 5;
  int32* first = ref value;
  int32* second = first;

  ret strong_count(first); // 2
}
```

Function arg by-value shared pointer retain/copy davranışıyla test edilir.

### 11.2 Unique Pointer `^`

Unique pointer copy edilemez; move gerekir.

```cstar
main() :: int32 {
  int32 first = 3;
  int32 second = 9;
  int32^ target = ref first;
  int32^ source = ref second;

  target := source;

  ret deref target;
}
```

Function arg transfer:

```cstar
take(int32^ owned) :: int32 {
  ret deref owned;
}

main() :: int32 {
  int32 value = 7;
  int32^ owned = ref value;
  ret take(move owned);
}
```

`owned` move edildikten sonra yeniden kullanılamaz.

### 11.3 `nomove`

`nomove` unique pointer parametresinin taşınmasını engeller:

```cstar
inspect(nomove int32^ owned) :: int32 {
  ret deref owned;
}
```

Bu, protocol state değildir; sadece ownership-flow modifier'dır.

## 12. String Literal ve C ABI

String literal C ABI yüzeyinde immutable non-owning pointer gibi ele alınır.

Legal:

```cstar
main() :: int32 {
  const char* text = "hello";
  readonly char* ro = "world";
  print(text);
  ret 0;
}
```

Legal parametre:

```cstar
length_hint(const char* text) :: int32 {
  ret 1;
}

main() :: int32 {
  ret length_hint("hello");
}
```

Illegal:

```cstar
main() :: int32 {
  char* text = "mutable";       // diagnostic
  constptr char* p = "wrong";   // diagnostic
  char^ owned = "owned";        // diagnostic
  ret 0;
}
```

Neden?

- Literal storage mutable değildir.
- Literal ownership iddiası taşımaz.
- `char^` unique ownership gerektirir; string literal ise compiler/global storage üstünden gelir.
- `constptr char*` pointer'ın kendisini sabitler ama target'ı immutable yapmaz; string literal için güvenli hedef `const char*` veya `readonly char*` olmalıdır.

## 13. Native Import, Include ve Modül Görünürlüğü

### 13.1 Native Import

Tek fonksiyon import:

```cstar
import abs(int32 value) :: int32 from "std:crt";

main() :: int32 {
  ret abs(-5);
}
```

Block import:

```cstar
import from "std:crt" {
  printf(const char* fmt, ...) :: int32;
}

main() :: int32 {
  printf("value=%d\n", cast<int32>(7));
  ret 0;
}
```

Variadic native import desteklenir. C* içinde variadic function body yazmak henüz yoktur; bu yüzey C ABI çağrısı içindir.

### 13.2 Include ve Alias

```cstar
include "../modules/public_static_module.cstar" as cfg

main() :: int32 {
  ret cfg.double_limit();
}
```

Modül erişiminde `public` önemlidir. Include edilen dosyadaki public function/static state dışarıdan alias ile çağrılabilir; private/default yüzey dışarı açılmaz.

Future hedef:

- Public function yanında public struct, trait ve enum gibi global havuz nesneleri de alias üzerinden type pozisyonunda erişilebilir.
- `mod.Type` bugünkü source-merge mimarisinde public type'ın gerçek adına çözülür; bu nedenle constructor/member erişiminde mevcut MVP hâlâ unqualified public type adını kullanır.
- Private top-level type alias erişimi `Unknown type` diagnostic'i üretir.
- Include edilen public struct içinde field default private kabul edilir; module dışından yalnız `public` field okunup yazılabilir.
- Struct'ın kendi method'ları private field'lara erişebilir; public method private state'i güvenli bir API arkasında saklayabilir.
- Instance olmadan `StructType.field` erişimi geçersizdir. Instance method çağrısı da `StructType.method()` ile yapılamaz; static method için canonical yazım `StructType::method()` kalır.
- `public static` dışarı açılabilir static sembol; sadece `static` default-private kabul edilmelidir.

```cstar
include "../modules/public_types_module.cstar" as types

struct LocalReader with types.ModuleReadable {
  int32 value;

  constructor(int32 initial) {
    self.value = initial;
  }

  read :: int32 {
    ret self.value;
  }
}

take_point(types.ModulePoint point) :: int32 {
  ret point.sum() + point.x + point.y;
}

main() :: int32 {
  types.ModulePoint point = ModulePoint(7, 5);
  types.ModuleColor color = ModuleColor.Green;
  LocalReader reader = LocalReader(4);

  if (color == ModuleColor.Green) {
    ret take_point(point) + reader.read() + 3;
  }

  ret 1;
}
```

Private module field ve instance'sız member erişimi diagnostic üretir:

```cstar
include "../modules/public_types_module.cstar" as types

main() :: int32 {
  types.ModulePoint point = ModulePoint(7, 5);
  ret point.cachedHash; // diagnostic: private module field
}
```

```cstar
struct Point {
  public int32 x;
}

main() :: int32 {
  ret Point.x; // diagnostic: struct type instance değildir
}
```

### 13.3 Export

```cstar
export from "std:math" {
  abs(int32 value) :: int32;
}
```

Export/import tarafı native ABI yüzeyiyle ilişkilidir; bugün MVP seviyesindedir.

## 14. Runtime ve `std/core.cstar`

Compiler geçici builtin'ler sağlar:

```cstar
print("hello\n");
int64 value = input_int();
char* text = input_string();
clear_screen();
flush_output();
sleep_ms(16);
enable_raw_input();
int32 key = read_key();
disable_raw_input();
```

Ancak daha düzenli kullanım için `std/core.cstar` önerilir:

```cstar
include "../../std/core.cstar" as core

main() :: int32 {
  core.core_print("score=");
  core.core_print_i32(7);
  core.core_print("\n");

  int32 value = core.core_read_i32();
  ret value;
}
```

`std/core.cstar` bugün şunları sunar:

- `core_print(const char* text)`
- `core_println(const char* text)`
- `core_print_char(int32 ch)`
- `core_print_i32(int32 value)`
- `core_print_i64(int64 value)`
- `core_print_f64(float64 value)`
- `core_read_i32()`
- `core_read_i64()`
- `core_read_string()`

`core_read_string()` geçici olarak static `char[256]` buffer kullanır. Gerçek string type ve allocator-backed dynamic buffer geldiğinde burası değişecektir.

## 15. Enum, Flags Enum ve Option

### 15.1 Scalar Enum

```cstar
enum Color : uint8 {
  Red,
  Green,
  Blue = 7,
}

main() :: int32 {
  Color color = Color.Green;
  if (color == Color.Green) {
    ret 1;
  }
  ret 0;
}
```

Kurallar:

- Explicit repr zorunludur.
- Member erişimi `Enum.Member` biçimindedir.
- Unknown member diagnostic üretir.
- Farklı enum tipleri karıştırılamaz.

### 15.2 Flags Enum

```cstar
flags enum FileMode : uint32 {
  Read = 1,
  Write = 2,
  Append = 4,
}

main() :: int32 {
  FileMode mode = FileMode.Read | FileMode.Write;
  FileMode inverted = ~mode;

  if ((mode & FileMode.Write) == FileMode.Write) {
    ret 1;
  }

  ret 0;
}
```

Kurallar:

- Flags enum member değerleri explicit olmalıdır.
- Değerler `0` veya power-of-two olmalıdır.
- `|`, `&`, `^`, `~` desteklenir.
- Scalar enum üzerinde bitwise operator diagnostic üretir.

### 15.3 Option

Enum üzerinde pattern benzeri branch:

```cstar
enum TokenKind : uint8 {
  Identifier,
  Integer,
  End,
}

score(TokenKind kind) :: int32 {
  option (kind) {
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

  ret 9;
}
```

Default branch için `_` kullanılabilir:

```cstar
option (kind) {
  TokenKind.Identifier: {
    ret 1;
  },
  _: {
    ret 0;
  }
}
```

Compiler enum option exhaustiveness, duplicate branch ve type mismatch durumlarını diagnostic ile yakalar.

## 16. Struct, Constructor, Destructor ve Drop

### 16.1 Struct Layout

```cstar
struct Point {
  int32 x;
  int32 y;
}

main() :: int32 {
  Point p;
  p.x = 2;
  p.y = 3;
  ret p.x + p.y;
}
```

Struct by-value storage gerçek layout taşır. Direct self-by-value recursive field yasaktır:

```cstar
struct Node {
  Node next; // diagnostic
}
```

Pointer ile recursive yapı future allocator/lifetime kurallarıyla anlamlıdır.

### 16.2 Method ve `self`

```cstar
struct Point {
  int32 x;
  int32 y;

  length() :: int32 {
    ret self.x + self.y;
  }

  move_x(int32 dx) :: void {
    self.x += dx;
    ret;
  }
}

main() :: int32 {
  Point p;
  p.x = 2;
  p.y = 3;
  p.move_x(4);
  ret p.length();
}
```

No-param method sugar:

```cstar
struct Counter {
  int32 value;

  read :: int32 {
    ret self.value;
  }
}
```

### 16.3 Constructor

Constructor allocation yapmaz; var olan storage'ı initialize eder.

```cstar
struct Meter {
  int32 value;

  constructor(int32 initial) {
    self.value = initial;
    ret;
  }
}

main() :: int32 {
  Meter meter = Meter(7);
  ret meter.value;
}
```

Constructor yoksa `Meter()` initializer diagnostic üretir.

### 16.4 Destructor ve Drop

```cstar
struct Meter {
  int32 value;

  constructor(int32 initial) {
    self.value = initial;
    ret;
  }

  destructor() {
    self.value = 0;
    ret;
  }
}

main() :: int32 {
  Meter meter = Meter(7);
  int32 before = meter.value;
  drop meter;

  ret before;
}
```

`drop value;` explicit early release'dir. Dropped value tekrar kullanılınca diagnostic üretir.

Destructor doğrudan çağrılmaz:

```cstar
resource.destructor(); // diagnostic
```

`drop resource;` kullanılır.

### 16.5 New

`new` compiler-reserved allocation operator'dür:

```cstar
struct Meter {
  int32 value;

  constructor(int32 initial) {
    self.value = initial;
    ret;
  }

  destructor() {
    ret;
  }
}

main() :: int32 {
  Meter^ meter = new Meter(9);
  int32 value = meter.value;
  drop meter;
  ret value;
}
```

Shared allocation:

```cstar
Meter* shared_meter = shared new Meter(7);
```

Explicit allocator MVP/proposal yüzeyi:

```cstar
Meter^ meter = new(arena) Meter(7);
```

Kullanıcı `new()` method yazmaz; `operator new` da compiler-reserved'dür.

### 16.6 Struct Pointer Receiver

Unique/shared struct pointer üzerinden field/method erişimi MVP'de desteklenir:

```cstar
Meter^ owned = new Meter(9);
int32 value = owned.value;
drop owned;
```

## 17. Trait ve Value Operator

### 17.1 Static Trait Conformance

```cstar
trait Measurable {
  value() :: int32;
}

struct Meter with Measurable {
  int32 raw;

  value() :: int32 {
    ret self.raw;
  }
}
```

Trait inheritance değildir:

- Field inject etmez.
- Runtime dispatch oluşturmaz.
- `with Trait` static conformance check yapar.

Eksik method diagnostic üretir.

### 17.2 Value Operator Overloading

Struct value semantics için operator tanımlanabilir:

```cstar
struct Rank {
  int32 value;

  constructor(int32 value) {
    self.value = value;
  }

  operator ==(Rank rhs) :: bool {
    ret self.value == rhs.value;
  }

  operator <(Rank rhs) :: bool {
    ret self.value < rhs.value;
  }
}

main() :: int32 {
  Rank low = Rank(2);
  Rank high = Rank(7);

  if (low < high) {
    ret 1;
  }

  ret 0;
}
```

Desteklenen value operator ailesi:

- Statement düzeyinde `++x`, `x++`, `--x`, `x--` scalar değişkenlerde çalışır.
- User-defined value operator desteği `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `<=`, `>` ve `>=` ailesine odaklanır.
- `++` ve `--` için user-defined overload yoktur; bunlar değişken mutasyonu olarak ele alınır.
- Lifecycle ve ownership davranışı operator overload ile değiştirilmez.

Lifecycle ve ownership operatorleri compiler-reserved'dür:

```text
new, delete, move, copy, drop
```

## 18. Diagnostic Rehberi

Compiler diagnostic formatı dosya, satır, sütun, severity, kod ve caret marker içerir.

Örnek:

```text
examples/type_checker/core/unary_plus_pointer.cstar:7:14 error[CST2001]:
Unary '+' requires a numeric operand

7 |   int32 y = +p;
  |              ^
```

Sık diagnostic kategorileri:

- `CST1001`: parser/syntax/proposal kontrollü hata.
- `CST2001`: genel semantic/type-check hatası.
- `CST2100`: qualifier/string literal gibi daha özel semantic kategori.
- `CST2101`: ownership/qualifier özel akış hataları.
- `CST2105`: lifecycle/drop/destructor hataları.

Diagnostic yazarken hedef:

- Assert/crash değil, controlled diagnostic.
- Hata yanlış yerde değil, hatayı üreten expression/symbol üstünde olmalı.
- Proposal özelliği sessizce parse edilmemeli; "implemented değil" diagnostic vermeli.

## 19. Cookbook

Bu bölüm küçük problemler için doğrudan kullanılabilir örnekler verir.

### 19.1 Exit Code ile Program Sonucu Döndür

```cstar
main() :: int32 {
  ret 42;
}
```

Runner bunu `expected-exit: 42` ile doğrular.

### 19.2 Console'a Yazdır

```cstar
main() :: int32 {
  print("hello C*\n");
  ret 0;
}
```

Daha düzenli std/core:

```cstar
include "../../std/core.cstar" as core

main() :: int32 {
  core.core_println("hello C*");
  core.core_print_i32(7);
  core.core_print("\n");
  ret 0;
}
```

### 19.3 Kullanıcıdan Sayı Oku

```cstar
include "../../std/core.cstar" as core

main() :: int32 {
  core.core_print("number: ");
  int32 value = core.core_read_i32();
  ret value;
}
```

### 19.4 Basit Hesap Makinesi Çekirdeği

```cstar
include "../../std/core.cstar" as core

main() :: int32 {
  core.core_print("a: ");
  int32 a = core.core_read_i32();

  core.core_print("b: ");
  int32 b = core.core_read_i32();

  int32 sum = a + b;
  core.core_print("sum=");
  core.core_print_i32(sum);
  core.core_print("\n");

  ret sum;
}
```

### 19.5 Array Son Elemanını Oku

```cstar
main() :: int32 {
  int32 values[4] = (3, 5, 7, 9);
  ret values[-1];
}
```

### 19.6 Matrix Üzerinde Dinamik Index

```cstar
main() :: int32 {
  int32 matrix[2:3] = ((1, 2, 3),
                       (4, 5, 6));
  int32 row = 1;
  int32 col = -1;
  ret matrix[row:col];
}
```

### 19.7 Pointer ile Out Parametre

```cstar
write_value(int32& out) :: void {
  out = 12;
  ret;
}

main() :: int32 {
  int32 value = 0;
  write_value(ref value);
  ret value;
}
```

### 19.8 Readonly Pointer ile Güvenli Okuma

```cstar
read_value(readonly int32* p) :: int32 {
  ret deref p;
}

main() :: int32 {
  int32 value = 7;
  ret read_value(ref value);
}
```

### 19.9 Unique Pointer Transfer Et

```cstar
consume(int32^ owned) :: int32 {
  ret deref owned;
}

main() :: int32 {
  int32 value = 9;
  int32^ owned = ref value;
  ret consume(move owned);
}
```

### 19.10 Struct ile Küçük Domain Modeli

```cstar
struct Score {
  int32 value;

  constructor(int32 initial) {
    self.value = initial;
    ret;
  }

  add(int32 delta) :: void {
    self.value += delta;
    ret;
  }

  read() :: int32 {
    ret self.value;
  }
}

main() :: int32 {
  Score score = Score(10);
  score.add(5);
  ret score.read();
}
```

### 19.11 Enum ile Durum Seç

```cstar
enum Mode : uint8 {
  Idle,
  Run,
  Stop,
}

score(Mode mode) :: int32 {
  option (mode) {
    Mode.Idle: {
      ret 0;
    },
    Mode.Run: {
      ret 1;
    },
    _: {
      ret 2;
    }
  }

  ret 9;
}
```

### 19.12 Native `printf` Çağır

```cstar
import from "std:crt" {
  printf(const char* fmt, ...) :: int32;
}

main() :: int32 {
  printf("value=%d long=%lld float=%f\n",
         cast<int32>(7),
         cast<int64>(99),
         2.5);
  ret 0;
}
```

### 19.13 Modül Include Et

```cstar
include "../modules/math_module.cstar" as math

main() :: int32 {
  ret math.add_from_module(2, 5);
}
```

Include edilen sembol public değilse alias dışından erişim diagnostic üretmelidir.

### 19.14 Diagnostic Test Yaz

```cstar
// expected: diagnostic
// expected-code: CST2001

main() :: int32 {
  int32 x = 1;
  int32* p = ref x;
  int32 y = +p;

  ret y;
}
```

### 19.15 Smoke Test Yaz

```cstar
// expected: pass
// expected-exit: 9

main() :: int32 {
  int32 value = 9;
  ret value;
}
```

## 20. Proposal Haritası

Bu bölüm gelecekteki tasarımı özetler. Buradaki kodların tamamı bugün compiler tarafından çalıştırılmak zorunda değildir; çoğu `examples/papers/*.cstar` altında `expected: diagnostic (proposal)` olarak tutulur.

### 20.1 Nullability

Plan:

- `T*`, `T^`, `T&` non-null by default.
- Nullable pointer explicit `?`: `T*?`, `T^?`.
- `T&` nullable değildir.
- `nil` yalnız nullable pointer context içinde legal.
- `if (p)` nullable pointer'ı true branch içinde non-null'a narrow eder.

Örnek proposal:

```cstar
read_nullable(int32*? maybe) :: int32 {
  if (maybe) {
    ret deref maybe;
  }

  ret 0;
}
```

### 20.2 Allocator ve `new?`

Plan:

- `new Type(args)` default allocator/runtime.
- `new(allocator) Type(args)` explicit allocator.
- `shared new Type(args)` shared ownership.
- Fallible allocation explicit `new?`.
- Infallible `new` nil döndürmez.

Örnek proposal:

```cstar
Meter^ owned = new(arena) Meter(7);
Meter^? maybe = new?(arena) Meter(7);
```

Primitive allocation da proposal'da legal kabul edilir:

```cstar
int32^ owned_value = new(arena) int32(7);
```

### 20.3 Protocol / Typestate

Plan:

- Protocol state safety içindir, allocation veya dispatch değildir.
- Default protocol static/provable.
- `dynamic protocol` runtime fallback'tir.
- `.=` sadece dynamic/provability-gap transition için ayrılır.
- Scope-exit state cleanup `ret`, `throw`, `break`, `continue` edge'lerinde çalışmalıdır.

Örnek:

```cstar
protocol FileState for FileHandle {
  state closed, opened;

  default closed;

  closed -> opened :: open();
  opened -> closed :: close();

  read() :: !closed;
  scope_exit opened -> closed :: close();
}
```

### 20.4 Dynamic Trait

Bugün trait static conformance'tır. Future dynamic trait object explicit olmalıdır:

```cstar
dynamic Writer& writer
dynamic Writer* shared_writer
dynamic Writer^ owned_writer
```

Ownership ekseni `&`, `*`, `^` ile aynı kalır; sadece dispatch dynamic olur.

### 20.5 Tagged Data

Enum bugün scalar tag olarak çalışır. Future `tagged` sugar açık tag + storage layout'a inecektir:

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
```

Desugar prensibi:

- ABI-visible tag.
- Variant storage explicit layout.
- `packet.Data.len` tag-aware checked access.

### 20.6 Macro, Attribute ve Reflection

Macro text replacement değildir; typed AST substitution hedeflenir:

```cstar
macro clamp($value: expr, $min: expr, $max: expr) -> expr {
  (($value) < ($min) ? ($min) : (($value) > ($max) ? ($max) : ($value)))
}
```

Attribute proposal:

```cstar
attribute DebugLayout for struct {
  $emit function debug_layout() :: void {
    print("type ", name($item), " size=", sizeof($item));
    ret;
  }
}
```

Reflection ilk yüzeyi:

- `name($item)`
- `fields($item)`
- `methods($item)`
- `has_attribute(item, Name)`
- `attribute_args(item, Name)`
- `sizeof(Type)`
- `alignof(Type)`

### 20.7 Async ve Concurrency

Plan:

- `async` task/thread boundary effect.
- `await` suspension/ownership checkpoint.
- `Send` ve `Sync` marker trait/capability.
- `T^` task boundary'den ancak explicit `move` ile geçer.
- `T*` retain/copy veya explicit move ile geçer.
- Borrowed `&` lifetime proof ister.

Örnek proposal:

```cstar
consume_on_worker(int32^ owned) async :: int32 {
  ret deref owned;
}

main_task() async :: int32 {
  int32 value = 7;
  int32^ owned = ref value;
  ret await consume_on_worker(move owned);
}
```

## 21. Test ve Geliştirme Akışı

### 21.1 Smoke Suite

Smoke suite çalışan özelliklerin regression setidir:

```powershell
.\tools\run_examples.bat --suite smoke
```

Kategoriler:

```text
examples/smoke/core/
examples/smoke/casts/
examples/smoke/arrays/
examples/smoke/control_flow/
examples/smoke/functions/
examples/smoke/imports/
examples/smoke/pointers/
examples/smoke/ownership/
examples/smoke/runtime/
examples/smoke/enums/
examples/smoke/structs/
```

`examples/smoke/modules/` helper dosyaları içerir ve runner tarafından skip edilir.

### 21.2 Type Checker Suite

```powershell
.\tools\run_examples.bat --suite type_checker --expect-diagnostics
```

Bu suite bilerek hata üretmesi beklenen programları çalıştırır. Hedef:

- Controlled diagnostic.
- No crash.
- No assert.
- Beklenen diagnostic code varsa doğru code.

### 21.3 Yeni Özellik Eklerken

1. Önce proposal veya doküman kararını yaz.
2. Minimal positive smoke ekle.
3. En az bir negative type_checker örneği ekle.
4. Parser diagnostic doğru yerde mi kontrol et.
5. Semantic pass sonucu doğru tip döndürüyor mu kontrol et.
6. Codegen LLVM assertion üretmeden çalışıyor mu kontrol et.
7. Full smoke ve full typechecker çalıştır.

### 21.4 Canonical Son Kontrol Komutları

```powershell
cmake --build build-ucrt
.\tools\run_examples.bat --suite smoke
.\tools\run_examples.bat --suite type_checker --expect-diagnostics
```

Son doğrulama anında beklenen durum:

```text
smoke:       143 total, 140 pass, 3 skip
typechecker: 112 total, 110 diagnostic, 1 pass, 1 skip
```

Bu sayılar yeni örnekler eklendikçe artabilir; önemli olan fail/crash/assert sayısının sıfır kalmasıdır.
