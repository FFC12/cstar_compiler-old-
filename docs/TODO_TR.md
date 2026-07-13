# C* Devam Planı ve TODO

Bu belge iki kaynağı birleştirir:

- Bugün gerçekten çalışan derleyici hattı.
- `examples/papers/*.cstar`, `grammar.cfg` ve `DOKUMANTASYON_TR.md` içindeki dil proposal'ı.

Amaç büyük vizyonu kaybetmeden, dili küçük ve çalışan adımlarla büyütmek.

## Güncel Durum

Tamamlanan altyapı:

- MSYS2 UCRT64 geliştirme ortamı kuruldu.
- LLVM/Clang 22.1.8 ile build alınıyor.
- Build sistemi tek bir Windows port toolchain'ine kilitli değil:
  - MSYS2/UCRT64 LLVM bulunursa otomatik Ninja + Clang + `build-ucrt`.
  - Visual Studio/MSVC için `build` klasörü ve MSVC uyumlu `LLVM_DIR` destekleniyor.
  - Linux/macOS tarafında `build.sh` aynı CMake akışını kullanıyor.
- VSCode F5 debug akışı hem repo klasörü hem parent workspace için eklendi.
- `build.bat`, `build.sh`, `tools/run_examples.ps1` güncellendi.
- `tools/run_examples.bat` eklendi; Windows execution policy'ye takılmadan suite parametreleri geçirilebiliyor.
- Smoke runner `// expected-exit: N` etiketini okuyup generated executable'ın process exit status değerini doğruluyor.
- Compiler banner ve diagnostic çıktıları `include/diagnostics/*` altında toplandı.
- Diagnostic formatı dosya yolu, satır, sütun, severity, hata kodu ve caret marker gösterecek hale getirildi.
- Renkli console çıktısı tek helper üzerinden yönetiliyor; `NO_COLOR` ortam değişkeni destekleniyor.
- Lexer EOF / out-of-bounds hataları düzeltildi.
- LLVM 22 opaque pointer API uyumluluğu için ilk geçiş düzeltmeleri yapıldı.
- Backend `.ll -> .s -> .exe -> run` hattı Clang'a taşındı.
- Backend Clang yolu CMake'den geliyor; gerekirse runtime'da `CSTAR_CLANG` ile override ediliyor.
- Generated `.ll`, `.s` ve executable çıktıları artık çalışma klasörünün root'una yazılmıyor; varsayılan olarak `.cstar-out/` altına alınır.
- LLVM module target triple configure zamanında `clang -dumpmachine` ile set ediliyor.
- Generated programın non-zero exit code'u artık compiler hatası sayılmıyor.

Tamamlanan dil/codegen parçaları:

- `ret;`
- `ret expr;`
- Lokal primitive değişken deklarasyonu.
- Global mutable primitive değişken.
- Uninitialized local primitive default init: şimdilik zero-init.
- `char`, `float32`, `float64` primitive smoke'ları.
- Float literal fractional kısmı lexer'da korunuyor (`0.5`, `4.5` gibi).
- Basit arithmetic expression.
- Integer ve floating point `+`, `-`, `*`, `/`, `%` codegen.
- Comparison/logical expression.
- Lokal scalar assignment:
  - `=`
  - `+=`
  - `-=`
  - `*=`
  - `/=`
  - `%=`
  - `>>=`
  - `<<=`
  - `&=`
  - `|=`
  - `^=`
- Lokal array element assignment:
  - `arr[0] = value;`
  - `arr[0] += value;`
- Primitive argümanlı ve return değerli basit function call:
  - `ret add(1, 2);`
  - `int32 x = add(1, 2);`
  - `foo();`
- Pointer/ref memory MVP genişletildi:
  - `read_ptr(ref x);`
  - fonksiyon içinde `ret deref p;`
  - `int32* p = ref x;`
  - `deref p = value;`
  - `deref p += value;`
  - `**pp = value;`
  - `int32* q = deref pp;`
  - `identity(ref x) :: int32*`
  - Qualifier/ownership kuralları ayrı TODO'dur.
- Geçici dirty builtin:
  - `print(...)` CRT `printf` çağrısına indiriliyor.
  - `input_int()` CRT `scanf("%lld", ...)` çağrısına indiriliyor ve `int64` döndürür.
  - String literal kaçışları için temel `\n`, `\t`, `\"`, `\\` decode ediliyor.
  - Bu stdlib/native interop tasarımı gelince yeniden ele alınacak.

Çalışan smoke seti:

```text
examples/smoke/minimal.cstar
examples/smoke/local_int.cstar
examples/smoke/global_variable.cstar
examples/smoke/uninitialized_local.cstar
examples/smoke/void_return.cstar
examples/smoke/char_literal.cstar
examples/smoke/float32_arithmetic.cstar
examples/smoke/float64_arithmetic.cstar
examples/smoke/bool_literal.cstar
examples/smoke/binary_expr.cstar
examples/smoke/comparison_expr.cstar
examples/smoke/not_equal_expr.cstar
examples/smoke/logical_expr.cstar
examples/smoke/assignment.cstar
examples/smoke/assignment_cast.cstar
examples/smoke/cast_numeric.cstar
examples/smoke/array_element_read.cstar
examples/smoke/array_element_assignment.cstar
examples/smoke/array_element_shortcut_assignment.cstar
examples/smoke/function_call.cstar
examples/smoke/function_call_initializer.cstar
examples/smoke/function_call_statement.cstar
examples/smoke/function_call_cast_argument.cstar
examples/smoke/forward_function_call.cstar
examples/smoke/function_call_symbol_argument.cstar
examples/smoke/function_call_pointer_argument.cstar
examples/smoke/pointer_variable_initializer.cstar
examples/smoke/dereference_assignment.cstar
examples/smoke/dereference_assignment_shortcut.cstar
examples/smoke/multi_level_dereference_assignment.cstar
examples/smoke/pointer_from_pointer_initializer.cstar
examples/smoke/pointer_return.cstar
examples/smoke/unsafe_cast_int_to_pointer.cstar
examples/smoke/print.cstar
examples/smoke/if_statement.cstar
examples/smoke/if_condition_scalar_conversion.cstar
examples/smoke/if_condition_pointer_conversion.cstar
examples/smoke/if_else_statement.cstar
examples/smoke/if_elif_else_statement.cstar
examples/smoke/nested_if_statement.cstar
examples/smoke/if_fallthrough_statement.cstar
```

Interactive örnek:

```text
examples/interactive/calculator.cstar
```

`// expected: interactive` ile işaretli örnekler normal suite ve `all` içinde skip edilir; doğrudan compiler ile çalıştırıldığında kullanıcıdan input bekler.

Doğrulama komutu:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\tools\run_examples.ps1
```

Diğer örnek setleri bilinçli olarak ayrı çalıştırılır:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\tools\run_examples.ps1 -Suite type_checker -ExpectDiagnostics
powershell -NoProfile -ExecutionPolicy Bypass -File .\tools\run_examples.ps1 -Suite functions
powershell -NoProfile -ExecutionPolicy Bypass -File .\tools\run_examples.ps1 -Suite variables
powershell -NoProfile -ExecutionPolicy Bypass -File .\tools\run_examples.ps1 -Suite papers
powershell -NoProfile -ExecutionPolicy Bypass -File .\tools\run_examples.ps1 -Suite interactive
```

## Örnek Setlerinin Güncel Anlamı

`examples/smoke/`:

- Her zaman yeşil kalması gereken küçük çalışan compiler çekirdeği.
- Yeni özellik eklenirken önce buraya küçük positive smoke eklenir.
- `// expected-exit: N` varsa `ret N;` ile üretilen process exit status değeri doğrulanır; bu console output değildir.
- Güncel durumda 41/41 dosya başarılı.

`examples/type_checker/`:

- Büyük kısmı bilerek hata üretmesi beklenen semantic/type-check örnekleridir.
- Exit code `1` çoğu dosya için kabul edilebilir diagnostic olabilir.
- Assert/crash kabul edilemez; önce bunlar izole edilmeli.
- Güncel durumda `-ExpectDiagnostics` ile 19/19 dosya kontrollü diagnostic üretiyor, crash/assert yok.

Tamamlanan crash/assert düzeltmesi:

```text
examples/type_checker/000.cstar
examples/type_checker/001.cstar
examples/type_checker/003.cstar
examples/type_checker/007.cstar
examples/type_checker/008.cstar
```

Bu dosyalar daha önce lexer sonrası parser/semantic tarafında `std::deque::operator[]` assert'ine düşüyordu. Expression parser'daki boş `parenthesesPos`, `sparenthesesPos` ve `ternaryPos` erişimleri guard edildi. Artık crash yerine kontrollü diagnostic yoluna giriyorlar.

Kontrollü diagnostic üreten type-checker seti:

```text
examples/type_checker/000.cstar
examples/type_checker/001.cstar
examples/type_checker/002.cstar
examples/type_checker/003.cstar
examples/type_checker/004.cstar
examples/type_checker/005.cstar
examples/type_checker/006.cstar
examples/type_checker/007.cstar
examples/type_checker/008.cstar
examples/type_checker/009.cstar
examples/type_checker/010.cstar
examples/type_checker/011.cstar
examples/type_checker/012.cstar
examples/type_checker/013.cstar
examples/type_checker/014.cstar
examples/type_checker/015.cstar
examples/type_checker/016.cstar
examples/type_checker/017.cstar
examples/type_checker/018.cstar
```

Bu dosyalar parser/pass hattına giriyor ve semantic diagnostic üretebiliyor. Mesajların doğruluğu ayrıca test edilmeli.

`examples/functions/`, `examples/variables/`, `examples/papers/`:

- Orijinal dil vizyonunu/proposal'ı gösterir.
- Şu an tamamını çalıştırmak hedef değil.
- Her dosyadan küçük MVP smoke çıkarılacak; compiler geliştikçe orijinal örnekler tekrar yeşile çekilecek.

Güncel sınıflandırma:

- `examples/functions/` güncel durumda 1 başarılı, 7 kontrollü diagnostic, 0 crash/assert.
- `examples/functions/007.cstar` çalışıyor.
- `examples/functions/000.cstar`, `001.cstar`, `002.cstar`, `003.cstar`, `004.cstar`, `005.cstar`, `006.cstar` kontrollü diagnostic üretiyor.
- `examples/functions/000.cstar` ve `004.cstar` pointer/user-defined parametre syntax'ı yüzünden parse diagnostic üretiyor.
- `examples/functions/001.cstar`, `002.cstar`, `005.cstar`, `006.cstar` semantic/type diagnostic üretiyor.
- `examples/functions/003.cstar` pass0 symbol/redefinition diagnostic üretiyor; artık pass1'e devam edip crash/non-1 üretmiyor.
- `examples/variables/000.cstar` artık generic function call/type attribute syntax'ını parse ediyor; kalan diagnostic'ler dosyanın proposal/stres örneği gibi aynı global sembolleri tekrar tanımlamasından geliyor.
- `examples/variables/` güncel durumda 1 kontrollü diagnostic, 0 crash/assert.
- `examples/papers/policy.cstar` policy syntax henüz parser'da olmadığı için parse diagnostic üretiyor.
- `examples/papers/syntax.cstar` include syntax'ı için kontrollü `not implemented` diagnostic üretiyor; parser artık takılmıyor.
- `examples/papers/` güncel durumda 2 kontrollü diagnostic, 0 crash/assert.

## Aşama 0 - Çalışma Zemini

Durum: büyük ölçüde tamamlandı.

Tamamlananlar:

- Build ortamı.
- VSCode F5:
  - Varsayılan `Debug cstar: Ninja/Clang` config'i `build-ucrt/cstar.exe` üzerinden çalışıyor.
  - `Debug cstar: Visual Studio` config'i MSVC uyumlu LLVM kurulumu ile `build/Debug/cstar.exe` üzerinden çalışacak şekilde ayrıldı.
- Smoke runner.
- Minimal çalışan örnekler.
- CMake build dosyaları toolchain'e göre ayrıldı:
  - MSYS2/UCRT64: `build-ucrt`
  - Visual Studio/MSVC: `build`
- MSYS2 LLVM ile Visual Studio generator'ını yanlışlıkla karıştıran build yolu engellendi.
- Target triple override uyarısı giderildi.

Kalanlar:

- Visual Studio/MSVC gerçek doğrulaması:
  - `C:\Program Files\LLVM\lib\cmake\llvm` benzeri MSVC uyumlu LLVM kurulumu ile test edilecek.
  - MSYS2 LLVM paketleri MSVC linker ile karıştırılmayacak.
- Example runner:
  - Smoke default çalışır.
  - Type-checker diagnostic/crash ayrımı yapar.
  - Her suite için beklenen durum manifest'i eklenecek.
- Diagnostic sistemi:
  - Her semantic hata call-site'ı generic `CST2001` yerine özel hata kodlarına ayrılacak.
  - Diagnostic mesaj metinleri Türkçe/İngilizce terminoloji açısından standartlaştırılacak.
  - Parser/semantic recovery stratejisi belirlenecek; tek hatada çıkılan ve devam edilebilen modlar ayrılacak.
- CLI driver modları tamamlandı:
  - `--emit=<ir|asm|obj|exe>`
  - `--emit-llvm`
  - `--emit-asm`
  - `--build-exe`
  - `--run`
  - `--no-run`
  - `--output-dir <path>`
  - `--verbose`
  - `--stats`
  - Varsayılan davranış executable üretip çalıştırmadan durur; test runner açıkça `--run` kullanır.
  - Pass süreleri, Total LoC, output path ve generated exit code yalnızca `--stats` ile görünür.
- Data layout açıkça set edilmeli.
- Build uyarıları sınıflandırılmalı:
  - gerçek bug
  - LLVM deprecation
  - eski AST tasarım borcu

## Aşama 1 - Minimum Çalışan Dil Çekirdeği

Bu aşamanın hedefi: küçük C* programları gerçekten executable davranışı üretsin.

Durum: tamamlandı.

Bu aşamada açık TODO kalmadı. Proposal'a ait ama minimum çekirdek olmayan takipler ilgili sonraki aşamalara taşındı.

### 1.1 Return

Durum: tamamlandı.

Örnek:

```cstar
main() :: int32 {
    ret 0;
}
```

Tamamlanan:

- `ret;` void fonksiyon içinde smoke ile doğrulandı:
  - `examples/smoke/void_return.cstar`
- Process exit code için `main` tarafında `int32` dönüş kullanımı korunuyor; `void main` sistem/CRT tarafında exit code sözleşmesi vermediği için smoke sözleşmesi değildir.

### 1.2 Primitive Değişkenler

Durum: tamamlandı.

Örnek:

```cstar
main() :: int32 {
    int32 x = 7;
    ret x;
}
```

Tamamlanan:

- Global mutable variable davranışı test edildi:
  - `examples/smoke/global_variable.cstar`
- Uninitialized local primitive için current rule: zero-init.
  - `examples/smoke/uninitialized_local.cstar`
- `char`, `float32`, `float64` smoke eklendi:
  - `examples/smoke/char_literal.cstar`
  - `examples/smoke/float32_arithmetic.cstar`
  - `examples/smoke/float64_arithmetic.cstar`
- Float literal lexer bug'ı düzeltildi; fractional kısım artık token text içinde korunur.

### 1.3 Arithmetic Expression

Durum: tamamlandı.

Örnek:

```cstar
main() :: int32 {
    int32 x = 1 + 2 * 3;
    ret x;
}
```

Tamamlanan:

- Comparison result type netleşti:
  - `<`, `<=`, `>`, `>=`, `==`, `!=` codegen sonucu `bool/i1` üretir.
  - Return veya variable initializer hedef tipi primitive integer/bool ise mevcut cast yolu ile `0/1` değerine iner.
- `!=` operatorü AST/parser/codegen hattına eklendi.
- Boolean/logical expression smoke eklendi:
  - `examples/smoke/bool_literal.cstar`
  - `examples/smoke/comparison_expr.cstar`
  - `examples/smoke/not_equal_expr.cstar`
  - `examples/smoke/logical_expr.cstar`
- Floating point arithmetic smoke eklendi:
  - `examples/smoke/float32_arithmetic.cstar`
  - `examples/smoke/float64_arithmetic.cstar`
- Signed integer arithmetic LLVM wrap flag'leri doğru yöne çekildi:
  - signed: `nsw`
  - unsigned: `nuw`

### 1.4 Assignment

Durum: tamamlandı.

Örnek:

```cstar
main() :: int32 {
    int32 x = 1;
    x = 2;
    ret x;
}
```

Tamamlanan:

- Scalar symbol assignment:
  - `x = 1;`
  - `x += 1;`
- Dereference assignment:
  - `*p = 1;`
  - `deref p = 1;`
- Dereference shortcut assignment:
  - `deref p += 1;`
- Çok seviyeli dereference assignment:
  - `**pp = 1;`
- Array element read:
  - `ret arr[0];`
- Array element assignment:
  - `arr[0] = 1;`
- Array element shortcut assignment:
  - `arr[0] += 1;`
- Assignment type cast kuralı smoke ile test edildi:
  - `examples/smoke/assignment_cast.cstar`
- Const/readonly assignment negative testleri eklendi:
  - `examples/type_checker/016.cstar`
  - `examples/type_checker/017.cstar`
- Assignment RHS symbol lookup artık sol tarafın eski `symbolId`'siyle değil, statement'ın bulunduğu scope konumuyla yapılır.

### 1.5 If / Elif / Else

Durum: temel parser/codegen smoke ile doğrulandı.

Tamamlanan:

- Minimal `if` smoke:
  - `examples/smoke/if_statement.cstar`
- `if/else` smoke:
  - `examples/smoke/if_else_statement.cstar`
- `if/elif/else` smoke:
  - `examples/smoke/if_elif_else_statement.cstar`
- İç içe `if/else` smoke:
  - `examples/smoke/nested_if_statement.cstar`
- Branch içinde fallthrough statement + return smoke:
  - `examples/smoke/if_fallthrough_statement.cstar`
- Branch terminator tekrarları engellendi:
  - `ret` üreten branch bloklarının arkasına fazladan `br merge` basılmıyor.
- Condition codegen artık `int`, `float`, `bool/i1` ve pointer değerleri için zero/null karşılaştırmasına indirgeniyor.
- Condition conversion smoke'ları eklendi:
  - `examples/smoke/if_condition_scalar_conversion.cstar`
  - `examples/smoke/if_condition_pointer_conversion.cstar`
- Generic function call/type attribute parser yolu `func<A****>()` ve `func<int32*>()` gibi pointer seviyeli tip argümanlarını syntax olarak kabul ediyor.

Tamamlanan dokümantasyon kuralı:

- Condition dönüşüm kuralı dil dokümanında netleşti: `int`, `float`, `bool/i1` ve pointer condition değerleri zero/null karşılaştırmasına indirgenir.

### 1.6 Parser Crash Triage

Durum: minimum çekirdek suite'lerinde crash/assert/progress takılması yok; tamamlandı.

Tamamlanan:

- `examples/type_checker/000.cstar`
- `examples/type_checker/001.cstar`
- `examples/type_checker/003.cstar`
- `examples/type_checker/007.cstar`
- `examples/type_checker/008.cstar`
- `examples/functions/003.cstar`
- `examples/papers/syntax.cstar`

Not: Proposal'a ait ama minimum çekirdeğe dahil olmayan parser takipleri ilgili aşamalara taşındı:

- loop/range syntax: Aşama 5.
- array parametreleri ve çok boyutlu indexing: Aşama 4.
- `f++`, `++f2` codegen: Aşama 5 sonrası statement genişletmesi.
- `if` expression/PHI tasarımı: Aşama 5 sonrası expression tasarımı.

## Aşama 2 - Fonksiyon Sistemi

Proposal'daki dilin kullanılabilir olması için fonksiyon çağrıları erken gelmeli.

### 2.1 Function Call

Durum: MVP çalışıyor.

Tamamlanan:

- Primitive argümanlı call:
  - `add(1, 2)`
- Return value kullanımı:
  - `ret add(1, 2);`
- Call sonucunu variable initializer içinde kullanma:
  - `int32 x = add(1, 2);`
- Parametresiz call statement:
  - `foo();`
- Function signature table:
  - pass0 içinde fonksiyon return tipi ve parametre listesi toplanıyor.
  - pass1 içinde bilinmeyen fonksiyon, argüman sayısı ve temel scalar argüman tipi diagnostic'i üretiliyor.
  - `foo();` gibi call statement artık semantic pass tarafından da ziyaret ediliyor.
- Smoke:
  - `examples/smoke/function_call.cstar`
  - `examples/smoke/function_call_initializer.cstar`
  - `examples/smoke/function_call_statement.cstar`
  - `examples/smoke/forward_function_call.cstar`
  - `examples/smoke/function_call_symbol_argument.cstar`
  - `examples/smoke/function_call_pointer_argument.cstar`
  - `examples/smoke/pointer_variable_initializer.cstar`
  - `examples/smoke/pointer_return.cstar`
- Negative diagnostic:
  - `examples/type_checker/009.cstar`
  - `examples/type_checker/010.cstar`
  - `examples/type_checker/013.cstar`

- Function return type kontrolü:
  - Tamamlandı: `int32 x = returns_bool();` gibi çağrılar net diagnostic üretiyor.
  - Negative diagnostic:
    - `examples/type_checker/011.cstar`
    - `examples/type_checker/012.cstar`
- Function argument kontrolünü genişlet:
  - Tamamlandı: primitive symbol argüman tipleri.
  - Tamamlandı: explicit `cast<T>(expr)` ile function argument geçme.
  - Tamamlandı: tek seviyeli primitive pointer argümana `ref x` geçirme ve callee içinde `deref p` okuma için smoke/codegen MVP.
  - Tamamlandı: pointer return smoke (`identity(int32* p) :: int32*`).
  - Kalan: qualifier argümanları ve array parametreleri.
- Forward declaration call:
  - Tamamlandı: `foo` çağrıldığı noktadan sonra tanımlansa da codegen çalışıyor.
- Import/forward declaration call:
  - `import puts(constptr char*) :: int32;`

### 2.2 Parametre Codegen

Durum: primitive parametreler için temel alloca/store ve load davranışı çalışıyor.

Tamamlanan:

- `int32 add(int32 a, int32 b) :: int32` smoke.
- Parametreleri expression içinde kullanma:
  - `ret a + b;`
- Explicit cast edilmiş argümanı parametreye geçme:
  - `examples/smoke/function_call_cast_argument.cstar`
- Parametre type mismatch negative test:
  - `examples/type_checker/010.cstar`
  - `examples/type_checker/013.cstar`
- Tek seviyeli primitive pointer/ref parametre codegen smoke:
  - `examples/smoke/function_call_pointer_argument.cstar`

TODO:

- Qualifier parametre codegen testleri.
- Array parametreleri.

### 2.3 Cast

Durum: MVP çalışıyor.

Tamamlanan:

- `CastOpAST` artık ayrı `ExprKind::CastExpr` kimliği taşır.
- `preVisit(CastOpAST&)` explicit hedef tipi doğrular.
- `visit(CastOpAST&)` LLVM IR üretir.
- `cast<T>(expr)` safe cast MVP:
  - primitive numerik dönüşümler.
  - pointer -> pointer dönüşümleri.
  - pointer/value kategori geçişi reddedilir.
- `unsafe_cast<T>(expr)` looser cast MVP:
  - integer -> pointer.
  - pointer -> integer.
  - pointer -> pointer.
  - normal numerik dönüşümlerde safe cast helper'ını kullanır.
- Smoke:
  - `examples/smoke/cast_numeric.cstar`
  - `examples/smoke/function_call_cast_argument.cstar`
  - `examples/smoke/unsafe_cast_int_to_pointer.cstar`
- Negative diagnostic:
  - `examples/type_checker/018.cstar`

TODO:

- `expr as T` syntax kararı ve parser/codegen yolu.
- User-defined type cast kuralları.
- Qualifier-aware cast kuralları.

## Aşama 3 - Memory Model: Pointer, Reference, Qualifier

Bu C* proposal'ının en karakteristik kısmı.

### 3.1 Pointer ve Reference

Durum: primitive pointer/ref memory MVP çalışıyor.

Tamamlanan:

- `examples/smoke/function_call_pointer_argument.cstar`
- `examples/smoke/pointer_variable_initializer.cstar`
- `examples/smoke/dereference_assignment.cstar`
- `examples/smoke/dereference_assignment_shortcut.cstar`
- `examples/smoke/multi_level_dereference_assignment.cstar`
- `examples/smoke/pointer_from_pointer_initializer.cstar`
- `examples/smoke/pointer_return.cstar`
- `examples/type_checker/014.cstar`
- `ref x` codegen'i yerel/global sembolün adresini üretir.
- `deref p` codegen'i beklenen tipe göre pointer'dan load üretir; pointer sonuçlar da desteklenir.
- `int32* p = ref x;` ile tek seviyeli primitive pointer variable initializer çalışır.
- `deref p = value;` ve `*p = value;` assignment target olarak çalışır.
- `deref p += value;` gibi shortcut assignment'lar pointer hedefte çalışır.
- `**pp = value;` çok seviyeli dereference assignment çalışır.
- `int32* q = deref pp;` pointer'dan pointer okuma initializer içinde çalışır.
- `identity(int32* p) :: int32* { ret p; }` gibi pointer return çalışır.
- Pointer olmayan sembole `deref` assignment kontrollü diagnostic üretir.

TODO:

- `int32& r`
- Pointer/ref/qualifier semantic diagnostics:
  - `constptr`
  - `constref`
  - `readonly`

### 3.2 Ownership Pointer `^`

Durum: parser/type info içinde niyet var, runtime/semantic garanti yok.

TODO:

- `^` için net MVP kuralı yaz:
  - sadece type-level syntax mı?
  - move-only semantik mi?
  - alias yasağı mı?
- MVP uygulanmadan önce proposal karar belgesi oluştur.

### 3.3 Qualifier

TODO:

- `const`
- `constptr`
- `constref`
- `readonly`
- Positive/negative assignment testleri.
- Pointer target/value const ayrımını netleştir.

## Aşama 4 - Arrays ve Indexing

Durum: tek boyutlu MVP çalışıyor; çok boyutlu/index semantics kırılgan.

Tamamlanan:

- Tek boyutlu local array:
  - `int32 arr[2] = (1, 2);`
- Array element read:
  - `ret arr[0];`
- Array element assignment:
  - `arr[0] = 3;`
- Array element shortcut assignment:
  - `arr[0] += 3;`

TODO:

- Çok boyutlu array flattening kuralını düzelt.
- `arr[a:b]` gibi colon index assignment codegen.
- Bounds warning/error politikasını netleştir.

## Aşama 5 - Kontrol Akışı

### 5.1 Loop

Proposal hedefleri:

- while-style:
  - `loop (x < 10) { ... }`
- iterable:
  - `loop(data in arr) { ... }`
- indexed iterable:
  - `loop(index, data in arr) { ... }`
- range:
  - `loop(i in [0, 100]) { ... }`

TODO:

- Önce while-style MVP.
- Sonra array iterable.
- `break` / `continue` parser ve codegen.

### 5.2 Option / Match Benzeri Yapı

Proposal seviyesinde.

TODO:

- Syntax kararını netleştir.
- Parser eklemeden önce küçük tasarım notu yaz.

## Aşama 6 - Import / Package / Native Interop

Proposal hedefleri:

- `include`
- `include involved`
- `import ... from "lib"`
- `export ... from "module"`

Mevcut kritik bug:

- `include` branch'i parser içinde boş olduğu için infinite loop riski vardı; artık kontrollü diagnostic veriyor.

TODO:

- `include` için gerçek parser/AST tasarımı:
  - `include involved { ... }`
  - `include { ... }`
  - `include "module" as alias`
- Basit `import func(...) :: type;` call codegen ile bağlanmalı.
- `from "lib"` syntax sonra.

## Aşama 7 - User-defined Types

Proposal hedefleri:

- `struct`
- `trait`
- `policy`
- custom allocator benzeri fikirler.

Durum:

- `SPEC_DEFINED` var.
- Gerçek struct/trait/policy parser yok.
- Defined type table dolmuyor.

TODO:

- Önce `struct` MVP tasarla.
- Field layout + LLVM struct type.
- Constructor/function call ayrımını çöz.
- Trait/policy en sona bırakılmalı.

## Aşama 8 - Metaprogramming ve İleri Proposal

Proposal'da görünen ama çok sonraya bırakılacak başlıklar:

- `attribute`
- directive/macro sistemi
- `$` ve `#` tabanlı compile-time hook'lar
- `async` / `await`
- `except` / `throw`
- compile-time error/runtime error hook'ları

Bu başlıklar çekirdek dil stabil olmadan uygulanmamalı.

## Bir Sonraki En İyi Adım

Tamamlanan son adım:

- Forward declaration/codegen sıralaması düzeltildi:

```text
main() :: int32 {
    ret add(1, 2);
}

add(int32 a, int32 b) :: int32 {
    ret a + b;
}
```

Semantic pass zaten fonksiyonu biliyordu; codegen tarafında bütün fonksiyon prototipleri body üretilmeden önce module'a ekleniyor.

Sıradaki teknik iş:

- Kontrol akışı smoke:

```cstar
main() :: int32 {
    if (1) {
        ret 1;
    }
    ret 0;
}
```

Function call MVP genişledi. Argüman sayısı/tipi diagnostic'i ve forward declaration codegen desteği eklendi. Bir sonraki adım `if`/`else` için minimal smoke ve branch codegen stabilizasyonu.
