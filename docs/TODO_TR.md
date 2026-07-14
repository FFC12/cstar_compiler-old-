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
- Example runner `// expected-code: CSTNNNN` etiketini okuyup diagnostic suite'lerinde beklenen hata kodunu doğruluyor.
- Compiler banner ve diagnostic çıktıları `include/diagnostics/*` altında toplandı.
- Diagnostic formatı dosya yolu, satır, sütun, severity, hata kodu ve caret marker gösterecek hale getirildi.
- Renkli console çıktısı tek helper üzerinden yönetiliyor; `NO_COLOR` ortam değişkeni destekleniyor.
- Lexer EOF / out-of-bounds hataları düzeltildi.
- LLVM 22 opaque pointer API uyumluluğu için ilk geçiş düzeltmeleri yapıldı.
- LLVM 22 deprecated `CreateGlobalStringPtr` API kullanımı kaldırıldı.
- Backend `.ll -> .s -> .exe -> run` hattı Clang'a taşındı.
- Backend Clang yolu CMake'den geliyor; gerekirse runtime'da `CSTAR_CLANG` ile override ediliyor.
- Backend target triple configure zamanında Clang driver'ın efektif cc1 triple'ından alınır; gerekirse runtime'da `CSTAR_TARGET_TRIPLE` ile override edilebilir.
- LLVM module target data layout configure zamanında backend Clang'den alınır ve module'a yazılır.
- Backend `.ll -> .s/.o/.exe` komutları module ile aynı target triple'ı kullanır.
- Generated `.ll`, `.s`, object ve executable çıktıları artık çalışma klasörünün root'una yazılmıyor; varsayılan olarak `.cstar-out/` altına alınır.
- Generated programın non-zero exit code'u artık compiler hatası sayılmıyor.
- Generated program exit code'u POSIX'te `system()` wait status'ünden gerçek process exit code'a normalize edilir.

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
- Native runtime / CRT interop MVP:
  - `include/codegen/native_runtime.hpp` ve `src/codegen/native_runtime.cpp` altında toplandı.
  - `print(...)` CRT `printf` çağrısına indiriliyor.
  - `input_int()` token bazlı `scanf("%255s", ...)` + `atoll` ile `int64` döndürür; sayı olmayan token consume edilir ve `0` olur.
  - `input_string()` CRT `scanf("%255s", ...)` çağrısına indiriliyor ve doğrudan `print(input_string())` gibi kullanılabilen kısa C string pointer döndürür.
  - `clear_screen()` ANSI terminal temizleme escape'ine indiriliyor; aktif console demoları için MVP yüzey.
  - `flush_output()` CRT `fflush(NULL)` çağrısına indiriliyor; frame tabanlı terminal renderlarında output buffer'ını hemen boşaltır.
  - `sleep_ms(ms)` POSIX `usleep(ms * 1000)` çağrısına indiriliyor; ileride platform-neutral stdlib/native interop altında standartlaştırılacak.
  - `enable_raw_input()` / `disable_raw_input()` POSIX `stty` üzerinden non-canonical, non-blocking terminal modunu açıp kapatır.
  - `read_key()` CRT `getchar()` ile tuş yokken `-1`, tuş varken `int32` byte değeri döndürür; WASD/ok tuşu oyunları için geçici native runtime yüzeyi.
  - String literal kaçışları için temel `\n`, `\t`, `\"`, `\\` decode ediliyor.
  - Bu katman ileride stdlib/native interop ABI'sinin bağlanacağı giriş noktasıdır.

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
examples/smoke/as_cast_numeric.cstar
examples/smoke/array_element_read.cstar
examples/smoke/array_element_assignment.cstar
examples/smoke/array_element_shortcut_assignment.cstar
examples/smoke/array_param_read.cstar
examples/smoke/array_param_write.cstar
examples/smoke/function_call.cstar
examples/smoke/function_call_initializer.cstar
examples/smoke/function_call_statement.cstar
examples/smoke/function_call_cast_argument.cstar
examples/smoke/forward_function_call.cstar
examples/smoke/function_call_symbol_argument.cstar
examples/smoke/function_call_pointer_argument.cstar
examples/smoke/export_block_from_module.cstar
examples/smoke/import_block_from_crt.cstar
examples/smoke/import_block_function.cstar
examples/smoke/import_function_abs.cstar
examples/smoke/import_function_from_crt.cstar
examples/smoke/import_function_named_param.cstar
examples/smoke/include_module_function.cstar
examples/smoke/include_module_public_static.cstar
examples/smoke/const_value.cstar
examples/smoke/const_pointer.cstar
examples/smoke/reference_param.cstar
examples/smoke/constref_param.cstar
examples/smoke/constptr_param.cstar
examples/smoke/constptr_pointer.cstar
examples/smoke/readonly_param.cstar
examples/smoke/readonly_pointer.cstar
examples/smoke/readonly_multi_level_pointer.cstar
examples/smoke/nomove_unique_param_read.cstar
examples/smoke/pointer_variable_initializer.cstar
examples/smoke/shared_pointer_assignment_count.cstar
examples/smoke/shared_pointer_copy_count.cstar
examples/smoke/shared_pointer_function_arg_count.cstar
examples/smoke/shared_pointer_move_assignment.cstar
examples/smoke/unique_pointer.cstar
examples/smoke/unique_pointer_function_move_arg.cstar
examples/smoke/unique_pointer_move_assignment.cstar
examples/smoke/unique_pointer_move_init.cstar
examples/smoke/unique_pointer_return_move.cstar
examples/smoke/dereference_assignment.cstar
examples/smoke/dereference_assignment_shortcut.cstar
examples/smoke/expression_newline_continuation.cstar
examples/smoke/multi_level_dereference_assignment.cstar
examples/smoke/multi_level_dereference_read.cstar
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
examples/smoke/loop_while_statement.cstar
examples/smoke/loop_false_skip.cstar
examples/smoke/loop_nested_if_statement.cstar
examples/smoke/loop_pointer_condition.cstar
examples/smoke/loop_break_statement.cstar
examples/smoke/loop_continue_statement.cstar
examples/smoke/loop_break_continue_nested_if.cstar
examples/smoke/loop_nested_loop_continue.cstar
examples/smoke/multidim_array_assignment.cstar
examples/smoke/multidim_array_dynamic_index.cstar
examples/smoke/multidim_array_param_read.cstar
examples/smoke/multidim_array_param_write.cstar
examples/smoke/multidim_array_read.cstar
examples/smoke/multidim_array_shortcut_assignment.cstar
examples/smoke/struct_field_read.cstar
examples/smoke/struct_field_assignment.cstar
examples/smoke/struct_function_param_return.cstar
examples/smoke/struct_nested_field.cstar
examples/smoke/struct_method_self.cstar
examples/smoke/struct_method_no_param_sugar.cstar
examples/smoke/struct_constructor_default.cstar
examples/smoke/struct_constructor_args.cstar
examples/smoke/struct_unique_pointer_field_method.cstar
examples/smoke/struct_shared_pointer_field_method.cstar
examples/smoke/struct_scope_method_receiver.cstar
examples/smoke/struct_destructor_method.cstar
examples/smoke/struct_pointer_destructor_method.cstar
examples/smoke/struct_static_new_factory.cstar
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
- Güncel durumda module helper dosyaları hariç 106/106 dosya başarılı.

`examples/type_checker/`:

- Büyük kısmı bilerek hata üretmesi beklenen semantic/type-check örnekleridir.
- Exit code `1` çoğu dosya için kabul edilebilir diagnostic olabilir.
- `// expected-code: CSTNNNN` etiketi varsa runner diagnostic kodunu da doğrular.
- Assert/crash kabul edilemez; önce bunlar izole edilmeli.
- Güncel durumda `-ExpectDiagnostics` ile 64/64 dosya kontrollü diagnostic üretiyor, crash/assert yok.

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
examples/type_checker/019.cstar
examples/type_checker/020.cstar
examples/type_checker/021.cstar
examples/type_checker/022.cstar
examples/type_checker/023.cstar
examples/type_checker/024.cstar
examples/type_checker/025.cstar
examples/type_checker/026.cstar
examples/type_checker/027.cstar
examples/type_checker/028.cstar
examples/type_checker/029.cstar
examples/type_checker/030.cstar
examples/type_checker/031.cstar
examples/type_checker/032.cstar
examples/type_checker/033.cstar
examples/type_checker/034.cstar
examples/type_checker/035.cstar
examples/type_checker/036.cstar
examples/type_checker/037.cstar
examples/type_checker/038.cstar
examples/type_checker/039.cstar
examples/type_checker/040.cstar
examples/type_checker/041.cstar
examples/type_checker/042.cstar
examples/type_checker/043.cstar
examples/type_checker/044.cstar
examples/type_checker/045.cstar
examples/type_checker/046.cstar
examples/type_checker/047.cstar
examples/type_checker/048.cstar
examples/type_checker/049.cstar
examples/type_checker/050.cstar
examples/type_checker/051.cstar
examples/type_checker/052.cstar
examples/type_checker/053.cstar
examples/type_checker/054.cstar
examples/type_checker/055.cstar
examples/type_checker/056.cstar
```

Bu dosyalar parser/pass hattına giriyor ve semantic diagnostic üretebiliyor. Kritik semantic sınıflar `expected-code` etiketiyle doğrulanabilir.

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
- `examples/papers/syntax.cstar` artık include/import/export yüzeyini parse ediyor; ilk kontrollü diagnostic `attribute Area<T>` proposal alanında üretiliyor.
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
- macOS/Homebrew LLVM akışında `arm64-apple-darwin*` vs `arm64-apple-macosx*` target ayrışması giderildi.
- LLVM shared CMake target'ı varsa link'te component arşivleri yerine shared `LLVM` target'ı kullanılır; yoksa component fallback korunur.
- CLI driver modları tamamlandı:
  - `--emit=<ir|asm|obj|staticlib|dynamiclib|exe>`
  - `--emit-llvm`
  - `--emit-asm`
  - `--emit-staticlib`
  - `--emit-dynamiclib`
  - `--build-exe`
  - `--run`
  - `--no-run`
  - `--output-dir <path>`
  - `--verbose`
  - `--stats`
  - Varsayılan davranış executable üretip çalıştırmadan durur; test runner açıkça `--run` kullanır.
  - Pass süreleri, output path ve generated exit code kontrollü biçimde raporlanır.
- Data layout açıkça set edildi.
- Build uyarıları sınıflandırıldı ve bu aşamadaki gerçek build uyarıları temizlendi:
  - Target triple override: gerçek target modelleme bug'ıydı, düzeltildi.
  - LLVM deprecation: `CreateGlobalStringPtr` kullanımı kaldırıldı.
  - Duplicate LLVM static library link warning: shared LLVM target tercih edilerek giderildi.
  - Eski AST tasarım borçları çalışma zamanı davranışını bozmayan ayrı dil aşamalarına taşındı.

Kalanlar:

- Visual Studio/MSVC gerçek doğrulaması:
  - `C:\Program Files\LLVM\lib\cmake\llvm` benzeri MSVC uyumlu LLVM kurulumu ile test edilecek.
  - MSYS2 LLVM paketleri MSVC linker ile karıştırılmayacak.
- Example runner:
  - Her suite için beklenen durum manifest'i eklenecek.
- Diagnostic sistemi:
  - Her semantic hata call-site'ı generic `CST2001` yerine özel hata kodlarına ayrılacak.
  - Diagnostic mesaj metinleri Türkçe/İngilizce terminoloji açısından standartlaştırılacak.
  - Parser/semantic recovery stratejisi belirlenecek; tek hatada çıkılan ve devam edilebilen modlar ayrılacak.

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
- `:=` token'ı assignment grammar'ına eklendi; C* içinde type inference değil unique ownership transfer intent'i olarak kullanılır.
- `.=` lexer/parser düzeyinde ayrı token olarak tanınır; dynamic protocol/provability-gap assignment lowering netleşene kadar kontrollü proposal diagnostic üretir.

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
  - `examples/smoke/reference_param.cstar`
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
  - Tamamlandı: qualifier argümanları.
  - Tamamlandı: tek ve çok boyutlu array parametreleri.
- Forward declaration call:
  - Tamamlandı: `foo` çağrıldığı noktadan sonra tanımlansa da codegen çalışıyor.
- Import/forward declaration call:
  - Tamamlandı: basit native import bildirimi ve external call codegen.
  - İsimsiz ABI parametresi: `import abs(int32) :: int32;`
  - İsimli proposal/doküman formu: `import abs(int32 value) :: int32;`
  - Smoke:
    - `examples/smoke/import_function_abs.cstar`
    - `examples/smoke/import_function_named_param.cstar`

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
- Primitive reference parametre codegen smoke:
  - `examples/smoke/reference_param.cstar`
- Qualifier parametre codegen smoke:
  - `examples/smoke/constref_param.cstar`
  - `examples/smoke/constptr_param.cstar`
  - `examples/smoke/readonly_param.cstar`
- Qualifier parametre negative diagnostic:
  - `examples/type_checker/020.cstar`
  - `examples/type_checker/034.cstar`
  - `examples/type_checker/035.cstar`
- Tek boyutlu array parametre codegen smoke:
  - `examples/smoke/array_param_read.cstar`
  - `examples/smoke/array_param_write.cstar`
- Array parametre negative diagnostic:
  - `examples/type_checker/036.cstar`
  - `examples/type_checker/037.cstar`

İleri aşama: Bu alt aşamada açık madde kalmadı. Çok boyutlu array ve colon indexing Aşama 4'te tamamlandı.

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
  - pointer qualifier stripping reddedilir.
- `expr as T` safe cast syntax'ı:
  - `4.5 as int32`
  - parser/codegen yolu `CastOpAST::C_AS` ile çalışır.
- `unsafe_cast<T>(expr)` looser cast MVP:
  - integer -> pointer.
  - pointer -> integer.
  - pointer -> pointer.
  - normal numerik dönüşümlerde safe cast helper'ını kullanır.
- User-defined type cast MVP kuralı:
  - `struct`/defined type sistemi gelene kadar controlled diagnostic üretir.
  - codegen assert'e düşmez.
- Smoke:
  - `examples/smoke/cast_numeric.cstar`
  - `examples/smoke/as_cast_numeric.cstar`
  - `examples/smoke/function_call_cast_argument.cstar`
  - `examples/smoke/unsafe_cast_int_to_pointer.cstar`
- Negative diagnostic:
  - `examples/type_checker/018.cstar`
  - `examples/type_checker/038.cstar`
  - `examples/type_checker/039.cstar`
  - `examples/type_checker/040.cstar`

İleri aşama: Bu alt aşamada açık madde kalmadı. Gerçek user-defined cast/conversion overload kuralları `struct`/`trait` sistemi geldikten sonra Aşama 7 altında tasarlanacak.

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
- `examples/smoke/multi_level_dereference_read.cstar`
- `examples/smoke/pointer_from_pointer_initializer.cstar`
- `examples/smoke/pointer_return.cstar`
- `examples/smoke/const_value.cstar`
- `examples/smoke/const_pointer.cstar`
- `examples/smoke/reference_param.cstar`
- `examples/smoke/constref_param.cstar`
- `examples/smoke/constptr_pointer.cstar`
- `examples/smoke/readonly_pointer.cstar`
- `examples/smoke/readonly_multi_level_pointer.cstar`
- `examples/type_checker/014.cstar`
- `examples/type_checker/019.cstar`
- `examples/type_checker/020.cstar`
- `examples/type_checker/021.cstar`
- `examples/type_checker/022.cstar`
- `examples/type_checker/023.cstar`
- `examples/type_checker/024.cstar`
- `examples/type_checker/025.cstar`
- `examples/type_checker/026.cstar`
- `ref x` codegen'i yerel/global sembolün adresini üretir.
- `deref p` codegen'i beklenen tipe göre pointer'dan load üretir; pointer sonuçlar da desteklenir.
- `int32& x` primitive reference parametreler çağıranın storage'ına alias olur.
- Reference parametre çağrısı açık `ref value` ister; çıplak value geçişi diagnostic üretir.
- Reference parametre gövde içinde normal sembol gibi okunur ve assignment çağıranın değerini günceller.
- `const int32 value` okunabilir, yeniden assignment kontrollü diagnostic üretir.
- `const int32* p` pointer hedef değerini salt-okunur yapar; `p = ref other;` serbesttir.
- `const int32* p` için `deref p = value;` kontrollü diagnostic üretir.
- `constref int32& x` primitive reference parametreler mutable storage'a read-only alias olarak bağlanır.
- `constref` parametre gövde içinde değer olarak okunabilir; assignment kontrollü diagnostic üretir.
- `constptr int32* p = ref value;` pointer adresini sabitler, `deref p` ile hedef değer okunup yazılabilir.
- `constptr` pointer'a yeniden adres assignment kontrollü diagnostic üretir.
- `readonly int32* p = ref value;` pointer adresini ve hedef değeri salt-okunur kabul eder.
- `readonly` pointer'da `p = ref other;` ve `deref p = value;` kontrollü diagnostic üretir.
- `**pp` gibi çok seviyeli dereference read doğrudan expression içinde çalışır.
- `readonly int32**` çok seviyeli pointer read desteklenir; `**pp = value;` diagnostic üretir.
- `const int32** p = ref mutable_pointer;` MVP'de const-hole riski nedeniyle diagnostic üretir.
- `int32* p = ref x;` ile tek seviyeli primitive pointer variable initializer çalışır.
- `deref p = value;` ve `*p = value;` assignment target olarak çalışır.
- `deref p += value;` gibi shortcut assignment'lar pointer hedefte çalışır.
- `**pp = value;` çok seviyeli dereference assignment çalışır.
- `int32* q = deref pp;` pointer'dan pointer okuma initializer içinde çalışır.
- `identity(int32* p) :: int32* { ret p; }` gibi pointer return çalışır.
- Pointer olmayan sembole `deref` assignment kontrollü diagnostic üretir.
- Pointer/ref/qualifier semantic diagnostics standardize edildi:
  - `CST2100`: qualifier mismatch.
  - `CST2101`: invalid qualifier/type combination.
  - `CST2102`: const assignment.
  - `CST2103`: constptr pointer reassignment.
  - `CST2104`: readonly assignment.
  - Safe cast qualifier stripping artık `CST2100` üretir.
  - Qualifier mismatch ve invalid qualifier mesajları standartlaştırıldı.
  - `examples/type_checker/016.cstar`, `017.cstar`, `020.cstar`, `021.cstar`, `022.cstar`, `023.cstar`, `024.cstar`, `025.cstar`, `026.cstar`, `027.cstar`, `034.cstar`, `035.cstar`, `039.cstar` dosyaları `expected-code` ile doğrulanır.
- `tools/run_examples.ps1` diagnostic suite'lerinde `expected-code` kontrolü yapar.

İleri aşama: Bu alt aşamada açık madde kalmadı. Ownership transfer ve lifetime başlıkları Aşama 3.2 altında takip edilecek.

### 3.2 Ownership Pointer `^`

Durum: unique `^` ve shared `*` pointer ayrımı compiler çekirdeğinde gerçek semantik taşır.

Tamamlanan:

- `*` shareable pointer artık raw pointer ABI değildir; LLVM IR'da compiler-owned shared handle `{ data: ptr, strong: i64* }` olarak taşınır.
- Shared pointer copy, assignment ve move lowering'i compiler çekirdeğinde yapılır.
- Shared pointer strong-count işlemleri atomic `rmw`/atomic load ile thread-safe üretilir.
- `strong_count(ptr)` compiler builtin'i atomic strong-count değerini döndürür.
- `:=` deklarasyon ve assignment tarafında pointer ownership transfer intent'i olarak parse edilir.
- Shared pointer `:=`/`move` source'u moved kabul edilir; tekrar kullanım `CST2105` üretir.
- `int32^ target := source;` ve `target := source;` smoke ile çalışır.
- Unique pointer doğrudan kopyalanamaz; `int32^ target = source;` `CST2105` diagnostic üretir.
- Moved-after-use takibi hem `^` hem shared `*` pointer için semantic pass'te yapılır.
- Function argument/return ownership transfer kuralları tamamlandı:
  - by-value `^` parametre plain unique pointer copy'sini `CST2105` ile reddeder.
  - by-value `^` parametre `move pointer` ile açık transfer kabul eder.
  - by-value `^` return için `ret move pointer;` gerekir; `ret pointer;` `CST2105` üretir.
  - by-value shared `*` parametre plain symbol ile retain/copy yapar.
  - function argument/return `move` source'u null'a çekilir ve semantic pass'te moved kabul edilir.
- Thread boundary transferleri, async/task ownership ve `Send`/`Sync` benzeri marker tasarımı tamamlandı:
  - `async` function effect ve `await` expression reserved proposal keyword olarak lexer'a eklendi.
  - `async`/`await` henüz lowering üretmez; parser `CST1001` controlled diagnostic verir.
  - Gelecek `spawn`/task boundary kuralı: by-value `^` yalnızca `move` ile taşınır, plain unique copy yasaktır.
  - Shared `*` task boundary'de atomic retain/copy veya açık `move` transfer kullanır.
  - `Send`/`Sync` runtime vtable değil, trait/capability marker olarak tasarlanır; scheduler lowering bu marker'ları kontrol edecek.
- `nomove`/policy proposal'ı ile uyumlandırma tamamlandı:
  - `nomove` type qualifier değil, parametre seviyesinde ownership-flow modifier olarak tutulur.
  - `nomove int32^ pointer` ve `nomove constptr int32^ pointer` parser tarafından kabul edilir.
  - `nomove` yalnızca by-value pointer parametrelerde geçerlidir; scalar/reference kullanım `CST2105` üretir.
  - `nomove` parametre okunabilir ve dereference edilebilir, fakat `move`, `:=`, function argument move veya return move kaynağı olamaz.
  - Bu karar policy/protocol ile gizli hook oluşturmaz; protocol state güvenliği ayrı, `nomove` ownership-flow kısıtı ayrı kalır.
- Aynı type içinde `*` ve `^` karışımı parser diagnostic üretir.
- `CST2105`: ownership/move semantic diagnostic.
- `examples/smoke/shared_pointer_copy_count.cstar`
- `examples/smoke/shared_pointer_assignment_count.cstar`
- `examples/smoke/shared_pointer_function_arg_count.cstar`
- `examples/smoke/shared_pointer_move_assignment.cstar`
- `examples/smoke/unique_pointer.cstar`
- `examples/smoke/unique_pointer_function_move_arg.cstar`
- `examples/smoke/unique_pointer_move_init.cstar`
- `examples/smoke/unique_pointer_move_assignment.cstar`
- `examples/smoke/unique_pointer_return_move.cstar`
- `examples/smoke/nomove_unique_param_read.cstar`
- `examples/type_checker/028.cstar`
- `examples/type_checker/029.cstar`
- `examples/type_checker/030.cstar`
- `examples/type_checker/031.cstar`
- `examples/type_checker/032.cstar`
- `examples/type_checker/033.cstar`
- `examples/type_checker/041.cstar`
- `examples/type_checker/042.cstar`
- `examples/type_checker/043.cstar`
- `examples/type_checker/044.cstar`
- `examples/type_checker/045.cstar`
- `examples/type_checker/046.cstar`
- `examples/type_checker/047.cstar`
- `examples/type_checker/048.cstar`

Kalan:

- Scope çıkışı/destructor lowering ile final strong-count release.
- Heap allocation/control-block layout `new`/allocator sistemi ile birleştirilecek.

### 3.3 Qualifier

Tamamlanan:

- Qualifier diagnostic kodları ayrıldı:
  - `CST2100`: qualifier mismatch.
  - `CST2101`: invalid qualifier/type combination.
  - `CST2102`: const assignment.
  - `CST2103`: constptr pointer reassignment.
  - `CST2104`: readonly assignment.
  - `examples/type_checker/027.cstar`
- Per-level qualifier metadata temeli eklendi:
  - `SymbolInfo::qualifierLevels` index `0` final target value, index `N` pointer object level `N` olacak şekilde tutulur.
  - Mevcut prefix syntax bu metadata'ya conservative biçimde map edilir.
  - `const` depth `0` target value olarak işaretlenir.
  - `constptr` en dış pointer object seviyesine işlenir.
  - `constref` reference target value seviyesine işlenir.
  - `readonly` tüm mevcut seviyelere işlenir.
  - Henüz yeni surface syntax eklenmedi; bu sonraki proposal adımıdır.
- `const int32` scalar okuma ve assignment reddi:
  - `examples/smoke/const_value.cstar`
  - `examples/type_checker/016.cstar`
- `const int32*` target/value ayrımı:
  - pointer adresi değiştirilebilir.
  - hedef değer `deref` ile okunabilir.
  - hedef değere `deref` ile yazma reddedilir.
  - `examples/smoke/const_pointer.cstar`
  - `examples/type_checker/024.cstar`
- `constref int32&` parametreye `ref value` ile mutable storage bağlama.
- `constref` parametreyi değer olarak okuma.
- `constref` parametreye assignment reddi:
  - `examples/smoke/constref_param.cstar`
  - `examples/type_checker/020.cstar`
- `constptr int32*` parametre callee içinde target write'a izin verir, pointer reassignment reddedilir:
  - `examples/smoke/constptr_param.cstar`
  - `examples/type_checker/034.cstar`
- `readonly int32*` parametre callee içinde target read'e izin verir, target write reddedilir:
  - `examples/smoke/readonly_param.cstar`
  - `examples/type_checker/035.cstar`
- `constptr int32*` initializer, dereference read/write ve pointer address reassignment reddi:
  - `examples/smoke/constptr_pointer.cstar`
  - `examples/type_checker/021.cstar`
- `readonly int32*` initializer, dereference read ve address/value assignment reddi:
  - `examples/smoke/readonly_pointer.cstar`
  - `examples/type_checker/022.cstar`
  - `examples/type_checker/023.cstar`
- Çok seviyeli pointer qualifier MVP:
  - `**pp` doğrudan read expression olarak çalışır.
  - `readonly int32**` doğrudan read expression olarak çalışır.
  - `readonly int32**` üzerinden hedef yazma reddedilir.
  - `const int32**` mutable pointer zincirine bağlanmaz.
  - `examples/smoke/multi_level_dereference_read.cstar`
  - `examples/smoke/readonly_multi_level_pointer.cstar`
  - `examples/type_checker/025.cstar`
  - `examples/type_checker/026.cstar`

Kalan:

- Per-level qualifier syntax tasarımı:
  - `const int32*` mevcut prefix syntax olarak korunur.
  - Pointer seviyesine özel qualifier yazımı için proposal netleştirilecek.

## Aşama 4 - Arrays ve Indexing

Durum: sabit boyutlu tek ve çok boyutlu array MVP tamamlandı.

Tamamlanan:

- Tek boyutlu local array:
  - `int32 arr[2] = (1, 2);`
- Array element read:
  - `ret arr[0];`
- Array element assignment:
  - `arr[0] = 3;`
- Array element shortcut assignment:
  - `arr[0] += 3;`
- Çok boyutlu array syntax'ı C* proposal formuna sadıktır:
  - declaration/parametre: `int32 matrix[2:3]`
  - initializer: `((1, 2, 3), (4, 5, 6))`
  - read: `matrix[1:2]`
  - assignment: `matrix[1:2] = 9;`
  - shortcut assignment: `matrix[1:2] += 1;`
- Flattening kuralı row-major olarak netleşti:
  - Kaynak syntax katmanlı initializer kullanır; codegen flat LLVM array storage üretir.
  - `[rows:cols]` için linear index `row * cols + col`.
  - Daha yüksek boyutta kural soldan sağa `linear = linear * next_dim + index`.
- Çok boyutlu local array read/write smoke:
  - `examples/smoke/multidim_array_read.cstar`
  - `examples/smoke/multidim_array_assignment.cstar`
  - `examples/smoke/multidim_array_shortcut_assignment.cstar`
  - `examples/smoke/multidim_array_dynamic_index.cstar`
- Çok boyutlu array parametre read/write smoke:
  - `examples/smoke/multidim_array_param_read.cstar`
  - `examples/smoke/multidim_array_param_write.cstar`
- Bounds politikası MVP:
  - Sabit out-of-range index için compile-time warning üretilir.
  - Dinamik index için runtime bounds check şimdilik üretilmez.
  - Runtime checked array/slice modeli ileride stdlib/safety mode altında ele alınacak.
- Expression parser newline dayanıklılığı:
  - Delimiter içi veya operator/comma sonrası satır sonları expression devamı kabul edilir.
  - `((1, 2, 3),\n (4, 5, 6))`, `1 +\n2`, `func(a,\nb)` ve `arr[1:\n2]` smoke ile korunur.

İleri aşama: Bu aşamada açık MVP maddesi kalmadı. Slice/range indexing ve runtime bounds check ayrı ileri aşamaya taşındı.

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

Tamamlanan:

- While-style MVP tamamlandı:
  - `examples/smoke/loop_while_statement.cstar`
  - `examples/smoke/loop_false_skip.cstar`
  - `examples/smoke/loop_nested_if_statement.cstar`
  - `examples/smoke/loop_pointer_condition.cstar`
- `break` / `continue` parser, semantic diagnostic ve codegen tamamlandı:
  - `examples/smoke/loop_break_statement.cstar`
  - `examples/smoke/loop_continue_statement.cstar`
  - `examples/smoke/loop_break_continue_nested_if.cstar`
  - `examples/smoke/loop_nested_loop_continue.cstar`
  - `examples/type_checker/049.cstar`
  - `examples/type_checker/050.cstar`
- Range loop MVP tamamlandı:
  - `examples/smoke/loop_range_statement.cstar`
  - `examples/smoke/loop_range_break_continue.cstar`
- Array iterable MVP tamamlandı:
  - `examples/smoke/loop_array_iterable.cstar`
  - `examples/smoke/loop_array_indexed_iterable.cstar`
  - `examples/smoke/loop_array_param_iterable.cstar`

İleri aşama: Bu alt aşamada temel loop yüzeyi tamamlandı. Sequence/trait tabanlı genel iterable, reverse/step range ve bounds politikası ileride stdlib/trait aşamasına taşındı.

### 5.2 Option / Match Benzeri Yapı

Proposal seviyesinde.

Karar:

- Canonical keyword `option` kalır; ayrı `match` keyword'ü şimdilik eklenmez.
- İlk yüzey statement odaklıdır, expression/value döndürmez:
  - `option (value) { pattern: { ... }, _: { ... } }`
- `_` default branch anlamına gelir.
- Pattern MVP sadece literal scalar/char/bool ve `_` kabul eder; range/destructuring/guard sonra.
- Branch gövdeleri normal statement scope'u olur; `ret`, `break`, `continue` kendi bağlam kurallarını korur.
- Exhaustiveness kontrolü ilk MVP'de yalnızca `bool` için düşünülebilir; genel enum/struct pattern exhaustiveness `enum`/`struct` sonrası.

Tamamlanan:

- Function body içinde `option` artık parser'ı takmaz; controlled proposal diagnostic üretir.
  - `examples/type_checker/051.cstar`

İleri aşama: Gerçek parser/AST/codegen `enum` ve temel user-defined type tasarımından sonra açılacak. Bu alt aşamada açık parser güvenlik/tasarım maddesi kalmadı.

## Aşama 6 - Import / Package / Native Interop

Proposal hedefleri:

- `include`
- `include involved`
- `import ... from "lib"`
- `export ... from "module"`

Tamamlanan:

- `include` branch'i parser içinde boş olduğu için infinite loop riski vardı; artık gerçek grammar'a bağlı.
- `include involved { ... }`, `include { ... }` ve `include "module" as alias` parse ediliyor.
- Yerel `.cstar` include dosyaları ana compilation unit'e parse/merge ediliyor.
  - `examples/smoke/include_module_function.cstar`
  - `examples/smoke/modules/math_module.cstar`
- Basit `import func(...) :: type;` call codegen ile bağlandı.
  - `import abs(int32) :: int32;`
  - `import abs(int32 value) :: int32;`
  - Import parametre adı ABI için opsiyonel; semantic pass imzayı korur ama forward deklarasyon parametresini local symbol gibi kaydetmez.
- `import func(...) :: type from "lib";` parse edilir ve native link metadata'sına taşınır.
- `import { ... }` ve `import from "lib" { ... }` blokları parse/codegen akışına bağlı.
- `export func(...) :: type from "module";` ve `export from "module" { ... }` forward declaration olarak parse edilir.
- `export area<Circle>(...) :: type;` gibi generic function attribute syntax'ı declaration yüzeyinde parse edilir.
- Executable üretirken ana dosyada `export` varsa warning üretilir; `--emit=staticlib` veya `--emit=dynamiclib` önerilir.
- `--emit=staticlib` object + `ar rcs` ile static library üretir.
- `--emit=dynamiclib` backend clang `-shared` ile platform dynamic library üretir.
- `include ... as alias` function-call lookup'a bağlı:
  - `math.add_from_module(...)`
  - `examples/smoke/include_module_function.cstar`
- `public`/default-private module visibility MVP'si tamamlandı:
  - `public` function/variable declaration'ları include edilen local module'den ana compilation unit'e açılır
  - modifier yazılmayan declaration default private kabul edilir
  - `import`/`export` visibility değil, native/linkage ABI yüzeyidir
  - private module function'ına alias üzerinden erişim controlled diagnostic üretir
  - `examples/type_checker/052.cstar`
- Module-level `static` MVP'si tamamlandı:
  - static function LLVM tarafında internal linkage alır
  - static global variable internal linkage/storage davranışını korur
  - static function non-static global symbol/function kullanamaz
  - `examples/smoke/include_module_public_static.cstar`
  - `examples/type_checker/053.cstar`
  - `examples/type_checker/054.cstar`
- Native link normalizasyonu:
  - `"m"` -> Unix/macOS için `-lm`
  - `"foo.lib"`, `.a`, `.so`, `.dylib` ve path değerleri doğrudan linker'a geçer
  - `"std:math"` gibi logical module kaynakları linker argümanına çevrilmez

Kalan:

- Bu aşamada açık MVP maddesi kalmadı.
- Gerçek namespace/type module sistemi ve `struct`/user-defined type modül export/import davranışı Aşama 7 ile birlikte tasarlanacak.
- Not: Bugünkü include modeli source-level public declaration merge yapar; public function body içinde private module helper lowering'i gerçek module object/scope modeliyle birlikte genişletilecek.

## Aşama 7 - User-defined Types

Proposal hedefleri:

- `struct`
- `trait`
- `protocol`
- `dynamic protocol`
- custom allocator benzeri fikirler.

Durum:

- `SPEC_DEFINED` var.
- Lexer `struct`, `trait`, `protocol`, `dynamic`, `state`, `with`, `constructor`, `destructor`, `allocator`, `except`, `throw`, `defer`, `self`, `is`, `macro` keyword'lerini tanır.
- `struct Name { field; ... }` MVP grammar'ı parse edilir.
- Struct declaration pass0 type table'a ve `StructTable` metadata'sına kaydedilir.
- Primitive field layout LLVM `StructType` olarak üretilir.
- Struct local/global variable zero-init storage üretir.
- Struct field read `value.field` syntax'ı ile çalışır.
- Struct field assignment target çalışır:
  - `value.field = expr;`
  - `value.field += expr;`
- Struct by-value function parametre ve return çalışır.
- Nested/by-value struct field layout ve recursive field-chain access çalışır:
  - `line.start.x`
  - `line.start.x = expr;`
- Unknown field ve duplicate field controlled diagnostic üretir.
- Direct self-by-value struct field controlled diagnostic üretir:
  - `examples/type_checker/057.cstar`
- Struct method syntax ve `self` lowering MVP'si çalışır:
  - Methodlar `StructName.method(self&, ...)` internal fonksiyonuna iner.
  - `value.method(args)` çağrısı receiver'ı implicit `self` argümanı yapar.
  - `self.field` read/write caller struct storage'ına referans üzerinden iner.
  - Parantezsiz no-param method tanımı desteklenir: `read :: int32 { ... }`.
  - `examples/smoke/struct_method_self.cstar`
  - `examples/smoke/struct_method_no_param_sugar.cstar`
- Unknown method controlled diagnostic üretir:
  - `examples/type_checker/058.cstar`
- Constructor/function call ayrımı MVP'si tamamlandı:
  - `constructor(args) { ... }` struct body içinde parse edilir.
  - Constructor internal olarak `StructName.constructor(self&, ...)` fonksiyonuna iner.
  - `StructName value = StructName(args);` local initializer zero-init storage oluşturup constructor'ı o storage üzerinde çağırır.
  - Constructor initializer yalnız local by-value struct için açıktır.
  - Constructor yoksa controlled diagnostic üretilir.
  - `examples/smoke/struct_constructor_default.cstar`
  - `examples/smoke/struct_constructor_args.cstar`
  - `examples/type_checker/059.cstar`
- Pointer/shared handle receiver ve field access MVP'si tamamlandı:
  - `Point^ owned = ref p; owned.x` unique pointer pointee alanına iner.
  - `Counter* shared = ref c; shared.value` compiler-owned shared handle içindeki data pointer'a iner.
  - `owned.method(args)` ve `shared.method(args)` receiver'ı auto-deref edip implicit `self&` parametresine pointee adresini geçirir.
  - Shared handle copy/assignment atomic retain/release davranışını korur; struct field/method erişimi bu handle üstünden raw pointer fallback'e düşmez.
  - `examples/smoke/struct_unique_pointer_field_method.cstar`
  - `examples/smoke/struct_shared_pointer_field_method.cstar`
  - `examples/smoke/struct_scope_method_receiver.cstar`
  - `examples/type_checker/060.cstar`
- Static struct method MVP'si tamamlandı:
  - `static method(...)` struct body içinde implicit `self` almadan parse edilir.
  - `Type::method(args)` yalnız static struct method çağrısıdır.
  - Instance çağrıları için `value.method(args)`, `owned.method(args)` ve `shared.method(args)` kullanılır.
  - `value::method(args)` / `Type::non_static(args)` controlled diagnostic üretir.
  - `examples/smoke/struct_scope_method_receiver.cstar`
  - `examples/type_checker/062.cstar`
- Static `new` factory MVP'si tamamlandı:
  - `new` struct içinde lifecycle factory adı olarak ayrıldı.
  - `new` static olmak zorundadır ve `Type::new(args)` formuyla çağrılır.
  - İlk MVP'de `new` aynı struct type'ını by-value döndürür; heap allocation/control-block bağlama allocator aşamasına kaldı.
  - Non-static `new` controlled semantic diagnostic üretir.
  - `examples/smoke/struct_static_new_factory.cstar`
  - `examples/type_checker/063.cstar`
- Explicit destructor method MVP'si tamamlandı:
  - `destructor(...) { ... }` struct body içinde method olarak parse edilir.
  - Internal lowering normal method modeliyle `StructName.destructor(self&, ...)` fonksiyonuna iner.
  - `value.destructor()` ve `ptr.destructor()` explicit çağrı olarak çalışır.
  - Otomatik scope-exit çağrısı, ownership release ve allocator/new entegrasyonu bu MVP'nin parçası değildir.
  - `allocator` hook'u trait/allocator aşamasına kadar controlled parser diagnostic üretir.
  - `examples/smoke/struct_destructor_method.cstar`
  - `examples/smoke/struct_pointer_destructor_method.cstar`
  - `examples/type_checker/061.cstar`
- `trait`, `protocol`, `dynamic protocol` ve ileri lifetime keyword'leri için controlled proposal diagnostic korunur.
- Eski `policy for T { ... }` runtime hook modeli ve `policy protocol` çift isimli form superseded kabul edildi.
- Ana yön `protocol Name for Type { ... }`: compile-time typestate/state contract.
- `static protocol` gereksizdir; static/provable davranış default kabul edilir.
- `dynamic protocol` açık runtime maliyeti isteyen durumlar içindir.
- `.=` token'ı parser'da tanınır, fakat yalnızca dynamic/provability-gap protocol lowering netleşince codegen'e alınacak; bugün proposal diagnostic üretir.

Kalan:

- Heap allocation/control-block layout'u `new`/allocator sistemiyle birleştir.
- Shared `*` handle ve unique `^` ile otomatik struct lifetime/release entegrasyonu.
- `protocol` parser tasarımı:
  - `protocol FileState for FileHandle { ... }`
  - `state closed, opened;`
  - `default closed;`
  - `closed -> opened :: open();`
  - `read() :: !closed;`
  - `scope_exit :: closed;`
- Protocol state'lerini mevcut qualifier/state slot'una bağla:
  - `opened FileHandle^`
  - `closed FileHandle^`
  - `const opened FileHandle^`
- Pass0 symbol/type table:
  - protocol adı, hedef type, state seti, default state.
  - transition table ve forbidden-call table.
- Pass1 flow analysis:
  - method call sonrası state transition.
  - return type state match.
  - moved pointer ile state taşınması.
  - `scope_exit` required state diagnostic.
- `dynamic protocol`:
  - explicit runtime tag field.
  - `.=` için görünür/desugar edilebilir switch lowering.
  - hidden hook/table/dispatch yok.
- Eski `policy for T` örnekleri dokümanda “superseded legacy proposal” olarak tutulacak; compiler ana grammar'ına alınmayacak.

### 7.1 Static

Durum:

- Local static, static member, init-order ve thread-safe static initialization yok.

Tamamlanan:

- Lexer ve parser `static` token'ını module-level function/variable declaration modifier olarak kabul eder.
- Global `static` internal linkage/storage davranışı alır.
- Function-level `static` LLVM internal linkage alır.
- Static function yalnızca static global state'e ve static function'lara erişebilir.
- Non-static global erişimi semantic diagnostic üretir:
  - `examples/type_checker/053.cstar`
- Non-static function call semantic diagnostic üretir:
  - `examples/type_checker/054.cstar`
- Struct-level static method MVP'si tamamlandı:
  - `static name(...)` / `static name :: type` struct body içinde self'siz method üretir.
  - `Type::name(args)` static method çağrısına iner.
  - `examples/smoke/struct_scope_method_receiver.cstar`
  - `examples/type_checker/062.cstar`

Kalan:

- Local static storage duration.
- Local static için one-time thread-safe init planı.
- Static member ve method/member static grammar kararları.

### 7.2 Struct

Tamamlanan:

- `struct Name { field; ... }` parser/AST.
- Field layout ve LLVM `StructType`.
- Field access/read: `value.field`.
- Field assignment target:
  - `value.field = expr;`
  - `value.field += expr;`
- By-value struct function parametre ve return.
- Nested/by-value struct field layout ve chained field access/assignment.
- Direct self-by-value field diagnostic:
  - `examples/type_checker/057.cstar`
- Method syntax ve `self` lowering MVP'si:
  - `value.method(args)`
  - implicit `self&`
  - `self.field` read/write
  - no-param method sugar: `name :: type { ... }`
  - `examples/smoke/struct_method_self.cstar`
  - `examples/smoke/struct_method_no_param_sugar.cstar`
  - `examples/type_checker/058.cstar`
- Constructor/function call ayrımı MVP'si:
  - `constructor(args) { ... }`
  - `StructName value = StructName(args);`
  - implicit constructor `self&`
  - local by-value struct initializer lowering
  - `examples/smoke/struct_constructor_default.cstar`
  - `examples/smoke/struct_constructor_args.cstar`
  - `examples/type_checker/059.cstar`
- Pointer/shared handle field access ve method receiver MVP'si:
  - `ptr.field` unique `^` ve shared `*` struct handle'larında pointee alanına iner.
  - `ptr.method(args)` unique/shared receiver'ı implicit `self&` için pointee adresine indirger.
  - `examples/smoke/struct_unique_pointer_field_method.cstar`
  - `examples/smoke/struct_shared_pointer_field_method.cstar`
  - `examples/smoke/struct_scope_method_receiver.cstar`
  - `examples/type_checker/060.cstar`
- Static struct method MVP'si:
  - `static method(...)`
  - `Type::method(args)`
  - `::` instance receiver için kullanılmaz.
  - `examples/smoke/struct_scope_method_receiver.cstar`
  - `examples/type_checker/062.cstar`
- Static `new` factory MVP'si:
  - `static new(...) :: Type`
  - `Type::new(args)`
  - non-static `new` diagnostic
  - `examples/smoke/struct_static_new_factory.cstar`
  - `examples/type_checker/063.cstar`
- Explicit destructor method MVP'si:
  - `destructor(...) { ... }`
  - `value.destructor()`
  - `ptr.destructor()`
  - `examples/smoke/struct_destructor_method.cstar`
  - `examples/smoke/struct_pointer_destructor_method.cstar`
  - `examples/type_checker/061.cstar`

Kalan:

- Heap allocation/control-block layout'u `new`/allocator sistemiyle birleştir.
- Otomatik destructor/scope-exit lowering.
- Shared `*` handle ve unique `^` ile otomatik struct lifetime/release entegrasyonu.
- `syntax.cstar` içindeki `struct Shape<T> from Area<T>` formunu ilk MVP'de parse etmeye çalışma; önce field/method struct çekirdeği, sonra attribute binding.
- Constructor MVP local by-value struct initializer için çalışır; `new`/allocator, destructor/scope-exit, shared retain/release ve custom allocator modeli aynı lifetime planına bağlanmalı.

### 7.3 Trait

Kalan:

- `trait Name { ... }` parser/AST.
- Trait requirement table.
- `struct T with TraitA, TraitB` grammar.
- Compile-time conformance check.
- Dynamic dispatch yok; monomorphized/static dispatch varsayılan.
- Generic bound syntax proposal'ı.
- Allocator capability için trait kullanılabilir; allocation failure eski policy hook yerine explicit `except`/`throw` veya result-like dönüş modeliyle tasarlanmalı.

## Aşama 8 - Metaprogramming ve İleri Proposal

Proposal'da görünen ama çok sonraya bırakılacak başlıklar:

- `attribute`
- directive/macro sistemi
- `$` ve `#` tabanlı compile-time hook'lar
- `async` / `await`
- `except` / `throw`
- compile-time error/runtime error hook'ları

Bu başlıklar çekirdek dil stabil olmadan uygulanmamalı.

### 8.1 Attribute

Durum:

- Lexer `attribute` keyword'ünü tanır.
- Parser `attribute` için controlled proposal diagnostic üretir.
- AST/semantic yok.

Kalan:

- `attribute Name<T> { ... }` grammar'ını proposal olarak netleştir.
- Attribute'ın trait'ten farkını yaz:
  - trait: type capability/contract.
  - attribute: compile-time transformation/reflection helper.
- `$0`, `$1`, `match`, `cterror` gibi compile-time expression yüzeyini tasarla.
- İlk MVP sadece parse + controlled diagnostic olmalı; expansion sonra.

### 8.2 Macro / Directive

Durum:

- Lexer `#` ve `$` tanır.
- Lexer `macro` keyword'ünü tanır.
- `@` şu an unhandled.
- Parser macro/directive AST yok.

Kalan:

- `macro name(args) { ... }` ile `#directive` ayrımını netleştir.
- Macro expansion zamanı:
  - parse-time mı,
  - semantic-time mı,
  - IR-before-codegen mi?
- Hijyen/hygiene kuralları.
- Error reporting ve source span mapping.
- `@directive` syntax'ı lexer'a alınacaksa token ve parser recovery tasarımı.
- Protocol ile macro/directive karıştırılmamalı; protocol gizli hook sistemi olmayacak.

## Bir Sonraki En İyi Adım

Tamamlanan son adım:

- Struct MVP genişletildi:

```cstar
struct Point {
    int32 x;
    int32 y;
}

struct Line {
    Point start;
    Point end;
}

sum(Point p) :: int32 {
    ret p.x + p.y;
}

main() :: int32 {
    Line line;
    line.start.x = 3;
    line.end.y += 1;
    ret sum(line.start) + line.end.y;
}
```

Parser/AST, StructTable metadata, LLVM `StructType`, zero-init storage, by-value param/return, nested field GEP zinciri, method/self lowering, constructor initializer MVP'si ve direct self-by-value diagnostic doğrulandı.

Sıradaki teknik iş:

- Aşama 7'de pointer/shared handle field access ve method receiver kararını netleştirmek.
- Ardından `new`/allocator ve destructor/scope-exit entegrasyonunu shared/unique lifetime modeliyle bağlamak.
- Sonra `trait` requirement table ve `protocol` parser/flow tasarımına geçmek.
