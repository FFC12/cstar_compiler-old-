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
- Seed'li stress/fuzz runner eklendi:
  - `tools/stress_fuzz.py` external dependency gerektirmeden rastgele C* programları üretir, invalid case'ler oluşturur ve `examples/smoke` + `std` corpus'unu mutate eder.
  - Diagnostic normal kabul edilir; crash, assert, segfault, sanitizer failure ve timeout bug sayılır.
  - Reproducer/log çıktıları `tests/stress/out/` altında tutulur.
  - Plan ve kullanım notları `tests/stress/README_TR.md` içindedir.
- C/C++ karşılaştırmalı performance benchmark runner eklendi:
  - `tools/perf_benchmark.py` aynı workload'u C*, C ve C++ kaynak olarak üretir.
  - Compile wall time, executable size, runtime wall time ve process return code ölçülür.
  - C median bazlı ratio tablosu üretir; runtime signal/crash durumunu ayrı işaretler.
  - Ham sonuçlar `tests/performance/out/results.csv`, rapor `tests/performance/out/REPORT.md` altındadır.
  - Plan ve kullanım notları `tests/performance/README_TR.md` içindedir.
  - Benchmark'ın yakaladığı loop içi local `alloca` stack büyümesi düzeltildi; local/param/loop slot'ları function entry block'ta allocate edilir. Regression: `examples/smoke/control_flow/loop_local_array_pressure.cstar`.
- `examples/smoke/` ve `examples/type_checker/` artık düz dosya yığını değildir; runner recursive çalıştığı için testler konuya göre alt klasörlere ayrıldı. `modules/` klasörleri yalnızca include helper dosyalarıdır ve suite tarafından skip edilir.
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
- Scalar enum MVP:
  - `enum Name : uint8 { A, B = 7 }`
  - explicit repr zorunlu.
  - canonical member erişimi `Name.A`.
  - enum local/param/return storage underlying integer'a iner.
  - unknown member ve enum-type mismatch diagnostic üretir.
- Flags enum MVP:
  - `flags enum FileMode : uint32 { Read = 1, Write = 2 }`
  - explicit bit value zorunlu.
  - değerler `0` veya power-of-two olmalı.
  - bitwise `|`, `&`, `^`, unary `~`, `|=`, `&=`, `^=` çalışır.
  - scalar enum bitwise kullanım, duplicate value ve repr overflow diagnostic üretir.
- Basit arithmetic expression.
- Integer ve floating point `+`, `-`, `*`, `/`, `%` codegen.
- Comparison/logical expression.
- Prefix/postfix increment-decrement statement:
  - `++x;`
  - `x++;`
  - `--x;`
  - `x--;`
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
  - `char*` / `const char*` CRT/string ABI'si raw C pointer olarak korunur; string literal `const char*` parametreye ve include edilen module fonksiyonlarına geçebilir.
  - Variadic native import/call eklendi:
    - `printf(const char* fmt, ...) :: int32;`
    - `...` parametre listesinin sonunda yer alır.
    - LLVM function declaration `isVarArg=true` üretilir.
    - Fixed parametreler normal type-check alır; `...` argümanları C ABI çağıran sorumluluğundadır.
    - C* içinde variadic function body yazmak henüz yoktur; bu yüzey native import/export ABI içindir.
  - Stdlib MVP modüler ve framework yönlü hale getirildi:
    - `std/print.cstar`: CRT `printf`/`scanf`/`getchar`/`fflush` üstünden `write`, `writeln`, `write_i32`, `write_i64`, `write_f64`, `write_char`, `line`, `flush`, `read_i32`, `read_i64`, `read_token`; ayrıca `TextWriter` trait'i, `ConsoleWriter` struct'ı, `StdPrintSurface` attribute'u ve modül içi `print_line` macro'su.
    - `std/math.cstar`: `Metric64` trait'i, `Vec2i` value type'ı, `StdMathValue` attribute'u, modül içi clamp/abs macro'ları, `Vec2i` operator `+`, `-`, `==`, `abs_i64`, `min_i64`, `max_i64`, `clamp_i64`, `sign_i64`, `gcd_i64`, `lerp_i64`, `vec2i`, `distance_i64`, bounded add/sub helper'ları.
    - `std/time.cstar`: `TickValue` trait'i, `Instant`, `Duration`, `StdTimeValue` attribute'u, `Duration` operator `+`/`-`, `now`, `from_ticks`, `elapsed`, `elapsed_ticks`, `duration_from_ticks`, `duration_from_ms`, `ticks_to_ms`, `seconds_to_ms`, `clamp_timeout_ms`.
    - `std/network.cstar`: `Protocol`, `SecureEndpoint` trait'i, `Endpoint`, `StdNetworkValue` attribute'u, `Endpoint == Endpoint`, endpoint method'ları, `default_port`, `is_secure`, `status_success`, `status_redirect`, `retry_backoff_ms`, endpoint helper'ları.
    - `std/fs.cstar`: CRT `fopen`/`fgetc`/`fclose` üstünden caller-owned `char[4096]` buffer'a text file okuma, `FileReadResult` value type'ı, `ReadableFile` trait'i, `StdFileValue` attribute'u, `fs_ok` public macro'su ve deterministic result kind helper'ları.
    - `std/core.cstar` eski `core_*` API için geriye uyumluluk dosyası olarak kalır.
    - `examples/smoke/stdlib/*.cstar` yeni stdlib modüllerini ayrı ayrı, advanced feature kombinasyonlarıyla ve `std_comprehensive_framework.cstar` içinde birlikte doğrular.
    - `public macro` declaration'ları source include alias üzerinden `alias.macroName(...)` qualified compile-time API olarak export edilir; `examples/smoke/stdlib/std_public_macro_alias.cstar` ve stdlib advanced smoke'ları `math.math_clamp_i64`, `net.http_success`, `time.time_seconds`, `io.print_line`, `fs.fs_ok` çağrılarını doğrular.
    - `examples/smoke/runtime/core_print_read.cstar` ve `core_read_string.cstar` legacy core wrapper'larını doğrulamaya devam eder.
  - `examples/smoke/imports/import_variadic_printf.cstar` doğrudan `printf("%s %d %lld %f", ...)` ABI doğrulamasıdır.
  - Function call ownership codegen'i `char*`/`const char*` parametrelerini shared pointer gibi retain etmeyecek şekilde düzeltildi; CRT string ABI raw pointer kalır.
  - `void*` opaque native handle ABI'si raw pointer olarak standartlaştırıldı; GLFW/CRT gibi C interop yüzeylerinde monitor/window/file handle değerleri shared pointer control-block'a sarılmaz.
  - `ref arr[index]` codegen'i eklendi; array element adresi C ABI pointer olarak alınabilir.
  - Variadic array symbol argument codegen'i array value yerine storage pointer geçirir; `scanf("%255s", buffer)` çalışır.
  - Function parameter list parsing comma sonrası ve kapanış parantezi öncesi newline/comment trivia'ya dayanıklı hale getirildi; `examples/smoke/core/function_param_newline_continuation.cstar` ile doğrulanır.
  - Include edilen `.cstar` kaynak dosyalardaki `public macro` tanımları preprocess öncesi toplanır ve alias-qualified macro call expansion'a açılır.
  - Native link metadata linker flag değil logical library adı taşır: `import from "OpenGL" { ... }` macOS'ta backend tarafından `-framework OpenGL`, `import from "glfw" { ... }` Unix/macOS'ta `-lglfw`, Windows'ta `glfw.lib` olarak çözülür. Legacy `-lfoo` / `-framework Foo` formları normalize edilir ama canonical C* source yüzeyi değildir.
  - Bu katman ileride stdlib/native interop ABI'sinin bağlanacağı giriş noktasıdır.
  - Açık takip: `va_list`/`va_arg` gibi C* içinde variadic body yazma modeli ayrı dil tasarımı gerektirir.

Çalışan smoke seti artık kategori bazlıdır:

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
examples/smoke/stdlib/
examples/smoke/modules/        # helper; runner skip eder
```

Güncel smoke doğrulama sonucu:

```text
Toplam: 145, Basarili: 141, Diagnostic: 0, Skipped: 4, Hatali: 0, ExitMismatch: 0, CodeMismatch: 0, Crash/Assert: 0
```

Eski düz liste notları tarihsel bağlam için aşağıda kalabilir; canonical dosya yerleşimi artık yukarıdaki kategori ağacıdır.

```text
examples/smoke/core/minimal.cstar
examples/smoke/core/local_int.cstar
examples/smoke/core/global_variable.cstar
examples/smoke/uninitialized_local.cstar
examples/smoke/void_return.cstar
examples/smoke/char_literal.cstar
examples/smoke/float32_arithmetic.cstar
examples/smoke/float64_arithmetic.cstar
examples/smoke/bool_literal.cstar
examples/smoke/scalar_enum.cstar
examples/smoke/flags_enum.cstar
examples/smoke/binary_expr.cstar
examples/smoke/comparison_expr.cstar
examples/smoke/not_equal_expr.cstar
examples/smoke/logical_expr.cstar
examples/smoke/increment_decrement.cstar
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
examples/smoke/imports/include_module_function.cstar
examples/smoke/imports/include_module_public_static.cstar
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
examples/smoke/struct_scope_exit_destructor.cstar
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
- Dosyalar konu bazlı alt klasörlerdedir: `core`, `casts`, `arrays`, `control_flow`, `functions`, `imports`, `pointers`, `ownership`, `runtime`, `enums`, `structs`. `modules` helper klasörüdür.
- Güncel durumda runner'ın skip ettiği module helper dosyaları hariç 134/134 smoke dosyası başarılı; toplam 137 smoke dosyasının 3 tanesi bilinçli skip edilir.

`examples/type_checker/`:

- Büyük kısmı bilerek hata üretmesi beklenen semantic/type-check örnekleridir.
- Exit code `1` çoğu dosya için kabul edilebilir diagnostic olabilir.
- `// expected-code: CSTNNNN` etiketi varsa runner diagnostic kodunu da doğrular.
- Assert/crash kabul edilemez; önce bunlar izole edilmeli.
- Dosyalar konu bazlı alt klasörlerdedir: `core`, `casts`, `arrays`, `control_flow`, `functions`, `imports`, `pointers`, `ownership`, `runtime`, `enums`, `structs`, `traits`, `proposals`. `modules` helper klasörüdür.
- Güncel durumda `-ExpectDiagnostics` ile 117 dosyada 113 kontrollü diagnostic, 2 positive/pass ve 2 module helper skip var; crash/assert yok.

Tamamlanan crash/assert düzeltmesi:

```text
examples/type_checker/core/000.cstar
examples/type_checker/core/001.cstar
examples/type_checker/core/003.cstar
examples/type_checker/core/007.cstar
examples/type_checker/core/008.cstar
```

Bu dosyalar daha önce lexer sonrası parser/semantic tarafında `std::deque::operator[]` assert'ine düşüyordu. Expression parser'daki boş `parenthesesPos`, `sparenthesesPos` ve `ternaryPos` erişimleri guard edildi. Artık crash yerine kontrollü diagnostic yoluna giriyorlar.

Kontrollü diagnostic üreten type-checker seti kategori bazlıdır:

```text
examples/type_checker/arrays/
examples/type_checker/casts/
examples/type_checker/control_flow/
examples/type_checker/core/
examples/type_checker/enums/
examples/type_checker/functions/
examples/type_checker/imports/
examples/type_checker/ownership/
examples/type_checker/pointers/
examples/type_checker/proposals/
examples/type_checker/runtime/
examples/type_checker/structs/
examples/type_checker/traits/
examples/type_checker/modules/        # helper; runner skip eder
```

Eski düz liste notları tarihsel bağlam için aşağıda kalabilir; canonical dosya yerleşimi artık yukarıdaki kategori ağacıdır.

```text
examples/type_checker/core/000.cstar
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
examples/type_checker/imports/052.cstar
examples/type_checker/053.cstar
examples/type_checker/054.cstar
examples/type_checker/055.cstar
examples/type_checker/056.cstar
examples/type_checker/057.cstar
examples/type_checker/058.cstar
examples/type_checker/059.cstar
examples/type_checker/060.cstar
examples/type_checker/061.cstar
examples/type_checker/062.cstar
examples/type_checker/063.cstar
examples/type_checker/064.cstar
examples/type_checker/065.cstar
examples/type_checker/066.cstar
examples/type_checker/067.cstar
examples/type_checker/068.cstar
examples/type_checker/069.cstar
examples/type_checker/070.cstar
examples/type_checker/071.cstar
examples/type_checker/072.cstar
examples/type_checker/073.cstar
examples/type_checker/074.cstar
examples/type_checker/075.cstar
examples/type_checker/functions/076.cstar
examples/type_checker/077.cstar
examples/type_checker/078.cstar
examples/type_checker/079.cstar
examples/type_checker/080.cstar
examples/type_checker/081.cstar
examples/type_checker/082.cstar
examples/type_checker/083.cstar
examples/type_checker/084.cstar
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
- `examples/papers/policy.cstar` artık eski policy/hook fikrinin hangi ayrı concept dosyalarına bölündüğünü anlatan kısa concept map dosyasıdır.
- `examples/papers/syntax.cstar` artık include/import/export ve genel syntax vitrini olarak tutuluyor; macro/metaprogramming/enum/protocol/concurrency proposal denemeleri canonical olarak concept dosyalarına ayrıldı.
- `examples/papers/` altındaki proposal dosyaları bilinçli olarak mevcut compiler'da controlled diagnostic üretir; crash/assert kabul edilemez.

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
- Parametre seviyesinde implicit cast izni syntax'ı:
  - Tamamlandı: `bool y` formu primitive parametreyi implicit cast edilebilir kabul eder.
  - Tamamlandı: `x bool` formu aynı primitive tipi taşır ama sembolün scope içinde implicit cast edilmesini yasaklar.
  - Smoke: `examples/smoke/castable_param.cstar`
  - Negative diagnostic: `examples/type_checker/functions/076.cstar`
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

Kalan tasarım kararı: pointer nullability.

- Bugünkü codegen `move` sonrası source pointer'ı null'a çekebilir ve condition conversion pointer için null karşılaştırması yapar; yani IR/runtime seviyesinde null kavramı fiilen vardır.
- Dil yüzeyinde her pointer'ın default-nullable kabul edilmesi C/C++ tarzı sessiz hata alanı açar; ama pointer varken null ihtiyacını yok saymak da sistem programlama gerçekliğine uymaz. C* yönü:
  - Normal `T*`, `T^`, `T&` değerleri default non-null kabul edilmeli.
  - Nullable pointer açık type surface ile yazılmalı: önerilen canonical form `T*?` ve `T^?`.
  - `T&` nullable olmamalı; reference/borrow var olan storage'a alias demektir.
  - `nil` literal yalnız nullable pointer bağlamında type-check edilmeli.
  - `null` kelimesi C interop dokümanlarında anılabilir ama canonical literal `nil` olmalı.
  - `deref` non-null kanıtı olmayan nullable pointer'da reddedilmeli; `if (p)` veya protocol/flow proof sonrası scope içinde non-null narrow edilmeli.
  - `unsafe_cast<T*>(0)` gibi raw interop yolları ayrı unsafe escape hatch olarak kalmalı.
  - Moved-from pointer'ın iç temsilde null'a çekilmesi kullanıcı tarafından “nullable value” olarak görülmemeli; semantic state `moved` olarak kalmalı ve yeniden initialize edilmeden kullanım `CST2105` üretmeli.
- Tamamlanan MVP:
  - Var olan non-null pointer davranışı korundu.
  - Nullable pointer syntax'ı `T*?` ve `T^?` olarak parser/AST/semantic hattına eklendi.
  - `T&?` reddedilir; reference nullable olmaz.
  - `nil` expression literal olarak parse edilir ve yalnız nullable pointer bağlamında kabul edilir.
  - Non-null `T*`, `T^` ve `T&` bağlamında `nil` `CST2100` diagnostic üretir.
  - Nullable pointer non-null pointer'a sessiz atanamaz; `if (p)` true branch'i içinde non-null narrowing yapılır.
  - `deref p` nullable pointer için proof öncesi `CST2100` üretir.
  - `new? Type(args)` sonucu `Type^?`, `shared new? Type(args)` sonucu `Type*?` kabul edilir.
  - Shared nullable allocator smoke'u explicit allocator payload + strong-count metadata release hattını alloc/free sayaçlarıyla doğrular.
  - Heap-backed allocator smoke'ları `free(ptr)` içinde CRT free çağırır; sayaçlar yalnız compiler hook çağrısını değil gerçek native heap release niyetini de doğrular.
- Testler:
  - `examples/type_checker/pointers/036.cstar`
  - `examples/type_checker/pointers/037.cstar`
  - `examples/type_checker/pointers/038.cstar`
  - `examples/type_checker/pointers/040.cstar`
  - `examples/type_checker/pointers/041.cstar`
  - `examples/type_checker/pointers/042.cstar`
  - `examples/smoke/pointers/nullable_pointer_nil.cstar`
  - `examples/smoke/pointers/nullable_pointer_flow_deref.cstar`
  - `examples/smoke/ownership/nullable_shared_allocator_release.cstar`
  - `examples/smoke/ownership/unique_allocator_drop_releases.cstar`
  - `examples/smoke/ownership/unique_allocator_scope_exit_releases.cstar`

İleri aşama: flow proof motoru nested boolean expressions/protocol proofs ile genişletilecek; allocator failure runtime policy'si Aşama 8.5 altında görünür abort/effect/result modeliyle sıkılaştırılacak.

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

- Scope çıkışı/destructor lowering ile final strong-count release tamamlandı:
  - local shared handle alias'ları scope çıkışında reverse-order atomic release üretir.
  - by-value shared pointer parametreleri function exit/return yolunda release edilir.
  - `ReleaseSharedPointer` artık `atomicrmw sub 1` üretir; önceki `sub -1` strong-count artırma bug'ı giderildi.
  - heap shared owner son release'i gördüğünde destructor + data free + control-count free hattına iner.
  - `examples/smoke/ownership/shared_pointer_scope_release.cstar`
  - `examples/smoke/ownership/shared_pointer_param_scope_release.cstar`
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
  - Tek elemanlı local/global array initializer artık doğru array storage üretir:
    - `examples/smoke/arrays/single_element_array_initializer.cstar`
    - `examples/smoke/arrays/global_single_element_array_initializer.cstar`
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
  - Sabit negatif index boyut içinde kaldığı sürece sondan indeksleme olarak normalize edilir: `arr[-1]` son eleman, `matrix[-1:-1]` son satır/son kolon olur.
  - Sabit index pozitif tarafta veya negatif tarafta array sınırını aşarsa compile-time error üretilir; codegen'e out-of-bounds GEP bırakılmaz.
  - Array dimension'ları mevcut MVP'de compile-time positive integer literal olmak zorundadır; `int32[count]` gibi runtime-sized array syntax'ı controlled diagnostic üretir.
  - Initializer eleman sayısı `product(dimensions)` ile eşleşmek zorundadır; eksik/fazla initializer controlled diagnostic üretir.
  - Local primitive/pointer array storage 1 MiB üstüne çıkarsa stack overflow riskine karşı controlled diagnostic üretir; büyük buffer için heap/allocator-backed model kullanılmalıdır.
  - Dinamik index için runtime bounds check şimdilik üretilmez.
  - Runtime checked array/slice modeli ileride stdlib/safety mode altında ele alınacak.
- Regression diagnostic:
  - `examples/smoke/arrays/array_negative_index_read.cstar`
  - `examples/smoke/arrays/array_negative_index_assignment.cstar`
  - `examples/smoke/arrays/array_dynamic_negative_index_read.cstar`
  - `examples/smoke/arrays/multidim_array_negative_index_read.cstar`
  - `examples/smoke/arrays/multidim_array_dynamic_negative_index_read.cstar`
  - `examples/type_checker/arrays/array_constant_index_oob_read.cstar`
  - `examples/type_checker/arrays/array_constant_negative_index_read.cstar`
  - `examples/type_checker/arrays/array_constant_index_oob_assignment.cstar`
  - `examples/type_checker/arrays/array_constant_negative_index_oob_assignment.cstar`
  - `examples/type_checker/arrays/array_initializer_too_few.cstar`
  - `examples/type_checker/arrays/array_initializer_too_many.cstar`
  - `examples/type_checker/arrays/array_non_constant_dimension.cstar`
  - `examples/type_checker/arrays/array_param_non_constant_dimension.cstar`
  - `examples/type_checker/arrays/array_param_zero_dimension.cstar`
  - `examples/type_checker/arrays/local_array_stack_too_large.cstar`
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

Enum odaklı statement MVP tamamlandı.

Karar:

- Canonical keyword `option` kalır; ayrı `match` keyword'ü şimdilik eklenmez.
- İlk yüzey statement odaklıdır, expression/value döndürmez:
  - `option (value) { Enum.Member: { ... }, _: { ... } }`
- `_` default branch anlamına gelir.
- Pattern MVP sadece `Enum.Member` ve `_` kabul eder; literal scalar/char/bool, range/destructuring/guard sonra.
- Branch gövdeleri normal statement scope'u olur; `ret`, `break`, `continue` kendi bağlam kurallarını korur.
- Exhaustiveness kontrolü scalar/flags enum değerleri için çalışır: `_` yoksa bütün enum üyeleri açıkça ele alınmalıdır.
- Duplicate enum branch, farklı enum type pattern'i ve birden fazla `_` default diagnostic üretir.
- `_` varsa default fallback olarak sona iner; kaynakta yazıldığı sıradan bağımsız olarak kalan değerleri yakalar.

Tamamlanan:

- Gerçek parser/AST/semantic/codegen hattı:
  - `include/ast/option_stmt.hpp`
  - `src/parser/branch.cpp`
  - `src/visitor/previsit.cpp`
  - `src/visitor/visitor.cpp`
- Positive smoke:
  - `examples/smoke/enums/option_enum_exhaustive.cstar`
  - `examples/smoke/enums/option_enum_default.cstar`
- Negative diagnostic:
  - `examples/type_checker/enums/085.cstar` eksik enum branch.
  - `examples/type_checker/enums/086.cstar` farklı enum pattern type.
  - `examples/type_checker/enums/087.cstar` duplicate enum branch.
  - `examples/type_checker/control_flow/051.cstar` literal pattern henüz desteklenmediği için diagnostic kalır.

İleri aşama: `option` expression formu, literal/bool/char pattern, range/guard pattern, tagged payload destructuring ve `_` default kullanan enum option için "yeni enum member eklendiğinde branch default'a düşüyor" warning'i.

### 5.3 Ternary Expression `cond ? a : b`

Durum:

- MVP tamamlandı.
- Lexer ve expression parser tarafında `?` / `:` token ve `BinaryOpAST::extra` temsili kullanılır.
- Pass 1, ternary expression için condition/branch type contract kontrolü yapar.
- Codegen, side-effect-free scalar/pointer branch'leri LLVM `select` ile üretir.

Karar notu:

- C* için ternary bir statement değil expression'dır.
- MVP value-producing expression bağlamlarında açıktır:
  - `int32 x = cond ? a : b;`
  - `ret cond ? a : b;`
  - function argument: `foo(flag ? left : right);`
- `cond` dönüşümü `if` condition ile aynıdır: `bool`, integer, float ve pointer zero/null karşılaştırmasına iner.
- Branch type birleştirme kuralı:
  - Aynı type: doğrudan kabul.
  - Primitive numeric: mevcut safe implicit conversion/data-loss warning kurallarıyla ortak hedef type.
  - Enum: aynı enum type zorunlu; farklı enum type reddedilmeli.
  - Pointer MVP: aynı pointee/ownership/qualifier şekli zorunlu; qualifier stripping reddedilir.
  - Struct/user-defined: aynı type zorunlu; conversion overload gelene kadar farklı user-defined type reddedilmeli.
  - `void` branch kabul edilmez; side-effect statement seçimi için `if` kullanılmalı.
- Codegen:
  - Side-effect içermeyen scalar/pointer branch için `select`.
  - Branch expression içinde call/assignment/allocation gibi side-effect doğuran ifade şimdilik diagnostic üretir; doğru uzun vadeli lowering basic block + PHI gerektirir.

Tamamlanan smoke/regression:

- `examples/smoke/control_flow/ternary_initializer.cstar`
- `examples/smoke/control_flow/ternary_return.cstar`
- `examples/smoke/control_flow/ternary_function_argument.cstar`
- `examples/smoke/control_flow/ternary_pointer_condition.cstar`
- `examples/smoke/control_flow/ternary_newline_continuation.cstar`
- `examples/type_checker/control_flow/ternary_branch_type_mismatch.cstar`
- `examples/type_checker/control_flow/ternary_void_branch.cstar`

Kalan ileri iş:

- Side-effect branch'leri için basic block + PHI lowering.
- Pointer qualifier widening modelini çok seviyeli qualifier syntax'ı oturduktan sonra genişletmek.
- Enum/struct ternary için daha zengin regression seti.

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
  - `examples/smoke/imports/include_module_function.cstar`
  - `examples/smoke/modules/math_module.cstar`
- Logical std package include resolver eklendi:
  - `include "std:math" as math` repo `std/math.cstar` dosyasına çözülür.
  - `include { "std:math" "std:math:abs_i64" }` block formu aynı resolver üzerinden çalışır.
  - `include involved { "std:math" }` otomatik/package include yüzeyi olarak parse edilip std source include'a bağlanır.
  - Member target formu (`std:math:abs_i64`) bugünkü MVP'de package dosyasını dahil eder; sembol bazlı filtreleme/re-export ileride namespace/type identity aşamasında derinleşir.
  - Aliaslı std include public macro export'u da toplar: `math.math_abs_i64(...)`.
  - `examples/smoke/imports/include_std_package.cstar`
- Basit `import func(...) :: type;` call codegen ile bağlandı.
  - `import abs(int32) :: int32;`
  - `import abs(int32 value) :: int32;`
  - Import parametre adı ABI için opsiyonel; semantic pass imzayı korur ama forward deklarasyon parametresini local symbol gibi kaydetmez.
- `import func(...) :: type from "lib";` parse edilir ve native link metadata'sına taşınır.
- `import { ... }` ve `import from "lib" { ... }` blokları parse/codegen akışına bağlı.
- C ABI variadic import/call tamamlandı:
  - `import from "std:crt" { printf(const char* fmt, ...) :: int32; }`
  - `...` yalnız parametre listesinin sonunda geçerlidir.
  - Eksik fixed parametre diagnostic üretir.
- `export func(...) :: type from "module";` ve `export from "module" { ... }` forward declaration olarak parse edilir.
- `export area<Circle>(...) :: type;` gibi generic function attribute syntax'ı declaration yüzeyinde parse edilir.
- Executable üretirken ana dosyada `export` varsa warning üretilir; `--emit=staticlib` veya `--emit=dynamiclib` önerilir.
- `--emit=staticlib` object + `ar rcs` ile static library üretir.
- `--emit=dynamiclib` backend clang `-shared` ile platform dynamic library üretir.
- `include ... as alias` function-call lookup'a bağlı:
  - `math.add_from_module(...)`
  - `examples/smoke/imports/include_module_function.cstar`
- `public`/default-private module visibility MVP'si tamamlandı:
  - `public` function/variable declaration'ları include edilen local module'den ana compilation unit'e açılır
  - modifier yazılmayan declaration default private kabul edilir
  - `import`/`export` visibility değil, native/linkage ABI yüzeyidir
  - private module function'ına alias üzerinden erişim controlled diagnostic üretir
  - `examples/type_checker/imports/052.cstar`
- Top-level type visibility hedef kuralı netleştirildi:
  - `public struct`, `public trait` ve `public enum` de module global API havuzuna açılmalıdır; module API'si yalnız function/variable değildir.
  - `mod.Type` alias syntax'ı type pozisyonlarında parse edilir ve bugünkü source-merge mimarisinde public type'ın gerçek adına çözülür.
  - Public top-level type include smoke tamamlandı:
    - `examples/smoke/imports/include_module_public_types.cstar`
    - `examples/smoke/modules/public_types_module.cstar`
  - `public struct`, `public enum`, `public trait` ve `struct with mod.Trait` conformance bu smoke içinde doğrulanır.
  - Private top-level type alias erişimi controlled diagnostic üretir:
    - `examples/type_checker/imports/private_module_type_alias.cstar`
    - `examples/type_checker/modules/private_types_module.cstar`
  - Uninitialized local defined-type declaration için unknown type kontrolü initializer varlığına bağlı olmaktan çıkarıldı; diagnostic yerine codegen/pass crash'e düşme yolu kapatıldı.
  - Struct member visibility MVP'si tamamlandı:
    - Include edilen public struct field'ları default private kabul edilir; module dışından yalnız `public` field okunup yazılabilir.
    - Struct'ın kendi method'ları private field'lara erişebilir; bu sayede public method private state'i kapsülleyebilir.
    - Instance olmadan `StructType.field` erişimi controlled diagnostic üretir.
    - Instance method için `StructType.method()` çağrısı controlled diagnostic üretir; static method canonical yazımı `StructType::method()` kalır.
    - `examples/type_checker/imports/private_module_field_access.cstar`
    - `examples/type_checker/structs/struct_type_field_access.cstar`
    - `examples/type_checker/structs/struct_type_instance_method_call.cstar`
- Module-level `static` MVP'si tamamlandı:
  - static function LLVM tarafında internal linkage alır
  - static global variable internal linkage/storage davranışını korur
  - `static` tek başına private kabul edilir; visibility açmaz
  - `public static`, source-level include API'sine açılan ama native/linkage tarafında internal kalan declaration anlamına gelmelidir
  - static function non-static global symbol/function kullanamaz
  - `examples/smoke/imports/include_module_public_static.cstar`
  - `examples/type_checker/053.cstar`
  - `examples/type_checker/054.cstar`
- Native link normalizasyonu:
  - `"m"` -> Unix/macOS için `-lm`
  - `"foo.lib"`, `.a`, `.so`, `.dylib` ve path değerleri doğrudan linker'a geçer
  - `"std:math"` gibi logical module kaynakları linker argümanına çevrilmez

Kalan:

- String literal / `const char*` ABI hardening:
  - Karar netleşti: string literal storage modeli uzun vadede `const char[N]`, expression yüzeyi C ABI bağlamında `const char*` decay eder.
  - String literal mutable target'a implicit geçmez. Güvenli hedefler `const char*` ve `readonly char*` kabul edilir.
  - `constptr char*` güvenli değildir; C* modelinde yalnız pointer adresini sabitler, hedef `char` değerini mutable bırakır. Bu yüzden string literal için `char*` ile aynı sınıfta reddedilir.
  - `char^`, `const char^` ve `readonly char^` de desteklenmez. String literal static/immutable storage'dır; unique pointer ise ownership/free/drop iddiası taşır. Owned mutable string gerekiyorsa ileride explicit copy/allocator API'si kullanılmalıdır.
  - Bu kural `CST2100` semantic qualifier diagnostic'i ile sabitlendi:
    - `examples/smoke/runtime/string_literal_const_char_variable.cstar`
    - `examples/smoke/runtime/string_literal_readonly_char_variable.cstar`
    - `examples/type_checker/runtime/string_literal_mutable_char_param.cstar`
    - `examples/type_checker/runtime/string_literal_mutable_char_variable.cstar`
    - `examples/type_checker/runtime/string_literal_constptr_char_param.cstar`
    - `examples/type_checker/runtime/string_literal_constptr_char_variable.cstar`
    - `examples/type_checker/runtime/string_literal_unique_char_variable.cstar`
    - `examples/type_checker/runtime/string_literal_const_unique_char_variable.cstar`
    - `examples/type_checker/runtime/string_literal_readonly_unique_char_variable.cstar`
  - `char*` ve `const char*` bugün CRT/string ABI için raw C pointer kalır; compiler-owned shared handle semantiğine karışmamalı. Bu karar dokümanda korunmalı.
  - Escape decode genişletilmeli: `\0`, `\r`, `\b`, `\xNN`, ileride UTF-8 policy.
  - Embedded null içeren literal için length bilgisi olmayan `const char*` kullanımında uyarı/policy düşünülmeli; gerçek string/slice modeli gelince `StringView { ptr, len }` benzeri stdlib tipi önerilir.
  - String literal global storage dedup/constant linkage ve module-level lifetime netleştirilmeli.
  - Kalan test adayları: escape decode smoke, include edilen module fonksiyonuna string literal geçişi, mutable `char*` implicit reddinin return/assignment varyantları.
- Gerçek namespace/type module sistemi ve `struct`/`trait`/`enum` type modül export/import davranışı Aşama 7 ile birlikte tamamlanacak:
  - Bugünkü MVP'de alias ile gelen API'de `mod.Type` canonical yazımdır; semantic isim source-merge nedeniyle şimdilik unqualified public type adına iner.
  - Gerçek namespace ownership geldiğinde `mod.Type` isim çakışmalarını da taşıyan stable type identity üretmelidir.
  - Gerçek namespace ownership geldiğinde bugünkü source-merge tabanlı field visibility kontrolü stable module identity üzerine taşınmalıdır.
  - `public static` function/global include smoke; bare `static` alias erişimi private diagnostic.
- Not: Bugünkü include modeli source-level public declaration merge yapar; public function body içinde private module helper lowering'i gerçek module object/scope modeliyle birlikte genişletilecek.

## Aşama 7 - User-defined Types

Proposal hedefleri ve kapsam kararı:

- `struct`
- `trait`
- custom allocator benzeri fikirler.
- `protocol` / `dynamic protocol`: state-flow ve runtime tag gerektirdiği için Aşama 8+ ileri proposal kapsamına taşındı; Aşama 7 çıkışında keyword rezervasyonu ve controlled diagnostic yeterlidir.

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
- `new` allocation entry kararı tamamlandı:
  - Constructor by-value initialization için ana yoldur; by-value factory özel bir lifecycle yüzeyi değildir.
  - `new` static method değildir; heap allocation/control-block/allocator seçimli compiler-recognized allocation operator'dır.
  - `new Type(args)` unique heap allocation + constructor lowering üretir.
  - `shared new Type(args)` shared handle + atomic strong-count control-block üretir.
  - `new(allocator) Type(args)` ve `shared new(allocator) Type(args)` parser/semantic yüzeyi vardır; allocator değerinin `Allocator` conformance'ı aranır.
  - Non-static `new`, by-value `static new(...) :: Type` ve user-defined lifecycle operator'ları controlled diagnostic üretir.
  - `examples/smoke/struct_unique_new_operator.cstar`
  - `examples/smoke/struct_shared_new_operator.cstar`
  - `examples/type_checker/063.cstar`
  - `examples/type_checker/064.cstar`
- Destructor/drop/scope-exit MVP'si tamamlandı:
  - `destructor(...) { ... }` struct body içinde method olarak parse edilir.
  - Internal lowering normal method modeliyle `StructName.destructor(self&, ...)` fonksiyonuna iner.
  - `drop value;` statement'ı destructor'ı çağırır, değeri dropped/moved state'e çeker ve sonraki kullanım `CST2105` diagnostic üretir.
  - By-value local struct variable için function return/implicit scope-exit öncesinde destructor otomatik çağrılır.
  - `value.destructor()` ve `ptr.destructor()` normal kullanıcı yüzeyi değildir; `CST2105` ile `drop value;` öneren diagnostic üretir.
  - `allocator` hook'u trait/allocator aşamasına kadar controlled parser diagnostic üretir.
  - `examples/smoke/struct_destructor_method.cstar`
  - `examples/smoke/struct_pointer_destructor_method.cstar`
  - `examples/smoke/struct_scope_exit_destructor.cstar`
  - `examples/type_checker/061.cstar`
  - `examples/type_checker/065.cstar`
  - `examples/type_checker/066.cstar`
- Trait MVP'si tamamlandı:
  - `trait Name { requirement(...) :: Type; }` parser/AST ve requirement table üretir.
  - `struct T with TraitA, TraitB` grammar'ı parse edilir.
  - Pass1 compile-time conformance check yapar.
  - `Allocator` capability `new(allocator) Type(args)` semantic kontrolünde kullanılır.
  - `examples/smoke/trait_struct_conformance.cstar`
  - `examples/type_checker/068.cstar`
  - `examples/type_checker/069.cstar`
- `protocol`, `dynamic protocol`, `dynamic Trait` ve ileri lifetime keyword'leri için controlled proposal diagnostic/rezervasyon korunur.
- Eski `policy for T { ... }` runtime hook modeli ve `policy protocol` çift isimli form superseded kabul edildi.
- Ana yön `protocol Name for Type { ... }`: compile-time typestate/state contract.
- `static protocol` gereksizdir; static/provable davranış default kabul edilir.
- `dynamic protocol` açık runtime maliyeti isteyen durumlar içindir.
- `.=` token'ı parser'da tanınır, fakat yalnızca dynamic/provability-gap protocol lowering netleşince codegen'e alınacak; bugün proposal diagnostic üretir.
- `examples/papers/struct.cstar` final struct/lifecycle proposal olarak sadeleştirildi:
  - `new` static method değil, compiler-recognized allocation operator.
  - Kullanıcı `operator new/delete/shared_new/shared_delete` yazmaz.
  - User-defined operator overloading yalnız value operator'ları içindir.
  - Struct data inheritance yoktur; layout reuse composition ile yapılır.
- Allocation customization artık canonical olarak `examples/papers/allocator.cstar` içindedir:
  - Allocation customization yalnız `#[lang(allocator)]` ile işaretlenmiş canonical allocator trait'i ile yapılır; trait adı tek başına özel değildir.
  - Compiler `new(allocator) Type(args)` lowering'ini allocator + constructor + ownership runtime ile synthesize eder.
- Dynamic dispatch canonical olarak `examples/papers/trait.cstar` içindedir:
  - Dynamic dispatch yalnız açık `dynamic Trait` yüzeyiyle mümkündür.

Son kontrol ve ileri takip:

- User-defined value operator overloading MVP'si tamamlandı:
  - `operator +`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `<=`, `>` ve `>=` struct method ABI'siyle çalışır.
  - `examples/smoke/struct_value_operator_add.cstar`
  - `examples/smoke/struct_value_operator_arithmetic.cstar`
  - `examples/smoke/struct_value_operator_comparison.cstar`
  - lifecycle/ownership operator'larının user overload olarak reddi diagnostic ile korunur.
  - `examples/type_checker/067.cstar`
- `operator index` ve zengin overload resolution Aşama 8+ ileri operator tasarımına taşındı.
- Allocator-backed shared control metadata contract'ı güncellendi:
  - Bugünkü MVP explicit allocator'ı hem payload data hem strong-count metadata için kullanır.
  - Strong-count metadata ayrı allocation'dır; compiler ileride aynı contract altında fused layout üretebilir.
  - Failure policy explicit signature/effect modeline bağlanacak.
- `protocol` parser/flow tasarımı Aşama 8+ ileri proposal olarak kaldı:
  - `protocol FileState for FileHandle { ... }`
  - `state closed, opened;`
  - `default closed;`
  - `closed -> opened :: open();`
  - `read() :: !closed;`
  - `scope_exit opened -> closed :: close();`
- Protocol state'lerini mevcut qualifier/state slot'una bağla:
  - `opened FileHandle^`
  - `closed FileHandle^`
  - `const opened FileHandle^`
  - Çoklu protocol state slot'u: `opened locked FileHandle^` tek birleşik enum değil, protocol başına ayrı state bilgisidir.
- Pass0 symbol/type table:
  - protocol adı, hedef type, state seti, default state.
  - transition table ve forbidden-call table.
- Pass1 flow analysis:
  - method call sonrası state transition.
  - return type state match.
  - moved pointer ile state taşınması.
  - `scope_exit` required state diagnostic.
  - `scope_exit A -> B :: method();` cleanup edge'lerinin `ret`, `throw`, `break`, `continue` yollarına eklenmesi.
  - cleanup transition'larının cleanup-safe/noexcept olması; fallible cleanup için explicit user code zorunluluğu.
- `dynamic protocol` Aşama 8+ ileri proposal olarak kaldı:
  - explicit runtime tag field.
  - `.=` için görünür/desugar edilebilir switch lowering.
  - hidden hook/table/dispatch yok.
  - `--show-desugar` ile `.=` dynamic check ve scope-exit cleanup edge'leri gösterilmeli.
- Eski `policy for T` örnekleri dokümanda “superseded legacy proposal” olarak tutulacak; compiler ana grammar'ına alınmayacak.

### 7.1 Static

Durum:

- Module-level static ve struct static method çalışır.
- Local static storage duration ve static data member bu struct/lifetime modelinde açıkça kapalıdır; parser module-level static state'e yönlendiren diagnostic üretir.

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
- Local static declaration diagnostic:
  - `examples/type_checker/070.cstar`
- Static data member diagnostic:
  - `examples/type_checker/071.cstar`

Kalan:

- Bu alt aşamada açık MVP maddesi kalmadı.
- Local static gerekiyorsa ileride ayrı thread-safe one-time init proposal'ı olarak açılacak; şimdiki canonical yüzey module-level static state'tir.

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
- `new` allocation entry kararı:
  - Constructor by-value initialization için yeterlidir.
  - `new` static method değildir; allocation operator'dır.
  - `new` by-value factory olarak kullanılamaz.
  - Kullanıcı `operator new/delete/shared_new/shared_delete` yazamaz.
  - Allocation customization yalnız `Allocator` trait ile yapılır.
  - Compiler `new(allocator) Type(args)` lowering'ini synthesize eder.
  - `new Type(args)`, `shared new Type(args)`, `new(allocator) Type(args)` ve `shared new(allocator) Type(args)` parser/AST/semantic/codegen hattındadır.
  - unique `^` drop/scope-exit destructor + free çağırır.
  - shared `*` drop/scope-exit atomic strong-count release yapar; son release destructor + free çağırır.
  - `examples/smoke/struct_unique_new_operator.cstar`
  - `examples/smoke/struct_shared_new_operator.cstar`
  - `examples/smoke/struct_value_operator_add.cstar`
  - `examples/type_checker/067.cstar`
  - `examples/type_checker/063.cstar`
  - `examples/type_checker/064.cstar`
- Destructor/drop/scope-exit MVP'si:
  - `destructor(...) { ... }`
  - `drop value;`
  - by-value local struct için return/implicit scope-exit destructor çağrısı
  - direct `value.destructor()` diagnostic
  - `examples/smoke/struct_destructor_method.cstar`
  - `examples/smoke/struct_pointer_destructor_method.cstar`
  - `examples/smoke/struct_scope_exit_destructor.cstar`
  - `examples/type_checker/061.cstar`
  - `examples/type_checker/065.cstar`
  - `examples/type_checker/066.cstar`

Kalan:

- Bu alt aşamada açık MVP maddesi kalmadı.

İleri takip:

- `operator index` ve zengin overload resolution Aşama 8+ operator tasarımına taşındı.
- Struct data inheritance eklenmeyecek:
  - `extends` yok.
  - layout reuse composition.
  - `struct T with Trait` yalnız conformance.
- `syntax.cstar` içindeki struct trait bağlama örneği canonical `struct Shape<T> with Area<T>` yönüne çevrildi; `from` struct inheritance/modelleme için kullanılmayacak.

### 7.3 Trait

Tamamlanan:

- `trait Name { ... }` parser/AST.
- Trait requirement table.
- `struct T with TraitA, TraitB` grammar.
- Compile-time conformance check.
- Allocator capability final struct proposal'a göre trait üzerinden yürür:
  - user-defined `operator new` yok.
  - Allocator conformance `new(allocator) Type(args)` semantic kontrolü için zorunlu; compiler bunu trait adına göre değil `#[lang(allocator)]` language item kaydına göre bulur.
  - `examples/smoke/trait_struct_conformance.cstar`
  - `examples/type_checker/068.cstar`
  - `examples/type_checker/069.cstar`

Kalan:

- Bu alt aşamada açık MVP maddesi kalmadı.

İleri takip:

- Dynamic dispatch implicit değildir; monomorphized/static dispatch varsayılan.
- Açık dynamic dispatch için `dynamic Trait` grammar/ABI Aşama 8+ ileri proposal olarak kaldı:
  - explicit trait object representation.
  - vtable/runtime maliyeti görünür olacak.
  - `with Trait` otomatik `dynamic Trait` üretmeyecek.
  - Bugün `dyn` keyword'ü proposal diagnostic üretir:
    - `examples/type_checker/074.cstar`
- Generic bound syntax proposal'ı.
- Allocation failure eski policy hook yerine explicit `except`/`throw` veya result-like dönüş modeliyle tasarlanmalı.
- `protocol`/`dynamic protocol` grammar ve flow analysis Aşama 8+ ileri proposal olarak kaldı:
  - `examples/type_checker/075.cstar`

## Aşama 8 - Metaprogramming ve İleri Proposal

Proposal'da görünen ama çok sonraya bırakılacak başlıklar:

- `attribute`
- directive/macro sistemi
- `$` ve `#` tabanlı compile-time hook'lar
- allocator failure/effect modeli ve shared control-block runtime contract'ı
- `protocol` / `dynamic protocol` typestate flow analysis
- scalar enum / flags enum / explicit tagged layout
- açık `dynamic Trait` trait-object ABI
- `async` / `await`
- `except` / `throw`
- compile-time error/runtime error hook'ları

Bu başlıklar çekirdek dil stabil olmadan uygulanmamalı.
Canonical proposal örnekleri artık concept bazlı dosyalara ayrılmıştır:

- `examples/papers/metaprogramming.cstar`: macro, directive, attribute, reflection.
- `examples/papers/enum.cstar`: scalar enum, flags enum, explicit tagged layout.
- `examples/papers/trait.cstar`: static trait ve açık dynamic trait object ABI.
- `examples/papers/protocol.cstar`: typestate, dynamic protocol, `.=` ve scope-exit cleanup.
- `examples/papers/allocator.cstar`: allocator capability ve `new` lowering.
- `examples/papers/nullability.cstar`: non-null default pointer, nullable `T*?`/`T^?` ve `nil`.
- `examples/papers/concurrency.cstar`: async/task ownership, `Send`/`Sync`, `nomove`.

`examples/papers/struct.cstar` yalnız struct/lifecycle/value operator kararlarını taşır. `examples/papers/syntax.cstar` artık bu ileri denemeleri taşımıyor.

Concept split kararı:

- Eski `policy for T { ... }` modeli ana grammar'a alınmayacak.
- Allocation, protocol state, trait dispatch, cleanup, error flow, macro expansion ve async boundary tek hook sistemi altında birleşmeyecek.
- Runtime maliyeti olan her şey source syntax'ta görünür olacak.
- `--show-desugar` macro expansion, `.=` protocol check, scope-exit cleanup ve allocator/new lowering gibi compiler-synthesized adımları gösterebilmeli.

### 8.1 Attribute

Durum:

- Lexer `attribute` keyword'ünü tanır.
- Lexer `@` token'ını ve `for` keyword'ünü tanır.
- Parser `attribute Name for struct { ... }` declaration grammar'ını dengeli block skip ile okur.
- Parser `@Name(args)` item annotation grammar'ını okur.
- `AttributeAST` eklendi; 8.1 MVP hedefi olarak sadece `for struct` kabul edilir.
- Attribute declaration ve annotation yüzeyi compiler pipeline'ında kabul edilir; `for struct` metadata MVP'si codegen'i bozmaz.
- Attribute body şimdilik dengeli token block olarak saklanmadan skip edilir; gerçek `$emit` generation aşağıdaki backlog'dadır.
- Attribute adı checked hale getirildi:
  - Duplicate attribute definition diagnostic üretir.
  - Bilinmeyen `@Name` annotation diagnostic üretir.
- Doküman attribute/trait/protocol ayrımını açıklar:
  - trait: type capability/contract.
  - protocol: value state/typestate contract.
  - attribute: item/type üzerinde checked compile-time transform.
- Positive smoke:
  - `examples/smoke/metaprogramming/attribute_struct_metadata.cstar`
- Negative diagnostic:
  - `examples/type_checker/proposals/084.cstar`
  - `examples/type_checker/proposals/085.cstar`

8.2+ expansion/reflection backlog:

- Reflection yüzeyi:
  - `name($item)`, `fields($item)`, `methods($item)`.
  - field metadata: name/type/visibility/qualifier/offset.
  - method metadata: name/params/return/static.
  - `has_attribute(...)`, `attribute_args(...)`, `sizeof(Type)`, `alignof(Type)`.
- `fields($item)`, `methods($item)`, `$emit`, `$item`, `#for`, `#error` gibi compile-time reflection/generation yüzeylerini uygula.
- Generated declaration'lar normal pass0/pass1 semantic kontrollerinden geçmeli.
- Attribute expansion order:
  - source item parse.
  - item/field/method metadata register.
  - attribute item emission.
  - original + generated item pass0/pass1.
- Attribute layout'u gizlice değiştirmemeli, private field'ı public yapmamalı, runtime metadata üretmemeli ve trait/protocol conformance'ı bypass etmemeli.
- Function body rewrite ilk MVP'ye alınmamalı; gerekirse ayrı ve daha riskli macro capability olarak tasarlanmalı.
- İleride target kind genişletmesi:
  - `function`
  - `enum`
  - `field`
  - `module`
- Expansion gelene kadar 8.1 tamam sayılır; gerçek item emission 8.2 macro/directive ve reflection altyapısıyla birlikte açılmalı.

### 8.2 Macro / Directive

Durum:

- Lexer `#` ve `$` tanır.
- Lexer `macro` keyword'ünü tanır.
- `@` artık attribute annotation token'ıdır.
- Parser `macro name($arg: kind, ...) -> expr|stmt|item|type { ... }` grammar'ını dengeli block skip ile okur.
- Macro parametre kind seti parser seviyesinde net:
  - `expr`
  - `stmt`
  - `item`
  - `type`
  - `ident`
  - `tokens`
- Macro return kind seti parser seviyesinde net:
  - `expr`
  - `stmt`
  - `item`
  - `type`
- `MacroAST` ve `DirectiveAST` eklendi.
- Compile-time preprocess pass eklendi:
  - macro declaration'ları token stream'den toplanır ve normal AST parse'a düşmeden kaldırılır.
  - macro call'ları declaration body token'larıyla expand edilir.
  - `$param` token'ları çağrı argüman token'larıyla değiştirilir.
  - macro body linefeed/comment token'ları expression expansion öncesi normalize edilir.
  - Expansion fixed-point pass olarak çalışır; `#if` block'u içinden gelen macro definition/call'lar sonraki pass'te çözülür.
  - Source include alias macro export eklendi:
    - `public macro` declaration'ları include edilen dosyadan preprocess öncesi taranır.
    - Çağrı syntax'ı `alias.macroName(args)` şeklindedir.
    - `examples/smoke/stdlib/std_public_macro_alias.cstar`, `std_math_advanced.cstar`, `std_network_endpoint.cstar`, `std_time_duration.cstar`, `std_print_writer.cstar` bu yüzeyi doğrular.
- `expr`, `stmt`, `item`, `type` return kind'ları smoke seviyesinde çalışır.
- `#warning "message"` compile-time warning üretir ve derleme devam eder.
- `#error "message"` compile-time parser error üretir ve derlemeyi durdurur.
- `#if true/false { ... } else { ... }` block selection çalışır.
- `#if target.os == "..."` / `!=` ve `#if target.arch == "..."` / `!=` target condition yüzeyi çalışır.
- `feature("name")` ve `cfg("name")` koşulları şimdilik false döner; `!feature(...)` formu bu sayede fallback seçebilir.
- Positive smoke:
  - `examples/smoke/metaprogramming/macro_expression.cstar`
  - `examples/smoke/metaprogramming/macro_statement.cstar`
  - `examples/smoke/metaprogramming/macro_item.cstar`
  - `examples/smoke/metaprogramming/macro_type.cstar`
  - `examples/smoke/metaprogramming/directive_warning.cstar`
  - `examples/smoke/metaprogramming/directive_if_true.cstar`
  - `examples/smoke/metaprogramming/directive_if_false_skip.cstar`
  - `examples/smoke/metaprogramming/directive_if_target.cstar`
  - `examples/smoke/metaprogramming/directive_if_macro_definition.cstar`
- Negative diagnostic:
  - `examples/type_checker/proposals/079.cstar`
  - `examples/type_checker/proposals/080.cstar`
  - `examples/type_checker/proposals/081.cstar`
  - `examples/type_checker/proposals/082.cstar`
  - `examples/type_checker/proposals/083.cstar`

İleri macro expansion/directive evaluation backlog:

- Mevcut macro expansion zamanı parse öncesi token-stream preprocess'tir; ileride AST/source-map aware expansion katmanı gerekebilir.
- `ident` ve `tokens` kind'ları parse/arity düzeyinde kabul edilir; bunlar için daha sıkı shape validation ileride yapılmalı.
- Hijyen/hygiene kuralları.
- Error reporting ve source span mapping.
- Macro local isimleri caller scope'una sızmamalı.
- Macro diagnostic'i hem macro tanımını hem çağrı yerini gösterebilmeli.
- `cfg(...)`, `feature(...)`, `target.*` değerlerini CLI/build config tarafına bağla.
- `build.mode`, `target.os`, `target.arch`, `feature("name")`, `cfg("key")`, `cfg_int("key")` gibi değerlerin nereden besleneceği CLI/build config tarafında tasarlanmalı.
- `#if` expression grammar'ı `&&`, `||`, parentheses ve richer constant expression ile genişletilmeli.
- Protocol ile macro/directive karıştırılmamalı; protocol gizli hook sistemi olmayacak.
- Macro/directive MVP'si tamam sayılır; sonraki sırada 8.3 enum/tagged layout değerlendirmesi var.

### 8.3 Enum / Explicit Tagged Layout

Durum:

- Lexer `enum` ve `flags` keyword'lerini tanır.
- Parser/AST/pass0/pass1/codegen scalar enum ve flags enum MVP'si çalışır:
  - C-like grammar: `enum Color : uint8 { Red, Green, Blue = 7 }`.
  - Flags grammar: `flags enum FileMode : uint32 { Read = 1, Write = 2 }`.
  - Explicit repr tüm enum'lar için zorunlu tutulur.
  - Scalar enum member value assignment implicit incremental veya explicit integer literal olabilir.
  - Flags enum member değerleri explicit yazılır; `0` veya tek bit/power-of-two olmak zorundadır.
  - Enum repr overflow ve duplicate member value controlled diagnostic üretir.
  - Canonical erişim `Color.Green`; unqualified member erişimi açılmadı.
  - Enum local variable, function parametre ve return storage'ı underlying integer type'a iner.
  - Unknown enum member ve farklı enum type atanması controlled diagnostic üretir.
  - Scalar enum yalnızca equality/inequality karşılaştırması kabul eder.
  - Flags enum `|`, `&`, `^`, unary `~` ve mevcut shortcut assignment lowering'i üzerinden `|=`, `&=`, `^=` kabul eder; arithmetic operatörler legal değildir.
  - Unary `~` repr-width complement üretir: `uint32` flags enum için 32-bit storage'ın tüm bitleri terslenir. Declared flag set'e geri sıkıştırmak isteyen kod `& KnownMask` kullanmalıdır.
  - Positive smoke: `examples/smoke/scalar_enum.cstar`, `examples/smoke/flags_enum.cstar`, `examples/smoke/enums/flags_enum_unary_not.cstar`.
  - Negative diagnostic: `examples/type_checker/077.cstar`, `examples/type_checker/078.cstar`, `examples/type_checker/079.cstar`, `examples/type_checker/080.cstar`, `examples/type_checker/081.cstar`, `examples/type_checker/082.cstar`, `examples/type_checker/083.cstar`, `examples/type_checker/enums/084.cstar`.
- `tagged` canonical proposal yüzeyidir; flags enum artık MVP olarak parser/semantic/codegen hattına indirildi.
- `option` statement enum pattern matching için temel çalışan yüzeydir:
  - `option(value) { Enum.Member: { ... }, _: { ... } }`
  - `_` yoksa enum üyeleri exhaustive olmak zorundadır.
  - Duplicate branch, enum type mismatch ve birden fazla default diagnostic üretir.
  - Positive smoke: `examples/smoke/enums/option_enum_exhaustive.cstar`, `examples/smoke/enums/option_enum_default.cstar`.
  - Negative diagnostic: `examples/type_checker/enums/085.cstar`, `examples/type_checker/enums/086.cstar`, `examples/type_checker/enums/087.cstar`.

Kalan:

- `uint128` ve full-width unsigned enum literal modeli lexer/parser numeric storage büyütülünce tekrar ele alınmalı; mevcut MVP member literal değerlerini `uint64_t` sınırında tutar.
- Signed/unsigned repr boundary testleri genişletilmeli.
- Payload gereken durumda canonical model explicit `TokenKind + struct Token` layout'u olmalı.
- `tagged Packet : uint8 { ... }` sugar'ı explicit `tag + storage` layout'una inecek şekilde tasarlanmalı.
- `tagged` desugar contract:
  - generated `PacketTag` enum repr açık.
  - generated `Packet` struct ABI-visible alignment/storage taşır.
  - heap allocation veya hidden runtime metadata üretmez.
- Variant construction syntax: `Packet.Data { bytes: bytes, len: len }`.
- Variant field access: `packet.Data.len`.
  - Pass1 tag'i kanıtlayabiliyorsa direct field access.
  - Kanıtlayamıyorsa ilk MVP controlled diagnostic.
  - Runtime checked access ayrı safety feature olarak sonra değerlendirilecek.
- `_` default kullanılan enum option için yeni enum member eklendiğinde warning üretme policy'si ayrıca tasarlanmalı.
- Smoke/type-checker adayları:
  - tagged variant access without proven tag diagnostic.
  - `uint128` enum literal sınırı diagnostic/pass.

### 8.4 Dynamic Trait Object

Durum:

- `trait` static conformance MVP çalışır.
- `dyn` keyword'ü proposal diagnostic üretir; canonical yön kısa `dyn` değil `dynamic Trait`.
- `with Trait` otomatik trait object üretmez.

Kalan:

- `dynamic Trait&`, `dynamic Trait*`, `dynamic Trait^` grammar'ı.
- Representation contract: `{ data: void*, vtable: constptr TraitVTable* }`.
- `dynamic ref value as Trait` / `dynamic move value as Trait` erase syntax'ı; yalnız value type trait'i sağlıyorsa legal.
- `unsafe_cast` trait object üretememeli; vtable doğruluğu compiler sorumluluğu olmalı.
- Vtable method imzası üretimi ve dispatch lowering.
- Dynamic trait method call:
  - `writer.write(data)` vtable dispatch'e inmeli.
  - return/param ABI static trait requirement'ı ile uyumlu olmalı.
- Ownership davranışı handle marker üzerinden ayrılmalı: borrowed `&`, shared `*`, unique `^`.
- Public/export ABI için generated vtable struct ve data pointer repr'ı açık olmalı.
- Type erasure diagnostics:
  - trait'i sağlamayan type için `dynamic ref value as Trait` reddi.
  - moved-after-use ve shared retain/release davranışı normal ownership sistemiyle aynı kalmalı.

### 8.5 Allocator / New Lowering Contract

Durum:

- `new Type(args)`, `shared new Type(args)`, `new(allocator) Type(args)` ve `shared new(allocator) Type(args)` MVP yüzeyi çalışır.
- `new? Type(args)` ve `shared new? Type(args)` nullable ownership handle sonucu üretir (`T^?` / `T*?`).
- Allocator trait conformance semantic kontrolünde kullanılır; canonical allocator capability `#[lang(allocator)]` metadata'sı ile kayıtlı trait'tir.
- `examples/papers/allocator.cstar` canonical proposal dosyasıdır.
- `examples/papers/nullability.cstar` nullable pointer ve `nil` canonical proposal dosyasıdır.
- Shared handle güçlü sayacı MVP olarak vardır; explicit allocator kullanıldığında payload storage ve strong-count metadata aynı allocator domain'inden allocate/free edilir.
- Heap-backed allocator örneklerinde `free` method'u gerçek CRT `free(ptr)` çağırır; arena allocator gibi region tabanlı allocator'larda no-op free ancak allocator'ın açık tasarım kararı olabilir.

Kalan:

- `new(allocator) Type(args)` lowering'ini `--show-desugar` ile görünür yap:
  - `alloc(sizeof(Type), alignof(Type))`
  - placement constructor call
  - unique/shared handle attach
  - destructor + `free` cleanup edge
- `shared new(allocator) Type(args)` control metadata contract'ı:
  - explicit allocator payload storage ve strong-count metadata'yı birlikte sahiplenir.
  - current MVP strong-count metadata'yı ayrı `alloc(sizeof(i64), alignof(i64))` ile alır.
  - last release -> destructor -> allocator.free(payload) -> allocator.free(strong-count).
  - compiler exact layout'u sahiplenir; ileride payload + metadata tek fused allocation'a indirilebilir, ama default heap'e yarım kaçış olmamalı.
- Allocation failure policy:
  - gizli policy hook yok.
  - `new T(args)` ve `shared new T(args)` default olarak infallible/non-null kabul edilmeli; allocation failure ilk runtime MVP'de abort veya görünür compiler runtime error olabilir.
  - Fallible allocation ayrı yüzeydir: `new? T(args)` ve `shared new? T(args)`.
  - `new? T(args)` sonucu `T^?`, `shared new? T(args)` sonucu `T*?` olur.
  - `except`/`throw` veya explicit result-like return modeli olgunlaşınca fallible allocation bu effect/result modeliyle de ifade edilebilir.
  - null raw pointer dönen allocator, infallible `new` içinde sessiz null pointer üretmemeli; ya visible failure'a çevrilmeli ya da `new?` yoluyla nullable sonuç vermeli.
- Primitive allocation policy:
  - `new` yalnız constructor'lı struct/resource type'lara hapsedilmemeli; C* için allocation operator'ı sized concrete storage üretir.
  - `new int32(7)`, `new float64(1.0)`, `shared new bool(true)` gibi primitive heap allocation formları legal olmalı.
  - Primitive `new` lowering'i: allocate `sizeof(T)`, initializer store, `T^` veya `T*` handle döndür; destructor yok, drop/release yalnız storage free eder.
  - Struct `new` lowering'i: allocate storage, zero/init, constructor call, destructor + free cleanup edge.
  - Array gibi sized concrete type'lar için ileride `new int32[4]((1, 2, 3, 4))` değerlendirilebilir.
  - Illegal target'lar: `void`, unsized/representation'ı belirsiz type, concrete type bilinmeyen dynamic trait object.
  - Generic wrapper tipleri (`Box<T>`, `Cell<T>` vb.) primitive allocation için zorunlu dil mekanizması olmamalı; ileride stdlib convenience olarak gelebilir.
- Test adayları:
  - allocator olmayan değerle `new(allocator)` diagnostic'i korunmalı.
  - allocator-backed unique allocation pass.
  - allocator-backed shared allocation pass.
  - allocator language item custom trait-name smoke.
  - allocator trait-name-without-lang diagnostic.
  - allocator-backed shared control metadata allocation/free smoke.
  - allocator-backed unique `drop` allocation/free smoke.
  - allocator-backed unique scope-exit allocation/free smoke.
  - destructor + allocator.free çağrı sırası smoke.
  - `new int32(7)` smoke.
  - `shared new float64(1.0)` smoke.
  - `new? int32(7)` primitive fallible allocation smoke.

### 8.6 Protocol / Typestate

Durum:

- `.=` token olarak tanınır ve protocol proposal diagnostic üretir.
- `protocol`, `dynamic protocol`, `state`, `is` lexer seviyesinde ayrılmıştır.
- `examples/papers/protocol.cstar` canonical proposal dosyasıdır.
- Eski `policy for T` ve `policy protocol` superseded kabul edilir.

Kalan:

- `protocol Name for Type { ... }` parser/AST:
  - protocol adı.
  - target type.
  - state seti.
  - default state.
  - transition table.
  - forbidden-call table.
  - scope-exit transition table.
- Static/provable protocol default olmalı; `static protocol` syntax'ı eklenmemeli.
- State-qualified type yüzeyi:
  - `opened FileHandle^`
  - `closed FileHandle^`
  - `const opened FileHandle^`
  - çoklu state slot: `opened locked FileHandle^`
- Pass1 flow analysis:
  - method call sonrası state transition.
  - return type state match.
  - moved pointer ile state bilgisinin taşınması.
  - forbidden call diagnostic: `read() :: !closed`.
  - `state is ...` / `value is state` kontrolü.
- `scope_exit A -> B :: method();` cleanup edge:
  - `ret`, `throw`, `break`, `continue` yollarında çalışmalı.
  - cleanup order reverse acquisition order.
  - cleanup transition cleanup-safe/noexcept kabul edilmeli.
  - fallible cleanup explicit user code gerektirmeli.
- Çoklu protocol state slot'u tek birleşik enum'a indirgenmemeli; her protocol kendi named slot'una sahip olmalı.
- `dynamic protocol`:
  - explicit runtime discriminant/tag field.
  - `.=` yalnız dynamic/provability-gap transition için.
  - hidden hook table/hashmap/virtual dispatch yok.
  - `.=` lowering switch/check olarak `--show-desugar` ile görünür olmalı.
- Test adayları:
  - valid open/read/close state flow pass.
  - closed file read diagnostic.
  - scope-exit cleanup on return.
  - scope-exit cleanup on throw.
  - `.=` dynamic protocol controlled diagnostic -> ileride lowering smoke.

### 8.7 Effects / Except / Throw / Defer

Durum:

- `except`, `throw`, `defer` keyword'leri lexer seviyesinde tanınır.
- Parser/AST/effect semantic yok veya proposal diagnostic seviyesindedir.
- Protocol scope-exit modeli `throw` yollarını da kapsayacak şekilde tasarlandı.

Kalan:

- Function signature effect grammar:
  - `fn(...) except ErrorType :: ReturnType`
  - no-effect function'dan fallible call yapıldığında diagnostic.
- `throw Expr;` statement AST/semantic.
- Error type olarak ilk MVP'de scalar enum veya explicit result-like type tercih edilmeli.
- Unwind/lowering stratejisi:
  - ilk MVP structured early-return lowering olabilir.
  - gerçek platform exception ABI'si sonraya bırakılabilir.
- `defer` statement:
  - explicit cleanup block.
  - protocol `scope_exit` ile çakışmayan net sıra.
  - reverse lexical order.
- Cleanup safety:
  - defer/scope-exit içinden throw policy'si netleşmeli.
  - fallible cleanup açıkça handle edilmeli.
- `--show-desugar` defer ve scope-exit cleanup edge'lerini göstermeli.

### 8.8 Concurrency / Async / Task Ownership

Durum:

- `async`, `await`, `nomove` lexer/parser seviyesinde kısmen tanınır; async proposal diagnostic üretir.
- `nomove` ownership-flow kısıtı bugünkü type-checker setinde çalışır.
- `examples/papers/concurrency.cstar` canonical proposal dosyasıdır.

Kalan:

- `async` function effect grammar ve AST:
  - `fn(...) async :: T`
  - `fn(...) async except E :: T` kombinasyonu ileride.
- `await expr` expression/statement lowering.
- Task return handle veya future type modeli:
  - implicit compiler task handle mı,
  - explicit stdlib `Task<T>` mı?
- Task boundary ownership rules:
  - `T^` yalnız explicit `move` ile crossing.
  - moved unique value task record'a taşınmalı.
  - `T*` by-value crossing atomic retain/copy yapmalı.
  - borrowed `&` crossing yalnız structured concurrency/lifetime proof varsa legal.
- `Send`/`Sync` marker:
  - runtime vtable değil trait/capability marker.
  - scheduler lowering boundary check üretmeli.
- `nomove`:
  - protocol state değil.
  - hidden policy hook değil.
  - async/task boundary relay'i engellemeli.
- Structured concurrency:
  - `task_group` veya benzeri scope-bound join modeli tasarlanmalı.
  - child task parent scope'u aşamamalı.
- Runtime backend:
  - ilk MVP single-thread event loop mu OS thread mi netleştir.
  - CRT/std native sleep/read_key gibi geçici console builtin'leriyle karışmamalı.
- Test adayları:
  - async proposal diagnostic korunmalı.
  - unique without move diagnostic.
  - moved-after-await diagnostic.
  - shared pointer retain across task smoke.
  - borrowed reference task escape diagnostic.

### 8.9 Proposal Dosyaları ve Controlled Diagnostic Bakımı

Durum:

- `examples/papers/*.cstar` concept bazlı proposal belgeleri olarak ayrıldı.
- Bu dosyaların bugünkü amacı tamamen compile olmak değil, tasarım yüzeyini canlı tutmak ve compiler'ın crash/assert üretmeden controlled diagnostic vermesini sağlamaktır.

Kalan:

- Her proposal dosyasının başındaki `// expected: diagnostic (proposal)` etiketi korunmalı.
- `tools/run_examples.ps1 -Suite papers -ExpectDiagnostics` çıktısı crash/assert içermemeli.
- Yeni proposal syntax eklenince önce ilgili `examples/papers/<concept>.cstar` güncellenmeli.
- Bir proposal parçası gerçek MVP'ye indiğinde:
  - küçük positive smoke `examples/smoke/` altına çıkarılmalı.
  - negatif semantic örnek `examples/type_checker/` altına eklenmeli.
  - `DOKUMANTASYON_TR.md` ve bu TODO dosyasında “proposal”dan “MVP/çalışıyor” durumuna taşınmalı.
- `policy.cstar` concept map olarak kalmalı; yeni runtime hook syntax'ı eklemek için kullanılmamalı.

## Bir Sonraki En İyi Adım

Tamamlanan son adım:

- Scalar enum hardening ve flags enum MVP compiler hattına indirildi:
  - `EnumAST`, `EnumTable`, parser/pass0/pass1/codegen desteği eklendi.
  - `enum Name : repr { A, B = 7 }` grammar'ı çalışıyor.
  - `flags enum Name : repr { A = 1, B = 2 }` grammar'ı çalışıyor.
  - `Color.Green` lookup, enum local/param/return ve mismatch diagnostic'leri doğrulandı.
  - repr overflow, duplicate value, scalar enum bitwise rejection ve flags explicit/power-of-two value diagnostic'leri doğrulandı.
  - Flags enum `|`, `&`, `^`, `|=`, `&=`, `^=` akışı smoke ile doğrulandı.
  - Flags enum unary `~` repr-width complement olarak codegen/type-check hattına bağlandı; scalar enum'da diagnostic üretir.
  - `examples/smoke/scalar_enum.cstar`
  - `examples/smoke/flags_enum.cstar`
  - `examples/smoke/enums/flags_enum_unary_not.cstar`
  - `examples/type_checker/077.cstar`
  - `examples/type_checker/078.cstar`
  - `examples/type_checker/079.cstar`
  - `examples/type_checker/080.cstar`
  - `examples/type_checker/081.cstar`
  - `examples/type_checker/082.cstar`
  - `examples/type_checker/083.cstar`
  - `examples/type_checker/enums/084.cstar`

Önceki tamamlanan planlama adımı:

- Proposal dosyaları concept bazlı ayrıldı:
- `policy.cstar`: concept map / eski hook modelinin ayrıştırılması.
- `struct.cstar`: struct/lifecycle/value operator.
- `allocator.cstar`: allocator capability ve `new` lowering.
- `nullability.cstar`: nullable pointer ve `nil` modeli.
- `trait.cstar`: static trait + explicit `dynamic Trait`.
- `protocol.cstar`: typestate, `.=` ve scope-exit cleanup.
- `enum.cstar`: scalar enum, flags enum, tagged layout.
- `metaprogramming.cstar`: macro/directive/attribute/reflection.
- `concurrency.cstar`: async/task ownership, `Send`/`Sync`, `nomove`.

Bu dosyalar compiler ile controlled diagnostic verir; crash/assert kabul edilmez.

Sıradaki teknik iş:

- Önce Aşama 8.9 bakımını sürekli yeşil tut: papers suite controlled diagnostic vermeli.
- Sonra `tagged` desugar veya `protocol` flow analysis'e geçmek daha sağlıklı olur; çünkü tagged access ve protocol state proof aynı statik kanıtlama altyapısını paylaşabilir.
- Enum tarafında kalan küçük/ileri işler:
  - full-width `uint128` enum literal storage.
  - tagged explicit layout ve variant construction/access.
