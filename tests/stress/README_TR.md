# C* Fuzz ve Stress Test Planı

Bu klasör compiler'ın hız, dayanıklılık ve bug yüzeyini ölçmek için kullanılan
test stratejisini tutar. Amaç yalnızca "program derleniyor mu?" kontrolü değil;
parser assert'i, semantic crash'i, codegen crash'i, sonsuz döngü, çok yavaş
case ve beklenmeyen backend davranışlarını yakalamaktır.

## Katmanlar

1. **Smoke suite**
   - El yazımı küçük pozitif programlar.
   - Beklenen exit code ile çalıştırılır.
   - Dilin çalışan yüzeyini korur.

2. **Type-checker suite**
   - Kontrollü diagnostic bekleyen negatif programlar.
   - Crash/assert olmamalı; hata diagnostic olarak dönmeli.

3. **Generated fuzz**
   - Seed'li rastgele ama çoğunlukla geçerli C* programları üretir.
   - Arithmetic, if/else, loop, array, struct ve method yüzeyini zorlar.

4. **Invalid fuzz**
   - Bilerek hatalı program üretir.
   - Compiler'ın kontrollü diagnostic üretmesini bekler.
   - Crash, timeout veya assert bug sayılır.

5. **Corpus mutation**
   - `examples/smoke` ve `std` altındaki gerçek C* dosyalarını token/karakter
     düzeyinde bozar.
   - Parser dayanıklılığını ölçer.

## Çalıştırma

Önce compiler build edilir:

```sh
cmake --build build
```

Sonra fuzz/stress runner:

```sh
python3 tools/stress_fuzz.py --cases 200 --mutations 80 --seed 12666
```

Opsiyonlar:

```text
--cases N       Generated valid/invalid case sayısı.
--mutations N   Corpus mutation case sayısı.
--seed N        Reproducible random seed.
--timeout S     Tek case timeout süresi.
--out-dir PATH  Repro/log klasörü.
```

Çıktılar:

```text
tests/stress/out/generated/   Üretilen case dosyaları
tests/stress/out/failures/    Crash/timeout repro dosyaları ve logları
tests/stress/out/summary.txt  Özet, slowest case listesi
```

## Başarı Kriteri

Fuzz runner için diagnostic beklenebilir; her rastgele programın geçerli olması
amaç değildir. Başarısızlık yalnız şunlardır:

- compiler process crash'i
- assert/segfault
- timeout/hang
- sanitizer/traceback benzeri internal failure

Bu sayede invalid syntax bile "başarılı" kabul edilir, yeter ki compiler bunu
kontrollü diagnostic olarak kapatsın.

## İlk Odak Alanları

- `include "std:..."` logical resolver
- public macro alias expansion
- nested expression/newline continuation
- pointer/reference/ownership flow
- struct field/method/lifecycle
- native ABI pointer lowering
- array initializer ve multidim indexing

Bu alanlar son eklenen özelliklerin en fazla kesiştiği yerlerdir; regression
riski yüksek olduğu için fuzz corpus'u özellikle bunları sık üretmelidir.
