# C* Performance Benchmark Planı

Bu klasör C* compiler'ının gerçek performans profilini C ve C++ compiler'larıyla
karşılaştırmak için kullanılan benchmark planını tutar. Fuzz/stress testlerinden
farkı şudur: Burada amaç crash yakalamak değil, aynı makinede ölçülebilir
compile time, executable size ve runtime baseline üretmektir.

## Çalıştırma

```sh
cmake --build build
python3 tools/perf_benchmark.py --work 250000 --compile-iters 5 --run-iters 20
```

Hızlı smoke ölçümü:

```sh
python3 tools/perf_benchmark.py --work 50000 --compile-iters 2 --run-iters 5
```

Default C/C++ build profili `-O0`'dır; bu C*'ın bugünkü açık optimizer profili
taşımayan backend hattıyla daha adil bir debug baseline verir. Release compiler
karşılaştırması için:

```sh
python3 tools/perf_benchmark.py --work 250000 --c-opt -O2 --cpp-opt -O2
```

## Ölçülenler

- `scalar_loop`: integer arithmetic ve while-style loop.
- `function_pressure`: çok sayıda küçük helper function call.
- `array_pressure`: fixed array update/read loop.

Her benchmark C*, C ve C++ kaynak olarak üretilir. Runner şunları ölçer:

- compile wall time
- generated executable size
- runtime wall time
- generated program return code

Return code benchmark sonucudur; `ret value` process exit code'una iner.
Negatif return code platform signal'ı demektir ve C* tarafında runtime crash
olarak yorumlanmalıdır.

## Çıktılar

```text
tests/performance/out/src/        Üretilen C*, C ve C++ kaynakları
tests/performance/out/bin/        C/C++ executable çıktıları
tests/performance/out/cstar-bin/  C* executable/IR/asm çıktıları
tests/performance/out/results.csv Ham sonuçlar
tests/performance/out/REPORT.md   Markdown tablo
```

## Yorumlama

Bu benchmark C* için ilk baseline'dır; C/C++ ile birebir olgun compiler yarışı
değil. C* şu anda kendi frontend'i + LLVM IR + backend clang driver hattından
geçer. C/C++ ise doğrudan platform clang/gcc driver'ını kullanır. Bu yüzden
compile time farkları özellikle pipeline overhead'ini görünür kılar.

Runtime sonuçları basit integer workload'larıdır. İleride struct, ownership,
stdlib, native interop ve büyük module graph senaryoları ayrı benchmark setleri
olarak eklenmelidir.
