# C* Data Structures Reality Check

Bu klasör C*'ın mevcut dil yüzeyiyle klasik algoritma ve veri yapılarının ne
kadar doğal yazılabildiğini ölçmek için eklenmiştir. Dosyalar smoke test gibi
çalıştırılabilir örneklerdir; amaç yalnız demo değil, compiler sınırlarını da
görünür kılmaktır.

## Bugünkü sonuç

- Node tabanlı heap allocation artık primitive ve struct için çalışır.
- `T[N]` fixed array parametreleri boyut güvenli ve kopyasız parameter view gibi
  kullanılabilir.
- `T[]` runtime span parametreleri `span data` ve `span data[a..b]` ile
  kullanılabilir.
- `struct`, method, constructor, nullable pointer, ownership pointer ve `drop`
  temel data structure örneklerini taşır.

## Eksik kalan state-of-art parçalar

- Generic `Vector<T>`, `HashMap<K,V>` gibi container'lar için generic struct ve
  generic function modeli henüz tamamlanmalı.
- Heap array allocation (`new int32[capacity]`) ve resize/reallocate contract'ı
  eksik; bu olmadan gerçek dinamik vector/hash-table sabit buffer veya CRT
  pointer bridge ile sınırlı kalır.
- Atomic/concurrency primitive'leri dil yüzeyinde yok; shared pointer refcount
  runtime'da atomic olsa da kullanıcı `Atomic<T>`, CAS, mutex, memory order gibi
  yapı taşlarını doğrudan yazamıyor.
- Iterator/range ergonomisi gelişiyor ama container protokolleri henüz standart
  değil.
- Bounds-check policy ve unsafe escape hatch'leri daha görünür diagnostic ve
  optimizasyon kontrolü istemektedir.

