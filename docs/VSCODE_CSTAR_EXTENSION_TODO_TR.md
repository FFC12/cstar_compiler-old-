# C* VS Code Extension TODO

Bu belge `vscode-cstar-syntax` paketinin editor/UX yol haritasidir. `docs/TODO_TR.md`
dosyasindan ayridir ve ona dokunmaz.

## Tamamlananlar

- `.cstar` language contribution ve TextMate grammar.
- Comment, string, char, number, primitive type ve operator scope'lari.
- Struct, trait, protocol, enum, tagged, macro, attribute, lifecycle ve ownership keyword'leri.
- Generic/type parameter `<...>` icin ayri scope.
- Language configuration: bracket/comment pairs, indentation rules, word pattern.
- Snippets: `main`, function, struct, trait, loop, import/include, operator.
- Extension PNG icon ve `.cstar` file icon.
- Extension Development Host icin `.vscode/launch.json`.

## Production Hazirlik

- [ ] Grammar snapshot test sistemi kur.
- [ ] Smoke/type_checker/papers orneklerinden tokenization fixture'lari uret.
- [ ] Snippet/basic keyword completion kapsamlarini netlestir; semantik completion'in LSP/analyzer tarafina ait oldugunu README'de belirt.
- [ ] VSIX paket adini ve publisher bilgisini gercek yayin hedefiyle kesinlestir.
- [ ] Marketplace icin `LICENSE`, `CHANGELOG.md`, repository URL ve kategori metadatasini tamamla.
- [ ] README'ye ekran goruntusu ve kurulum yollarini ekle.
- [ ] `vsce package` ciktisini CI'da dogrula.
- [ ] Icon'u 128/256/512 PNG varyantlariyla polish et.

## Grammar Iyilestirmeleri

- [ ] `dynamic Trait&`, `opened FileHandle^` gibi protocol-state + type kombinasyonlarini daha hassas scope'la.
- [ ] `operator +(T rhs)` declaration ve operator call ayrimini testle.
- [ ] Macro body icinde `$emit`, `$field`, `$item`, `#for` tokenlarini daha semantik scope'la.
- [ ] Tagged enum variant initializer `Packet.Data { bytes: bytes }` icin variant/property ayrimi ekle.
- [ ] `import from` ve `export from` block basliklarini snapshot testle.
- [ ] String icindeki printf placeholder kapsamlarini genislet.

## Analyzer Entegrasyonu

- [ ] `@vscode-cstar-static-analyzer` server'ini extension client ile baslat.
- [ ] LSP completion provider'i extension client tarafindan expose et; extension icinde yalniz snippets/static fallback kalsin.
- [ ] Analyzer enable/disable setting'i ekle.
- [ ] Analyzer path override setting'i ekle.
- [ ] Diagnostics source ve code aksiyon linklerini README'de dokumante et.
- [ ] Semantic tokens icin LSP tarafinda ikinci katman tasarla.
