# C* Language Support VS Code Extension

Bu paket, `.cstar` dosyalari icin VS Code syntax highlighting, snippets, file icon,
temel editor ayarlari ve otomatik baslayan static analyzer destegi saglar.

## Kapsam

- module yuzeyi: `include`, `as`, `import from`, `export`
- visibility ve storage: `public`, `private`, `static`, `const`, `nomove`
- ownership: `ref`, `deref`, `move`, `new`, `shared new`, `drop`, `strong_count`
- lifecycle: `struct`, `constructor`, `destructor`, static ve instance erisimleri
- sozlesmeler: `trait`, `with`, `protocol`, `attribute`, `macro`, `operator`
- protocol typestate: `protocol Name for Type`, `state`, `default`, `scope_exit`, `A -> B :: method()`
- effect/cleanup: `except`, `throw`, `defer`, `scope_exit`
- kontrol akisi: `if`, `elif`, `else`, `match`, `case`, `option`, `loop`, `for in`, `break`, `continue`, `ret`
- native interop: C string/char escape'leri, `printf` placeholder'lari ve variadic `...`
- operatorler: `::`, `.`, `:=`, `.=` ve sembolik aritmetik/karsilastirma/mantiksal operatorler
- snippets: `main`, `fn`, `struct`, `trait`, `protocol`, `dynprotocol`, `fnexcept`, `defer`, `enum`, `flagenum`, `option`, `attribute`, `macroexpr`, `loopr`, `loopi`, `ife`, `importfrom`, `includeas`, `operator`
- analyzer: acik dosyalarda diagnostics ve LSP completion
- icon: extension icon ve `.cstar` file icon

`<` ve `>` editor bracket-pair olarak tanimli degildir. C* bu karakterleri hem
karsilastirma operatoru hem de `cast<T>` / tip generic yuzeyi icin kullandigi
icin bracket-pair renklendirmesi operatorleri hatali bicimde kirmizi gosterebiliyordu.
TextMate grammar generic algisini yalniz `cast<T>`, `unsafe_cast<T>` ve PascalCase
tip generic formuyla sinirlar; `a < b` ve `a > b` normal operator olarak kalir.

Sembolik operatorler C*'a ozel `meta.operator.*.cstar` veya uygun punctuation
scope'lariyla renklendirilir. Analyzer diagnostics ve completion calisir;
highlight katmani sade TextMate grammar olarak kalir.

## Analyzer Ayarlari

- `cstar.enableAnalyzer`: bundled analyzer'i ac/kapat.
- `cstar.analyzer.serverPath`: custom `server.js` yolu; bos ise bundled server kullanilir.
- `cstar.analyzer.nodePath`: analyzer'i calistiracak Node executable.
- `cstar.analyzer.trace`: JSON-RPC trace loglarini output channel'a yazar.

Komut paletinde `C*: Restart Static Analyzer` komutu analyzer process'ini yeniden baslatir.

## Gelistirme Modunda Calistirma

```bash
code @vscode-cstar-syntax
```

Sonra `F5` ile Extension Development Host ac ve bir `.cstar` dosyasi yukle.

## VSIX Paketleme

```bash
cd @vscode-cstar-syntax
vsce.cmd package --allow-missing-repository --out cstar-syntax-0.5.0.vsix
```

## VSIX Kurulum

```bash
code --install-extension cstar-syntax-0.5.0.vsix --force
```
