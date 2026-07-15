# C* Language Support VS Code Extension

Bu paket, `.cstar` dosyalari icin VS Code syntax highlighting, snippets, file icon,
temel editor ayarlari ve otomatik baslayan static analyzer destegi saglar.

## Kapsam

- module yuzeyi: `include`, `as`, `import from`, `export`
- visibility ve storage: `public`, `private`, `static`, `const`, `nomove`
- ownership: `ref`, `deref`, `move`, `new`, `shared new`, `drop`, `strong_count`
- lifecycle: `struct`, `constructor`, `destructor`, static ve instance erisimleri
- sozlesmeler: `trait`, `with`, `protocol`, `attribute`, `macro`, `operator`
- kontrol akisi: `if`, `elif`, `else`, `loop`, `for in`, `break`, `continue`, `ret`
- native interop: C string/char escape'leri, `printf` placeholder'lari ve variadic `...`
- operatorler: `::`, `.`, `:=`, `.=` ve sembolik aritmetik/karsilastirma/mantiksal operatorler
- snippets: `main`, `fn`, `struct`, `trait`, `loopr`, `loopi`, `ife`, `importfrom`, `includeas`, `operator`
- analyzer: acik dosyalarda diagnostics ve LSP completion
- icon: extension icon ve `.cstar` file icon

Sembolik operatorler `punctuation.operator.*` scope'lariyla renklendirilir; bu sayede temalar
normal C* operatorlerini hata veya diagnostic token'i gibi gostermek zorunda kalmaz.

## Analyzer Ayarlari

- `cstar.enableAnalyzer`: bundled analyzer'i ac/kapat.
- `cstar.analyzer.serverPath`: custom `server.js` yolu; bos ise bundled server kullanilir.
- `cstar.analyzer.nodePath`: analyzer'i calistiracak Node executable.
- `cstar.analyzer.trace`: JSON-RPC trace loglarini output channel'a yazar.

Komut paletinde `C*: Restart Static Analyzer` komutu analyzer process'ini yeniden baslatir.

## Gelistirme Modunda Calistirma

```bash
code vscode-cstar-syntax
```

Sonra `F5` ile Extension Development Host ac ve bir `.cstar` dosyasi yukle.

## VSIX Paketleme

```bash
cd vscode-cstar-syntax
vsce.cmd package --allow-missing-repository --out cstar-syntax-0.3.0.vsix
```

## VSIX Kurulum

```bash
code --install-extension cstar-syntax-0.3.0.vsix --force
```
