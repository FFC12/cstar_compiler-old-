# C* Syntax VS Code Extension

Bu paket, `.cstar` dosyalari icin VS Code syntax highlighting ve temel editor ayarlarini saglar.

## Kapsam

- module yuzeyi: `include`, `as`, `import from`, `export`
- visibility ve storage: `public`, `private`, `static`, `const`, `nomove`
- ownership: `ref`, `deref`, `move`, `new`, `shared new`, `drop`, `strong_count`
- lifecycle: `struct`, `constructor`, `destructor`, static ve instance erisimleri
- sozlesmeler: `trait`, `with`, `protocol`, `attribute`, `macro`, `operator`
- kontrol akisi: `if`, `elif`, `else`, `loop`, `for in`, `break`, `continue`, `ret`
- native interop: C string/char escape'leri, `printf` placeholder'lari ve variadic `...`
- operatorler: `::`, `.`, `:=`, `.=` ve sembolik aritmetik/karsilastirma/mantiksal operatorler

Sembolik operatorler `punctuation.operator.*` scope'lariyla renklendirilir; bu sayede temalar
normal C* operatorlerini hata veya diagnostic token'i gibi gostermek zorunda kalmaz.

## Gelistirme modunda calistirma

```bash
code vscode-cstar-syntax
```

Sonra `F5` ile Extension Development Host ac ve bir `.cstar` dosyasi yukle.

## VSIX paketleme

```bash
cd vscode-cstar-syntax
npx --yes @vscode/vsce package --allow-missing-repository --out cstar-syntax-0.1.0.vsix
```

## VSIX kurulum

```bash
code --install-extension cstar-syntax-0.1.0.vsix
```

Bu paket yalniz syntax/editor destegi icerir. Semantic diagnostic, completion ve navigation
ayri bir C* language server yuzeyine aittir.
