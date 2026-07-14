# C* Static Analyzer TODO

Bu belge `@vscode-cstar-static-analyzer` klasorundeki language server/analyzer
calismasinin yol haritasidir. Ana `docs/TODO_TR.md` dosyasindan ayridir.

## Kaynak Kararlar

- VS Code TextMate grammar hizli syntax renklendirme icindir.
- Derin semantic bilgi icin Language Server modeli kullanilacak.
- LSP server ayri process olarak calisip `textDocument/publishDiagnostics` ile editor'e diagnostic yollar.
- Server dependency-free Node ile baslar; ileride gerekirse `vscode-languageclient` client'ina gecilebilir.

## Tamamlanan Production Entegrasyonu

- [x] LSP stdio framing.
- [x] `initialize`, `shutdown`, `exit`.
- [x] Full document sync: `didOpen`, `didChange`, `didClose`.
- [x] `publishDiagnostics`.
- [x] LSP `completionProvider` capability.
- [x] CLI analiz komutu.
- [x] VS Code extension icinde bundled server olarak paketlenme.
- [x] VS Code acilinca `.cstar` dosyalari icin otomatik server baslatma.
- [x] Analyzer restart komutu.
- [x] `cstar.enableAnalyzer`, custom server path, node path ve trace ayarlari.
- [x] Output channel ve JSON-RPC trace yuzeyi.
- [x] Diagnostics debounce.

## Tamamlanan Core Analyzer MVP

- [x] Lexer-lite: comment/string/char masking ve C* operator/token yuzeyi.
- [x] Token stream'e stable diagnostic span bilgisi.
- [x] Unterminated comment/string/char diagnostics.
- [x] Delimiter matching diagnostics.
- [x] Loop disi `break` / `continue` diagnostics.
- [x] `ret` outside function diagnostic'i.
- [x] Parser-lite: top-level item, function signature, struct field/method ve block scope parse.
- [x] Function/struct/trait/protocol/enum/tagged/macro/module alias symbol table MVP.
- [x] Basit duplicate top-level declaration uyarisi.
- [x] Basit unknown type diagnostic'i.
- [x] Trait conformance icin method existence warning MVP.
- [x] `*` ve `^` pointer marker karisimi diagnostic'i.

## Tamamlanan Completion MVP

- [x] Keyword/type completion: `ret`, `if`, `loop`, primitive type ve qualifier onerileri.
- [x] Top-level item completion: `struct`, `trait`, `enum`, `import`, `include`, `public`, `static`.
- [x] Symbol table tabanli local variable, function, struct type ve trait name completion.
- [x] Member completion: `value.` icin struct field/method.
- [x] Static/member completion: `Type::` icin static method/enum variant onerileri.
- [x] Import/include alias completion: `math.` gibi module alias yuzeyleri icin MVP.
- [x] Snippet completion ile LSP completion ayrimi: editor snippets extension'da, semantic oneriler analyzer'da.

## Kalan Derin Semantik Isler

- [ ] C++ compiler lexer'iyle birebir uyum testi.
- [ ] `expected-code` etiketli type_checker ornekleriyle analyzer snapshot testleri.
- [ ] Duplicate function overload kurallarini gercek C* semantigine gore ayir.
- [ ] Full unknown symbol diagnostics.
- [ ] `include` path resolve ve module visibility kontrolu.
- [ ] `const`, `constptr`, `constref`, `readonly`, `nomove` kurallarini editor diagnostic olarak modelle.
- [ ] Unique pointer copy/move flow analizi.
- [ ] Dropped-after-use ve moved-after-use analizi.
- [ ] Reference parametreye ciplak value gecirme diagnostic'i.
- [ ] Safe cast / unsafe cast kategori kurallari.
- [ ] Workspace trust ve remote workspace davranisi.
- [ ] Incremental parsing stratejisi.
