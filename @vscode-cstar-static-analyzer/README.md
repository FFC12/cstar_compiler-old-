# C* Static Analyzer Server

Bu klasor C* icin baslangic seviye, dependency-free bir Language Server Protocol
server iskeleti icerir. Amac compiler'in yerine gecmek degil; editor icinde hizli
diagnostic verecek cekirdegi kurmak.

## Su an desteklenenler

- LSP `initialize`, `shutdown`, `exit`
- `textDocument/didOpen`, `didChange`, `didClose`
- `textDocument/publishDiagnostics`
- `textDocument/completion` icin keyword, type, symbol, module alias, member ve snippet onerileri
- Unterminated block comment, string ve char literal diagnostics
- Eslesmeyen `{}`, `[]`, `()` diagnostics
- `break` / `continue` icin loop disi kullanim diagnostics
- Basit duplicate top-level declaration uyarilari
- Struct/trait conformance icin hafif editor uyarilari
- Qualifier kombinasyonlari icin hizli editor diagnostics
- Ownership-flow icin local, conservative moved-value ve unique-copy diagnostics
- Protocol, effect, enum/flags, macro/attribute, nullable/new? ve stdlib helper keyword/completion sozlugu
- `.=` dynamic protocol yuzeyi icin bilgi seviyesinde proposal diagnostic
- CLI ile tek dosya analizi

Analyzer compiler'in yerine gecmez. Hedefi editor icinde hizli geri bildirim
vermek ve C* yuzeyini dogru completion/highlight ile desteklemektir; nihai karar
her zaman `cstar.exe` pipeline'indadir.

## Calistirma

```powershell
cd @vscode-cstar-static-analyzer
npm run analyze -- ..\examples\smoke\minimal.cstar
```

LSP server olarak:

```powershell
npm start
```

VS Code client baglantisi sonraki adimdir. Resmi VS Code Language Server rehberi
client'i normal VS Code extension olarak, server'i ayri Node process olarak
calistirir; bu klasor su anda server tarafini baslatir.

## Tasarim notu

Server su anda ek paket kullanmadan LSP framing'i kendisi yapar. Bu, ilk adimi
offline ve repo-icinde tutar. Production client entegrasyonunda
`vscode-languageclient` kullanmak daha dogru olacaktir.
