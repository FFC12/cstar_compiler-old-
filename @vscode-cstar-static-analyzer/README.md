# C* Static Analyzer Server

Bu klasor C* icin baslangic seviye, dependency-free bir Language Server Protocol
server iskeleti icerir. Amac compiler'in yerine gecmek degil; editor icinde hizli
diagnostic verecek cekirdegi kurmak.

## Su an desteklenenler

- LSP `initialize`, `shutdown`, `exit`
- `textDocument/didOpen`, `didChange`, `didClose`
- `textDocument/publishDiagnostics`
- Unterminated block comment, string ve char literal diagnostics
- Eslesmeyen `{}`, `[]`, `()` diagnostics
- `break` / `continue` icin loop disi kullanim diagnostics
- Basit duplicate top-level declaration uyarilari
- CLI ile tek dosya analizi

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
