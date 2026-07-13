# C* Syntax VS Code Extension

Bu klasor, `.cstar` dosyalari icin VS Code syntax highlighting ve temel editor ayarlarini ekler.

## Kapsam

- `.cstar` dosya eslestirmesi
- `//` ve `/* */` yorumlari
- String, char, sayi ve sabit renklendirme
- C* primitive tipleri, pointer/reference/operator tokenlari
- `ret`, `if`, `elif`, `loop`, `include`, `import`, `export`, `attribute`, `prototype`, `enum`, `struct`, `trait`, `policy`, `macro` gibi cekirdek ve proposal keyword'leri
- `@directive`, `#directive`, `$func`, `$line`, `$0` gibi meta/directive sembolleri
- Bracket matching ve auto-closing pairs

## Gelistirme modunda calistirma

1. VS Code'da bu klasoru ac:

   ```powershell
   code vscode-cstar-syntax
   ```

2. `F5` ile Extension Development Host baslat.
3. Acilan yeni VS Code penceresinde bir `.cstar` dosyasi ac.

## Lokal kurulum

VS Code extension klasorune kopyalayarak kullanabilirsin:

```powershell
$target = "$env:USERPROFILE\.vscode\extensions\cstar-project.cstar-syntax-0.1.0"
New-Item -ItemType Directory -Force -Path $target
Copy-Item -Recurse -Force .\vscode-cstar-syntax\* $target
```

Sonra VS Code'u yeniden baslat.

## VSIX paketleme

`vsce` kurulu degilse:

```powershell
npm install -g @vscode/vsce
```

Paket olustur:

```powershell
cd vscode-cstar-syntax
vsce package
```

Olusan `.vsix` dosyasini VS Code'da `Extensions: Install from VSIX...` komutu ile kurabilirsin.
