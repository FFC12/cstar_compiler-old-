$ErrorActionPreference = "Stop"

$ValidSuites = @("smoke", "type_checker", "functions", "variables", "papers", "interactive", "all")
$BuildDir = $null
$ExamplesDir = $null
$Suite = "smoke"
$All = $false
$ExpectDiagnostics = $false

function Show-Usage {
  Write-Host "Usage:"
  Write-Host "  powershell -File .\tools\run_examples.ps1 [suite] [options]"
  Write-Host ""
  Write-Host "Suites:"
  Write-Host "  smoke, type_checker, functions, variables, papers, interactive, all"
  Write-Host ""
  Write-Host "Options:"
  Write-Host "  -Suite <name> | --suite <name>"
  Write-Host "  -BuildDir <path> | --build-dir <path>"
  Write-Host "  -ExamplesDir <path> | --examples-dir <path>"
  Write-Host "  -All | --all"
  Write-Host "  -ExpectDiagnostics | --expect-diagnostics"
  Write-Host "  -Help | --help"
}

for ($Index = 0; $Index -lt $args.Count; $Index++) {
  $Arg = $args[$Index]
  switch -Regex ($Arg) {
    '^(-h|--help|-Help)$' {
      Show-Usage
      exit 0
    }
    '^(-Suite|--suite)$' {
      $Index += 1
      if ($Index -ge $args.Count) {
        throw "$Arg icin suite adi bekleniyor."
      }
      $Suite = $args[$Index]
      break
    }
    '^(-BuildDir|--build-dir)$' {
      $Index += 1
      if ($Index -ge $args.Count) {
        throw "$Arg icin build klasoru bekleniyor."
      }
      $BuildDir = $args[$Index]
      break
    }
    '^(-ExamplesDir|--examples-dir)$' {
      $Index += 1
      if ($Index -ge $args.Count) {
        throw "$Arg icin examples klasoru bekleniyor."
      }
      $ExamplesDir = $args[$Index]
      break
    }
    '^(-All|--all)$' {
      $All = $true
      $Suite = "all"
      break
    }
    '^(-ExpectDiagnostics|--expect-diagnostics)$' {
      $ExpectDiagnostics = $true
      break
    }
    default {
      if ($ValidSuites -contains $Arg) {
        $Suite = $Arg
      } else {
        throw "Bilinmeyen arguman: $Arg. Yardim icin --help kullan."
      }
    }
  }
}

if (-not ($ValidSuites -contains $Suite)) {
  throw "Gecersiz suite: $Suite. Gecerli suite'ler: $($ValidSuites -join ', ')"
}

$Root = Resolve-Path (Join-Path $PSScriptRoot "..")
$BuildCandidates = @()
if ($BuildDir) {
  $BuildCandidates += $BuildDir
} else {
  $BuildCandidates += (Join-Path $Root "build")
  $BuildCandidates += (Join-Path $Root "build-ucrt")
}

if (-not $ExamplesDir) {
  if ($All) {
    $Suite = "all"
  }

  $ExamplesDir = if ($Suite -eq "all") {
    Join-Path $Root "examples"
  } else {
    Join-Path $Root "examples\$Suite"
  }
}

$CstarCandidates = @()
foreach ($Candidate in $BuildCandidates) {
  $CstarCandidates += (Join-Path $Candidate "cstar.exe")
  $CstarCandidates += (Join-Path $Candidate "cstar")
  $CstarCandidates += (Join-Path $Candidate "Debug\cstar.exe")
  $CstarCandidates += (Join-Path $Candidate "Debug\cstar")
  $CstarCandidates += (Join-Path $Candidate "Release\cstar.exe")
  $CstarCandidates += (Join-Path $Candidate "Release\cstar")
}

$Cstar = $CstarCandidates | Where-Object { Test-Path -LiteralPath $_ } | Select-Object -First 1
if (-not $Cstar) {
  throw "cstar executable bulunamadi. Aranan yollar: $($CstarCandidates -join ', ')"
}

$ResolvedBuildDir = Split-Path -Parent $Cstar
if ((Split-Path -Leaf $ResolvedBuildDir) -in @("Debug", "Release")) {
  $ResolvedBuildDir = Split-Path -Parent $ResolvedBuildDir
}

$RunDir = Join-Path $ResolvedBuildDir "example-runs"
New-Item -ItemType Directory -Force -Path $RunDir | Out-Null

$OldPath = $env:PATH
if (Test-Path -LiteralPath "C:\msys64\ucrt64\bin") {
  $env:PATH = "C:\msys64\ucrt64\bin;C:\msys64\usr\bin;$env:PATH"
}

$Files = Get-ChildItem -LiteralPath $ExamplesDir -Filter "*.cstar" -Recurse |
  Sort-Object FullName

if ($Files.Count -eq 0) {
  throw "Calistirilacak .cstar dosyasi bulunamadi: $ExamplesDir"
}

$Failures = @()
$Diagnostics = @()
$Crashes = @()
$ExitMismatches = @()
$CodeMismatches = @()
$Skipped = @()
foreach ($File in $Files) {
  $RelativeName = Resolve-Path -Relative $File.FullName
  if (($File.FullName -split '[\\/]' | Where-Object { $_ -eq "modules" }).Count -gt 0) {
    Write-Host "[SKIP] $RelativeName (module helper)" -ForegroundColor DarkGray
    $Skipped += $RelativeName
    continue
  }

  $ExpectedKind = $null
  $ExpectedExit = $null
  $ExpectedCodes = @()
  foreach ($Line in (Get-Content -LiteralPath $File.FullName -TotalCount 8)) {
    if ($Line -match '^\s*//\s*expected:\s*(.+?)\s*$') {
      $ExpectedKind = $Matches[1].Trim()
    }

    if ($Line -match '^\s*//\s*expected-exit:\s*(-?\d+)\s*$') {
      $ExpectedExit = [int]$Matches[1]
    }

    if ($Line -match '^\s*//\s*expected-code:\s*(.+?)\s*$') {
      $ExpectedCodes = $Matches[1].Split(",") |
        ForEach-Object { $_.Trim() } |
        Where-Object { $_.Length -gt 0 }
    }
  }

  if ($ExpectedKind -and $ExpectedKind.ToLowerInvariant().StartsWith("interactive") -and $Suite -ne "interactive") {
    Write-Host "[SKIP] $RelativeName (interactive)" -ForegroundColor DarkGray
    $Skipped += $RelativeName
    continue
  }

  Write-Host "[RUN] $RelativeName" -ForegroundColor Cyan

  Push-Location $RunDir
  try {
    $CompilerOutput = & $Cstar $File.FullName --run --stats 2>&1
    $ExitCode = $LASTEXITCODE
  } finally {
    Pop-Location
  }

  $CompilerOutput | ForEach-Object { Write-Host $_ }
  $ActualGeneratedExit = $null
  foreach ($Line in $CompilerOutput) {
    if ($Line -match 'Generated program exited with code\s+(-?\d+)') {
      $ActualGeneratedExit = [int]$Matches[1]
    }
  }

  if ($ExitCode -eq 0) {
    if ($null -ne $ExpectedExit -and $ActualGeneratedExit -ne $ExpectedExit) {
      Write-Host "[EXIT] $RelativeName expected=$ExpectedExit actual=$ActualGeneratedExit" -ForegroundColor Red
      $Failures += $RelativeName
      $ExitMismatches += $RelativeName
    } else {
      $ExitSuffix = if ($null -ne $ActualGeneratedExit) { " (exit $ActualGeneratedExit)" } else { "" }
      Write-Host "[OK] $RelativeName$ExitSuffix" -ForegroundColor Green
    }
  } elseif ($ExpectDiagnostics -and $ExitCode -eq 1) {
    $JoinedOutput = $CompilerOutput -join "`n"
    $MissingCodes = @()
    foreach ($ExpectedCode in $ExpectedCodes) {
      if ($JoinedOutput -notmatch "error\[$([regex]::Escape($ExpectedCode))\]") {
        $MissingCodes += $ExpectedCode
      }
    }

    if ($MissingCodes.Count -gt 0) {
      Write-Host "[CODE] $RelativeName missing=$($MissingCodes -join ',')" -ForegroundColor Red
      $Failures += $RelativeName
      $CodeMismatches += $RelativeName
    } else {
      $CodeSuffix = if ($ExpectedCodes.Count -gt 0) { " [$($ExpectedCodes -join ',')]" } else { "" }
      Write-Host "[DIAG] $RelativeName$CodeSuffix" -ForegroundColor Yellow
      $Diagnostics += $RelativeName
    }
  } else {
    Write-Host "[FAIL] $RelativeName ($ExitCode)" -ForegroundColor Red
    $Failures += $RelativeName
    if ($ExitCode -ne 1) {
      $Crashes += $RelativeName
    }
  }
}

Write-Host ""
Write-Host ("Suite: {0}" -f $Suite)
Write-Host ("Toplam: {0}, Basarili: {1}, Diagnostic: {2}, Skipped: {3}, Hatali: {4}, ExitMismatch: {5}, CodeMismatch: {6}, Crash/Assert: {7}" -f $Files.Count, ($Files.Count - $Failures.Count - $Diagnostics.Count - $Skipped.Count), $Diagnostics.Count, $Skipped.Count, $Failures.Count, $ExitMismatches.Count, $CodeMismatches.Count, $Crashes.Count)

if ($Failures.Count -gt 0) {
  $env:PATH = $OldPath
  exit 1
}

$env:PATH = $OldPath
