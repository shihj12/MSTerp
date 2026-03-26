# build-installer.ps1
# Full build pipeline: portable R -> packages -> C# publish -> InnoSetup installer.
# Run from the project root (parent of desktop-webview2/).

param(
    [switch]$SkipR,
    [switch]$SkipPackages
)

$ErrorActionPreference = "Stop"

Write-Host "======================================" -ForegroundColor Cyan
Write-Host "  MSTerp Desktop Build Pipeline" -ForegroundColor Cyan
Write-Host "  (WebView2 + InnoSetup)" -ForegroundColor Cyan
Write-Host "======================================" -ForegroundColor Cyan
Write-Host ""

$BuildStart = Get-Date

# -- Step 1: Prepare portable R --------------------------------
if (-Not $SkipR) {
    Write-Host "[1/4] Preparing portable R..." -ForegroundColor White
    & "$PSScriptRoot\..\..\desktop\scripts\prepare-r-portable.ps1"
    if ($LASTEXITCODE -ne 0) { throw "Step 1 failed: prepare-r-portable.ps1" }
    Write-Host ""
} else {
    Write-Host "[1/4] Skipped portable R (-SkipR)" -ForegroundColor DarkGray
}

# -- Step 2: Install R packages --------------------------------
if (-Not $SkipPackages) {
    Write-Host "[2/4] Installing R packages..." -ForegroundColor White
    & "$PSScriptRoot\..\..\desktop\scripts\install-r-packages.ps1"
    if ($LASTEXITCODE -ne 0) { throw "Step 2 failed: install-r-packages.ps1" }
    Write-Host ""
} else {
    Write-Host "[2/4] Skipped package install (-SkipPackages)" -ForegroundColor DarkGray
}

# -- Step 3: Build C# project ----------------------------------
Write-Host "[3/4] Publishing C# WebView2 wrapper..." -ForegroundColor White
$CsprojPath = Join-Path $PSScriptRoot "..\MSTerp.csproj"
dotnet publish $CsprojPath -c Release -r win-x64 --self-contained false -o (Join-Path $PSScriptRoot "..\publish")
if ($LASTEXITCODE -ne 0) { throw "Step 3 failed: dotnet publish" }
Write-Host ""

# -- Step 4: Build InnoSetup installer -------------------------
Write-Host "[4/4] Building InnoSetup installer..." -ForegroundColor White
$IssPath = Join-Path $PSScriptRoot "..\installer.iss"

# Find InnoSetup compiler
$IsccPaths = @(
    "C:\Program Files (x86)\Inno Setup 6\ISCC.exe",
    "C:\Program Files\Inno Setup 6\ISCC.exe",
    (Get-Command iscc -ErrorAction SilentlyContinue).Source
) | Where-Object { $_ -and (Test-Path $_) }

if ($IsccPaths.Count -eq 0) {
    throw "InnoSetup compiler (ISCC.exe) not found. Install from https://jrsoftware.org/isdl.php"
}

$Iscc = $IsccPaths[0]
Write-Host "  Using: $Iscc" -ForegroundColor DarkGray
& $Iscc $IssPath
if ($LASTEXITCODE -ne 0) { throw "Step 4 failed: InnoSetup" }
Write-Host ""

# -- Summary ---------------------------------------------------
$Duration = (Get-Date) - $BuildStart
$DurationMin = [math]::Round($Duration.TotalMinutes, 1)

Write-Host "======================================" -ForegroundColor Green
Write-Host "  Build complete ($DurationMin min)" -ForegroundColor Green
Write-Host "======================================" -ForegroundColor Green
Write-Host ""
Write-Host "Output:" -ForegroundColor White

$DistDir = Join-Path $PSScriptRoot "..\dist"
if (Test-Path $DistDir) {
    Get-ChildItem -Path $DistDir -File | ForEach-Object {
        $sizeMB = [math]::Round($_.Length / 1MB, 1)
        Write-Host "  $($_.Name) ($sizeMB MB)"
    }
} else {
    Write-Host "  (dist directory not found -- check for errors above)"
}
