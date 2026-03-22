# build-installer.ps1
# Full build pipeline: portable R -> packages -> Electron installer.
# Run from the project root (parent of desktop/).

param(
    [switch]$SkipR,
    [switch]$SkipPackages
)

$ErrorActionPreference = "Stop"

Write-Host "======================================" -ForegroundColor Cyan
Write-Host "  MSTerp Desktop Build Pipeline" -ForegroundColor Cyan
Write-Host "======================================" -ForegroundColor Cyan
Write-Host ""

$BuildStart = Get-Date

# -- Step 1: Prepare portable R --------------------------------
if (-Not $SkipR) {
    Write-Host "[1/4] Preparing portable R..." -ForegroundColor White
    & "$PSScriptRoot\prepare-r-portable.ps1"
    Write-Host ""
} else {
    Write-Host "[1/4] Skipped portable R (--SkipR)" -ForegroundColor DarkGray
}

# -- Step 2: Install R packages --------------------------------
if (-Not $SkipPackages) {
    Write-Host "[2/4] Installing R packages..." -ForegroundColor White
    & "$PSScriptRoot\install-r-packages.ps1"
    Write-Host ""
} else {
    Write-Host "[2/4] Skipped package install (--SkipPackages)" -ForegroundColor DarkGray
}

# -- Step 3: npm install --------------------------------------
Write-Host "[3/4] Installing Electron dependencies..." -ForegroundColor White
Push-Location desktop
npm install
if ($LASTEXITCODE -ne 0) { Pop-Location; throw "Step 3 failed: npm install" }
Pop-Location
Write-Host ""

# -- Step 4: Build installer ----------------------------------
Write-Host "[4/4] Building Electron installer..." -ForegroundColor White
Push-Location desktop
npm run dist
if ($LASTEXITCODE -ne 0) { Pop-Location; throw "Step 4 failed: electron-builder" }
Pop-Location
Write-Host ""

# -- Summary ---------------------------------------------------
$Duration = (Get-Date) - $BuildStart
$DurationMin = [math]::Round($Duration.TotalMinutes, 1)

Write-Host "======================================" -ForegroundColor Green
Write-Host "  Build complete ($DurationMin min)" -ForegroundColor Green
Write-Host "======================================" -ForegroundColor Green
Write-Host ""
Write-Host "Output:" -ForegroundColor White

$DistDir = "desktop\dist"
if (Test-Path $DistDir) {
    Get-ChildItem -Path $DistDir -File | ForEach-Object {
        $sizeMB = [math]::Round($_.Length / 1MB, 1)
        Write-Host "  $($_.Name) ($sizeMB MB)"
    }
} else {
    Write-Host "  (dist directory not found -- check for errors above)"
}
