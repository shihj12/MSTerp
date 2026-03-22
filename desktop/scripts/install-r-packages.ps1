# install-r-packages.ps1
# Installs all required R packages into the portable R library.
# Run from the project root (parent of desktop/).

param(
    [string]$RPortableDir = "R-portable",
    [string]$SetupScript = "scripts\setup_shiny_py_r_packages.R"
)

$ErrorActionPreference = "Stop"

$Rscript = "$RPortableDir\bin\Rscript.exe"

Write-Host "=== MSTerp: Install R Packages ===" -ForegroundColor Cyan

# -- Validate portable R ---------------------------------------
if (-Not (Test-Path $Rscript)) {
    throw "Portable R not found at $Rscript. Run prepare-r-portable.ps1 first."
}

if (-Not (Test-Path $SetupScript)) {
    throw "Package setup script not found at $SetupScript."
}

# -- Set library path ------------------------------------------
$LibPath = "$((Resolve-Path $RPortableDir).Path)\library"
$env:R_LIBS_USER = $LibPath
Write-Host "R_LIBS_USER = $LibPath"

# -- Install packages -----------------------------------------
Write-Host "Running package installation script..."
Write-Host "  Rscript: $Rscript"
Write-Host "  Script:  $SetupScript"
Write-Host ""

& $Rscript $SetupScript

if ($LASTEXITCODE -ne 0) {
    throw "Package installation failed with exit code $LASTEXITCODE"
}

Write-Host "" -ForegroundColor Green
Write-Host "Package installation complete." -ForegroundColor Green

# -- Strip package bloat ---------------------------------------
Write-Host "Stripping package documentation and test files..."

$StripPatterns = @("help", "html", "doc", "tests", "demo", ".Rcheck")
$TotalSaved = 0

Get-ChildItem -Path $LibPath -Directory | ForEach-Object {
    $pkgDir = $_.FullName
    foreach ($pattern in $StripPatterns) {
        $subDir = Join-Path $pkgDir $pattern
        if (Test-Path $subDir) {
            $size = (Get-ChildItem -Recurse -File $subDir | Measure-Object -Property Length -Sum).Sum
            Remove-Item -Recurse -Force $subDir
            $TotalSaved += $size
        }
    }
}

$TotalSavedMB = [math]::Round($TotalSaved / 1MB, 1)
Write-Host "Stripped $TotalSavedMB MB of package documentation." -ForegroundColor Yellow

# -- Report final size -----------------------------------------
$FinalSize = (Get-ChildItem -Recurse -File $LibPath | Measure-Object -Property Length -Sum).Sum
$FinalSizeMB = [math]::Round($FinalSize / 1MB, 1)
Write-Host "=== Package library ready: $LibPath ($FinalSizeMB MB) ===" -ForegroundColor Green
