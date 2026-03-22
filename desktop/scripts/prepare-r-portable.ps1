# prepare-r-portable.ps1
# Downloads and strips R into a portable runtime for bundling with Electron.
# Run from the project root (parent of desktop/).

param(
    [string]$RVersion = "4.5.3",
    [string]$OutputDir = "R-portable"
)

$ErrorActionPreference = "Stop"

$InstallerUrl = "https://cran.r-project.org/bin/windows/base/R-$RVersion-win.exe"
$InstallerFile = "R-$RVersion-win.exe"

Write-Host "=== MSTerp: Prepare Portable R $RVersion ===" -ForegroundColor Cyan

# -- Download R installer --------------------------------------
if (-Not (Test-Path $InstallerFile)) {
    Write-Host "Downloading R $RVersion from CRAN..."
    Invoke-WebRequest -Uri $InstallerUrl -OutFile $InstallerFile -UseBasicParsing
    Write-Host "Downloaded: $InstallerFile"
} else {
    Write-Host "Using cached installer: $InstallerFile"
}

# -- Silent extract --------------------------------------------
if (Test-Path $OutputDir) {
    Write-Host "Removing existing $OutputDir..."
    Remove-Item -Recurse -Force $OutputDir
}

Write-Host "Extracting R to $OutputDir (silent install)..."
$proc = Start-Process -FilePath ".\$InstallerFile" -ArgumentList "/VERYSILENT", "/DIR=$((Resolve-Path .).Path)\$OutputDir", "/SUPPRESSMSGBOXES", "/NORESTART" -Wait -NoNewWindow -PassThru
if ($proc.ExitCode -ne 0) {
    throw "R installer exited with code $($proc.ExitCode)"
}

if (-Not (Test-Path "$OutputDir\bin\Rscript.exe")) {
    throw "R extraction failed -- Rscript.exe not found in $OutputDir\bin\"
}
Write-Host "R extracted successfully." -ForegroundColor Green

# -- Strip unnecessary files -----------------------------------
Write-Host "Stripping unnecessary files to reduce size..."

$StripDirs = @(
    "$OutputDir\doc",
    "$OutputDir\tests",
    "$OutputDir\Tcl",
    "$OutputDir\share\locale",
    "$OutputDir\library\translations"
)

$TotalSaved = 0
foreach ($dir in $StripDirs) {
    if (Test-Path $dir) {
        $size = (Get-ChildItem -Recurse -File $dir | Measure-Object -Property Length -Sum).Sum
        $sizeMB = [math]::Round($size / 1MB, 1)
        Remove-Item -Recurse -Force $dir
        Write-Host "  Removed $dir ($sizeMB MB)"
        $TotalSaved += $size
    }
}

$TotalSavedMB = [math]::Round($TotalSaved / 1MB, 1)
Write-Host "Total space saved: $TotalSavedMB MB" -ForegroundColor Yellow

# -- Report final size -----------------------------------------
$FinalSize = (Get-ChildItem -Recurse -File $OutputDir | Measure-Object -Property Length -Sum).Sum
$FinalSizeMB = [math]::Round($FinalSize / 1MB, 1)
Write-Host "=== Portable R ready: $OutputDir ($FinalSizeMB MB) ===" -ForegroundColor Green
