# Helper to compile the waspc/libs/* packages locally and in CI.
# It will then move it into the Cabal data dir (and thus, the installer archive in CI releases).
# PSVersion 5.1 or higher is required.

# Gets the directory of where this script lives.
$dir = Split-Path -Parent $MyInvocation.MyCommand.Path
$wascpDir = Resolve-Path "$dir/.."
$dataLibsDir = Join-Path $wascpDir "data/Generator/libs"

# Clean up old libs.
if (Test-Path $dataLibsDir) {
    Remove-Item -Path $dataLibsDir -Recurse -Force
}
New-Item -Path $dataLibsDir -ItemType Directory -Force | Out-Null

$libDirs = Get-ChildItem -Path "$wascpDir/libs" -Directory

foreach ($lib in $libDirs) {
    $libDir = $lib.FullName
    $libName = $lib.Name

    Write-Host "Installing $libName lib ($libDir)"

    Push-Location $libDir
    try {
        npm install
        # Clean up old lib tarballs.
        Remove-Item -Path "./*.tgz" -Force -ErrorAction SilentlyContinue
        npm pack
        Copy-Item -Path "./*.tgz" -Destination $dataLibsDir
    }
    finally {
        Pop-Location
    }
}
