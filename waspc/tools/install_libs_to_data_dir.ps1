# Helper to compile the waspc/libs/* packages locally and in CI.
# It will then move it into the Cabal data dir (and thus, the installer archive in CI releases).
# PSVersion 5.1 or higher is required.

# Gets the directory of where this script lives.
$dir = Split-Path -Parent $MyInvocation.MyCommand.Path
$wascpDir = Resolve-Path "$dir/.."
$dataLibsDir = Join-Path $wascpDir "data/Generator/libs"

if ($env:USE_RANDOM_LIB_VERSION) {
    $randomHex = [guid]::NewGuid().ToString("N").Substring(0, 16)
    $libVersion = "0.0.0-dev-$randomHex"
    Write-Host "USE_RANDOM_LIB_VERSION is set - using random version for cache busting: $libVersion"
} else {
    $libVersion = & "$wascpDir/run.bat" get-waspc-version | Select-Object -Last 1
    Write-Host "Setting lib versions to: $libVersion"
}

# Clean up old libs.
if (Test-Path $dataLibsDir) {
    Remove-Item -Path $dataLibsDir -Recurse -Force
}
New-Item -Path $dataLibsDir -ItemType Directory -Force | Out-Null

$manifestFile = Join-Path $dataLibsDir "manifest.json"
$manifest = @{}

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

        $packageJsonObj = Get-Content -Path "package.json" -Raw | ConvertFrom-Json
        $packageName = $packageJsonObj.name
        $originalTarballFileName = (Get-Item "./*.tgz").Name

        # Extract the package name prefix from the original tarball
        # e.g., wasp.sh-lib-auth-0.0.0.tgz -> wasp.sh-lib-auth
        $tarballPrefix = $originalTarballFileName -replace '-[^-]+\.tgz$', ''

        $newTarballFileName = "$tarballPrefix-$libVersion.tgz"

        if ($originalTarballFileName -ne $newTarballFileName) {
            Rename-Item -Path $originalTarballFileName -NewName $newTarballFileName
            Write-Host "Renamed $originalTarballFileName -> $newTarballFileName"
        } else {
            Write-Host "Tarball already has the correct name: $newTarballFileName"
        }

        $manifest[$packageName] = $newTarballFileName

        Copy-Item -Path $newTarballFileName -Destination $dataLibsDir
    }
    finally {
        Pop-Location
    }
}

$manifest | ConvertTo-Json | Set-Content -Path $manifestFile
Write-Host "Generated manifest at: $manifestFile"
