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

        if ($env:USE_RANDOM_LIB_VERSION) {
            $tarballPrefix = $originalTarballFileName -replace '-[^-]+\.tgz$', ''
            $randomHex = [guid]::NewGuid().ToString("N").Substring(0, 16)
            $randomLibVersion = "0.0.0-dev-$randomHex"
            $newTarballFileName = "$tarballPrefix-$randomLibVersion.tgz"
            Rename-Item -Path $originalTarballFileName -NewName $newTarballFileName
            Write-Host "Renamed $originalTarballFileName -> $newTarballFileName"
        } else {
            $newTarballFileName = $originalTarballFileName
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
