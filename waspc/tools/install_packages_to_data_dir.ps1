# Helper to compile the waspc/packages/* packages locally and in CI.
# It will then move them into the Cabal data dir (and thus, the installer archive in CI releases).
# PSVersion 5.1 or higher is required.

# Gets the directory of where this script lives.
$dir = Split-Path -Parent $MyInvocation.MyCommand.Path
$wascpDir = Resolve-Path "$dir/.."
$dataPackagesDir = Join-Path $wascpDir "data/packages"

# Clean up old packages in data dir.
if ([string]::IsNullOrEmpty($dataPackagesDir)) {
    throw "dataPackagesDir must be set before cleanup"
}
if (Test-Path $dataPackagesDir) {
    Remove-Item -Path $dataPackagesDir -Recurse -Force
}
New-Item -Path $dataPackagesDir -ItemType Directory -Force | Out-Null

$packageDirs = Get-ChildItem -Path "$wascpDir/packages" -Directory

foreach ($package in $packageDirs) {
    $packageDir = $package.FullName
    $packageName = $package.Name
    
    # We're only installing the dependencies here to verify that the build
    # works, that's why the node_modules folder is removed immediately after.
    # The real dependency installation happens in Haskell.
    Write-Host "Installing $packageName ($packageDir)"
    
    Push-Location $packageDir
    try {
        npm install
        npm run build
        Remove-Item -Path "./node_modules" -Recurse -Force
    }
    finally {
        Pop-Location
    }
}

Push-Location $wascpDir
try {
    Copy-Item -Path "./packages/*" -Destination $dataPackagesDir -Recurse
}
finally {
    Pop-Location
}
