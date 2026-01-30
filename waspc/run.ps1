# The script duplicates some part of logics from run shell script.
# We plan to merge these two scripts into one platform-agnostic script in the future to avoid duplication.
# Requires PowerShell 5.1 or higher.

param(
    [string]$Command = ""
)

$PROJECT_ROOT = Split-Path -Parent $MyInvocation.MyCommand.Path
$REPOSITORY_ROOT = Split-Path -Parent $PROJECT_ROOT

# Building
$WASP_PACKAGES_COMPILE = "Get-ChildItem `"$PROJECT_ROOT\data\packages\*\package.json`" | ForEach-Object { Push-Location (Split-Path `$_); npm install; npm run build; Pop-Location }"
$BUILD_HS_CMD = "cabal build all"
$BUILD_ALL_CMD = "$WASP_PACKAGES_COMPILE -and $BUILD_HS_CMD"
$RUN_CMD="cabal --project-dir=${PROJECT_ROOT} run wasp-cli -- $Args"
switch ($Command) {
    "build" {
        Invoke-Expression $BUILD_ALL_CMD
    }
    "build:hs" {
        Invoke-Expression $BUILD_HS_CMD
    }
    "build:packages" {
        Invoke-Expression $WASP_PACKAGES_COMPILE
    }
    # NOTE: --experimental-strip-types allows Node to run TypeScript directly.
    # This flag became the default in Node v22.18+, but we need it explicitly
    # for our minimum supported version (v22.12.0).
    "build:libs" {
        node --experimental-strip-types "$PROJECT_ROOT/tools/libs/build.ts"
    }
    "test:libs" {
        node --experimental-strip-types "$PROJECT_ROOT/tools/libs/test.ts"
    }
    "get-waspc-version" {
        node --experimental-strip-types "$PROJECT_ROOT/tools/get-waspc-version.ts"
    }
    "wasp-cli" {
        Invoke-Expression $RUN_CMD
    }
    Default {
        Write-Host "USAGE"
        Write-Host "  run <command>"
        Write-Host ""
        Write-Host "COMMANDS"
        Write-Host "  build             Builds the Haskell project + all sub-projects (i.e. TS packages)."
        Write-Host "  build:hs          Builds the Haskell project only."
        Write-Host "  build:packages    Builds the TypeScript projects under data/packages/."
        Write-Host "  build:libs        Builds the TypeScript libs under libs/."
        Write-Host "  test:libs         Runs tests for TypeScript libs under libs/."
        Write-Host "  get-waspc-version Gets the current version of waspc from the Haskell project."
        Write-Host "  wasp-cli <args>   Runs the dev version of wasp executable while forwarding arguments."
        Write-Host "                    Builds the project (hs) first if needed. Doesn't require you to be in the waspc project to run it."
    }
}
