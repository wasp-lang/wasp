# The script duplicates some part of logics from run shell script.
# We plan to merge these two scripts into one platform-agnostic script in the future to avoid duplication.
# Requires PowerShell 5.1 or higher.

param(
    [string]$Command = ""
)

$PROJECT_ROOT = Split-Path -Parent $MyInvocation.MyCommand.Path
$REPOSITORY_ROOT = Split-Path -Parent $PROJECT_ROOT
    
# Building
$WASP_PACKAGES_COMPILE = Join-Path $PROJECT_ROOT "tools\install_packages_to_data_dir.ps1"
$BUILD_HS_CMD = "cabal build all"
$BUILD_ALL_CMD = "$WASP_PACKAGES_COMPILE -and $BUILD_HS_CMD"
$RUN_CMD="cabal --project-dir=${PROJECT_ROOT} run wasp-cli -- $Args"

switch ($Command) {
    "build" {
        Invoke-Expression $BUILD_HS_CMD
    }
    "build:all" {
        Invoke-Expression $BUILD_ALL_CMD
    }
    "wasp-cli" {
        Invoke-Expression $RUN_CMD
    }
    Default {
        Write-Host "USAGE"
        Write-Host "  run <command>"
        Write-Host ""
        Write-Host "COMMANDS"
        Write-Host "  build             Builds the Haskell project."
        Write-Host "  build:all         Builds the Haskell project + all sub-projects (i.e. TS packages)."
        Write-Host "  wasp-cli <args>   Runs the dev version of wasp executable while forwarding arguments."
        Write-Host "                    Builds the project (hs) first if needed. Doesn't require you to be in the waspc project to run it."
    }
}
