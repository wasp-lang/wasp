import { existsSync, mkdirSync, readdirSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

// NOTE: These paths are also defined in:
// - https://github.com/wasp-lang/wasp/blob/main/waspc/cli/src/Wasp/Cli/FileSystem.hs (waspInstallationDirInHomeDir)
// - https://github.com/wasp-lang/get-wasp-sh/blob/master/installer.sh (WASP_LANG_DIR, NPM_MARKER_FILE)
const WASP_LANG_DIR = join(homedir(), ".local", "share", "wasp-lang");
const NPM_MARKER_FILE = join(WASP_LANG_DIR, ".uses-npm");

main();

function main() {
  // Skip checks on CI environments
  if (isCI()) {
    ensureNpmMarker();
    return;
  }

  // Check if installer Wasp exists without npm marker
  if (hasInstallerWasp() && !hasNpmMarker()) {
    printInstallerConflictError();
    process.exit(1);
  }

  // Ensure the npm marker exists
  ensureNpmMarker();
}

/**
 * Check if installer-based Wasp is present.
 * We detect this by checking if the wasp-lang directory exists and contains
 * subdirectories (which would be version installations).
 */
function hasInstallerWasp() {
  if (!existsSync(WASP_LANG_DIR)) {
    return false;
  }

  try {
    const entries = readdirSync(WASP_LANG_DIR, { withFileTypes: true });
    return entries.some((entry) => entry.isDirectory());
  } catch {
    return false;
  }
}

/**
 * Check if the npm marker file exists.
 */
function hasNpmMarker() {
  return existsSync(NPM_MARKER_FILE);
}

/**
 * Ensure the npm marker file exists.
 * This marks that the user is now using npm-based Wasp.
 */
function ensureNpmMarker() {
  try {
    if (!existsSync(WASP_LANG_DIR)) {
      mkdirSync(WASP_LANG_DIR, { recursive: true });
    }
    if (!existsSync(NPM_MARKER_FILE)) {
      // Use 'wx' flag for atomic creation (fails if file exists)
      writeFileSync(NPM_MARKER_FILE, "", { flag: "wx" });
    }
  } catch {
    // Ignore errors - marker file is best-effort
    // The directory might not be writable, or another process might have created it
  }
}

/**
 * Print an error message explaining the conflict.
 */
function printInstallerConflictError() {
  console.error(`
Error: Detected an existing installer-based Wasp installation.

You have Wasp installed via the curl installer script. To prevent conflicts,
please migrate to npm before installing the npm package.

To migrate, run:
  curl -sSL https://get.wasp.sh/installer.sh | sh -s migrate-to-npm

This will:
  1. Remove the installer-based Wasp
  2. Preserve your cache for telemetry continuity
  3. Allow you to install via npm

After migrating, retry: npm install -g @wasp.sh/wasp-cli
`);
}

/**
 * Check if running in a CI environment.
 */
function isCI() {
  // Keep in sync with the same list in:
  // - https://github.com/wasp-lang/wasp/blob/main/waspc/src/Wasp/Util.hs
  // - https://github.com/wasp-lang/get-wasp-sh/blob/master/installer.sh
  const CI_ENV_VARS = [
    "BUILD_ID",
    "BUILD_NUMBER",
    "CI",
    "CI_APP_ID",
    "CI_BUILD_ID",
    "CI_BUILD_NUMBER",
    "CI_NAME",
    "CONTINUOUS_INTEGRATION",
    "GITHUB_ACTIONS",
    "RUN_ID",
    "TRAVIS",
  ];

  return CI_ENV_VARS.some((envVar) => process.env[envVar]);
}
