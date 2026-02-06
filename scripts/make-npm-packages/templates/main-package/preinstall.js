import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

// These paths are also defined in:
// - https://github.com/wasp-lang/wasp/blob/main/waspc/cli/src/Wasp/Cli/FileSystem.hs
// - https://github.com/wasp-lang/get-wasp-sh/blob/master/installer.sh
// TODO: Do not hardcode: https://github.com/wasp-lang/wasp/issues/980
const WASP_LANG_DIR = path.join(os.homedir(), ".local", "share", "wasp-lang");
const NPM_MARKER_FILE = path.join(WASP_LANG_DIR, ".uses-npm");

main();

function main() {
  if (hasInstallerWasp() && !hasNpmMarker()) {
    printInstallerConflictError();
    process.exitCode = 1;
    return;
  }

  ensureNpmMarker();
}

function hasInstallerWasp() {
  try {
    // If the folder exists and has any subdirectories (for the installed version(s)),
    // we assume it's an installer-based Wasp installation.
    const entries = fs.readdirSync(WASP_LANG_DIR, { withFileTypes: true });
    return entries.some((entry) => entry.isDirectory());
  } catch {
    return false;
  }
}

function hasNpmMarker() {
  return fs.existsSync(NPM_MARKER_FILE);
}

function ensureNpmMarker() {
  try {
    fs.mkdirSync(WASP_LANG_DIR, { recursive: true });
    fs.writeFileSync(NPM_MARKER_FILE, "", {
      // We open the file in appending mode, so if we store some data in
      // the future, we won't overwrite it.
      flag: "a",
    });
  } catch {
    // Ignore errors - marker file is best-effort
  }
}

function printInstallerConflictError() {
  console.error(`
Error: Detected an existing installer-based Wasp installation.

You have Wasp installed via the curl installer script. To prevent conflicts,
please migrate to npm before installing the npm package.

To migrate, run:

  curl -sSL https://get.wasp.sh/installer.sh | sh -s -- migrate-to-npm

and follow the instructions.
`);
}
