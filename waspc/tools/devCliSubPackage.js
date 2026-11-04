// @ts-check

// The dev stand-in for the platform packages of `@wasp/cli`, which
// `tasks/build/dev-packages.ts` packs into the dev `@wasp/cli`. It finds the
// project it is installed in (the one whose package.json points at the dev
// `@wasp/wasp` tarball), and reads the binary and data dir paths from the
// config file next to that tarball.
// Its default export must match `SubPackageAPI` in
// `scripts/make-cli-npm-packages/src/schema/output-data.ts`, which `bin.js` reads.

import { existsSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

// Keep in sync with `DEV_CLI_CONFIG_FILE_NAME` in `tasks/build/dev-packages.ts`.
const DEV_CLI_CONFIG_FILE_NAME = "dev-cli.json";

/** @type {{ waspBinPath: string, dataDirPath: string }} */
const devCliConfig = JSON.parse(
  readFileSync(join(findDevPackagesDir(), DEV_CLI_CONFIG_FILE_NAME), "utf8"),
);

export default devCliConfig;

/**
 * Finds the dir of the dev tarballs, from the `@wasp/cli` override that
 * `wasp new` and the repo's apps write next to the `@wasp/wasp` dependency.
 * The override names the dev `@wasp/cli` tarball itself, so it's there whether
 * `@wasp/wasp` is a regular or a dev dependency.
 *
 * @returns {string}
 */
function findDevPackagesDir() {
  let dir = dirname(fileURLToPath(import.meta.url));
  while (true) {
    const packageJsonPath = join(dir, "package.json");
    if (existsSync(packageJsonPath)) {
      const packageJson = JSON.parse(readFileSync(packageJsonPath, "utf8"));
      const specifier = packageJson.overrides?.["@wasp/cli"];
      if (typeof specifier === "string" && specifier.startsWith("file:")) {
        return dirname(resolve(dir, specifier.slice("file:".length)));
      }
    }
    const parentDir = dirname(dir);
    if (parentDir === dir) {
      throw new Error(
        "This is a development build of @wasp/cli, but no package.json above it overrides @wasp/cli with the dev tarball.",
      );
    }
    dir = parentDir;
  }
}
