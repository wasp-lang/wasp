// @ts-check

// The dev stand-in for the platform packages of `@wasp/cli`. Its default
// export must match `SubPackageAPI` in `packages/cli/src/package-data.ts`.

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
 * Reads it from the `@wasp/cli` override, which names the dev `@wasp/cli`
 * tarball whether `@wasp/wasp` is a regular or a dev dependency.
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
