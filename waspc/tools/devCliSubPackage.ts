// TODO(#2262): Once the repo's Wasp apps join the pnpm workspace, they run the
// workspace's `@wasp/cli`, which has no `overrides` to find the dev packages
// through. Point it at the dev binary directly then.
//
// The dev stand-in for the platform packages of `@wasp/cli`.
//
// `tasks/build/dev-packages.ts` strips the types and puts the result into the
// dev `@wasp/cli`, so this file can only import types and Node.js built-ins.

import { existsSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import type { CliSubPackageAPI } from "../../packages/cli/src/package-data.ts";

// Keep in sync with `DEV_PACKAGES_MANIFEST_FILE_NAME` in `tasks/build/dev-packages.ts`.
const DEV_PACKAGES_MANIFEST_FILE_NAME = "dev-packages.json";

const devPackagesDir = findDevPackagesDir();

// `@wasp/cli` passes its environment on to the Wasp binary, which uses the dev
// packages only with this variable set (see `Wasp.Cli.DevWaspNpmPackages`).
process.env.WASP_DEV_PACKAGES_DIR ??= devPackagesDir;

const { waspBinPath, dataDirPath }: CliSubPackageAPI = JSON.parse(
  readFileSync(join(devPackagesDir, DEV_PACKAGES_MANIFEST_FILE_NAME), "utf8"),
);

export default { waspBinPath, dataDirPath } satisfies CliSubPackageAPI;

/**
 * Reads it from the `@wasp/cli` override, which names the dev `@wasp/cli`
 * tarball whether `@wasp/wasp` is a regular or a dev dependency.
 */
function findDevPackagesDir(): string {
  let dir = import.meta.dirname;
  while (true) {
    const packageJsonPath = join(dir, "package.json");
    if (existsSync(packageJsonPath)) {
      const packageJson: { overrides?: Record<string, unknown> } = JSON.parse(
        readFileSync(packageJsonPath, "utf8"),
      );
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
