// Finds the Wasp binary for a `@wasp/cli` that runs from this repo's sources.
//
// The repo's example apps get `@wasp/cli` through the pnpm workspace. That
// package wasn't built for publishing, so it has no `data.json` and no
// platform packages. `mise run build:dev-packages` records where the binary
// built in the repo is.

import * as fs from "node:fs";
import * as path from "node:path";
import { CLIError } from "./cli-error.js";
import type { CliSubPackageAPI } from "./package-data.js";

// Keep in sync with `tasks/build/dev-packages.ts`.
const devPackagesDirInRepo = path.join("waspc", ".dev-packages");
const devPackagesManifestFileName = "dev-packages.json";

export function getRepoDevSubPackage(packageRootDir: string): CliSubPackageAPI {
  // pnpm can put a copy of the package into `node_modules` at the repo root.
  const repoRootDir = findRepoRootDir(fs.realpathSync(packageRootDir));
  if (repoRootDir === undefined) {
    throw new CLIError(
      "This @wasp/cli has no `data.json` and isn't inside the Wasp repo, so it can't find a Wasp executable.",
    );
  }

  const devPackagesDir = path.join(repoRootDir, devPackagesDirInRepo);
  const manifestPath = path.join(devPackagesDir, devPackagesManifestFileName);
  if (!fs.existsSync(manifestPath)) {
    throw new CLIError(
      `Couldn't find ${manifestPath}. Run \`mise run build\` first.`,
    );
  }
  const { waspBinPath, dataDirPath }: CliSubPackageAPI = JSON.parse(
    fs.readFileSync(manifestPath, "utf8"),
  );

  // The Wasp binary uses the dev packages only with this variable set (see
  // `Wasp.Cli.DevWaspNpmPackages`), e.g. to build a Docker image of an app
  // that depends on `@wasp/wasp` through `workspace:`.
  process.env.WASP_DEV_PACKAGES_DIR ??= devPackagesDir;

  return { waspBinPath, dataDirPath };
}

function findRepoRootDir(fromDir: string): string | undefined {
  const isRepoRootDir = fs.existsSync(
    path.join(fromDir, "waspc", "waspc.cabal"),
  );
  if (isRepoRootDir) {
    return fromDir;
  }
  const parentDir = path.dirname(fromDir);
  return parentDir === fromDir ? undefined : findRepoRootDir(parentDir);
}
