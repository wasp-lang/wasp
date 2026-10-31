import { join } from "node:path";
import { getRepoRootPath, getWaspcDirPath } from "../utils.ts";

// The TypeScript packages that `waspc` runs as separate processes (see
// `Wasp.NodePackageFFI`). Their sources live in `<repo>/packages/`, and the
// build copies their artifacts into the Cabal data dir, where `waspc` runs them
// and installs their dependencies with npm on first use, so each gets an npm
// lockfile.
//
// TODO: These go away once they are dependencies of the Wasp npm package (#4434).
export const WASPC_PACKAGES: WaspcPackage[] = [
  { name: "deploy" },
  { name: "studio" },
];

export type WaspcPackage = {
  name: string;
};

// The packages a Wasp project depends on through `@wasp/wasp`. They are
// published with each release, and packed from this repo for development (see
// `tools/dev-packages`).
export const WASP_NPM_PACKAGE_NAMES = ["wasp", "cli", "spec"];

export function getPackageDirPath(packageName: string): string {
  return join(getRepoRootPath(), "packages", packageName);
}

export function getDataPackagesDirPath(): string {
  return join(getWaspcDirPath(), "data", "packages");
}
