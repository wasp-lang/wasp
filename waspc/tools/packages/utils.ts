import { join } from "node:path";
import { getRepoRootPath, getWaspcDirPath } from "../utils.ts";

// The TypeScript packages that `waspc` runs as separate processes (see
// `Wasp.NodePackageFFI`, which names the two kinds). Their sources live in
// `<repo>/packages/`, and the build copies their artifacts into the Cabal data
// dir, where `waspc` expects them.
//
// - runnable: `waspc` runs it from the data dir and installs its dependencies
//   there with npm on first use, so it gets an npm lockfile.
// - installable: `waspc` copies it into the user's project, whose lockfile
//   covers it.
//
// TODO: Both kinds go away once these packages are dependencies of the Wasp
// npm package (#4434).
export const WASPC_PACKAGES: WaspcPackage[] = [
  { name: "spec", kind: "installable" },
  { name: "deploy", kind: "runnable" },
  { name: "studio", kind: "runnable" },
];

export type WaspcPackage = {
  name: string;
  kind: "runnable" | "installable";
};

export function getWaspcPackageDirPath(packageName: string): string {
  return join(getRepoRootPath(), "packages", packageName);
}

export function getDataPackagesDirPath(): string {
  return join(getWaspcDirPath(), "data", "packages");
}
