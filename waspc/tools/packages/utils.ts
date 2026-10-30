import { join } from "node:path";
import { getRepoRootPath, getWaspcDirPath } from "../utils.ts";

// The TypeScript packages that `waspc` runs as separate processes (see
// `Wasp.NodePackageFFI`). Their sources live in `<repo>/packages/`, and the
// build copies their artifacts into the Cabal data dir, where `waspc` expects them.
// `waspc` installs the dependencies of the ones it runs from the data dir with
// npm on first use, so those get a generated `package-lock.json` there.
export const WASPC_PACKAGES: WaspcPackage[] = [
  { name: "spec", installedByWaspc: false },
  { name: "deploy", installedByWaspc: true },
  { name: "studio", installedByWaspc: true },
];

export type WaspcPackage = {
  name: string;
  installedByWaspc: boolean;
};

export function getWaspcPackageDirPath(packageName: string): string {
  return join(getRepoRootPath(), "packages", packageName);
}

export function getDataPackagesDirPath(): string {
  return join(getWaspcDirPath(), "data", "packages");
}
