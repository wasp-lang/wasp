import { join } from "node:path";
import { getRepoRootPath, getWaspcDirPath } from "../utils.ts";

// The TypeScript packages that `waspc` runs as separate processes (see
// `Wasp.NodePackageFFI`). Their sources live in `<repo>/packages/`, and the
// build copies their artifacts into the Cabal data dir, where `waspc` expects them.
// The npm ones ship with their package-lock.json, which waspc installs on
// first use. TODO: Move them to the workspace once they are part of the Wasp
// npm package.
export const WASPC_PACKAGES: WaspcPackage[] = [
  { name: "spec", packageManager: "pnpm" },
  { name: "deploy", packageManager: "npm" },
  { name: "studio", packageManager: "npm" },
];

export type WaspcPackage = {
  name: string;
  packageManager: "pnpm" | "npm";
};

// Packages published to npm at the Wasp version but not shipped in the data
// dir, the CLI wrapper and the `@wasp/wasp` project dependency.
export const PUBLISHED_WASP_PACKAGE_NAMES = ["cli", "wasp"];

export function getPackageDirPath(packageName: string): string {
  return join(getRepoRootPath(), "packages", packageName);
}

export function getDataPackagesDirPath(): string {
  return join(getWaspcDirPath(), "data", "packages");
}
