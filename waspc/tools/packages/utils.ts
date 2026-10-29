import { join } from "node:path";
import { getRepoRootPath, getWaspcDirPath } from "../utils.ts";

// The TypeScript packages that `waspc` runs as separate processes (see
// `Wasp.NodePackageFFI`). Their sources live in `<repo>/packages/`, and the
// build copies their artifacts into the Cabal data dir, where `waspc` expects them.
//
// `spec` is in the repo's pnpm workspace. The other three are npm packages,
// `waspc` ships them with their own `package-lock.json` and installs them
// with npm on first use.
// TODO: Move them to the workspace once they are part of the Wasp npm package.
export const WASPC_PACKAGES: WaspcPackage[] = [
  { name: "spec", packageManager: "pnpm" },
  { name: "deploy", packageManager: "npm" },
  { name: "prisma", packageManager: "npm" },
  { name: "studio", packageManager: "npm" },
];

export type WaspcPackage = {
  name: string;
  packageManager: "pnpm" | "npm";
};

export function getWaspcPackageDirPath(packageName: string): string {
  return join(getRepoRootPath(), "packages", packageName);
}

export function getDataPackagesDirPath(): string {
  return join(getWaspcDirPath(), "data", "packages");
}
