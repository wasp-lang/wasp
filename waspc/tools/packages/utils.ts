import { join } from "node:path";
import { getRepoRootPath, getWaspcDirPath } from "../utils.ts";

// The TypeScript packages that `waspc` runs as separate processes (see
// `Wasp.NodePackageFFI`). Their sources live in `<repo>/packages/`, and the
// build copies their artifacts into the Cabal data dir, where `waspc` expects them.
const WASPC_PACKAGE_NAMES = ["deploy", "prisma", "spec", "studio"];

export function getWaspcPackageDirPaths(): string[] {
  const packagesDirPath = join(getRepoRootPath(), "packages");
  return WASPC_PACKAGE_NAMES.map((name) => join(packagesDirPath, name));
}

export function getDataPackagesDirPath(): string {
  return join(getWaspcDirPath(), "data", "packages");
}
