import { basename, join } from "node:path";
import { discoverSubDirs, getWaspcDirPath } from "../utils.ts";

const SKELETON_DIR_NAME = "skeleton";

export function getStartersDirPath(): string {
  const waspcDirPath = getWaspcDirPath();
  return join(waspcDirPath, "data", "Cli", "starters");
}

export function getSkeletonDirPath(): string {
  return join(getStartersDirPath(), SKELETON_DIR_NAME);
}

export function getBundledStarterDirPaths(): string[] {
  return discoverSubDirs(getStartersDirPath()).filter(
    (dirPath) => basename(dirPath) !== SKELETON_DIR_NAME,
  );
}
