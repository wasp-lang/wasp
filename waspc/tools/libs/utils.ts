import { join } from "node:path";
import { getRepoRootPath, getWaspcDirPath, runCmd } from "../utils.ts";

export function getLibsDirPath(): string {
  return join(getRepoRootPath(), "packages", "libs");
}

export function getDataLibsDirPath(): string {
  return join(getWaspcDirPath(), "data", "Generator", "libs");
}

// One pnpm invocation runs the script in every lib, in dependency order. The
// filter is a path relative to the workspace root, and `runCmd` goes through
// a shell, so the glob is quoted for it.
export function runScriptInEveryLib(scriptName: "build" | "test"): void {
  runCmd("pnpm", ["--filter", "'./packages/libs/*'", "run", scriptName], {
    cwd: getRepoRootPath(),
    stdio: "inherit",
  });
}
