import { join } from "node:path";
import { getRepoRootPath, getWaspcDirPath, runCmd } from "../utils.ts";

export function getLibsDirPath(): string {
  return join(getRepoRootPath(), "packages", "libs");
}

export function getDataLibsDirPath(): string {
  return join(getWaspcDirPath(), "data", "Generator", "libs");
}

export function runScriptInEveryLib(scriptName: "build" | "test"): void {
  runCmd(
    "pnpm",
    ["--filter", "./packages/libs/*", "--fail-if-no-match", "run", scriptName],
    { cwd: getRepoRootPath(), stdio: "inherit" },
  );
}
