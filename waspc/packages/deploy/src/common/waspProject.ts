import fs from "fs";
import path from "node:path";

import { cd } from "zx";
import { WaspCliExe, WaspProjectDir } from "./brandedTypes.js";
import { waspSays } from "./terminal.js";
import { assertDirExists, assertDirPathIsAbsolute } from "./validation.js";
import { createCommandWithCwd } from "./zx.js";

export async function assertValidWaspProject(
  waspProjectDir: WaspProjectDir,
  waspExe: WaspCliExe,
): Promise<void> {
  const waspCli = createCommandWithCwd(waspExe, waspProjectDir);
  const result = await waspCli(["info"], {
    quiet: true,
    nothrow: true,
  });
  if (result.exitCode !== 0) {
    throw new Error(
      [
        "The supplied Wasp directory does not appear to be a valid Wasp project.",
        "Please double check your Wasp project directory.",
      ].join("\n"),
    );
  }
}

export function assertWaspProjectDirIsAbsoluteAndPresent(
  waspProjectDir: WaspProjectDir,
): void {
  const dirNameInError = "Wasp project directory";
  assertDirPathIsAbsolute(waspProjectDir, dirNameInError);
  assertDirExists(waspProjectDir, dirNameInError);
}

export async function ensureWaspProjectIsBuilt({
  waspProjectDir,
  waspExe,
}: {
  waspProjectDir: WaspProjectDir;
  waspExe: WaspCliExe;
}): Promise<void> {
  // NOTE: we assume that existance of the build directory means
  // that the project has been built.
  if (buildDirExists(waspProjectDir)) {
    return;
  }

  waspSays("Building your Wasp app...");
  const waspCli = createCommandWithCwd(waspExe, waspProjectDir);
  await waspCli(["build"]);
}

export function buildDirExists(waspProjectDir: WaspProjectDir): boolean {
  return fs.existsSync(getWaspBuildDir(waspProjectDir));
}

export function cdToServerBuildDir(waspProjectDir: WaspProjectDir): void {
  const serverBuildDir = getServerBuildDir(waspProjectDir);
  cd(serverBuildDir);
}

export function cdToClientBuildDir(waspProjectDir: WaspProjectDir): void {
  const clientBuildDir = getClientBuildDir(waspProjectDir);
  cd(clientBuildDir);
}

export function getServerBuildArtefactsDir(
  waspProjectDir: WaspProjectDir,
): string {
  return getServerBuildDir(waspProjectDir);
}

export function getClientBuildArtefactsDir(
  waspProjectDir: WaspProjectDir,
): string {
  const clientBuildDir = getClientBuildDir(waspProjectDir);
  return path.join(clientBuildDir, "build");
}

function getWaspBuildDir(waspProjectDir: WaspProjectDir): string {
  return path.join(waspProjectDir, ".wasp", "build");
}

export function getServerBuildDir(waspProjectDir: WaspProjectDir): string {
  // The server is built from the Wasp build directory.
  return path.join(getWaspBuildDir(waspProjectDir), ".");
}

export function getClientBuildDir(waspProjectDir: WaspProjectDir): string {
  return path.join(getWaspBuildDir(waspProjectDir), "web-app");
}
