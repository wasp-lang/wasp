import fs from "fs";
import path from "node:path";

import { WaspCliExe, WaspProjectDir } from "./brandedTypes.js";
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

export function buildDirExists(waspProjectDir: WaspProjectDir): boolean {
  return fs.existsSync(getWaspBuildDir(waspProjectDir));
}

export function getServerBuildArtefactsDir(
  waspProjectDir: WaspProjectDir,
): string {
  return getServerDeploymentDir(waspProjectDir);
}

export function getClientBuildArtefactsDir(
  waspProjectDir: WaspProjectDir,
): string {
  return path.join(getClientDeploymentDir(waspProjectDir), "build");
}

function getWaspBuildDir(waspProjectDir: WaspProjectDir): string {
  return path.join(waspProjectDir, ".wasp", "out");
}

export function getServerDeploymentDir(waspProjectDir: WaspProjectDir): string {
  // The server is built from the Wasp out directory.
  return path.join(getWaspBuildDir(waspProjectDir), ".");
}

export function getClientDeploymentDir(waspProjectDir: WaspProjectDir): string {
  // The client is deployed from the `.out/web-app` dir.
  return path.join(getWaspBuildDir(waspProjectDir), "web-app");
}

export function getClientBuildDir(waspProjectDir: WaspProjectDir): string {
  // The client is built from the project root dir.
  return path.join(waspProjectDir, ".");
}
