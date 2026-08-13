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
  const result = await waspCli(["show", "spec", "--json"], {
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

function getWaspBuildDir(waspProjectDir: WaspProjectDir): string {
  return path.join(waspProjectDir, ".wasp", "out");
}

export function getServerDeploymentDir(waspProjectDir: WaspProjectDir): string {
  // The server is built from the Wasp out directory.
  return path.join(getWaspBuildDir(waspProjectDir), ".");
}

export function getClientDeploymentDir(waspProjectDir: WaspProjectDir): string {
  // Where the client used to be deployed from, back when it was an app of its
  // own. Only `wasp deploy fly cmd --context client` still reaches for it.
  return path.join(getWaspBuildDir(waspProjectDir), "web-app");
}
