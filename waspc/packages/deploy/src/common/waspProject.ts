import fs from "fs";
import path from "node:path";

import { exit } from "process";
import { cd } from "zx";
import { createCommandWithDirectory } from "./cli.js";
import { WaspCliExe, WaspProjectDir } from "./cliArgs.js";
import { waspSays } from "./terminal.js";

export async function assertValidWaspProject(
  waspProjectDir: WaspProjectDir,
  waspExe: WaspCliExe,
): Promise<void> {
  try {
    const waspCli = createCommandWithDirectory(waspExe, waspProjectDir);
    await waspCli(["info"], {
      quiet: true,
    });
  } catch {
    waspSays(
      "The supplied Wasp directory does not appear to be a valid Wasp project.",
    );
    waspSays("Please double check your Wasp project directory.");
    exit(1);
  }
}

export function assertWaspProjectDirIsAbsoluteAndPresent(
  waspProjectDir: WaspProjectDir,
): void {
  if (!path.isAbsolute(waspProjectDir)) {
    waspSays("The supplied Wasp directory path must be absolute.");
    exit(1);
  }

  const waspProjectDirExists = fs.existsSync(waspProjectDir);
  if (!waspProjectDirExists) {
    waspSays("The Wasp directory does not exist.");
    exit(1);
  }
}

export function buildDirExists(waspProjectDir: WaspProjectDir): boolean {
  const waspBuildDir = getWaspBuildDir(waspProjectDir);
  return fs.existsSync(waspBuildDir);
}

export function cdToServerBuildDir(waspProjectDir: WaspProjectDir): void {
  const serverBuildDir = getServerBuildDir(waspProjectDir);
  cd(serverBuildDir);
}

export function cdToClientBuildDir(waspProjectDir: WaspProjectDir): void {
  const webAppBuildDir = getClientBuildDir(waspProjectDir);
  cd(webAppBuildDir);
}

export function getServerArtefactsDir(waspProjectDir: WaspProjectDir): string {
  return getServerBuildDir(waspProjectDir);
}

export function getWebAppArtefactsDir(waspProjectDir: WaspProjectDir): string {
  const webAppBuildDir = getClientBuildDir(waspProjectDir);
  return path.join(webAppBuildDir, "build");
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
