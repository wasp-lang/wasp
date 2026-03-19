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
    const output = result.stderr + "\n" + result.stdout;
    throw new Error(parseWaspInfoError(output));
  }
}

const VERSION_MISMATCH_PATTERN =
  /Your Wasp version does not match the app's requirements\./;

/**
 * Parses the output of a failed `wasp info` command and returns a
 * user-friendly error message. If the output indicates a version mismatch,
 * the message will include the relevant details from the Wasp CLI.
 * Otherwise, a generic "invalid project" message is returned.
 */
export function parseWaspInfoError(output: string): string {
  if (VERSION_MISMATCH_PATTERN.test(output)) {
    const lines = output.split("\n").map((l) => l.trim()).filter(Boolean);
    const relevantLines = lines.filter(
      (line) =>
        line.startsWith("Your Wasp version") ||
        line.startsWith("You are running Wasp") ||
        line.startsWith("This app requires Wasp"),
    );
    return [
      "Wasp version mismatch detected.",
      ...relevantLines,
      "Please install the correct version of Wasp before deploying.",
    ].join("\n");
  }

  return [
    "The supplied Wasp directory does not appear to be a valid Wasp project.",
    "Please double check your Wasp project directory.",
  ].join("\n");
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
