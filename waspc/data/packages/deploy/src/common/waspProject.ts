import fs from "fs";
import path from "node:path";
import * as z from "zod";

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

export function getClientBuildArtefactsDir(
  waspProjectDir: WaspProjectDir,
): string {
  return path.join(getClientDeploymentDir(waspProjectDir), "build");
}

function getWaspBuildDir(waspProjectDir: WaspProjectDir): string {
  return path.join(waspProjectDir, ".wasp", "out");
}

export function getWaspInfoPath(waspProjectDir: WaspProjectDir): string {
  return path.join(getWaspBuildDir(waspProjectDir), ".waspinfo");
}

const waspInfoSchema = z.object({
  // "single": the server serves the web client, one app/service to deploy.
  // "split": the client is a separate static app/service, the server is CORS'd.
  deploymentMode: z.enum(["single", "split"]).optional(),
});

export type WaspInfo = z.infer<typeof waspInfoSchema>;

export type DeploymentMode = NonNullable<WaspInfo["deploymentMode"]>;

// `wasp build` writes `.wasp/out/.waspinfo`, so call this only after the
// project has been built.
export function readWaspInfo(waspProjectDir: WaspProjectDir): WaspInfo {
  const waspInfoPath = getWaspInfoPath(waspProjectDir);

  let contents: string;
  try {
    contents = fs.readFileSync(waspInfoPath, "utf8");
  } catch {
    throw new Error(
      `Could not read ${waspInfoPath}. Run \`wasp build\` and retry.`,
    );
  }

  try {
    return waspInfoSchema.parse(JSON.parse(contents));
  } catch (error) {
    throw new Error(
      `${waspInfoPath} is not a valid .waspinfo file (${describeError(error)}). ` +
        "Run `wasp build` with the current Wasp CLI and retry.",
    );
  }
}

function describeError(error: unknown): string {
  if (error instanceof z.ZodError) {
    return z.prettifyError(error);
  }
  return error instanceof Error ? error.message : String(error);
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
