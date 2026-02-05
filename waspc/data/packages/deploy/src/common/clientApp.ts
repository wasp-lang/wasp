import fs from "fs";
import path from "node:path";

import { WaspProjectDir } from "./brandedTypes.js";
import { waspSays } from "./terminal.js";
import {
  getClientBuildArtefactsDir,
  getClientBuildDir,
  getClientDeploymentDir,
} from "./waspProject.js";
import { createCommandWithCwd } from "./zx.js";

const serverUrlEnvVarName = "REACT_APP_API_URL";

// SSR-related constants
// Must match the values in waspc Haskell code (WebAppGenerator.hs, VitePlugin/Common.hs)
const ssrServerFileName = "server-ssr.mjs";
const ssrEntryPointPath = "/@wasp/entry-server.tsx";
const ssrBuildOutDir = ".wasp/out/web-app/build-ssr";

export async function buildClient(
  serverUrl: string,
  { waspProjectDir }: { waspProjectDir: WaspProjectDir },
): Promise<string> {
  waspSays("Building web client for production...");

  const clientBuildDir = getClientBuildDir(waspProjectDir);
  const npmCli = createCommandWithCwd("npm", clientBuildDir);
  const npxCli = createCommandWithCwd("npx", clientBuildDir);

  const buildEnv = {
    env: {
      ...process.env,
      [serverUrlEnvVarName]: serverUrl,
    },
  };

  await npmCli(["install"]);

  // Run the standard client build
  await npxCli(["vite", "build"], buildEnv);

  // Check if SSR is enabled by looking for SSR server file
  const ssrEnabled = isSsrEnabled(waspProjectDir);

  if (ssrEnabled) {
    waspSays("SSR is enabled. Building SSR entry point...");
    await npxCli(
      ["vite", "build", "--ssr", ssrEntryPointPath, "--outDir", ssrBuildOutDir],
      buildEnv,
    );
  }

  return getClientBuildArtefactsDir(waspProjectDir);
}

/**
 * Checks if SSR is enabled by looking for the SSR server file in the web-app output.
 */
export function isSsrEnabled(waspProjectDir: WaspProjectDir): boolean {
  const clientDeploymentDir = getClientDeploymentDir(waspProjectDir);
  const ssrServerPath = path.join(clientDeploymentDir, ssrServerFileName);
  return fs.existsSync(ssrServerPath);
}

/**
 * Returns the SSR server file name for use in deployment.
 */
export function getSsrServerFileName(): string {
  return ssrServerFileName;
}
