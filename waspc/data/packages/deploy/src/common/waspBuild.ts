import { WaspCliExe, WaspProjectDir } from "./brandedTypes.js";
import { waspSays } from "./terminal.js";
import {
  DeploymentMode,
  getWaspInfoPath,
  readWaspInfo,
} from "./waspProject.js";
import { createCommandWithCwd } from "./zx.js";

export const ensureWaspProjectIsBuilt = createEnsureWaspProjectIsBuilt();

function createEnsureWaspProjectIsBuilt() {
  async function buildWaspApp({
    waspProjectDir,
    waspExe,
  }: {
    waspProjectDir: WaspProjectDir;
    waspExe: WaspCliExe;
  }): Promise<{ deploymentMode: DeploymentMode }> {
    waspSays("Building your Wasp app...");
    const waspCli = createCommandWithCwd(waspExe, waspProjectDir);
    await waspCli(["build"]);
    return { deploymentMode: getDeploymentMode(waspProjectDir) };
  }

  type BuildWaspApp = typeof buildWaspApp;

  // We want to build the Wasp project only once per CLI invocation.
  // Sometimes `ensureWaspProjectIsBuilt` is called multiple times
  // (e.g. in `setup` command, we call it and then again when deploying).
  let cachedWaspBuildResult: ReturnType<BuildWaspApp> | undefined = undefined;
  return (...params: Parameters<BuildWaspApp>): ReturnType<BuildWaspApp> => {
    if (cachedWaspBuildResult === undefined) {
      cachedWaspBuildResult = buildWaspApp(...params);
    }
    return cachedWaspBuildResult;
  };
}

// `wasp build` records the mode in `.wasp/out/.waspinfo`.
function getDeploymentMode(waspProjectDir: WaspProjectDir): DeploymentMode {
  const { deploymentMode } = readWaspInfo(waspProjectDir);

  if (deploymentMode === undefined) {
    waspSays(
      `Warning: ${getWaspInfoPath(waspProjectDir)} has no "deploymentMode" field. ` +
        "The project was probably built with an older Wasp CLI. Assuming split mode.",
    );
    return "split";
  }

  return deploymentMode;
}
