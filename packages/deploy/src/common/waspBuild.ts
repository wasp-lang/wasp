import { type ProcessOutput } from "zx";
import { WaspCliExe, WaspProjectDir } from "./brandedTypes.js";
import { waspSays } from "./terminal.js";
import { createCommandWithCwd } from "./zx.js";

export const ensureWaspProjectIsBuilt = createEnsureWaspProjectIsBuilt();

function createEnsureWaspProjectIsBuilt() {
  async function buildWaspApp({
    waspProjectDir,
    waspExe,
  }: {
    waspProjectDir: WaspProjectDir;
    waspExe: WaspCliExe;
  }): Promise<ProcessOutput> {
    waspSays("Building your Wasp app...");
    const waspCli = createCommandWithCwd(waspExe, waspProjectDir);
    return waspCli(["build"]);
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
