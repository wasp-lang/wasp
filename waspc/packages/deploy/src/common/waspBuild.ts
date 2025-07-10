import { type ProcessOutput } from "zx";
import { WaspCliExe, WaspProjectDir } from "./brandedTypes.js";
import { waspSays } from "./terminal.js";
import { createCommandWithCwd } from "./zx.js";

export const ensureWaspProjectIsBuilt = createEnsureWaspProjectIsBuilt();

function createEnsureWaspProjectIsBuilt() {
  let waspBuildResult: Promise<ProcessOutput> | undefined = undefined;

  async function ensureWaspProjectIsBuilt({
    waspProjectDir,
    waspExe,
  }: {
    waspProjectDir: WaspProjectDir;
    waspExe: WaspCliExe;
  }): Promise<ProcessOutput> {
    // We want to build the Wasp project only once per CLI invocation.
    // Sometimes `ensureWaspProjectIsBuilt` is called multiple times
    // (e.g. in `setup` command, we call it and then again when deploying).
    if (waspBuildResult !== undefined) {
      return waspBuildResult;
    }

    waspSays("Building your Wasp app...");
    const waspCli = createCommandWithCwd(waspExe, waspProjectDir);
    waspBuildResult = waspCli(["build"]);
    return waspBuildResult;
  }

  return ensureWaspProjectIsBuilt;
}
