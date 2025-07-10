import { WaspCliExe, WaspProjectDir } from "./brandedTypes.js";
import { waspSays } from "./terminal.js";
import { createCommandWithCwd } from "./zx.js";

export const ensureWaspProjectIsBuilt = createEnsureWaspProjectIsBuilt();

function createEnsureWaspProjectIsBuilt() {
  // We want to build the Wasp project only once per CLI invocation.
  // Sometimes `ensureWaspProjectIsBuilt` is called multiple times
  // (e.g. in `setup` command, we call it and then again when deploying).
  let isWaspBuildExecuted = false;

  async function ensureWaspProjectIsBuilt({
    waspProjectDir,
    waspExe,
  }: {
    waspProjectDir: WaspProjectDir;
    waspExe: WaspCliExe;
  }): Promise<void> {
    if (isWaspBuildExecuted) {
      return;
    }

    waspSays("Building your Wasp app...");
    const waspCli = createCommandWithCwd(waspExe, waspProjectDir);
    await waspCli(["build"]);
    isWaspBuildExecuted = true;
  }

  return ensureWaspProjectIsBuilt;
}
