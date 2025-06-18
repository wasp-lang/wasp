import { createCommandWithDirectory } from "./cli.js";
import { WaspCliExe, WaspProjectDir } from "./cliArgs.js";
import { waspSays } from "./terminal.js";

export function makeIdempotentWaspBuildCommand(options: {
  waspProjectDir: WaspProjectDir;
  waspExe: WaspCliExe;
  skipBuild?: boolean;
}): () => Promise<void> {
  const buildWasp = async () => {
    if (options.skipBuild) {
      return;
    }

    waspSays("Building your Wasp app...");

    const waspCli = createCommandWithDirectory(
      options.waspExe,
      options.waspProjectDir,
    );
    await waspCli(["build"]);
  };

  return makeIdempotent(buildWasp);
}

function makeIdempotent<F extends () => any>(fn: F): () => ReturnType<F> {
  let result: { value: ReturnType<F> } | null = null;

  return function idempotentFn() {
    if (!result) {
      result = { value: fn() };
    }
    return result.value;
  };
}
