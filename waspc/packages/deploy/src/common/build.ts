import { createCommandWithDirectory } from "./cli.js";
import { waspSays } from "./output.js";

export function makeIdempotentWaspBuild(options: {
  // NOTE: Not using branded types here becuase we still need to migrate
  // the Fly provider to use WaspProjectDir
  waspProjectDir: string;
  waspExe: string;
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
