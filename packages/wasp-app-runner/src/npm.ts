import type { PathToApp } from "./args.js";
import { createLogger } from "./logging.js";
import { spawnAndCollectOutput } from "./process.js";

export async function npmInstall({
  pathToApp,
}: {
  pathToApp: PathToApp;
}): Promise<void> {
  const logger = createLogger("npm-install");
  const { stderrData, exitCode } = await spawnAndCollectOutput({
    name: "npm-install",
    cmd: "npm",
    args: ["install"],
    cwd: pathToApp,
  });

  if (exitCode !== 0) {
    logger.error(`Failed to install Wasp project dependencies: ${stderrData}`);
    process.exit(1);
  }
}
