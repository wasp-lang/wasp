import * as fs from "node:fs";
import * as path from "node:path";
import type { PathToApp } from "./args.js";
import { createLogger } from "./logging.js";
import { spawnAndCollectOutput } from "./process.js";

type PackageManager = "npm" | "pnpm" | "bun";

/**
 * The files that tell a dir is installed with a package manager. A project
 * that moved off npm often still has npm's lockfile around, so npm comes last.
 *
 * Wasp finds a project's package manager the same way, see
 * `Wasp.Project.PackageManager` in waspc.
 */
const markerFileNamesByPackageManager: [PackageManager, string[]][] = [
  ["pnpm", ["pnpm-lock.yaml", "pnpm-workspace.yaml"]],
  ["bun", ["bun.lock", "bun.lockb"]],
  ["npm", ["package-lock.json"]],
];

export async function installDependencies({
  pathToApp,
}: {
  pathToApp: PathToApp;
}): Promise<void> {
  const packageManager = detectPackageManager(pathToApp);
  const logger = createLogger(`${packageManager}-install`);
  const { stderrData, exitCode } = await spawnAndCollectOutput({
    name: `${packageManager}-install`,
    cmd: packageManager,
    args: ["install"],
    cwd: pathToApp,
  });

  if (exitCode !== 0) {
    logger.error(`Failed to install Wasp project dependencies: ${stderrData}`);
    process.exit(1);
  }
}

/**
 * Goes by the nearest dir with a package manager's file. For a project in a
 * workspace that is the workspace root.
 */
function detectPackageManager(pathToApp: string): PackageManager {
  let dir = path.resolve(pathToApp);
  while (true) {
    for (const [
      packageManager,
      markerFileNames,
    ] of markerFileNamesByPackageManager) {
      if (markerFileNames.some((name) => fs.existsSync(path.join(dir, name)))) {
        return packageManager;
      }
    }
    const parentDir = path.dirname(dir);
    if (parentDir === dir) {
      return "npm";
    }
    dir = parentDir;
  }
}
