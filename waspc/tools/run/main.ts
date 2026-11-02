/// <reference types="node" />
// Uses only Node built-ins, so it can install the tools' dependencies before
// loading the modules that need them.

import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { fileURLToPath } from "node:url";

installToolsDependenciesIfMissing();
const { runCli } = await import("./cli.ts");
await runCli(process.argv.slice(2));

function installToolsDependenciesIfMissing(): void {
  const toolsNodeModulesDir = fileURLToPath(
    new URL("../node_modules", import.meta.url),
  );
  if (existsSync(toolsNodeModulesDir)) {
    return;
  }

  // Not `getRepoRootPath()` from utils.ts, which needs the dependencies.
  const repoRootDir = fileURLToPath(new URL("../../..", import.meta.url));
  // pnpm's output goes to stderr so commands like `get-waspc-version` keep a
  // clean stdout. The shell resolves `pnpm.cmd` on Windows.
  const { status } = spawnSync("pnpm install", {
    cwd: repoRootDir,
    stdio: ["inherit", process.stderr, "inherit"],
    shell: true,
  });
  if (status !== 0) {
    process.exit(status ?? 1);
  }
}
