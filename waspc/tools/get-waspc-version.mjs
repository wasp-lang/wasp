// @ts-check

// Gets the waspc version from the Cabal package.

import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const waspcDir = fileURLToPath(new URL("..", import.meta.url));

const result = spawnSync("cabal", ["repl", "waspc", "-v0"], {
  cwd: waspcDir,
  encoding: "utf-8",
  shell: true,
  input: "putStrLn $ Data.Version.showVersion Paths_waspc.version\n",
});

if (result.error) {
  throw result.error;
}

console.log(result.stdout.trim());
