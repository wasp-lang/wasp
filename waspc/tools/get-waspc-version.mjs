// @ts-check

// Gets the waspc version from the Cabal package.

import { spawnSync } from "node:child_process";

const waspcDir = new URL("..", import.meta.url).pathname;

const result = spawnSync("cabal", ["repl", "waspc", "-v0"], {
  cwd: waspcDir,
  encoding: "utf-8",
  input: "putStrLn $ Data.Version.showVersion Paths_waspc.version\n",
});

if (result.error) {
  throw result.error;
}

console.log(result.stdout.trim());
