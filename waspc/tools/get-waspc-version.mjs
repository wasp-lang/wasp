// @ts-check

// Gets the waspc version from the Cabal package.

import { execSync } from "node:child_process";

const waspcDir = new URL("..", import.meta.url).pathname;

const version = execSync(
  `echo 'putStrLn $ Data.Version.showVersion Paths_waspc.version' | cabal repl waspc -v0`,
  { cwd: waspcDir, encoding: "utf-8" },
).trim();

console.log(version);
