/// <reference types="node" />
// Gets the waspc version from the Cabal package file.

import { readFileSync } from "node:fs";
import { join } from "node:path";
import { getWaspcDirPath } from "./utils.ts";

const waspcDir = getWaspcDirPath();
const cabalFile = readFileSync(join(waspcDir, "waspc.cabal"), "utf-8");

// NOTE: It would be more precise to use something like:
// `cabal repl waspc -v0 <<< 'putStrLn $ Data.Version.showVersion Paths_waspc.version'`
// but running `cabal repl` was failing on Windows in the CI with the following error:
// https://github.com/wasp-lang/wasp/pull/3648#discussion_r2717163122
// We should investigate in the future if we can make it work again.
const match = cabalFile.match(/^version:\s*(.+)$/m);
if (!match) {
  throw new Error("Could not find version in waspc.cabal");
}

process.stdout.write(match[1].trim());
