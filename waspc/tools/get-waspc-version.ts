/// <reference types="node" />
// Gets the waspc version from the Cabal package file.

import { readFileSync } from "node:fs";
import { join } from "node:path";
import { getWaspcDirPath } from "./utils.ts";

let waspcVersion: string | undefined;

/**
 * Matches the `.cabal` files version line and
 * captures the version value with a capture group.
 */
export const cabalVersionRegex = /^version:\s*(.+)$/m;

export function getWaspcVersion() {
  if (waspcVersion !== undefined) return waspcVersion;

  const waspcDir = getWaspcDirPath();
  const cabalFile = readFileSync(join(waspcDir, "waspc.cabal"), "utf-8");

  // NOTE: It would be more precise to use something like:
  // `cabal repl waspc -v0 <<< 'putStrLn $ Data.Version.showVersion Paths_waspc.version'`
  // but running `cabal repl` was failing on Windows in the CI with the following error:
  // https://github.com/wasp-lang/wasp/pull/3648#discussion_r2717163122
  // We should investigate in the future if we can make it work again.
  const match = cabalFile.match(cabalVersionRegex);
  if (!match) {
    throw new Error("Could not find version in waspc.cabal");
  }

  waspcVersion = match[1].trim();
  return waspcVersion;
}

const isMain = import.meta.filename === process.argv[1];
if (isMain) {
  process.stdout.write(getWaspcVersion());
}
