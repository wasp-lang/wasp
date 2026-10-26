/// <reference types="node" />
// Checks that the package in the current directory has the same version as
// waspc. Used by the libs' and packages' `build` mise tasks.

import { assertPackageVersionMatchesWaspc, getPackageJson } from "./utils.ts";

try {
  const { name, version } = getPackageJson(process.cwd());
  assertPackageVersionMatchesWaspc(name, version);
} catch (e) {
  console.error(`ERROR: ${e instanceof Error ? e.message : String(e)}`);
  process.exitCode = 1;
}
