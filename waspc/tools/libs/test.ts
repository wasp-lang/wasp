/// <reference types="node" />
// Helper to test the packages/libs/* packages locally and in CI.

import { installWorkspaceDependencies, runCmd } from "../utils.ts";
import { getLibsDirPath } from "./utils.ts";

testLibs();

function testLibs(): void {
  installWorkspaceDependencies();

  console.log("Testing libs");
  runCmd("pnpm", ["--filter", "'./packages/libs/*'", "run", "test"], {
    cwd: getLibsDirPath(),
    stdio: "inherit",
  });
}
