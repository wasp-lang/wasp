/// <reference types="node" />
// Helper to test the waspc/libs/* packages locally and in CI.

import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { discoverLibDirs, getLibPackageJson, runCmd } from "./lib-utils.ts";

const waspcDirPath = fileURLToPath(new URL("../..", import.meta.url));
const dataLibsDirPath = join(waspcDirPath, "data", "Generator", "libs");

testLibs();

function testLibs(): void {
  const libDirs = discoverLibDirs(dataLibsDirPath);

  for (const libDir of libDirs) {
    testLib(libDir);
  }
}

function testLib(libDir: string): void {
  const { name: libName } = getLibPackageJson(libDir);

  console.log(`Testing ${libName} lib (${libDir})`);

  runCmd("npm", ["install"], { cwd: libDir, stdio: "inherit" });
  runCmd("npm", ["run", "test"], { cwd: libDir, stdio: "inherit" });
}
