/// <reference types="node" />
// Helper to test the waspc/libs/* packages locally and in CI.

import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { discoverSubDirs, getPackageJson, runCmd } from "../utils.ts";

const waspcDirPath = fileURLToPath(new URL("../..", import.meta.url));
const dataLibsDirPath = join(waspcDirPath, "data", "Generator", "libs");

testLibs();

function testLibs(): void {
  const libDirs = discoverSubDirs(dataLibsDirPath);

  for (const libDir of libDirs) {
    testLib(libDir);
  }
}

function testLib(libDir: string): void {
  const { name: libName } = getPackageJson(libDir);

  console.log(`Testing ${libName} lib (${libDir})`);

  runCmd("npm", ["install"], { cwd: libDir, stdio: "inherit" });
  runCmd("npm", ["run", "test"], { cwd: libDir, stdio: "inherit" });
}
