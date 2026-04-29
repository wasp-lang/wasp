/// <reference types="node" />
// Helper to test the waspc/libs/* packages locally and in CI.

import { discoverSubDirs, getPackageJson, runCmd } from "../utils.ts";
import { getDataLibsDirPath } from "./utils.ts";

const dataLibsDirPath = getDataLibsDirPath();

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
