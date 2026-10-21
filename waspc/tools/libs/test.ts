/// <reference types="node" />
// Helper to test the waspc/libs/* packages locally and in CI.

import { getPackageJson, runCmd } from "../utils.ts";
import {
  discoverLibDirsInDependencyOrder,
  getDataLibsDirPath,
} from "./utils.ts";

testLibs();

function testLibs(): void {
  const dataLibsDirPath = getDataLibsDirPath();
  const libDirs = discoverLibDirsInDependencyOrder(dataLibsDirPath);

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
