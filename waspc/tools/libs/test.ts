/// <reference types="node" />
// Helper to test the packages/libs/* packages locally and in CI.

import { runScriptInEveryLib } from "./utils.ts";

testLibs();

function testLibs(): void {
  // The libs depend on each other's `dist`, and `prepare` no longer builds it.
  console.log("Building libs");
  runScriptInEveryLib("build");

  console.log("Testing libs");
  runScriptInEveryLib("test");
}
