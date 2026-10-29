/// <reference types="node" />
// Helper to test the packages/libs/* packages locally and in CI.

import { installWorkspaceDependencies } from "../utils.ts";
import { runScriptInEveryLib } from "./utils.ts";

testLibs();

function testLibs(): void {
  installWorkspaceDependencies();

  console.log("Testing libs");
  runScriptInEveryLib("test");
}
