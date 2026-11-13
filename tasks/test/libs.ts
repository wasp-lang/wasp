#!/usr/bin/env node
//MISE description="Run tests on Wasp libs (types, unit, type-exports)."
/// <reference types="node" />

import { runScriptInEveryLib } from "../../waspc/tools/libs/utils.ts";

testLibs();

function testLibs(): void {
  // The libs depend on each other's `dist`.
  console.log("Building libs");
  runScriptInEveryLib("build");

  console.log("Testing libs");
  runScriptInEveryLib("test");
}
