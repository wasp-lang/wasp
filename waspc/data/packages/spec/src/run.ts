#!/usr/bin/env node

import { writeFileSync } from "fs";
import { parseProcessArgsOrThrow } from "./cli.js";
import { analyzeApp } from "./spec/appAnalyzer.js";
import { SpecUserError } from "./spec/specUserError.js";

try {
  await main(process.argv);
} catch (error) {
  if (error instanceof SpecUserError) {
    console.error(error.message);
    process.exitCode = 1;
  } else {
    throw error;
  }
}

/**
 * Main function that processes command line arguments, analyzes the user app,
 * and writes the output to a file.
 */
async function main(args: string[]): Promise<void> {
  const { waspTsSpecPath, tsconfigPath, declsJsonPath, entityNames } =
    parseProcessArgsOrThrow(args);

  const decls = await analyzeApp({
    waspTsSpecPath,
    tsconfigPath,
    entityNames,
  });

  writeFileSync(declsJsonPath, JSON.stringify(decls));
}
