#!/usr/bin/env node

import { writeFileSync } from "fs";
import { parseProcessArgsOrThrow } from "./cli.js";
import { analyzeApp } from "./spec/appAnalyzer.js";

main(process.argv).catch((error: unknown) => {
  console.error(error instanceof Error ? error.message : error);
  process.exitCode = 1;
});

/**
 * Main function that processes command line arguments, analyzes the user app,
 * and writes the output to a file.
 */
async function main(args: string[]): Promise<void> {
  const { waspTsSpecPath, tsconfigPath, declsJsonPath, entityNames } =
    parseProcessArgsOrThrow(args);

  const declsResult = await analyzeApp({
    waspTsSpecPath,
    tsconfigPath,
    entityNames,
  });

  if (declsResult.status === "error") {
    throw new Error(declsResult.error);
  }

  writeFileSync(declsJsonPath, JSON.stringify(declsResult.value));
}
