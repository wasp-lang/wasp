#!/usr/bin/env node

import { writeFileSync } from "fs";
import { parseProcessArgsOrThrow } from "./cli.js";
import { analyzeApp } from "./legacy/appAnalyzer.js";

main(process.argv);

/**
 * Main function that processes command line arguments, analyzes the user app,
 * and writes the output to a file.
 */
async function main(args: string[]): Promise<void> {
  const { waspTsSpecPath, outputFilePath, entityNames } =
    parseProcessArgsOrThrow(args);

  const declsResult = await analyzeApp(waspTsSpecPath, entityNames);

  if (declsResult.status === "error") {
    console.error(declsResult.error);
    process.exit(1);
  }

  writeFileSync(outputFilePath, JSON.stringify(declsResult.value));
}
