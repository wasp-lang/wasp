#!/usr/bin/env node

import { writeFileSync } from "fs";
import { pathToFileURL } from "url";
import { parseProcessArgsOrThrow } from "./cli.js";
import { analyzeApp } from "./spec/appAnalyzer.js";
import { compileWaspTsFileToJsFile } from "./spec-pipeline/compile/index.js";

main(process.argv).catch((error: unknown) => {
  console.error(error instanceof Error ? error.message : error);
  process.exitCode = 1;
});

/**
 * Main function that processes command line arguments, analyzes the user app,
 * and writes the output to a file.
 */
async function main(args: string[]): Promise<void> {
  const {
    waspTsSpecPath,
    tsconfigPath,
    compiledWaspTsSpecPath,
    declsJsonPath,
    entityNames,
  } = parseProcessArgsOrThrow(args);

  await compileWaspTsFileToJsFile({
    inputPath: waspTsSpecPath,
    tsconfigPath,
    outputPath: compiledWaspTsSpecPath,
  });

  const declsResult = await analyzeApp(
    pathToFileURL(compiledWaspTsSpecPath).href,
    entityNames,
  );

  if (declsResult.status === "error") {
    throw new Error(declsResult.error);
  }

  writeFileSync(declsJsonPath, JSON.stringify(declsResult.value));
}
