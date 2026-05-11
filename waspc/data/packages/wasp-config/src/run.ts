#!/usr/bin/env node

import { writeFileSync } from "fs";
import { basename, dirname, join } from "path";
import { pathToFileURL } from "url";
import { parseProcessArgsOrThrow } from "./cli.js";
import { analyzeApp } from "./legacy/appAnalyzer.js";
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
  const { waspTsSpecPath, tsconfigPath, outputFilePath, entityNames } =
    parseProcessArgsOrThrow(args);
  const compiledWaspJsPath = getCompiledWaspJsPath({
    waspTsSpecPath,
    outputFilePath,
  });

  await compileWaspTsFileToJsFile({
    inputPath: waspTsSpecPath,
    tsconfigPath,
    outputPath: compiledWaspJsPath,
  });

  const declsResult = await analyzeApp(
    pathToFileURL(compiledWaspJsPath).href,
    entityNames,
  );

  if (declsResult.status === "error") {
    throw new Error(declsResult.error);
  }

  writeFileSync(outputFilePath, JSON.stringify(declsResult.value));
}

function getCompiledWaspJsPath({
  waspTsSpecPath,
  outputFilePath,
}: {
  waspTsSpecPath: string;
  outputFilePath: string;
}): string {
  return join(
    dirname(outputFilePath),
    basename(waspTsSpecPath).replace(/\.ts$/, ".js"),
  );
}
