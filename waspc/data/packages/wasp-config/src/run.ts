#!/usr/bin/env node

import { writeFileSync } from "fs";
import { pathToFileURL } from "url";
import { parseProcessArgsOrThrow } from "./cli.js";
import { analyzeApp } from "./legacy/appAnalyzer.js";
import { compileWaspTsToJs } from "./spec-pipeline/compile.js";

main(process.argv).catch((error: unknown) => {
  console.error(error instanceof Error ? error.message : error);
  process.exitCode = 1;
});

/**
 * Main function that processes command line arguments, analyzes the user app,
 * and writes the output to a file.
 */
async function main(args: string[]): Promise<void> {
  if (args[2] === "compile") {
    compileSpecFile(args);
    return;
  }

  const { waspTsSpecPath, outputFilePath, entityNames } =
    parseProcessArgsOrThrow(args);

  const declsResult = await analyzeApp(
    pathToFileURL(waspTsSpecPath).href,
    entityNames,
  );

  if (declsResult.status === "error") {
    throw new Error(declsResult.error);
  }

  writeFileSync(outputFilePath, JSON.stringify(declsResult.value));
}

function compileSpecFile(args: string[]): void {
  if (args.length !== 6) {
    throw new Error(
      "Usage: node run.js compile <input file> <tsconfig file> <output js file>",
    );
  }

  const [_node, _runJs, _command, inputPath, tsconfigPath, outputPath] = args;
  if (
    typeof inputPath !== "string" ||
    typeof tsconfigPath !== "string" ||
    typeof outputPath !== "string"
  ) {
    throw new Error(
      "Compile input, tsconfig, and output paths must be strings.",
    );
  }

  compileWaspTsToJs({ inputPath, tsconfigPath, outputPath });
}
