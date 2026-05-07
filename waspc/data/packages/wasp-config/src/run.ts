#!/usr/bin/env node

import { readFileSync, realpathSync, writeFileSync } from "fs";
import { fileURLToPath, pathToFileURL } from "url";
import { parseProcessArgsOrThrow } from "./cli.js";
import { analyzeApp } from "./legacy/appAnalyzer.js";
import { compileWaspTsToJs } from "./spec-pipeline/compile.js";
import { rewrite } from "./spec-pipeline/rewrite.js";

if (isDirectRun(process.argv)) {
  main(process.argv).catch((error: unknown) => {
    console.error(error instanceof Error ? error.message : error);
    process.exit(1);
  });
}

function isDirectRun(args: string[]): boolean {
  const entrypointPath = args[1];
  if (!entrypointPath) return false;

  try {
    return (
      realpathSync(fileURLToPath(import.meta.url)) ===
      realpathSync(entrypointPath)
    );
  } catch {
    return false;
  }
}

/**
 * Main function that processes command line arguments, analyzes the user app,
 * and writes the output to a file.
 */
export async function main(args: string[]): Promise<void> {
  if (args[2] === "rewrite") {
    rewriteSpecFile(args);
    return;
  }

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
    console.error(declsResult.error);
    process.exit(1);
  }

  writeFileSync(outputFilePath, JSON.stringify(declsResult.value));
}

function rewriteSpecFile(args: string[]): void {
  if (args.length !== 5) {
    throw new Error("Usage: node run.js rewrite <input file> <output file>");
  }

  const [_node, _runJs, _command, inputPath, outputPath] = args;
  if (typeof inputPath !== "string" || typeof outputPath !== "string") {
    throw new Error("Rewrite input and output paths must be strings.");
  }

  const sourceText = readFileSync(inputPath, "utf8");
  const rewritten = rewrite(sourceText);
  writeFileSync(outputPath, rewritten, "utf8");
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
