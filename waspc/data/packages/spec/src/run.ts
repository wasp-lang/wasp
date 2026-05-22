#!/usr/bin/env node

import { writeFileSync } from "fs";
import { Decl } from "./appSpec.js";
import { parseProcessArgsOrThrow } from "./cli.js";
import { analyzeApp } from "./spec/appAnalyzer.js";
import { SpecUserError } from "./spec/specUserError.js";

await main(process.argv);

async function main(args: string[]): Promise<void> {
  const {
    waspTsSpecPath,
    tsconfigPath,
    projectRootDir,
    specResultPath,
    entityNames,
  } = parseProcessArgsOrThrow(args);

  const result = await analyze({
    waspTsSpecPath,
    tsconfigPath,
    projectRootDir,
    entityNames,
  });

  writeFileSync(specResultPath, JSON.stringify(result));
}

async function analyze(args: {
  waspTsSpecPath: string;
  tsconfigPath: string;
  projectRootDir: string;
  entityNames: string[];
}): Promise<SpecResult> {
  try {
    const decls = await analyzeApp(args);
    return { status: "ok", value: decls };
  } catch (error) {
    if (error instanceof SpecUserError) {
      return { status: "error", error: error.message };
    }
    throw error;
  }
}

/**
 * Result handed off to waspc through the spec result JSON file.
 *
 * IMPORTANT: This shape is part of the protocol with waspc. The Haskell side
 * decodes it in `Wasp.Project.WaspFile.TypeScript`. Keep them in sync.
 */
type SpecResult =
  | { status: "ok"; value: Decl[] }
  | { status: "error"; error: string };
