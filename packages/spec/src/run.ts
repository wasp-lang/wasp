import { writeFileSync } from "fs";
import { Decl } from "./appSpec.js";
import { makeCli } from "./cli.js";
import { analyzeApp } from "./spec/appAnalyzer.js";
import { WaspSpecUserError } from "./spec/waspSpecUserError.js";

await makeCli(async ({ specResultPath, ...analyzeArgs }) => {
  const result = await analyze(analyzeArgs);
  writeFileSync(specResultPath, JSON.stringify(result));
}).parseAsync();

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
    if (error instanceof WaspSpecUserError) {
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
