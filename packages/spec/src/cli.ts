import { Command } from "commander";

export interface AnalyzeArgs {
  waspTsSpecPath: string;
  tsconfigPath: string;
  projectRootDir: string;
  specResultPath: string;
  entityNames: string[];
}

/**
 * The CLI that waspc runs (see `Wasp.Project.WaspFile.TypeScript`). Commander
 * finds the arguments whether Node runs this file directly or through
 * `node --eval`.
 */
export function makeCli(
  analyze: (args: AnalyzeArgs) => Promise<void>,
): Command {
  const program = new Command();
  program
    .command("analyze")
    .argument("<waspTsSpecPath>", "path to main.wasp.ts")
    .argument("<tsconfigPath>", "path to tsconfig.wasp.json")
    .argument("<projectRootDir>", "project root dir")
    .argument("<specResultPath>", "path to the spec result JSON file")
    .argument("<entityNamesJson>", "entity names, as a JSON array")
    .action(
      (
        waspTsSpecPath: string,
        tsconfigPath: string,
        projectRootDir: string,
        specResultPath: string,
        entityNamesJson: string,
      ) =>
        analyze({
          waspTsSpecPath,
          tsconfigPath,
          projectRootDir,
          specResultPath,
          entityNames: getValidEntityNamesOrThrow(entityNamesJson),
        }),
    );
  return program;
}

function getValidEntityNamesOrThrow(entityNamesJson: string): string[] {
  const entities = JSON.parse(entityNamesJson);
  if (!Array.isArray(entities)) {
    throw new Error("The entity names JSON must be an array of entity names.");
  }
  return entities;
}
