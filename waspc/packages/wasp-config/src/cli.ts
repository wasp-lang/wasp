export function parseProcessArgsOrThrow(args: string[]): {
  waspTsSpecPath: string;
  outputFilePath: string;
  entityNames: string[];
} {
  if (args.length !== 5) {
    throw new Error(
      "Usage: node run.js <path to main.wasp.js> <path to output file> <entity names json>",
    );
  }

  const [_node, _runjs, waspTsSpecPath, outputFilePath, entityNamesJson] = args;
  if (
    typeof waspTsSpecPath !== "string" ||
    typeof outputFilePath !== "string" ||
    typeof entityNamesJson !== "string"
  ) {
    throw new Error(
      "All arguments must be strings: <path to main.wasp.js> <path to output file> <entity names json>",
    );
  }

  const entityNames = getValidEntityNamesOrThrow(entityNamesJson);

  return {
    waspTsSpecPath,
    outputFilePath,
    entityNames,
  };
}

function getValidEntityNamesOrThrow(entitiesJson: string): string[] {
  const entities = JSON.parse(entitiesJson);
  if (!Array.isArray(entities)) {
    throw new Error("The entities JSON must be an array of entity names.");
  }
  return entities;
}
