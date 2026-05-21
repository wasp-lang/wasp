export function parseProcessArgsOrThrow(args: string[]): {
  waspTsSpecPath: string;
  tsconfigPath: string;
  specResultPath: string;
  entityNames: string[];
} {
  if (args.length !== 7 || args[2] !== "analyze") {
    throw new Error(
      "Usage: node run.js analyze <path to main.wasp.ts> <path to tsconfig.wasp.json> <path to spec result JSON file> <entity names json>",
    );
  }

  const [
    _node,
    _runjs,
    _command,
    waspTsSpecPath,
    tsconfigPath,
    specResultPath,
    entityNamesJson,
  ] = args;
  if (
    typeof waspTsSpecPath !== "string" ||
    typeof tsconfigPath !== "string" ||
    typeof specResultPath !== "string" ||
    typeof entityNamesJson !== "string"
  ) {
    throw new Error(
      "All arguments must be strings: <path to main.wasp.ts> <path to tsconfig.wasp.json> <path to spec result JSON file> <entity names json>",
    );
  }

  const entityNames = getValidEntityNamesOrThrow(entityNamesJson);

  return {
    waspTsSpecPath,
    tsconfigPath,
    specResultPath,
    entityNames,
  };
}

function getValidEntityNamesOrThrow(entityNamesJson: string): string[] {
  const entities = JSON.parse(entityNamesJson);
  if (!Array.isArray(entities)) {
    throw new Error("The entity names JSON must be an array of entity names.");
  }
  return entities;
}
